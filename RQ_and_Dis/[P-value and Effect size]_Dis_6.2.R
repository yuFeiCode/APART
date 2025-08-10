# 2024-10-22 Why does APART Perform Well in Warning Prioritization?

library(tidyverse)
library(gridExtra)
library(lattice)
library(ModelMetrics)
library(caret)
library(reshape2)
library(car)
library(carData)
library(pROC)
library(effsize)
library(ScottKnottESD)
library(dplyr)
library(tibble)
library(stringr)
library(grid)
library(effsize)
library(ggpubr)
library(rcompanion)

PMD.result.dir = '../SATs/PMD/'
CheckStyle.result.dir = '../SATs/CheckStyle/'
ErrorProne.result.dir = '../SATs/Errorprone/'
Spotbugs.result.dir = '../SATs/Spotbugs/'

betterscan_ce.result.dir = '../SATs/betterscan-ce/'
codacy.result.dir = '../SATs/codacy/'
codeql.result.dir = '../SATs/codeql/'
sonarqube.result.dir = '../SATs/sonarqube/'


dis.save.fig.dir = './Dis_6.2_figures/'

preprocess <- function(x, reverse){
  colnames(x) <- c("variable","value")
  tmp <- do.call(cbind, split(x, x$variable))
  tmp <- tmp[, grep("value", names(tmp))]
  names(tmp) <- gsub(".value", "", names(tmp))
  df <- tmp
  ranking <- NULL
  
  if(reverse == TRUE)
  { 
    ranking <- (max(sk_esd(df)$group)-sk_esd(df)$group) +1 
  }
  else
  { 
    ranking <- sk_esd(df)$group 
  }
  
  x$rank <- paste("Rank",ranking[as.character(gsub("-", ".", x$variable))])
  return(x)
}

prediction_dir = '../Baseline-result/DeepLineDP/within-release/'

all_files = list.files(prediction_dir)

df_all <- NULL

for(f in all_files)
{
  df <- read.csv(paste0(prediction_dir, f))
  df_all <- rbind(df_all, df)
}
##############
line.ground.truth = select(df_all,  project, train, test, filename, file.level.ground.truth, prediction.prob, line.number, line.level.ground.truth, is.comment.line)
line.ground.truth = filter(line.ground.truth, is.comment.line== "False")  
line.ground.truth = distinct(line.ground.truth)



CEandNFCdir = "../Baseline-result/GLANCE_and_LineDP/Glance_MD_full_threshold/line_result/test/"

all_CEandNF_files = list.files(CEandNFCdir)

lineLevelMetrics <- NULL

for(f in all_CEandNF_files)
{
  df <- read.csv(paste0(CEandNFCdir, f))
  df$test = str_split_fixed(f, "-result", 2)[,1]
  lineLevelMetrics  <- rbind(lineLevelMetrics, df)
}

lineLevelMetrics = select(lineLevelMetrics, "predicted_buggy_lines", "predicted_buggy_line_numbers","predicted_buggy_score", "rank", "functioncall", "controlelements", "numbertokens", "test")
names(lineLevelMetrics) = c("filename", "line.number", "GLANCEscore", "rank", "NFC", "CE", "NT", "test")
lineLevelMetrics$filename = str_split_fixed(lineLevelMetrics$filename, ":", 2)[,1]
lineLevelMetrics$filename <- gsub("/", "_", lineLevelMetrics$filename)



normalize <- function(x) {
  min_x <- min(x)
  max_x <- max(x)
  if (max_x == min_x) {
    return(rep(0, length(x)))
  }
  return((x - min_x) / (max_x - min_x))
}


calculate_auc <- function(response, predictor) {

  if (length(unique(predictor)) == 1) {
    return(0.5)  
  }
  
  if (length(unique(response)) == 2) {

    return(as.numeric(auc(roc(response, predictor))))
  } else {

    return(as.numeric(multiclass.roc(response, predictor)$auc))
  }
}

get.SAT.result.only.for.actionable.warning = function(all_eval_releases, SAT.result.dir, SATname, line.ground.truth, lineLevelMetrics, save.fig.dir,IFA_y_limit, FPavg_y_limit)
{
  all.release.result = NULL
  
  ## get result from baseline
  for(rel in all_eval_releases)
  { 
    if (SATname == "PMD" || SATname == "ErrorProne" ) {
      allSATresult = read.csv(paste0(SAT.result.dir, rel, '-line-lvl-result.txt'), quote = "")
      if (SATname != "ErrorProne") {
        allSATresult$filename <- gsub("/", "_", allSATresult$filename)
      }
    } else {
      allSATresult = read.csv(paste0(SAT.result.dir, rel, '-result.csv'))
    }
    
    
    cur.df.file = filter(line.ground.truth, test==rel)
    cur.df.file = select(cur.df.file, filename, line.number, line.level.ground.truth)
    cur.df.file$filename <- gsub("/", "_", cur.df.file$filename)
    

    # PMD
    if (SATname != "CheckStyle" ){
      names(allSATresult) = c('filename','test.release','line_number', 'SAT_prediction_result', 'Priority')
      allSATresult$SAT_prediction_result <- ifelse(allSATresult$SAT_prediction_result %in% c("False", "FALSE"), 0, 1)
      allSATresult = allSATresult %>% filter(SAT_prediction_result == 1)    
    
    if (SATname == "CheckStyle"){
      allSATresult = select(allSATresult, filename, line, priority)
      names(allSATresult) = c('filename', 'line_number', 'Priority')
    }
    

    SAT.result = allSATresult %>% group_by(filename) %>% arrange(Priority, line_number, .by_group = TRUE) %>% mutate(rank = row_number())
    SAT.result = select(SAT.result,'filename','line_number','rank')
    names(SAT.result) = c('filename','line.number','rank')
    

    SAT.base.info = select(allSATresult, filename, line_number, Priority)
    names(SAT.base.info) = c('filename','line.number','Priority')
    
    

    SAT_F.result = SAT.base.info

    GLANCE_F = lineLevelMetrics %>% filter(test == rel) %>% group_by(filename) %>% mutate(severity = NFC*NT) %>% mutate(severity_normalized = normalize(severity))
    
    GLANCE_F = select(GLANCE_F, filename, line.number, severity_normalized)

    SAT_F.result = left_join(SAT_F.result, GLANCE_F, by=c('filename', 'line.number')) %>% mutate(
      severity_normalized = replace_na(severity_normalized, 0))
    
    SAT_F.result = left_join(SAT_F.result, cur.df.file, by = c("filename","line.number"))
    SAT_F.result = filter(SAT_F.result, !is.na(line.level.ground.truth))
  

    filtered.SAT_F.result <- SAT_F.result %>% group_by(filename) %>% filter(n_distinct(line.level.ground.truth) == 2) %>% ungroup()
    

    No_nor <- filtered.SAT_F.result %>% group_by(filename) %>% summarize(
        gt_levels = n_distinct(line.level.ground.truth),
        SAT_levels = n_distinct(Priority),
        SAT_F_levels = n_distinct(severity_normalized),
        auc_SAT = tryCatch(
          suppressMessages(calculate_auc(line.level.ground.truth, Priority)),
          error = function(e) NA_real_
        ),
        auc_SAT_F = tryCatch(
          suppressMessages(calculate_auc(line.level.ground.truth, 1000*Priority - severity_normalized)),
          error = function(e) NA_real_
        )
      ) %>%
      ungroup() %>% 
      mutate(test = rel)

    No_nor = No_nor %>% mutate(somersD_SAT = 2*(auc_SAT -0.5)) %>% mutate(somersD_SAT_F = 2*(auc_SAT_F -0.5))

    all.release.result = rbind(all.release.result, No_nor)
    
    print(paste0('finished ', rel))
  }
  
  sum.no.nor.result = all.release.result %>% summarise(somersD_SAT = mean(somersD_SAT), somersD_SAT_F = mean(somersD_SAT_F), .by=test)

  print('#####################[ Release-level ]########################')
  print('[ Release-level ] P-value:')
  print(wilcox.test(sum.no.nor.result$somersD_SAT_F, sum.no.nor.result$somersD_SAT,  paired = TRUE))
  print('[ Release-level ] Effect size:')
  z = wilcoxonZ(sum.no.nor.result$somersD_SAT_F, sum.no.nor.result$somersD_SAT,  paired = TRUE )
  print(z /sqrt(nrow(sum.no.nor.result)))
  
  

}


all_eval_releases = c('activemq-5.2.0','activemq-5.3.0','activemq-5.8.0',
                      'camel-2.10.0','camel-2.11.0', 
                      'derby-10.5.1.1',
                      'groovy-1_6_BETA_2', 
                      'hbase-0.95.2',
                      'hive-0.12.0', 
                      'jruby-1.5.0','jruby-1.7.0.preview1',
                      'lucene-3.0.0','lucene-3.1','wicket-1.5.3')


nor.total.result = NULL

get.SAT.result.only.for.actionable.warning(all_eval_releases, PMD.result.dir, "PMD", line.ground.truth, lineLevelMetrics, save.fig.dir, 20, 15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, CheckStyle.result.dir, "CheckStyle", line.ground.truth, lineLevelMetrics, save.fig.dir, 60, 75) 
get.SAT.result.only.for.actionable.warning(all_eval_releases, ErrorProne.result.dir, "ErrorProne", line.ground.truth, lineLevelMetrics, save.fig.dir, 30, 30)
get.SAT.result.only.for.actionable.warning(all_eval_releases, Spotbugs.result.dir, "Spotbugs", line.ground.truth, lineLevelMetrics, save.fig.dir, 10, 20)
get.SAT.result.only.for.actionable.warning(all_eval_releases, betterscan_ce.result.dir, "Betterscan-ce", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, codacy.result.dir, "Codacy", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, codeql.result.dir, "Codeql", line.ground.truth, lineLevelMetrics, save.fig.dir,20,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, sonarqube.result.dir, "Sonarqube", line.ground.truth, lineLevelMetrics, save.fig.dir,15,20)


