
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

PMD.result.dir = '../SATs/PMD/'
CheckStyle.result.dir = '../SATs/CheckStyle/'
ErrorProne.result.dir = '../SATs/Errorprone/'
Spotbugs.result.dir = '../SATs/Spotbugs/'

betterscan_ce.result.dir = '../SATs/betterscan-ce/'
codacy.result.dir = '../SATs/codacy/'
codeql.result.dir = '../SATs/codeql/'
sonarqube.result.dir = '../SATs/sonarqube/'

# unzip Baseline-result/LLM4SA/test SATs result
LLM_PMD.result.dir = '../Baseline-result/LLM4SA/test/PMD/'
LLM_CheckStyle.result.dir = '../Baseline-result/LLM4SA/test/CheckStyle/'
LLM_ErrorProne.result.dir = '../Baseline-result/LLM4SA/test/Errorprone/'
LLM_Spotbugs.result.dir = '../Baseline-result/LLM4SA/test/Spotbugs/'
LLM_Codacy.result.dir = '../Baseline-result/LLM4SA/test/Codacy/'
LLM_Betterscan_ce.result.dir = '../Baseline-result/LLM4SA/test/Betterscan-ce/'
LLM_Codeql.result.dir = '../Baseline-result/LLM4SA/test/Codeql/'
LLM_Sonarqube.result.dir = '../Baseline-result/LLM4SA/test/Sonarqube/'

RQ3.save.fig.dir = './RQ3_figure/'


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

get.top.k.tokens = function(df, k)
{
  top.k <- df %>% filter( is.comment.line=="False"  & file.level.ground.truth=="True" & prediction.label=="True" ) %>%
    group_by(test, filename) %>% top_n(k, token.attention.score) %>% select("project","train","test","filename","token") %>% distinct()
  
  top.k$flag = 'topk'
  
  return(top.k)
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

all_eval_releases = c('activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0',
                      'camel-2.10.0', 'camel-2.11.0' , 
                      'hive-0.12.0','derby-10.5.1.1' , 'groovy-1_6_BETA_2' , 'hbase-0.95.2',
                      'jruby-1.5.0', 'jruby-1.7.0.preview1',  
                      'lucene-3.0.0', 'lucene-3.1', 'wicket-1.5.3')

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



get.SAT.result.only.for.actionable.warning = function(all_eval_releases, LLM.result.dir, SAT.result.dir, SATname, line.ground.truth, lineLevelMetrics, save.fig.dir,IFA_y_limit, FPA_y_limit, Effort_y_limit)
{
  SAT.result.df = NULL 
  SAT_F.result.df = NULL
  SAT_LLM4SA.result.df = NULL

  
  final.result.df = NULL
  SAT_all_tp = 0
  SAT_F_all_tp = 0
  SAT_LLM4SA_all_tp = 0
  
  SAT_LLM4SA_all_warning = 0
  
  ## get result from baseline
  for(rel in all_eval_releases)
  { 
    if (SATname == "PMD" || SATname == "ErrorProne" ) {
      allSATresult = read.csv(paste0(SAT.result.dir, rel, '-line-lvl-result.txt'), quote = "")
      if (SATname != "ErrorProne" || SATname != 'Infer') {
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
    }
    
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
    GLANCE_F = select(lineLevelMetrics, test, filename, line.number, NT, NFC) %>% filter(test == rel)
    GLANCE_F = select(GLANCE_F, filename, line.number, NT, NFC)
    SAT_F.result = left_join(SAT_F.result, GLANCE_F, by=c('filename', 'line.number')) %>% mutate(NT = replace_na(NT, 0),NFC = replace_na(NFC, 0))
    SAT_F.result = SAT_F.result %>% group_by(filename) %>% arrange(-NFC*NT, Priority, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_F.result = select(SAT_F.result, filename, line.number, rank)
    

    LLM4SA = read.csv(paste0(LLM.result.dir,rel,'-line-lvl-result.txt'),quote="")
    names(LLM4SA) = c('filename','test','line.number','LLM_prediction_result')
    
    SAT_LLM4SA_galbol = filter(LLM4SA, LLM_prediction_result == 'real bug') %>% group_by(filename) %>% 
      mutate(LLM_R_number = sum(LLM_prediction_result == 'real bug')) %>% ungroup() %>% filter(LLM_R_number > 0)
    
    LLM4SA = LLM4SA %>% group_by(filename) %>% mutate(LLM_R_number = sum(LLM_prediction_result == 'real bug')) %>%
      select(filename, LLM_R_number) %>% filter(LLM_R_number > 0) %>% distinct()
    

    SAT_galbol = left_join(SAT.result, LLM4SA, by = c('filename')) %>% filter(LLM_R_number > 0) %>% group_by(filename) %>% slice(seq_len(first(LLM_R_number)))
    SAT_F_galbol = left_join(SAT_F.result, LLM4SA, by = c('filename')) %>% filter(LLM_R_number > 0) %>% group_by(filename) %>% slice(seq_len(first(LLM_R_number)))

    
    SAT.result.df = merge(SAT_galbol,cur.df.file,by =c('filename', 'line.number')) %>% group_by(filename) %>% mutate(TP = sum(line.level.ground.truth == 'True'))
    SAT_F.result.df = merge(SAT_F_galbol,cur.df.file,by =c('filename', 'line.number')) %>% group_by(filename) %>% mutate(TP = sum(line.level.ground.truth == 'True'))
    SAT_LLM4SA.result.df = merge(SAT_LLM4SA_galbol,cur.df.file,by =c('filename', 'line.number')) %>% group_by(filename) %>% mutate(TP = sum(line.level.ground.truth == 'True'))


    SAT.result.df = SAT.result.df %>% select(filename, TP) %>% distinct() 
    SAT_F.result.df = SAT_F.result.df %>% select(filename, TP) %>% distinct()
    SAT_LLM4SA.result.df = SAT_LLM4SA.result.df %>% select(filename, TP) %>% distinct() 

    SAT_total_TP = sum(SAT.result.df$TP)
    SAT_F_total_TP = sum(SAT_F.result.df$TP)
    SAT_LLM4SA_total_TP = sum(SAT_LLM4SA.result.df$TP)

    
    SAT_LLM4SA_total_warning = sum(LLM4SA$LLM_R_number)
    

    SAT_all_tp = SAT_all_tp + SAT_total_TP
    SAT_F_all_tp = SAT_F_all_tp + SAT_F_total_TP
    SAT_LLM4SA_all_tp = SAT_LLM4SA_all_tp + SAT_LLM4SA_total_TP

    
    SAT_LLM4SA_all_warning = SAT_LLM4SA_all_warning + SAT_LLM4SA_total_warning
    
    print(paste0(SATname, ': ', SAT_total_TP, ' / ', SAT_LLM4SA_total_warning))
    print(paste0(SATname, '-LLM4SA : ', SAT_LLM4SA_total_TP, ' / ', SAT_LLM4SA_total_warning))
    print(paste0(SATname, '-F : ', SAT_F_total_TP, ' / ', SAT_LLM4SA_total_warning))
    
    print(paste0('########## finished ', rel, '###########'))
    
    
 
    results_df <- data.frame(Project = character(),
                             Tool = character(),
                             PMD = character(),
                             stringsAsFactors = FALSE)
    

    results_df <- rbind(results_df, 
                       data.frame(Project = rel, 
                                  Tool = "SAT", 
                                  PMD = paste0(SAT_total_TP, " / ", SAT_LLM4SA_total_warning),
                                  stringsAsFactors = FALSE),
                       data.frame(Project = rel, 
                                  Tool = "SAT_LLM4SA", 
                                  PMD = paste0(SAT_LLM4SA_total_TP, " / ", SAT_LLM4SA_total_warning),
                                  stringsAsFactors = FALSE),

                       data.frame(Project = rel, 
                                  Tool = "SAT_F", 
                                  PMD = paste0(SAT_F_total_TP, " / ", SAT_LLM4SA_total_warning),
                                  stringsAsFactors = FALSE)
    )
    

    
    final.result.df = rbind(final.result.df ,results_df)
  }
  # write.csv(final.result.df, 'D:/Gitee-code/enhance_SATs/figures/(2024-10-19)RQ3_figures/TP对比数(update-2025-1-7).csv', row.names = FALSE)
  print(paste0('ALL-',SATname, ': ', SAT_all_tp, ' / ', SAT_LLM4SA_all_warning))
  print(paste0('ALL-',SATname, '-LLM4SA : ', SAT_LLM4SA_all_tp, ' / ', SAT_LLM4SA_all_warning))

  print(paste0('ALL-',SATname, '-F : ', SAT_F_all_tp, ' / ', SAT_LLM4SA_all_warning))
}

get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_PMD.result.dir, PMD.result.dir, "PMD", line.ground.truth, lineLevelMetrics, save.fig.dir, 20, 15, 2.5)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_CheckStyle.result.dir, CheckStyle.result.dir, "CheckStyle", line.ground.truth, lineLevelMetrics, save.fig.dir, 60, 90, 2.7) 
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_ErrorProne.result.dir, ErrorProne.result.dir, "ErrorProne", line.ground.truth, lineLevelMetrics, save.fig.dir, 30, 30, 2.7)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Spotbugs.result.dir, Spotbugs.result.dir, "Spotbugs", line.ground.truth, lineLevelMetrics, save.fig.dir, 10, 20, 2.7)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Betterscan_ce.result.dir, betterscan_ce.result.dir, "Betterscan-ce", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15, 2.8)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Codacy.result.dir, codacy.result.dir, "Codacy", line.ground.truth, lineLevelMetrics, save.fig.dir,10, 15, 2.5)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Codeql.result.dir, codeql.result.dir, "Codeql", line.ground.truth, lineLevelMetrics, save.fig.dir,10, 15, 2.5)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Sonarqube.result.dir, sonarqube.result.dir, "Sonarqube", line.ground.truth, lineLevelMetrics, save.fig.dir,15, 20, 2.7)

