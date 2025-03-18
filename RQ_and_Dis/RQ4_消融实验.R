
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

PMD.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/PMD/'
CheckStyle.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/CheckStyle/'
ErrorProne.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/Errorprone/test/'
Spotbugs.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/Spotbugs/'

betterscan_ce.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/betterscan-ce/'
codacy.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/codacy/'
codeql.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/codeql/'
sonarqube.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/sonarqube/'


linedp.result.dir = "D:/Gitee-code/Boosting deep line-level defect prediction with syntactic features/all_models_result/MIT-LineDP-update/line_result/test/"
n.gram.result.dir = "D:/Gitee-code/Boosting deep line-level defect prediction with syntactic features/all_models_result/n_gram_result/"

RQ4.save.fig.dir = 'D:/Gitee-code/enhance_SATs/figures/(2024-10-19)RQ4_figures/'


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


prediction_dir = 'D:/Gitee-code/Boosting deep line-level defect prediction with syntactic features/all_models_result/within-release/'


all_files = list.files(prediction_dir)

df_all <- NULL

for(f in all_files)
{
  df <- read.csv(paste0(prediction_dir, f))
  df_all <- rbind(df_all, df)
}

##############
line.ground.truth = select(df_all,  project, train, test, filename, file.level.ground.truth, prediction.prob, line.number, line.level.ground.truth, is.comment.line)
line.ground.truth = filter(line.ground.truth, is.comment.line== "False")  #2024-05-17: 获取所有文件中的行级ground-truth标签
line.ground.truth = distinct(line.ground.truth)




CEandNFCdir = "D:/Gitee-code/Boosting deep line-level defect prediction with syntactic features/all_models_result/Glance_MD_full_threshold_2024_05_14_add_NT_output/line_result/test/"

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

get.line.metrics.result = function(baseline.df, cur.df.file)
{

  baseline.df.with.ground.truth = merge(baseline.df, cur.df.file, by=c("filename", "line.number"))
  

  baseline.df.with.ground.truth = baseline.df.with.ground.truth %>% group_by(filename) %>%
    mutate(actionable.warning = ifelse(any(line.level.ground.truth == 'True'), 1, 0)) %>% 
    filter(actionable.warning == 1)
  
  

  sorted = baseline.df.with.ground.truth %>% group_by(filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())

  sorted = sorted %>% filter(totalSLOC >= 5) 
  filter.files = sorted %>% select(filename) %>% distinct()


  total_true = sorted %>%  group_by(filename) %>% summarize(total_true = sum(line.level.ground.truth == "True"))
  

  total_true = total_true %>% filter(total_true > 0)

  FPavg = sorted %>% group_by(filename) %>% mutate(FPI = cumsum(line.level.ground.truth == "False"), total_truth = sum(line.level.ground.truth == "True")) %>% 
    filter(line.level.ground.truth == "True") %>% mutate(S.R = sum(FPI, na.rm = TRUE)) %>% mutate(FPavg = round( S.R / total_truth, digits = 2)) %>% 
    select(filename,FPavg) %>% distinct()
  
  FPavg = FPavg%>% arrange(filename)
  
  FPavg.list = FPavg$FPavg
  

  IFA = sorted %>% filter(line.level.ground.truth == "True") %>% group_by(filename)  %>% top_n(1, -order)

  IFA = IFA%>% arrange(filename)
  

  ifa.list = IFA$order - 1
  

  total_true = total_true%>% arrange(filename)
  

  

  fpa = sorted %>% merge(total_true) %>% group_by(filename) %>% arrange(order, .by_group=TRUE) %>% mutate(lineFPA = if_else    (line.level.ground.truth == 'True',  n()-order+1, 0 ) / (n() * total_true)) %>% summarize(FPA = sum(lineFPA) )
  fpa = fpa %>% arrange(filename)
  fpa.list = fpa$FPA
  
  top1 = sorted %>% merge(total_true) %>% group_by(filename) %>% filter(order <= 1)  %>%  summarize(top1 = sum(line.level.ground.truth == "True")/n())
  top1 = top1 %>% arrange(filename)
  top1.list = top1$top1
  
  top3 = sorted %>% merge(total_true) %>% group_by(filename) %>% filter(order <= 3)  %>%  summarize(top3 = sum(line.level.ground.truth == "True")/n())
  top3 = top3 %>% arrange(filename)
  top3.list = top3$top3
  
  top5 = sorted %>% merge(total_true) %>% group_by(filename) %>% filter(order <= 5)  %>%  summarize(top5 = sum(line.level.ground.truth == "True")/n())
  top5 = top5 %>% arrange(filename)
  top5.list = top5$top5
  
  result.df = data.frame(IFA$filename, ifa.list, fpa.list, top1.list, top3.list, top5.list, FPavg.list, IFA$totalSLOC)
  
  return(result.df)
}


add_median_row <- function(df) {

  numeric_columns <- sapply(df[, 2:(ncol(df) - 1)], is.numeric)
  

  medians <- round(apply(df[, 2:(ncol(df) - 1)][, numeric_columns], 2, median, na.rm = TRUE), 2)
  

  new_row <- rep(NA, ncol(df))
  new_row[2:(ncol(df) - 1)][numeric_columns] <- medians
  

  new_row[ncol(df)] <- tail(df[, ncol(df)], n = 1)
  

  df <- rbind(df, new_row)
  

  rownames(df)[nrow(df)] <- "Median"
  
  return(df)
}




add_mean_row <- function(df) {

  numeric_columns <- sapply(df[, 2:(ncol(df) - 1)], is.numeric)
  

  means <- apply(df[, 2:(ncol(df) - 1)][, numeric_columns], 2, mean, na.rm = TRUE)
  

  new_row <- rep(NA, ncol(df))
  new_row[2:(ncol(df) - 1)][numeric_columns] <- means

  new_row[ncol(df)] <- tail(df[, ncol(df)], n = 1)
  

  df <- rbind(df, new_row)
  

  rownames(df)[nrow(df)] <- "Mean"
  
  return(df)
}

get.SAT.result.only.for.actionable.warning = function(all_eval_releases, SAT.result.dir, SATname, line.ground.truth, lineLevelMetrics, save.fig.dir,IFA_y_limit, FPA_y_limit)
{
  SAT.result.df = NULL 
  pure_NT.result.df = NULL
  pure_NFC.result.df = NULL
  SAT_NT.result.df = NULL
  SAT_NFC.result.df = NULL
  SAT_F.result.df = NULL

  
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
    

    NFC_and_NT_info = lineLevelMetrics %>% filter(test == rel) %>% select(filename, line.number, NT, NFC)
    

    pure_NT.result = SAT.base.info
    pure_NT.result = left_join(pure_NT.result, NFC_and_NT_info, by=c('filename', 'line.number'))%>% mutate(
      NT = replace_na(NT, 0))
    pure_NT.result = pure_NT.result %>% group_by(filename) %>% arrange(-NT, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    pure_NT.result = select(pure_NT.result, filename, line.number, rank)
    
    

    pure_NFC.result = SAT.base.info
    pure_NFC.result = left_join(pure_NFC.result, NFC_and_NT_info, by=c('filename', 'line.number'))%>% mutate(
      NFC = replace_na(NFC, 0))
    pure_NFC.result = pure_NFC.result %>% group_by(filename) %>% arrange(-NFC, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    pure_NFC.result = select(pure_NFC.result, filename, line.number, rank)
    

    SAT_NT.result = SAT.base.info
    SAT_NT.result = left_join(SAT_NT.result, NFC_and_NT_info, by=c('filename', 'line.number'))%>% mutate(
      NT = replace_na(NT, 0))
    SAT_NT.result = SAT_NT.result %>% group_by(filename) %>% arrange(-NT, Priority, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_NT.result = select(SAT_NT.result, filename, line.number, rank)
    

    SAT_NFC.result = SAT.base.info
    SAT_NFC.result = left_join(SAT_NFC.result, NFC_and_NT_info, by=c('filename', 'line.number'))%>% mutate(
      NFC = replace_na(NFC, 0))
    SAT_NFC.result = SAT_NFC.result %>% group_by(filename) %>% arrange(-NFC, Priority, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_NFC.result = select(SAT_NFC.result, filename, line.number, rank)

    

    SAT_F.result = SAT.base.info
    SAT_F.result = left_join(SAT_F.result, NFC_and_NT_info, by=c('filename', 'line.number')) %>% mutate(
      NT = replace_na(NT, 0),
      NFC = replace_na(NFC, 0))
    SAT_F.result = SAT_F.result %>% group_by(filename) %>% arrange(-NFC*NT, Priority, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_F.result = select(SAT_F.result, filename, line.number, rank)
    

    SAT.eval.result = get.line.metrics.result(SAT.result, cur.df.file) %>% mutate(test=rel)
    pure_NT.eval.result = get.line.metrics.result(pure_NT.result, cur.df.file) %>% mutate(test=rel)
    pure_NFC.eval.result = get.line.metrics.result(pure_NFC.result, cur.df.file) %>% mutate(test=rel)
    SAT_NT.eval.result = get.line.metrics.result(SAT_NT.result, cur.df.file) %>% mutate(test=rel)
    SAT_NFC.eval.result = get.line.metrics.result(SAT_NFC.result, cur.df.file) %>% mutate(test=rel)
    SAT_F.eval.result = get.line.metrics.result(SAT_F.result, cur.df.file) %>% mutate(test=rel)

    
    
    SAT.result.df = rbind(SAT.result.df, SAT.eval.result)
    pure_NT.result.df = rbind(pure_NT.result.df, pure_NT.eval.result)
    pure_NFC.result.df = rbind(pure_NFC.result.df, pure_NFC.eval.result)
    SAT_NT.result.df = rbind(SAT_NT.result.df, SAT_NT.eval.result)
    SAT_NFC.result.df = rbind(SAT_NFC.result.df, SAT_NFC.eval.result)
    SAT_F.result.df = rbind(SAT_F.result.df, SAT_F.eval.result)

    
    print(paste0('finished ', rel))
  }
  

  sum_SAT.result.df = SAT.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  sum_pure_NT.result.df = pure_NT.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  sum_pure_NFC.result.df = pure_NFC.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  sum_SAT_NT.result.df = SAT_NT.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  sum_SAT_NFC.result.df = SAT_NFC.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  sum_SAT_F.result.df = SAT_F.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)

  names(sum_SAT.result.df) = c("release", "IFA",   "FPA", "top1","top3", "top5", "FPavg")
  names(sum_pure_NT.result.df) = c("release","IFA",   "FPA", "top1","top3", "top5", "FPavg")
  names(sum_pure_NFC.result.df) = c("release", "IFA",   "FPA", "top1","top3", "top5", "FPavg" )
  names(sum_SAT_NT.result.df) = c("release","IFA",  "FPA", "top1","top3", "top5", "FPavg")
  names(sum_SAT_NFC.result.df) = c("release", "IFA",   "FPA", "top1","top3", "top5", "FPavg" )
  names(sum_SAT_F.result.df) = c("release", "IFA",   "FPA", "top1","top3", "top5", "FPavg" )
  
  sum_SAT.result.df$technique = SATname
  sum_pure_NT.result.df$technique = "NT"
  sum_pure_NFC.result.df$technique = "NFC"
  sum_SAT_NT.result.df$technique = paste0(SATname, "_NT")
  sum_SAT_NFC.result.df$technique = paste0(SATname, "_NFC")
  sum_SAT_F.result.df$technique = paste0(SATname, "_F")
  
  dataframes <- list(sum_SAT.result.df, sum_pure_NT.result.df, sum_pure_NFC.result.df, sum_SAT_NT.result.df, sum_SAT_NFC.result.df, sum_SAT_F.result.df)
  

  dataframes.median = dataframes
  dataframes.mean = dataframes
  

  dataframes_processed <- lapply(dataframes.median, add_median_row)
  
  last_rows <- lapply(dataframes_processed, function(df) df[nrow(df), ])
  median.result <- do.call(rbind, last_rows)
  row.names(median.result) <- paste0("df", 1:length(dataframes_processed))
  
  write.csv(median.result,"D:/Gitee-code/enhance_SATs/figures/(2024-10-19)RQ4_figures/RQ4_median_table.csv")
  

  dataframes_processed <- lapply(dataframes.mean, add_mean_row)
  
  last_rows <- lapply(dataframes_processed, function(df) df[nrow(df), ])
  mean.result <- do.call(rbind, last_rows)
  row.names(mean.result) <- paste0("df", 1:length(dataframes_processed))
  
  write.csv(mean.result,"D:/Gitee-code/enhance_SATs/figures/(2024-10-19)RQ4_figures/RQ4_mean_table.csv")
  
  print(paste0('finished ',SATname))
  

  all.line.result.RQ4 = rbind(sum_SAT.result.df, sum_pure_NT.result.df, sum_pure_NFC.result.df, sum_SAT_NT.result.df, sum_SAT_NFC.result.df, sum_SAT_F.result.df)
  

  ifa.result.df = select(all.line.result.RQ4, c('technique', 'IFA'))
  fpa.result.df = select(all.line.result.RQ4, c('technique', 'FPA'))
  top1.result.df = select(all.line.result.RQ4, c('technique', 'top1'))
  top3.result.df = select(all.line.result.RQ4, c('technique', 'top3'))
  top5.result.df = select(all.line.result.RQ4, c('technique', 'top5'))
  FPavg.result.df = select(all.line.result.RQ4, c('technique', 'FPavg'))
  
  ifa.result.df = preprocess(ifa.result.df, TRUE)
  fpa.result.df = preprocess(fpa.result.df, FALSE)
  top1.result.df = preprocess(top1.result.df, FALSE)
  top3.result.df = preprocess(top3.result.df, FALSE)
  top5.result.df = preprocess(top5.result.df, FALSE)
  FPavg.result.df = preprocess(FPavg.result.df, TRUE)
  
  RQ4.save.fig.dir = paste0(RQ4.save.fig.dir, SATname, "/")
  
  if (!dir.exists(RQ4.save.fig.dir)) {
    if (!dir.create(RQ4.save.fig.dir, recursive = TRUE)) {
      stop("Could not create directory: ", RQ4.save.fig.dir)
    }
  }
  
  
 
  IFA_y_limit = IFA_y_limit
  FPA_y_limit = FPA_y_limit
  
  ggplot(ifa.result.df, aes(x=reorder(variable, value, FUN=mean), y=value)) + geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 2, color = "red") + coord_cartesian(ylim=c(0,IFA_y_limit)) +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("IFA") +
    xlab("") +
    theme(axis.text.x=element_text(angle = -60, hjust = 0))
  ggsave(paste0(RQ4.save.fig.dir,"IFA.pdf"),width=5,height=2.5)
  
  ggplot(fpa.result.df, aes(x=reorder(variable, value, FUN=mean), y=value)) + geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 2, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("FPA") +
    xlab("") +
    theme(axis.text.x=element_text(angle = -60, hjust = 0))
  ggsave(paste0(RQ4.save.fig.dir,"FPA.pdf"),width=5,height=2.5)
  
  ggplot(top1.result.df, aes(x=reorder(variable, value, FUN=mean), y=value)) + geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 2, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("Top1") +
    xlab("") +
    theme(axis.text.x=element_text(angle = -60, hjust = 0))
  ggsave(paste0(RQ4.save.fig.dir,"Top1.pdf"),width=5,height=2.5)
  
  ggplot(top3.result.df, aes(x=reorder(variable, value, FUN=mean), y=value)) + geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 2, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("Top3") +
    xlab("") +
    theme(axis.text.x=element_text(angle = -60, hjust = 0))
  ggsave(paste0(RQ4.save.fig.dir,"Top3.pdf"),width=5,height=2.5)
  
  ggplot(top5.result.df, aes(x=reorder(variable, value, FUN=mean), y=value)) + geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 2, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("Top5") +
    xlab("") +
    theme(axis.text.x=element_text(angle = -60, hjust = 0))
  ggsave(paste0(RQ4.save.fig.dir,"Top5.pdf"),width=5,height=2.5)
  
  ggplot(FPavg.result.df, aes(x=reorder(variable, value, FUN=mean), y=value)) + geom_boxplot() +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 2, color = "red") +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("FPavg") +
    xlab("") +
    theme(axis.text.x=element_text(angle = -60, hjust = 0))
  ggsave(paste0(RQ4.save.fig.dir,"FPavg.pdf"),width=5,height=2.5)
  
}


all_eval_releases = c('activemq-5.2.0','activemq-5.3.0','activemq-5.8.0',
                      'camel-2.10.0','camel-2.11.0', 
                      'derby-10.5.1.1',
                      'groovy-1_6_BETA_2', 
                      'hbase-0.95.2',
                      'hive-0.12.0', 
                      'jruby-1.5.0','jruby-1.7.0.preview1',
                      'lucene-3.0.0','lucene-3.1','wicket-1.5.3')


get.SAT.result.only.for.actionable.warning(all_eval_releases, PMD.result.dir, "PMD", line.ground.truth, lineLevelMetrics, save.fig.dir, 20, 15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, CheckStyle.result.dir, "CheckStyle", line.ground.truth, lineLevelMetrics, save.fig.dir, 60, 90) 
get.SAT.result.only.for.actionable.warning(all_eval_releases, ErrorProne.result.dir, "ErrorProne", line.ground.truth, lineLevelMetrics, save.fig.dir, 30, 30)
get.SAT.result.only.for.actionable.warning(all_eval_releases, Spotbugs.result.dir, "Spotbugs", line.ground.truth, lineLevelMetrics, save.fig.dir, 10, 20)
get.SAT.result.only.for.actionable.warning(all_eval_releases, betterscan_ce.result.dir, "Betterscan-ce", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, codacy.result.dir, "Codacy", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, codeql.result.dir, "Codeql", line.ground.truth, lineLevelMetrics, save.fig.dir,20,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, sonarqube.result.dir, "Sonarqube", line.ground.truth, lineLevelMetrics, save.fig.dir,15,20)
