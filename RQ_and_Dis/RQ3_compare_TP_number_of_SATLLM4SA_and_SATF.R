# 做一个附加的分析，在这个上下文中（不做任何过滤，分类，以SAT_LLM4SA的TP+FP数目为阈值来截断）比较NFC vs SAT_F，
# 看最后谁识别的TP总数更多
# https://www.notion.so/enhance-SAT-with-GLANCE-d0d64634586948e797267fc32e562786 查看当天实验记录（2024-08-31）

# 2024-07-13 ngram linedp 以及 deeplinedp都是左连接，这与原来的GLANCE不一样，这里尝试进行新的代码更待

# 2024-8-02 新增LLM4SA-PMD效果
# 2024-08-27 新增LLM4SA的全局序列

# 2025-01-07 新增 %>% mutate(NT = replace_na(NT, 0),NFC = replace_na(NFC, 0))
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


# 2024-08-02 新增加LLM4SA-PMD的结果
LLM_PMD.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/PMD/'
LLM_CheckStyle.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/CheckStyle/'
LLM_ErrorProne.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Errorprone/'
LLM_Spotbugs.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Spotbugs/'
LLM_Codacy.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Codacy/'
LLM_Betterscan_ce.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Betterscan-ce/'
LLM_Codeql.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Codeql/'
LLM_Sonarqube.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Sonarqube/'

RQ3.save.fig.dir = 'D:/Gitee-code/enhance_SATs/figures/(2024-10-25)SAT-F的排序有误，所有实验都需要重新跑/figures/RQ3_figure/'
# dir.create(file.path(save.fig.dir), showWarnings = FALSE)

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

all_eval_releases = c('activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0',
                      'camel-2.10.0', 'camel-2.11.0' , 
                      'hive-0.12.0','derby-10.5.1.1' , 'groovy-1_6_BETA_2' , 'hbase-0.95.2',
                      'jruby-1.5.0', 'jruby-1.7.0.preview1',  
                      'lucene-3.0.0', 'lucene-3.1', 'wicket-1.5.3')
# all_eval_releases = c('hive-0.12.0')
# （2）针对有actionable警告的文件/项目进行分析，看看F和G增强的排序是否有效  2024-07-07

###2023-10-30 用GLANCE_MD生成的，设置为file_threshold=1和line_threshold=1，得到代码行级的CE和NFC信息，用于SPLICE的排序计算
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
    
    # 分别处理各个不同的工具
    # PMD
    if (SATname != "CheckStyle" ){
      names(allSATresult) = c('filename','test.release','line_number', 'SAT_prediction_result', 'Priority')
      allSATresult$SAT_prediction_result <- ifelse(allSATresult$SAT_prediction_result %in% c("False", "FALSE"), 0, 1)
      allSATresult = allSATresult %>% filter(SAT_prediction_result == 1)      #2024-05-17: 只保留有警告的那些行做后续分析
    }
    
    if (SATname == "CheckStyle"){
      allSATresult = select(allSATresult, filename, line, priority)
      names(allSATresult) = c('filename', 'line_number', 'Priority')
    }
    
    #静态分析工具的自然排序
    SAT.result = allSATresult %>% group_by(filename) %>% arrange(Priority, line_number, .by_group = TRUE) %>% mutate(rank = row_number())
    SAT.result = select(SAT.result,'filename','line_number','rank')
    names(SAT.result) = c('filename','line.number','rank')
    
    # 任何工具都需要SAT最原始的priority信息
    SAT.base.info = select(allSATresult, filename, line_number, Priority)
    names(SAT.base.info) = c('filename','line.number','Priority')
    
    
    #用NFC*NT增强的排序
    SAT_F.result = SAT.base.info
    GLANCE_F = select(lineLevelMetrics, test, filename, line.number, NT, NFC) %>% filter(test == rel)
    GLANCE_F = select(GLANCE_F, filename, line.number, NT, NFC)
    SAT_F.result = left_join(SAT_F.result, GLANCE_F, by=c('filename', 'line.number')) %>% mutate(NT = replace_na(NT, 0),NFC = replace_na(NFC, 0))
    SAT_F.result = SAT_F.result %>% group_by(filename) %>% arrange(-NFC*NT, Priority, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_F.result = select(SAT_F.result, filename, line.number, rank)
    
    # LLS4SA 
    # LLM4SA每一份文件中按照real bug, false alarm, unknow的结果分别设置优先级为3,2,1，
    # 对于优先级别相同的行，按照自然顺序排序（例如line 3 and line 5的优先级都为3，则排序的时候还是保持line 3 在 line 5 的前面）
    # 排好序之后计算相应的指标
    # real > unknown > false 的效果更好
    LLM4SA = read.csv(paste0(LLM.result.dir,rel,'-line-lvl-result.txt'),quote="")
    names(LLM4SA) = c('filename','test','line.number','LLM_prediction_result')
    
    SAT_LLM4SA_galbol = filter(LLM4SA, LLM_prediction_result == 'real bug') %>% group_by(filename) %>% 
      mutate(LLM_R_number = sum(LLM_prediction_result == 'real bug')) %>% ungroup() %>% filter(LLM_R_number > 0)
    
    LLM4SA = LLM4SA %>% group_by(filename) %>% mutate(LLM_R_number = sum(LLM_prediction_result == 'real bug')) %>%
      select(filename, LLM_R_number) %>% filter(LLM_R_number > 0) %>% distinct()
    
    # merge 是随机进行merge的，导致我rank序列是混乱的
    SAT_galbol = left_join(SAT.result, LLM4SA, by = c('filename')) %>% filter(LLM_R_number > 0) %>% group_by(filename) %>% slice(seq_len(first(LLM_R_number)))
    SAT_F_galbol = left_join(SAT_F.result, LLM4SA, by = c('filename')) %>% filter(LLM_R_number > 0) %>% group_by(filename) %>% slice(seq_len(first(LLM_R_number)))
    # pure_NFC_galbol = left_join(pure_NFC.result, LLM4SA, by = c('filename')) %>% filter(LLM_R_number > 0) %>% group_by(filename) %>% slice(seq_len(first(LLM_R_number)))
    
    
    SAT.result.df = merge(SAT_galbol,cur.df.file,by =c('filename', 'line.number')) %>% group_by(filename) %>% mutate(TP = sum(line.level.ground.truth == 'True'))
    SAT_F.result.df = merge(SAT_F_galbol,cur.df.file,by =c('filename', 'line.number')) %>% group_by(filename) %>% mutate(TP = sum(line.level.ground.truth == 'True'))
    SAT_LLM4SA.result.df = merge(SAT_LLM4SA_galbol,cur.df.file,by =c('filename', 'line.number')) %>% group_by(filename) %>% mutate(TP = sum(line.level.ground.truth == 'True'))
    # pure_NFC.result.df = merge(pure_NFC_galbol, cur.df.file,by =c('filename', 'line.number')) %>% group_by(filename) %>% mutate(TP = sum(line.level.ground.truth == 'True'))
    
    # 统计数量做表格
    SAT.result.df = SAT.result.df %>% select(filename, TP) %>% distinct() 
    SAT_F.result.df = SAT_F.result.df %>% select(filename, TP) %>% distinct()
    SAT_LLM4SA.result.df = SAT_LLM4SA.result.df %>% select(filename, TP) %>% distinct() 
    # pure_NFC.result.df = pure_NFC.result.df %>% select(filename, TP) %>% distinct()
    # 对每个版本，任意文件f，LLM4SA会预测n个警告为"true"，其中n1为真实有缺陷，那么该文件LLM4SA的结果为n1/n；
    # 对一个SAT，该文件上的警告有排序，取前n个预测为有缺陷（与LLM4SA的数目对齐），
    # 其中n1'个为真实有缺陷，即结果为n1'/n。将该版本上所有文件的结果累加起来，LLM4SA的结果为N1/N，SAT的结果为N1'/N，填入表中
    # 在此过程中，不过滤任何文件（即使该文件上的警告数目为1，或者该文件上所有警告的真实标签都是无缺陷），和LLM4SA的使用场景一致。
    
    SAT_total_TP = sum(SAT.result.df$TP)
    SAT_F_total_TP = sum(SAT_F.result.df$TP)
    SAT_LLM4SA_total_TP = sum(SAT_LLM4SA.result.df$TP)
    # pure_NFC_total_TP = sum(pure_NFC.result.df$TP)
    
    SAT_LLM4SA_total_warning = sum(LLM4SA$LLM_R_number)
    
    # 所有版本放在一起看的
    SAT_all_tp = SAT_all_tp + SAT_total_TP
    SAT_F_all_tp = SAT_F_all_tp + SAT_F_total_TP
    SAT_LLM4SA_all_tp = SAT_LLM4SA_all_tp + SAT_LLM4SA_total_TP
    # pure_NFC_all_tp = pure_NFC_all_tp + pure_NFC_total_TP
    
    SAT_LLM4SA_all_warning = SAT_LLM4SA_all_warning + SAT_LLM4SA_total_warning
    
    print(paste0(SATname, ': ', SAT_total_TP, ' / ', SAT_LLM4SA_total_warning))
    print(paste0(SATname, '-LLM4SA : ', SAT_LLM4SA_total_TP, ' / ', SAT_LLM4SA_total_warning))
    # print(paste0('NFC : ', pure_NFC_total_TP, ' / ', SAT_LLM4SA_total_warning))
    print(paste0(SATname, '-F : ', SAT_F_total_TP, ' / ', SAT_LLM4SA_total_warning))
    
    print(paste0('########## finished ', rel, '###########'))
    
    
    # 创建一个空的数据框
    results_df <- data.frame(Project = character(),
                             Tool = character(),
                             PMD = character(),
                             stringsAsFactors = FALSE)
    
    # 将每次循环的结果添加到数据框中
    results_df <- rbind(results_df, 
                       data.frame(Project = rel, 
                                  Tool = "SAT", 
                                  PMD = paste0(SAT_total_TP, " / ", SAT_LLM4SA_total_warning),
                                  stringsAsFactors = FALSE),
                       data.frame(Project = rel, 
                                  Tool = "SAT_LLM4SA", 
                                  PMD = paste0(SAT_LLM4SA_total_TP, " / ", SAT_LLM4SA_total_warning),
                                  stringsAsFactors = FALSE),
                       # data.frame(Project = rel, 
                       #            Tool = "NFC", 
                       #            PMD = paste0(pure_NFC_total_TP, " / ", SAT_LLM4SA_total_warning),
                       #            stringsAsFactors = FALSE),
                       data.frame(Project = rel, 
                                  Tool = "SAT_F", 
                                  PMD = paste0(SAT_F_total_TP, " / ", SAT_LLM4SA_total_warning),
                                  stringsAsFactors = FALSE)
    )
    
    # results_df <- rbind(results_df, 
    #                     data.frame(Project = rel, 
    #                                Tool = "SAT", 
    #                                PMD = paste0(SAT_total_TP),
    #                                stringsAsFactors = FALSE),
    #                     data.frame(Project = rel, 
    #                                Tool = "SAT_LLM4SA", 
    #                                PMD = paste0(SAT_LLM4SA_total_TP),
    #                                stringsAsFactors = FALSE),
    #                     data.frame(Project = rel, 
    #                                Tool = "SAT_F", 
    #                                PMD = paste0(SAT_F_total_TP),
    #                                stringsAsFactors = FALSE)
    # )
    
    final.result.df = rbind(final.result.df ,results_df)
  }
  write.csv(final.result.df, 'D:/Gitee-code/enhance_SATs/figures/(2024-10-19)RQ3_figures/TP对比数(update-2025-1-7).csv', row.names = FALSE)
  print(paste0('ALL-',SATname, ': ', SAT_all_tp, ' / ', SAT_LLM4SA_all_warning))
  print(paste0('ALL-',SATname, '-LLM4SA : ', SAT_LLM4SA_all_tp, ' / ', SAT_LLM4SA_all_warning))
  # print(paste0('ALL-','NFC : ', pure_NFC_all_tp, ' / ', SAT_LLM4SA_all_warning))
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

