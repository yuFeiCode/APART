# 2024-07-13 ngram linedp 以及 deeplinedp都是左连接，这与原来的GLANCE不一样，这里尝试进行新的代码更待

# 2024-8-02 新增LLM4SA-PMD效果

# 2025-01-07 行级缺陷预测方法，不能使用priority信息，只有 [defect-proneness, lineno]来排序的, 改成 arrange(-deeplinedp.score, line.number, .by_group = TRUE)
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

# RQ1.save.fig.dir = 'D:/Gitee-code/enhance_SATs/figures/(2024-10-27)所有的工具都加了priority/figures/RQ1_figures(行级缺陷模型，只使用[缺陷预测分数, line nunber]，不采用priority信息)/'
# RQ2.save.fig.dir = 'D:/Gitee-code/enhance_SATs/figures/(2024-10-27)所有的工具都加了priority/figures/RQ2_figures(行级缺陷模型，只使用[缺陷预测分数, line nunber]，不采用priority信息)/'

# RQ1.save.fig.dir = 'D:/Gitee-code/enhance_SATs/figures/(2025-01-26update)新加入GLANCE-LR对比/RQ1_figures/'
# RQ2.save.fig.dir = 'D:/Gitee-code/enhance_SATs/figures/(2025-01-26update)新加入GLANCE-LR对比/RQ2_figures/'

RQ1.save.fig.dir = 'D:/Gitee-code/enhance_SATs/figures/(2025-02-07)保持RQ1-RQ3的图片大小一致/RQ1_figures/'
RQ2.save.fig.dir = 'D:/Gitee-code/enhance_SATs/figures/(2025-02-07)保持RQ1-RQ3的图片大小一致/RQ2_figures/'

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
  
  # x$rank <- paste("Rank",ranking[as.character(gsub("-", ".", x$variable))])
  x$rank <- paste("R",ranking[as.character(gsub("-", ".", x$variable))])
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
########### deeplinedp 增强的排序 #######
# deeplinedp 增强的排序
deeplinedp.result = df_all
deeplinedp.result[deeplinedp.result$is.comment.line == "True",]$token.attention.score = 0
tmp.top.k = get.top.k.tokens(deeplinedp.result, 1500)
merged_df_all = merge(deeplinedp.result, tmp.top.k, by=c('project', 'train', 'test', 'filename', 'token'), all.x = TRUE)
merged_df_all[is.na(merged_df_all$flag),]$token.attention.score = 0

## use top-k tokens 
sum_line_attn = merged_df_all %>% filter(file.level.ground.truth == "True" & prediction.label == "True" ) %>% group_by(test, filename,is.comment.line, file.level.ground.truth, prediction.label, line.number, line.level.ground.truth) %>%
  summarize(attention_score = sum(token.attention.score), num_tokens = n(),.groups = 'drop')

sum_line_attn  = sum_line_attn  %>% filter(is.comment.line == 'False') %>% select(test, filename, line.number, attention_score)
deeplinedp.sorted = sum_line_attn

##############
line.ground.truth = select(df_all,  project, train, test, filename, file.level.ground.truth, prediction.prob, line.number, line.level.ground.truth, is.comment.line)
line.ground.truth = filter(line.ground.truth, is.comment.line== "False")  #2024-05-17: 获取所有文件中的行级ground-truth标签
line.ground.truth = distinct(line.ground.truth)



# （2）针对有actionable警告的文件/项目进行分析，看看F和G增强的排序是否有效  2024-07-07

###2023-10-30 用GLANCE_MD生成的，设置为file_threshold=1和line_threshold=1，得到代码行级的CE和NFC信息，用于SPLICE的排序计算
CEandNFCdir = "D:/Gitee-code/Boosting deep line-level defect prediction with syntactic features/all_models_result/Glance_MD_full_threshold_2024_10_25_add_NT_output/line_result/test/"

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


#######2024-01-28 GLANCE-LR line-threshold=0.5 二级缺陷预测应该和DeepLineDP流程一样，要考虑到文件级分类器的影响，只有
#######被预测为有缺陷的文件，才会统计这些文件的CE NT NFC
GLANCE_LR.dir = "D:/Gitee-code/Boosting deep line-level defect prediction with syntactic features/all_models_result/BASE-Glance-LR(line_threshold=0.5)/line_result/test/"

GLANCE_LR_files = list.files(GLANCE_LR.dir)

GLANCE_LR_Metric <- NULL

for(f in GLANCE_LR_files)
{
  df <- read.csv(paste0(GLANCE_LR.dir, f))
  df$test = str_split_fixed(f, "-result", 2)[,1]
  GLANCE_LR_Metric  <- rbind(GLANCE_LR_Metric, df)
}

GLANCE_LR_Metric = select(GLANCE_LR_Metric, "predicted_buggy_lines", "predicted_buggy_line_numbers","predicted_buggy_score", "rank", "functioncall", "controlelements", "numbertokens", "test")
names(GLANCE_LR_Metric) = c("filename", "line.number", "GLANCEscore", "rank", "NFC", "CE", "NT", "test")
GLANCE_LR_Metric$filename = str_split_fixed(GLANCE_LR_Metric$filename, ":", 2)[,1]
GLANCE_LR_Metric$filename <- gsub("/", "_", GLANCE_LR_Metric$filename)


####################################################################################
get.line.metrics.result = function(baseline.df, cur.df.file)
{
  # 2024-07-07 只针对actionable warning的文件（项目级的）
  baseline.df.with.ground.truth = merge(baseline.df, cur.df.file, by=c("filename", "line.number"))
  
  # 2024-07-07 只针对actionable warning的文件
  baseline.df.with.ground.truth = baseline.df.with.ground.truth %>% group_by(filename) %>%
    mutate(actionable.warning = ifelse(any(line.level.ground.truth == 'True'), 1, 0)) %>% 
    filter(actionable.warning == 1)
  
  
  ## 同一文件内的行为一组，按line.score从大到小降序排列；每一组内独立编号order
  sorted = baseline.df.with.ground.truth %>% group_by(filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())
  # warning.files = select(sorted,filename) %>% distinct() %>% nrow()
  
  
  
  #2024-05-17: 只分析警告行数大于等于10的文件，太少了失去排序的意义
  sorted = sorted %>% filter(totalSLOC >= 5) 
  filter.files = sorted %>% select(filename) %>% distinct()
  # 记录filter.files的行数
  # number <- nrow(filter.files)
  # print(paste0(' filter files is ',number))
  
  ##统计每个有缺陷的文件中包含多少个有缺陷的代码行
  total_true = sorted %>%  group_by(filename) %>% summarize(total_true = sum(line.level.ground.truth == "True"))
  
  ##glance的预测结果中包含file.level.ground.truth == "FALSE"的文件，
  ##为使用DeepLineDP使用的性能指标，这些文件需要排除掉
  total_true = total_true %>% filter(total_true > 0)
  
  # 2024-07-09 FPavg：找到一个TP前平均审查的假警告数目
  FPavg = sorted %>% group_by(filename) %>% mutate(FPI = cumsum(line.level.ground.truth == "False"), total_truth = sum(line.level.ground.truth == "True")) %>% 
    filter(line.level.ground.truth == "True") %>% mutate(S.R = sum(FPI, na.rm = TRUE)) %>% mutate(FPavg = round( S.R / total_truth, digits = 2)) %>% 
    select(filename,FPavg) %>% distinct()
  
  FPavg = FPavg%>% arrange(filename)
  
  FPavg.list = FPavg$FPavg
  
  ## IFA:  每个文件一个IFA，在每个文件的行组中取order最低的行号，代表检查到第一个有缺陷的行时需要检查多少行
  IFA = sorted %>% filter(line.level.ground.truth == "True") %>% group_by(filename)  %>% top_n(1, -order)
  ## added 2023-09-16 确保按文件名排序
  IFA = IFA%>% arrange(filename)
  
  ## 注意要减1，第一个有缺陷语句前面的clean语句行数
  ifa.list = IFA$order - 1
  
  ## added 2023-09-16 确保按文件名排序
  total_true = total_true%>% arrange(filename)
  
  #added 2023-10-30, FPA：fault-percentile-average
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




get.SAT.result.only.for.actionable.warning = function(all_eval_releases, SAT.result.dir, SATname, line.ground.truth, lineLevelMetrics, save.fig.dir,IFA_y_limit, FPavg_y_limit)
{
  SAT.result.df = NULL 
  SAT_G.result.df = NULL 
  SAT_F.result.df = NULL
  SAT_glance.lr.result.df = NULL
  SAT_linedp.result.df = NULL
  SAT_ngram.result.df = NULL
  SAT_deeplinedp.result.df = NULL
  
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
    
    #用GLANCE增强的排序
    SAT_G.result = SAT.base.info
    GLANCE_G = select(lineLevelMetrics, test, filename, line.number, GLANCEscore, CE)%>% filter(test == rel)
    GLANCE_G = select(GLANCE_G, filename, line.number, GLANCEscore, CE)
    names(GLANCE_G) = c('filename','line.number','GLANCEscore', 'CE')
    # SAT_G.result = left_join(SAT_G.result, GLANCE_G, by=c('filename', 'line.number'))
    SAT_G.result = left_join(SAT_G.result, GLANCE_G, by=c('filename', 'line.number')) %>% mutate(
        GLANCEscore = replace_na(GLANCEscore, 0),
        CE = replace_na(CE, 0))
    
 
    SAT_G.result = SAT_G.result %>% group_by(filename) %>% arrange(-CE, -GLANCEscore, Priority,  line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_G.result = select(SAT_G.result, filename, line.number, rank)
    
    #用NFC*NT增强的排序
    SAT_F.result = SAT.base.info
    GLANCE_F = select(lineLevelMetrics, test, filename, line.number, NT, NFC) %>% filter(test == rel)
    GLANCE_F = select(GLANCE_F, filename, line.number, NT, NFC)
    # SAT_F.result = left_join(SAT_F.result, GLANCE_F, by=c('filename', 'line.number'))
    SAT_F.result = left_join(SAT_F.result, GLANCE_F, by=c('filename', 'line.number')) %>% mutate(
      NT = replace_na(NT, 0),
      NFC = replace_na(NFC, 0))
    
    SAT_F.result = SAT_F.result %>% group_by(filename) %>% arrange(-NFC*NT, Priority, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_F.result = select(SAT_F.result, filename, line.number, rank)

    # GLANCE-LR
    glance.lr.result = SAT.base.info
    GLANCE_LR = select(GLANCE_LR_Metric, test, filename, line.number, GLANCEscore, CE)%>% filter(test == rel)
    GLANCE_LR = select(GLANCE_LR, filename, line.number, GLANCEscore, CE)
    # SAT_G.result = left_join(SAT_G.result, GLANCE_G, by=c('filename', 'line.number'))
    SAT_glance.lr.result = left_join(glance.lr.result, GLANCE_LR, by=c('filename', 'line.number')) %>% mutate(
      GLANCEscore = replace_na(GLANCEscore, 0),
      CE = replace_na(CE, 0))
    
    SAT_glance.lr.result = SAT_glance.lr.result %>% group_by(filename) %>% arrange(-CE, -GLANCEscore, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_glance.lr.result = select(SAT_glance.lr.result, filename, line.number, rank)
    
    
    #ngram增强的排序
    n.gram.result = read.csv(paste0(n.gram.result.dir,rel,'-line-lvl-result.txt'), sep = "\t", quote = "")
    n.gram.result = select(n.gram.result, "file.name", "line.number",  "line.score")
    n.gram.result = distinct(n.gram.result)
    names(n.gram.result) = c('filename','line.number','n.gram.score')
    n.gram.result$line.number <- as.integer(n.gram.result$line.number)
    n.gram.result$filename <- gsub("/", "_", n.gram.result$filename)
    
    SAT_ngram.result = SAT.base.info
    # SAT_ngram.result = left_join(SAT_ngram.result, n.gram.result, by=c('filename', 'line.number'))
    SAT_ngram.result = left_join(SAT_ngram.result, n.gram.result, by=c('filename', 'line.number')) %>% mutate(
      n.gram.score = replace_na(n.gram.score, 0))
    
    SAT_ngram.result = SAT_ngram.result %>% group_by(filename) %>% arrange(-n.gram.score, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_ngram.result = select(SAT_ngram.result, filename, line.number, rank)

    
    # linedp增强的排序
    linedp.result = read.csv(paste0(linedp.result.dir,rel,'-result.csv'))
    linedp.result$filename = str_split_fixed(linedp.result$predicted_buggy_lines, ":", 2)[,1]
    linedp.result$line.number = str_split_fixed(linedp.result$predicted_buggy_lines, ":", 2)[,2]
    linedp.result$line.number <- as.integer(linedp.result$line.number)
    linedp.result$filename <- gsub("/", "_", linedp.result$filename)
    linedp.result = select(linedp.result,filename, line.number, predicted_buggy_score)
    names(linedp.result) = c('filename', 'line.number', 'linedp.score')
    
    SAT_linedp.result = SAT.base.info
    # SAT_linedp.result = left_join(SAT_linedp.result, linedp.result, by=c('filename', 'line.number'))
    SAT_linedp.result = left_join(SAT_linedp.result, linedp.result, by=c('filename', 'line.number')) %>% mutate(
      linedp.score = replace_na(linedp.score, 0))
    
    SAT_linedp.result = SAT_linedp.result %>% group_by(filename) %>% arrange(-linedp.score, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_linedp.result = select(SAT_linedp.result, filename, line.number, rank)

    
    # DeeplineDP增强的排序
    temp.deeplinedp.result = deeplinedp.sorted %>% filter(test == rel) %>% select(filename, line.number, attention_score)
    temp.deeplinedp.result$filename <- gsub("/", "_", temp.deeplinedp.result$filename)
    names(temp.deeplinedp.result) = c('filename','line.number','deeplinedp.score')
    
    SAT_deeplinedp.result = SAT.base.info
    # SAT_deeplinedp.result = left_join(SAT_deeplinedp.result, temp.deeplinedp.result, by=c('filename', 'line.number'))
    SAT_deeplinedp.result = left_join(SAT_deeplinedp.result, temp.deeplinedp.result, by=c('filename', 'line.number')) %>% mutate(
      deeplinedp.score = replace_na(deeplinedp.score, 0))
    
    SAT_deeplinedp.result = SAT_deeplinedp.result %>% group_by(filename) %>% arrange(-deeplinedp.score, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_deeplinedp.result = select(SAT_deeplinedp.result, filename, line.number, rank)

    
    # 2024-07-07 只针对actionable warning的文件（项目级的）
    ##"%>% mutate(test=rel)" 确保记录下每个target project的名称
    SAT.eval.result = get.line.metrics.result(SAT.result, cur.df.file) %>% mutate(test=rel)
    SAT_G.eval.result = get.line.metrics.result(SAT_G.result, cur.df.file) %>% mutate(test=rel)
    SAT_F.eval.result = get.line.metrics.result(SAT_F.result, cur.df.file) %>% mutate(test=rel)
    SAT_glance.lr.eval.result = get.line.metrics.result(SAT_glance.lr.result, cur.df.file) %>% mutate(test=rel)
    SAT_ngram.eval.result = get.line.metrics.result(SAT_ngram.result, cur.df.file) %>% mutate(test=rel)
    SAT_linedp.eval.result = get.line.metrics.result(SAT_linedp.result, cur.df.file) %>% mutate(test=rel)
    SAT_deeplinedp.eval.result = get.line.metrics.result(SAT_deeplinedp.result, cur.df.file) %>% mutate(test=rel)
    
    
    SAT.result.df = rbind(SAT.result.df, SAT.eval.result)
    SAT_G.result.df = rbind(SAT_G.result.df, SAT_G.eval.result)
    SAT_F.result.df = rbind(SAT_F.result.df, SAT_F.eval.result)
    SAT_glance.lr.result.df = rbind(SAT_glance.lr.result.df, SAT_glance.lr.eval.result)
    SAT_ngram.result.df = rbind(SAT_ngram.result.df, SAT_ngram.eval.result)
    SAT_linedp.result.df = rbind(SAT_linedp.result.df, SAT_linedp.eval.result)
    SAT_deeplinedp.result.df = rbind(SAT_deeplinedp.result.df, SAT_deeplinedp.eval.result)
    
    print(paste0('finished ', rel))
  }
  
  # 2024-07-07 只针对actionable warning的文件（项目级的）
  sum_SAT.result.df = SAT.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  sum_SAT_G.result.df = SAT_G.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  sum_SAT_F.result.df = SAT_F.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  sum_SAT_glance.lr.result.df = SAT_glance.lr.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  sum_SAT_ngram.result.df = SAT_ngram.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  sum_SAT_linedp.result.df = SAT_linedp.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  sum_SAT_deeplinedp.result.df = SAT_deeplinedp.result.df %>% summarise( IFA=median(ifa.list), fpa=median(fpa.list), top1=mean(top1.list), top3=mean(top3.list), top5=mean(top5.list), FPavg = median(FPavg.list), .by=test)
  
  names(sum_SAT.result.df) = c("release", "IFA",   "FPA", "top1","top3", "top5", "FPavg")
  names(sum_SAT_G.result.df) = c("release","IFA",   "FPA", "top1","top3", "top5", "FPavg")
  names(sum_SAT_F.result.df) = c("release", "IFA",   "FPA", "top1","top3", "top5", "FPavg" )
  names(sum_SAT_glance.lr.result.df) = c("release", "IFA",   "FPA", "top1","top3", "top5", "FPavg" )
  names(sum_SAT_ngram.result.df) = c("release","IFA",   "FPA", "top1","top3", "top5", "FPavg")
  names(sum_SAT_linedp.result.df) = c("release", "IFA",   "FPA", "top1","top3", "top5", "FPavg" )
  names(sum_SAT_deeplinedp.result.df) = c("release", "IFA",  "FPA", "top1","top3", "top5", "FPavg" )
  
  sum_SAT.result.df$technique = SATname
  sum_SAT_G.result.df$technique = paste0(SATname, "_G")
  sum_SAT_F.result.df$technique = paste0(SATname, "_F")
  sum_SAT_glance.lr.result.df$technique = "GLANCE-LR"
  sum_SAT_ngram.result.df$technique = "N-gram"
  sum_SAT_linedp.result.df$technique = "LineDP"
  sum_SAT_deeplinedp.result.df$technique = "DeepLineDP"

  
  all.line.result.RQ1 = rbind(sum_SAT.result.df, sum_SAT_G.result.df, sum_SAT_F.result.df)
  all.line.result.RQ2 = rbind(sum_SAT_F.result.df, sum_SAT_ngram.result.df, sum_SAT_linedp.result.df, sum_SAT_deeplinedp.result.df,sum_SAT_glance.lr.result.df)
  
  # 先画RQ1的图
  ifa.result.df = select(all.line.result.RQ1, c('technique', 'IFA'))
  fpa.result.df = select(all.line.result.RQ1, c('technique', 'FPA'))
  top1.result.df = select(all.line.result.RQ1, c('technique', 'top1'))
  top3.result.df = select(all.line.result.RQ1, c('technique', 'top3'))
  top5.result.df = select(all.line.result.RQ1, c('technique', 'top5'))
  FPavg.result.df = select(all.line.result.RQ1, c('technique', 'FPavg'))
  
  ifa.result.df = preprocess(ifa.result.df, TRUE)
  fpa.result.df = preprocess(fpa.result.df, FALSE)
  top1.result.df = preprocess(top1.result.df, FALSE)
  top3.result.df = preprocess(top3.result.df, FALSE)
  top5.result.df = preprocess(top5.result.df, FALSE)
  FPavg.result.df = preprocess(FPavg.result.df, TRUE)
  
  
  RQ1.save.fig.dir = paste0(RQ1.save.fig.dir, SATname, "/")
  
  if (!dir.exists(RQ1.save.fig.dir)) {
    if (!dir.create(RQ1.save.fig.dir, recursive = TRUE)) {
      stop("Could not create directory: ", RQ1.save.fig.dir)
    }
  }
  
  IFA_y_limit = IFA_y_limit
  FPavg_y_limit = FPavg_y_limit
  
  # 创建颜色向量
  variable_names <- c(paste0(SATname, '_F'), paste0(SATname, '_G'), SATname)
  fill_colors <- c(rgb(102, 204, 255, maxColorValue=255), rgb(255, 230, 153, maxColorValue=255), rgb(178, 178, 178, maxColorValue=255))
  line_colors <- c(rgb(0, 0, 255, maxColorValue=255), rgb(255, 165, 0, maxColorValue=255),rgb(95, 95, 95, maxColorValue=255))
  names(fill_colors) <- variable_names
  names(line_colors) <- variable_names
  
  
  
  # 创建箱式图
  # aes(x=reorder(variable, value, FUN=median) 指的是同一组内按照 中位值降序排序
  # aes(x=reorder(variable, -value, FUN=median) 指的是同一组内按照 中位值升序排序
  ifa <- ggplot(ifa.result.df, aes(x=reorder(variable, value, FUN=median), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) + # 调整箱式图线条和异常点大小
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red") +
    coord_cartesian(ylim=c(0, IFA_y_limit)) +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 7, face = "bold"),# 调整分面标签字体大小
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"), # 控制Rank 1 和Rank 2之间的距离
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) +  
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors) +
    scale_y_continuous(labels = scales::label_number(accuracy = 1))
  # 确定输出文件的宽度和高度（单位：英寸）
  output_width <- 2.3 / 2.54  # 将宽度从厘米转换为英寸
  output_height <- 2.89 / 2.54 # 假设高度按照宽高比调整，这里假设为0.6，如果有具体高度，可以直接设置
  
  
  ggsave(paste0(RQ1.save.fig.dir, "IFA.png"),  plot = ifa, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)
  
  # ggsave(paste0(RQ1.save.fig.dir,"IFA.pdf"),width=output_width,height=output_height)
  
  FPavg = ggplot(FPavg.result.df, aes(x=reorder(variable, value, FUN=mean), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red")  +
    coord_cartesian(ylim=c(0, FPavg_y_limit)) +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 7, face = "bold"),
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) + # 调整分面标签字体大小 
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors)+
    scale_y_continuous(labels = scales::label_number(accuracy = 1))
  ggsave(paste0(RQ1.save.fig.dir, "FPavg.png"), plot = FPavg, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)
  # ggsave(paste0(RQ1.save.fig.dir,"FPavg.pdf"),width=5,height=2.5)
  
  
  fpa = ggplot(fpa.result.df, aes(x=reorder(variable, -value, FUN=median), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 7, face = "bold"),
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) + # 调整分面标签字体大小 
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  ggsave(paste0(RQ1.save.fig.dir, "FPA.png"), plot = fpa, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)
  # ggsave(paste0(RQ1.save.fig.dir,"FPA.pdf"),width=5,height=2.5)
  
  top1 = ggplot(top1.result.df, aes(x=reorder(variable, -value, FUN=median), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 7, face = "bold"),
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) + # 调整分面标签字体大小 
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors)+
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  ggsave(paste0(RQ1.save.fig.dir, "top1.png"), plot = top1, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)
  # ggsave(paste0(RQ1.save.fig.dir,"Top1.pdf"),width=5,height=2.5)
  
  top3 = ggplot(top3.result.df, aes(x=reorder(variable, -value, FUN=median), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 7, face = "bold"),
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"), 
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) + # 调整分面标签字体大小 
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors)+
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  ggsave(paste0(RQ1.save.fig.dir, "top3.png"), plot = top3, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)

  top5 = ggplot(top5.result.df, aes(x=reorder(variable, -value, FUN=median), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 7, face = "bold"),
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) + # 调整分面标签字体大小 
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors)+
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  ggsave(paste0(RQ1.save.fig.dir, "top5.png"), plot = top5, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)
  # ggsave(paste0(RQ1.save.fig.dir,"Top5.pdf"),width=5,height=2.5)
  
  
  
  
  
  # 再画RQ2的图
  
  ifa.result.df = select(all.line.result.RQ2, c('technique', 'IFA'))
  fpa.result.df = select(all.line.result.RQ2, c('technique', 'FPA'))
  top1.result.df = select(all.line.result.RQ2, c('technique', 'top1'))
  top3.result.df = select(all.line.result.RQ2, c('technique', 'top3'))
  top5.result.df = select(all.line.result.RQ2, c('technique', 'top5'))
  FPavg.result.df = select(all.line.result.RQ2, c('technique', 'FPavg'))
  
  ifa.result.df = preprocess(ifa.result.df, TRUE)
  fpa.result.df = preprocess(fpa.result.df, FALSE)
  top1.result.df = preprocess(top1.result.df, FALSE)
  top3.result.df = preprocess(top3.result.df, FALSE)
  top5.result.df = preprocess(top5.result.df, FALSE)
  FPavg.result.df = preprocess(FPavg.result.df, TRUE)
  RQ2.save.fig.dir = paste0(RQ2.save.fig.dir, SATname, "/")
  
  if (!dir.exists(RQ2.save.fig.dir)) {
    if (!dir.create(RQ2.save.fig.dir, recursive = TRUE)) {
      stop("Could not create directory: ", RQ2.save.fig.dir)
    }
  }
  
  # 创建颜色向量
  variable_names <- c(paste0(SATname, '_F'),'N-gram', 'LineDP', 'DeepLineDP', 'GLANCE-LR')
  fill_colors <- c(rgb(102, 204, 255, maxColorValue=255), 
                   rgb(255, 201, 201, maxColorValue=255), 
                   rgb(140, 156, 213, maxColorValue=255),
                   rgb(199, 180, 151, maxColorValue=255),
                   rgb(230, 190, 255, maxColorValue=255))  # Light purple for GLANCE-LR
  
  line_colors <- c(rgb(0, 0, 255, maxColorValue=255), 
                   rgb(204, 0, 0, maxColorValue=255),      # 深红色
                   rgb(70, 78, 107, maxColorValue=255),    # 深蓝灰色
                   rgb(133, 120, 101, maxColorValue=255),  # 深棕色
                   rgb(147, 112, 219, maxColorValue=255))  # Medium purple for GLANCE-LR
  
  names(fill_colors) <- variable_names
  names(line_colors) <- variable_names
  
  
  
  ifa <- ggplot(ifa.result.df, aes(x=reorder(variable, value, FUN=median), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) + # 调整箱式图线条和异常点大小
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red") +
    coord_cartesian(ylim=c(0, IFA_y_limit)) +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 5, face = "bold"),# 调整分面标签字体大小
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"), # 控制Rank 1 和Rank 2之间的距离
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) +  
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors) +
    scale_y_continuous(labels = scales::label_number(accuracy = 1))
  # 确定输出文件的宽度和高度（单位：英寸）
  output_width <- 2.3 / 2.54  # 将宽度从厘米转换为英寸
  output_height <- 2.89 / 2.54 # 假设高度按照宽高比调整，这里假设为0.6，如果有具体高度，可以直接设置
  
  
  ggsave(paste0(RQ2.save.fig.dir, "IFA.png"),  plot = ifa, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)
  
  # ggsave(paste0(RQ2.save.fig.dir,"IFA.pdf"),width=5,height=2.5)
  
  FPavg = ggplot(FPavg.result.df, aes(x=reorder(variable, value, FUN=mean), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red")  +
    coord_cartesian(ylim=c(0, FPavg_y_limit)) +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 5, face = "bold"),
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) + # 调整分面标签字体大小 
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors) +
    scale_y_continuous(labels = scales::label_number(accuracy = 1))
  ggsave(paste0(RQ2.save.fig.dir, "FPavg.png"), plot = FPavg, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)
  
  
  fpa = ggplot(fpa.result.df, aes(x=reorder(variable, -value, FUN=median), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 5, face = "bold"),
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) + # 调整分面标签字体大小 
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors)+
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  ggsave(paste0(RQ2.save.fig.dir, "FPA.png"), plot = fpa, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)
  # ggsave(paste0(RQ1.save.fig.dir,"FPA.pdf"),width=5,height=2.5)
  
  top1 = ggplot(top1.result.df, aes(x=reorder(variable, -value, FUN=median), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 5, face = "bold"),
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) + # 调整分面标签字体大小 
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  ggsave(paste0(RQ2.save.fig.dir, "top1.png"), plot = top1, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)
  # ggsave(paste0(RQ1.save.fig.dir,"Top1.pdf"),width=5,height=2.5)
  
  top3 = ggplot(top3.result.df, aes(x=reorder(variable, -value, FUN=median), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 5, face = "bold"),
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"), 
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) + # 调整分面标签字体大小 
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors)+
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  ggsave(paste0(RQ2.save.fig.dir, "top3.png"), plot = top3, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)
  
  top5 = ggplot(top5.result.df, aes(x=reorder(variable, -value, FUN=median), y=value, fill=variable, color=variable)) + 
    geom_boxplot(width = 0.6, size = 0.3, outlier.size = 0.1, outlier.stroke = 0.8) +
    stat_summary(fun = mean, geom = "point", shape = 17, size = 0.7, color = "red")  +
    facet_grid(~rank, drop=TRUE, scales = "free", space = "free") +
    ylab("") +
    xlab("") +
    theme(plot.margin = unit(c(0, 0, -0.5, -0.4), "cm"),  # 去除图形边距
          axis.text.x = element_blank(),  # 清空X轴标签
          axis.ticks.x = element_blank(), # 移除X轴刻度
          axis.text.y = element_text(size = 8, margin = margin(0, 0, 0, 0)),
          axis.ticks.y = element_line(size = 0.5),# 调整Y轴刻度字体大小
          legend.position = "none", # 移除图例
          strip.text = element_text(size = 5, face = "bold"),
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.spacing = unit(0, "lines"),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA)) + # 调整分面标签字体大小 
    scale_fill_manual(values = fill_colors) +  # 指定颜色
    scale_color_manual(values = line_colors)+
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  ggsave(paste0(RQ2.save.fig.dir, "top5.png"), plot = top5, width = output_width, height = output_height, dpi = 600,units = "in",limitsize = FALSE)
  # ggsave(paste0(RQ1.save.fig.dir,"Top5.pdf"),width=5,height=2.5)
  
  
  
  
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
get.SAT.result.only.for.actionable.warning(all_eval_releases, CheckStyle.result.dir, "CheckStyle", line.ground.truth, lineLevelMetrics, save.fig.dir, 60, 75) 
get.SAT.result.only.for.actionable.warning(all_eval_releases, ErrorProne.result.dir, "ErrorProne", line.ground.truth, lineLevelMetrics, save.fig.dir, 30, 30)
get.SAT.result.only.for.actionable.warning(all_eval_releases, Spotbugs.result.dir, "Spotbugs", line.ground.truth, lineLevelMetrics, save.fig.dir, 10, 20)
get.SAT.result.only.for.actionable.warning(all_eval_releases, betterscan_ce.result.dir, "Betterscan-ce", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, codacy.result.dir, "Codacy", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, codeql.result.dir, "Codeql", line.ground.truth, lineLevelMetrics, save.fig.dir,20,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, sonarqube.result.dir, "Sonarqube", line.ground.truth, lineLevelMetrics, save.fig.dir,15,20)

