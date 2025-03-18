
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
library(ComplexHeatmap)
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

# 2024-08-02 新增加LLM4SA-PMD的结果
LLM_PMD.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/PMD/'
LLM_CheckStyle.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/CheckStyle/'
LLM_ErrorProne.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Errorprone/'
LLM_Spotbugs.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Spotbugs/'
LLM_Codacy.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Codacy/'
LLM_Betterscan_ce.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Betterscan-ce/'
LLM_Codeql.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Codeql/'
LLM_Sonarqube.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/LLM4SA/Sonarqube/'


Dis.figures.path.top1 = 'D:/Gitee-code/enhance_SATs/figures/(2025-01-26update)新加入GLANCE-LR对比/Dis_6.1_figures/top1_sets/'
Dis.figures.path.top3 = 'D:/Gitee-code/enhance_SATs/figures/(2025-01-26update)新加入GLANCE-LR对比/Dis_6.1_figures/top3_sets/'

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
########### deeplinedp  #######

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



###2023-10-30 用GLANCE_MD生成的，设置为file_threshold=1和line_threshold=1，得到代码行级的CE和NFC信息
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
get.top.N.TP = function(baseline.df, cur.df.file, N, test)
{

  baseline.df.with.ground.truth = merge(baseline.df, cur.df.file, by=c("filename", "line.number"))
  

  baseline.df.with.ground.truth = baseline.df.with.ground.truth %>% group_by(filename) %>%
    mutate(actionable.warning = ifelse(any(line.level.ground.truth == 'True'), 1, 0)) %>% 
    filter(actionable.warning == 1)
  
  
  sorted = baseline.df.with.ground.truth %>% group_by(filename) %>% arrange(rank, .by_group = TRUE) %>% mutate(order = row_number())%>% mutate(totalSLOC = n())
  

  sorted = sorted %>% filter(totalSLOC >= 5) 
  

  
  top.N.TP = sorted %>% group_by(filename) %>% filter(order <= N)
  top.N.TP = top.N.TP %>% mutate(element = paste0(test, "/", filename, "/", line.number)) %>% ungroup()
  top.N.TP = top.N.TP %>% select(element)
  
  return(top.N.TP)
}


get.SAT.result.only.for.actionable.warning = function(all_eval_releases, LLM.result.dir, SAT.result.dir, SATname, line.ground.truth, lineLevelMetrics, save.fig.dir,IFA_y_limit, FPavg_y_limit, top_n, Dis.result.dir)
{
  
  SAT.result.df = NULL 
  SAT_G.result.df = NULL 
  SAT_F.result.df = NULL
  SAT_glance.lr.result.df = NULL
  SAT_linedp.result.df = NULL
  SAT_ngram.result.df = NULL
  SAT_deeplinedp.result.df = NULL
  SAT_LLM4SA.result.df = NULL
  
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
      allSATresult = allSATresult %>% filter(SAT_prediction_result == 1)      #2024-05-17: 只保留有警告的那些行做后续分析
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
    

    SAT_G.result = SAT.base.info
    GLANCE_G = select(lineLevelMetrics, test, filename, line.number, GLANCEscore, CE)%>% filter(test == rel)
    GLANCE_G = select(GLANCE_G, filename, line.number, GLANCEscore, CE)
    names(GLANCE_G) = c('filename','line.number','GLANCEscore', 'CE')
    SAT_G.result = left_join(SAT_G.result, GLANCE_G, by=c('filename', 'line.number')) %>% mutate(
      GLANCEscore = replace_na(GLANCEscore, 0),
      CE = replace_na(CE, 0))
    
    SAT_G.result = SAT_G.result %>% group_by(filename) %>% arrange(-CE, -GLANCEscore, Priority,  line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_G.result = select(SAT_G.result, filename, line.number, rank)
    

    SAT_F.result = SAT.base.info
    GLANCE_F = select(lineLevelMetrics, test, filename, line.number, NT, NFC) %>% filter(test == rel)
    GLANCE_F = select(GLANCE_F, filename, line.number, NT, NFC)
    SAT_F.result = left_join(SAT_F.result, GLANCE_F, by=c('filename', 'line.number')) %>% mutate(
      NT = replace_na(NT, 0),
      NFC = replace_na(NFC, 0))
    
    SAT_F.result = SAT_F.result %>% group_by(filename) %>% arrange(-NFC*NT, Priority, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_F.result = select(SAT_F.result, filename, line.number, rank)
    
    # GLANCE-LR
    glance.lr.result = SAT.base.info
    GLANCE_LR = select(GLANCE_LR_Metric, test, filename, line.number, GLANCEscore, CE)%>% filter(test == rel)
    GLANCE_LR = select(GLANCE_LR, filename, line.number, GLANCEscore, CE)
    SAT_glance.lr.result = left_join(glance.lr.result, GLANCE_LR, by=c('filename', 'line.number')) %>% mutate(
      GLANCEscore = replace_na(GLANCEscore, 0),
      CE = replace_na(CE, 0))
    
    SAT_glance.lr.result = SAT_glance.lr.result %>% group_by(filename) %>% arrange(-CE, -GLANCEscore, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_glance.lr.result = select(SAT_glance.lr.result, filename, line.number, rank)
    
    
    #ngram
    n.gram.result = read.csv(paste0(n.gram.result.dir,rel,'-line-lvl-result.txt'), sep = "\t", quote = "")
    n.gram.result = select(n.gram.result, "file.name", "line.number",  "line.score")
    n.gram.result = distinct(n.gram.result)
    names(n.gram.result) = c('filename','line.number','n.gram.score')
    n.gram.result$line.number <- as.integer(n.gram.result$line.number)
    n.gram.result$filename <- gsub("/", "_", n.gram.result$filename)
    
    SAT_ngram.result = SAT.base.info
    SAT_ngram.result = left_join(SAT_ngram.result, n.gram.result, by=c('filename', 'line.number')) %>% mutate(
      n.gram.score = replace_na(n.gram.score, 0))
    
    SAT_ngram.result = SAT_ngram.result %>% group_by(filename) %>% arrange(-n.gram.score, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_ngram.result = select(SAT_ngram.result, filename, line.number, rank)
    
    
    # linedp
    linedp.result = read.csv(paste0(linedp.result.dir,rel,'-result.csv'))
    linedp.result$filename = str_split_fixed(linedp.result$predicted_buggy_lines, ":", 2)[,1]
    linedp.result$line.number = str_split_fixed(linedp.result$predicted_buggy_lines, ":", 2)[,2]
    linedp.result$line.number <- as.integer(linedp.result$line.number)
    linedp.result$filename <- gsub("/", "_", linedp.result$filename)
    linedp.result = select(linedp.result,filename, line.number, predicted_buggy_score)
    names(linedp.result) = c('filename', 'line.number', 'linedp.score')
    
    SAT_linedp.result = SAT.base.info
    SAT_linedp.result = left_join(SAT_linedp.result, linedp.result, by=c('filename', 'line.number')) %>% mutate(
      linedp.score = replace_na(linedp.score, 0))
    
    SAT_linedp.result = SAT_linedp.result %>% group_by(filename) %>% arrange(-linedp.score, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_linedp.result = select(SAT_linedp.result, filename, line.number, rank)
    
    
    # DeeplineDP
    temp.deeplinedp.result = deeplinedp.sorted %>% filter(test == rel) %>% select(filename, line.number, attention_score)
    temp.deeplinedp.result$filename <- gsub("/", "_", temp.deeplinedp.result$filename)
    names(temp.deeplinedp.result) = c('filename','line.number','deeplinedp.score')
    
    SAT_deeplinedp.result = SAT.base.info
    SAT_deeplinedp.result = left_join(SAT_deeplinedp.result, temp.deeplinedp.result, by=c('filename', 'line.number')) %>% mutate(
      deeplinedp.score = replace_na(deeplinedp.score, 0))
    
    SAT_deeplinedp.result = SAT_deeplinedp.result %>% group_by(filename) %>% arrange(-deeplinedp.score, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_deeplinedp.result = select(SAT_deeplinedp.result, filename, line.number, rank)
    
    

    LLM4SA = read.csv(paste0(LLM.result.dir,rel,'-line-lvl-result.txt'),quote="")
    LLM4SA = LLM4SA %>% mutate(Priority = case_when(
      PMD_prediction_result == 'real bug' ~ 3,
      PMD_prediction_result == 'false alarm' ~ 1,
      PMD_prediction_result == 'unknown' ~ 2,))
    LLM4SA = select(LLM4SA,filename, line_number, Priority)
    names(LLM4SA) = c('filename','line.number','LLM4SA.score')
    
    SAT_LLM4SA.result = SAT.base.info
    SAT_LLM4SA.result = left_join(SAT_LLM4SA.result, LLM4SA, by=c('filename', 'line.number')) %>% mutate(
      LLM4SA.score = replace_na(LLM4SA.score, 0))
    SAT_LLM4SA.result = SAT_LLM4SA.result %>% group_by(filename) %>% arrange(Priority, -LLM4SA.score, line.number, .by_group = TRUE) %>% mutate(rank = row_number()) %>% ungroup()
    SAT_LLM4SA.result = select(SAT_LLM4SA.result, filename, line.number, rank)
    
    
    

    SAT.eval.result = get.top.N.TP(SAT.result, cur.df.file, top_n, rel) 
    SAT_F.eval.result = get.top.N.TP(SAT_F.result, cur.df.file, top_n, rel) 
    SAT_glance.lr.eval.result = get.top.N.TP(SAT_glance.lr.result, cur.df.file, top_n, rel)
    SAT_ngram.eval.result = get.top.N.TP(SAT_ngram.result, cur.df.file, top_n, rel) 
    SAT_linedp.eval.result = get.top.N.TP(SAT_linedp.result, cur.df.file, top_n, rel) 
    SAT_deeplinedp.eval.result = get.top.N.TP(SAT_deeplinedp.result, cur.df.file, top_n, rel)
    SAT_LLM4SA.eval.result = get.top.N.TP(SAT_LLM4SA.result, cur.df.file, top_n, rel)
    
    SAT.result.df = rbind(SAT.result.df, SAT.eval.result)
    SAT_F.result.df  =  rbind(SAT_F.result.df, SAT_F.eval.result)
    SAT_ngram.result.df = rbind(SAT_ngram.result.df, SAT_ngram.eval.result)
    SAT_glance.lr.result.df = rbind(SAT_glance.lr.result.df, SAT_glance.lr.eval.result)
    SAT_linedp.result.df  =  rbind(SAT_linedp.result.df, SAT_linedp.eval.result)
    SAT_deeplinedp.result.df  =  rbind(SAT_deeplinedp.result.df, SAT_deeplinedp.eval.result)
    SAT_LLM4SA.result.df  =  rbind(SAT_LLM4SA.result.df, SAT_LLM4SA.eval.result)
    
    print(paste0('finished ', rel))
  }
  
  combined_df = rbind(SAT.result.df, SAT_F.result.df, SAT_glance.lr.result.df, SAT_linedp.result.df, SAT_ngram.result.df, SAT_deeplinedp.result.df,SAT_LLM4SA.result.df) %>% distinct()
  
  combined_df$SAT_F =0
  combined_df$SAT_GLANCE_LR =0
  combined_df$SAT =0
  combined_df$SAT_linedp=0
  combined_df$SAT_ngram=0
  combined_df$SAT_deeplinedp=0
  combined_df$SAT_LLM4SA =0
  

  combined_df$SAT_F <- ifelse(combined_df$element %in% SAT_F.result.df$element, 1, 0)
  combined_df$SAT_GLANCE_LR <- ifelse(combined_df$element %in% SAT_glance.lr.result.df$element, 1, 0)
  combined_df$SAT <- ifelse(combined_df$element %in% SAT.result.df$element, 1, 0)
  combined_df$SAT_linedp <- ifelse(combined_df$element %in% SAT_linedp.result.df$element, 1, 0)
  combined_df$SAT_ngram <- ifelse(combined_df$element %in% SAT_ngram.result.df$element, 1, 0)
  combined_df$SAT_deeplinedp <- ifelse(combined_df$element %in% SAT_deeplinedp.result.df$element, 1, 0)
  combined_df$SAT_LLM4SA <- ifelse(combined_df$element %in% SAT_LLM4SA.result.df$element, 1, 0)
  combined_df = combined_df[,-1]

  names(combined_df)= c('APART-F', 'GLANCE-LR','SAT','LineDP','N-gram','DeepLineDP', 'LLM4SA')
  
  lt = list()
  for(col in colnames(combined_df)) {
    lt[[col]] = rownames(combined_df)[combined_df[[col]] == 1]
  }
  

  m = make_comb_mat(lt)
  
  cs = comb_size(m)
  

  if (!dir.exists(paste0(Dis.result.dir, SATname, '/'))) {
    if (!dir.create(paste0(Dis.result.dir, SATname, '/'), recursive = TRUE)) {
      stop("Could not create directory: ", paste0(Dis.result.dir, SATname, '/'))
    }
  }
  
  png(paste0(Dis.result.dir, SATname, '/', SATname, "_Upset_plot.png"),
      width=3000, height=2000,res = 300)
  
  p = UpSet(m, 
        comb_order = order(comb_degree(m), -cs),
        top_annotation = HeatmapAnnotation(
          "Intersection size" = anno_barplot(cs, 
                                             border = FALSE, 
                                             gp = gpar(fill = "black"), 
                                             height = unit(12, "cm"),
                                             add_numbers = TRUE,
          ), 
          
          annotation_name_side = "left", 
          annotation_name_rot = 90),
        left_annotation = NULL, 
        right_annotation = NULL,
        show_row_names = TRUE,
        row_names_gp = gpar(fontsize = 9))
  print(p)
  dev.off()
  
}

all_eval_releases = c('activemq-5.2.0','activemq-5.3.0','activemq-5.8.0',
                      'camel-2.10.0','camel-2.11.0', 
                      'derby-10.5.1.1',
                      'groovy-1_6_BETA_2', 
                      'hbase-0.95.2',
                      'hive-0.12.0', 
                      'jruby-1.5.0','jruby-1.7.0.preview1',
                      'lucene-3.0.0','lucene-3.1',
                      'wicket-1.5.3')
                                            



######## top_n = 1 
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_PMD.result.dir, PMD.result.dir, "PMD", line.ground.truth, lineLevelMetrics, save.fig.dir, 20, 15, 1, Dis.figures.path.top1)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_CheckStyle.result.dir, CheckStyle.result.dir, "CheckStyle", line.ground.truth, lineLevelMetrics, save.fig.dir, 60, 75, 1, Dis.figures.path.top1) 
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_ErrorProne.result.dir, ErrorProne.result.dir, "ErrorProne", line.ground.truth, lineLevelMetrics, save.fig.dir, 30, 30, 1, Dis.figures.path.top1)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Spotbugs.result.dir, Spotbugs.result.dir, "Spotbugs", line.ground.truth, lineLevelMetrics, save.fig.dir, 10, 20, 1, Dis.figures.path.top1)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Betterscan_ce.result.dir, betterscan_ce.result.dir, "Betterscan-ce", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15, 1, Dis.figures.path.top1)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Codacy.result.dir, codacy.result.dir, "Codacy", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15, 1, Dis.figures.path.top1)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Codeql.result.dir, codeql.result.dir, "Codeql", line.ground.truth, lineLevelMetrics, save.fig.dir,20,15, 1, Dis.figures.path.top1)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Sonarqube.result.dir,sonarqube.result.dir, "Sonarqube", line.ground.truth, lineLevelMetrics, save.fig.dir,15,20, 1, Dis.figures.path.top1)

######## top_n = 3 
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_PMD.result.dir, PMD.result.dir, "PMD", line.ground.truth, lineLevelMetrics, save.fig.dir, 20, 15, 3, Dis.figures.path.top3)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_CheckStyle.result.dir, CheckStyle.result.dir, "CheckStyle", line.ground.truth, lineLevelMetrics, save.fig.dir, 60, 75, 3, Dis.figures.path.top3) 
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_ErrorProne.result.dir, ErrorProne.result.dir, "ErrorProne", line.ground.truth, lineLevelMetrics, save.fig.dir, 30, 30, 3, Dis.figures.path.top3)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Spotbugs.result.dir, Spotbugs.result.dir, "Spotbugs", line.ground.truth, lineLevelMetrics, save.fig.dir, 10, 20, 3, Dis.figures.path.top3)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Betterscan_ce.result.dir, betterscan_ce.result.dir, "Betterscan-ce", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15, 3, Dis.figures.path.top3)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Codacy.result.dir, codacy.result.dir, "Codacy", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15, 3, Dis.figures.path.top3)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Codeql.result.dir, codeql.result.dir, "Codeql", line.ground.truth, lineLevelMetrics, save.fig.dir,20,15, 3, Dis.figures.path.top3)
get.SAT.result.only.for.actionable.warning(all_eval_releases, LLM_Sonarqube.result.dir,sonarqube.result.dir, "Sonarqube", line.ground.truth, lineLevelMetrics, save.fig.dir,15,20, 3, Dis.figures.path.top3)
