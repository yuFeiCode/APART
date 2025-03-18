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

PMD.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/PMD/'
CheckStyle.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/CheckStyle/'
ErrorProne.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/Errorprone/test/'
Spotbugs.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/Spotbugs/'

betterscan_ce.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/betterscan-ce/'
codacy.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/codacy/'
codeql.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/codeql/'
sonarqube.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)/sonarqube/'
# betterscan_ce.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-06-28)add-priority-for-SAT/betterscan-ce/'
# codacy.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-06-28)add-priority-for-SAT/codacy/'
# codeql.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-06-28)add-priority-for-SAT/codeql/'
# sonarqube.result.dir = 'D:/Gitee-code/enhance_SATs/SAT_tool_result/(2024-06-28)add-priority-for-SAT/sonarqube/'


dis.save.fig.dir = 'D:/Gitee-code/enhance_SATs/figures/(2024-10-22)Dis_figures/'


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
  # x$rank <- paste("R",ranking[as.character(gsub("-", ".", x$variable))])
  return(x)
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

# Define normalization function
normalize <- function(x) {
  min_x <- min(x)
  max_x <- max(x)
  if (max_x == min_x) {
    return(rep(0, length(x)))
  }
  return((x - min_x) / (max_x - min_x))
}

# 定义 AUC 计算函数
calculate_auc <- function(response, predictor) {
  # 首先检查 predictor 是否只有一个类别
  if (length(unique(predictor)) == 1) {
    return(0.5)  # predictor 只有一个类别时返回 0.5
  }
  
  if (length(unique(response)) == 2) {
    # 二分类情况
    return(as.numeric(auc(roc(response, predictor))))
  } else {
    # 多分类情况
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
    SAT.result = select(SAT.result,'filename','line_number','Priority')
    names(SAT.result) = c('filename','line.number','Priority')
    
    
    #用NFC*NT增强的排序
    SAT_F.result = SAT.result
    #  不做归一化
    # GLANCE_F = lineLevelMetrics %>% filter(test == rel) %>% group_by(filename) %>% mutate(severity = NFC*NT)
    # 做归一化
    GLANCE_F = lineLevelMetrics %>% filter(test == rel) %>% group_by(filename) %>% mutate(severity = NFC*NT) %>% mutate(severity_normalized = normalize(severity))
    
    GLANCE_F = select(GLANCE_F, filename, line.number, severity_normalized)
    # SAT_F.result = left_join(SAT_F.result, GLANCE_F, by=c('filename', 'line.number'))
    # SAT_F.result = filter(SAT_F.result, !is.na(severity))
    # 先试一下 NA行直接将severity为0
    SAT_F.result = left_join(SAT_F.result, GLANCE_F, by=c('filename', 'line.number')) %>% mutate(
      severity_normalized = replace_na(severity_normalized, 0))
    
    SAT_F.result = left_join(SAT_F.result, cur.df.file, by = c("filename","line.number"))
    SAT_F.result = filter(SAT_F.result, !is.na(line.level.ground.truth))
  
    # 首先进行筛选,就是能算AUC的文件起码要满足俩个条件：
    # 1. priority的类别要>=2
    # 2. line.level.ground.truth的类别要 =2
    filtered.SAT_F.result <- SAT_F.result %>% group_by(filename) %>% filter(n_distinct(line.level.ground.truth) == 2) %>%
      ungroup()
    
    ######################## 不做归一化的处理 ####################
    # 计算 AUC
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
    No_nor = No_nor %>% mutate(Difference = somersD_SAT_F - somersD_SAT) %>% mutate(tool = SATname)
    
    all.release.result = rbind(all.release.result, No_nor)
    # nor.total.result <<- rbind(nor.total.result, No_nor)
    
    print(paste0('finished ', rel))
  }
  
  ############## release-level median ###############
  # sum.no.nor.result = all.release.result %>% summarise(auc_SAT = median(auc_SAT), auc_SAT_F = median(auc_SAT_F), .by=test)
  
  ############ release-level mean #############
  sum.no.nor.result = all.release.result %>% summarise(Difference = mean(Difference), .by=test) %>% mutate(tool = SATname)
  # print(nrow(sum.no.nor.result))
  
  # sum.no.nor.result = sum.no.nor.result %>% select(auc_SAT, auc_SAT_F) %>%
  #   pivot_longer(cols = c(auc_SAT, auc_SAT_F),names_to = "metric", values_to = "value") %>%
  #   mutate(model = case_when(
  #     metric == "auc_SAT" ~ "SAT",
  #     metric == "auc_SAT_F" ~ "SAT-F"
  #   )) %>%
  #   select(model, value) %>%
  #   arrange(model) %>% mutate(tool = SATname)
  
  ############# file-level ################
  # sum.no.nor.result = all.release.result %>% select(auc_SAT, auc_SAT_F) %>%
  #   pivot_longer(cols = c(auc_SAT, auc_SAT_F),names_to = "metric", values_to = "value") %>%
  #   mutate(model = case_when(
  #     metric == "auc_SAT" ~ "SAT",
  #     metric == "auc_SAT_F" ~ "SAT-F"
  #   )) %>%
  #   select(model, value) %>%
  #   arrange(model) %>% mutate(tool = SATname)
  
  nor.total.result <<- rbind(nor.total.result, sum.no.nor.result)
}


all_eval_releases = c('activemq-5.2.0','activemq-5.3.0','activemq-5.8.0',
                      'camel-2.10.0','camel-2.11.0', 
                      'derby-10.5.1.1',
                      'groovy-1_6_BETA_2', 
                      'hbase-0.95.2',
                      'hive-0.12.0', 
                      'jruby-1.5.0','jruby-1.7.0.preview1',
                      'lucene-3.0.0','lucene-3.1','wicket-1.5.3')

# 八个工具在未归一化上的总结果
nor.total.result = NULL

get.SAT.result.only.for.actionable.warning(all_eval_releases, PMD.result.dir, "PMD", line.ground.truth, lineLevelMetrics, save.fig.dir, 20, 15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, CheckStyle.result.dir, "CheckStyle", line.ground.truth, lineLevelMetrics, save.fig.dir, 60, 75) 
get.SAT.result.only.for.actionable.warning(all_eval_releases, ErrorProne.result.dir, "ErrorProne", line.ground.truth, lineLevelMetrics, save.fig.dir, 30, 30)
get.SAT.result.only.for.actionable.warning(all_eval_releases, Spotbugs.result.dir, "Spotbugs", line.ground.truth, lineLevelMetrics, save.fig.dir, 10, 20)
get.SAT.result.only.for.actionable.warning(all_eval_releases, betterscan_ce.result.dir, "Betterscan-ce", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, codacy.result.dir, "Codacy", line.ground.truth, lineLevelMetrics, save.fig.dir,10,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, codeql.result.dir, "Codeql", line.ground.truth, lineLevelMetrics, save.fig.dir,20,15)
get.SAT.result.only.for.actionable.warning(all_eval_releases, sonarqube.result.dir, "Sonarqube", line.ground.truth, lineLevelMetrics, save.fig.dir,15,20)

######### 普通的箱式图 #########
chazhi_box = ggplot(nor.total.result, aes(x=tool, y=Difference)) +
  geom_boxplot(fill="white") +
  stat_summary(fun = mean, geom = "point", shape = 17, size = 2, color = "red") +
  theme_minimal() +
  labs(x="Tool", y="Difference (D_SAT_F - D_SAT)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("D:/Gitee-code/enhance_SATs/figures/(2024-10-22)Dis_figures/（版本级 平均值 somer D）box_plot.png", plot = chazhi_box, width = 10, height = 6, dpi = 300)


######### 掐头去尾的箱式图 #############
chazhi_violin_box = ggplot(nor.total.result, aes(x=tool, y=Difference, fill=tool)) +
  geom_violin(trim=TRUE, alpha=0.5, linewidth=0.5, scale="width", adjust=1) +
  geom_boxplot(width=0.1, fill="white", color="black", alpha=0.7, linewidth=0.5) +
  stat_summary(fun=mean, geom="point", shape=17, size=1.5, color="red", show.legend=FALSE) +
  scale_fill_brewer(palette="Set3") +
  theme_minimal() +
  labs(x = '', y=expression(atop("Increase in Somers' D", "with NT*NFC added to warning line ranking"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=9, face="bold"),
        legend.position="none",
        axis.title.y = element_text(margin = margin(r = 20))) # 增加Y轴标签的右边距
ggsave("D:/Gitee-code/enhance_SATs/figures/(2024-10-22)Dis_figures/（版本级 平均值 somer D）violin_plot.png", plot = chazhi_violin_box, width = 10, height = 6, dpi = 300)




# Create violin plot
chazhi_violin_box = ggplot(nor.total.result, aes(x=tool, y=Difference)) +
  geom_violin(trim=TRUE, fill="lightblue", alpha=0.5) +
  geom_boxplot(width=0.1, fill="cornflowerblue") +
  stat_summary(fun=mean, geom="point", shape=17, size=2, color="red") +
  theme_minimal() +
  labs(x="Tool", y="Difference (D_SAT_F - D_SAT)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("D:/Gitee-code/enhance_SATs/figures/(2024-10-22)Dis_figures/（版本级 平均值 somer D）violin_plot.png", plot = chazhi_violin_box, width = 10, height = 6, dpi = 300)


# violin_box = ggviolin(nor.total.result, "tool", "value", color = "model", palette = c("#00AFBB", "#E7B800"), add = "boxplot") + 
#   scale_y_continuous(limits = c(0, 1))
# ggsave("D:/Gitee-code/enhance_SATs/figures/(2024-10-22)Dis_figures/（版本级 平均值）violin_plot.png", plot = violin_box, width = 10, height = 6, dpi = 300)
# 
# 
# normal_box = ggplot(nor.total.result, aes(x = tool, y = value, fill = model)) +
#   geom_boxplot() +
#   labs(x = "Tool", y = "Value", fill = "Model") +
#   theme_minimal() +
#   ylim(0, 2)
# 
# ggsave("D:/Gitee-code/enhance_SATs/figures/(2024-10-22)Dis_figures/（版本级 平均值）普通箱式图.png", plot = normal_box, width = 10, height = 6, dpi = 300)

# write.csv(nor.total.result, file = 'D:/Gitee-code/enhance_SATs/figures/(2024-10-22)Dis_figures/Dis_AUC.csv')
