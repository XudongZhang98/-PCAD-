library(readxl)
library(tidyverse)
library(table1)
library(xlsx)
shuju <- read_excel("数据.xlsx") %>% 
  na.omit() %>% 
  mutate(`年龄`=as.numeric(str_sub(`年龄`,1,2))) %>% 
  select(!"Gensini评分")

for (i in c(1,2,6:8)) {
  shuju[[i]] <- as.factor(shuju[[i]])
}
shuju$冠心病 <- factor(shuju$冠心病,levels = c("1","0"))
shuju$高血压 <- recode(shuju$高血压,"2"="0")
shuju$吸烟 <- recode(shuju$吸烟,"2"="0")
shuju$饮酒 <- recode(shuju$饮酒,"2"="0")
shuju$性别 <- recode(shuju$性别,"女"="0","男"="1")
write.xlsx(shuju,file = "数据.xlsx")
shuju$高血压 <- factor(shuju$高血压,levels = c("0","1"))
shuju$吸烟 <- factor(shuju$吸烟,levels = c("0","1"))
shuju$饮酒 <- factor(shuju$饮酒,levels = c("0","1"))
shuju$性别 <- factor(shuju$性别,levels = c("0","1"))
#########单因素#########
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}
statistic <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    statistic <- t.test(y ~ g)$statistic
  } else {
    # For categorical variables, perform a chi-squared test of independence
    statistic <- chisq.test(table(y, g))$statistic
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("",round(statistic,digits = 2))
}
table1(~ . | `冠心病`, 
       data=shuju, 
       overall=NULL,
       extra.col = list('x/t'=statistic,
                        'P-value'=pvalue)
       )
table1(~ . | `冠心病`, 
       data=shuju
)
#########两组三个指标对比箱式图#########
library(reshape2)
library(ggplot2)
library(ggpubr)
library(showtext)
showtext_auto()
shuju_1 <- shuju %>% 
  select(c("冠心病","TyG","TG/HDL-C","METS-IR")) %>% 
  reshape2::melt(id.vars = "冠心病")
my_comparisons <- list(c("0","1"))
ggplot(data = shuju,aes(x = `冠心病`, 
                          y = `TyG`, 
                          fill = `冠心病`))+ 
  scale_fill_manual(values = c("#CB997E","#6B705C")) +
  geom_violin(alpha = 0.4, position = position_dodge(width = .75),
              size = 0.8, color="black") +
  geom_boxplot(notch = TRUE, outlier.size = -1, 
               color="black", lwd=0.8, alpha = 0.7) +
  geom_point(shape = 21, size=2, 
             position = position_jitterdodge(), 
             color="black", alpha = 1) +
  scale_x_discrete(labels = c("1"="冠心病组","0"="非冠心病组"))+
  theme_bw() + 
  labs(x=NULL)+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.ticks = element_line(linewidth =0.2, color="black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  stat_compare_means(method="t.test",hide.ns = F,comparisons =my_comparisons,label="p.signif")

ggplot(data = shuju,aes(x = `冠心病`, 
                        y = `TG/HDL-C`, 
                        fill = `冠心病`))+ 
  scale_fill_manual(values = c("#CB997E","#6B705C")) +
  geom_violin(alpha = 0.4, position = position_dodge(width = .75),
              size = 0.8, color="black") +
  geom_boxplot(notch = TRUE, outlier.size = -1, 
               color="black", lwd=0.8, alpha = 0.7) +
  geom_point(shape = 21, size=2, 
             position = position_jitterdodge(), 
             color="black", alpha = 1) +
  scale_x_discrete(labels = c("1"="冠心病组","0"="非冠心病组"))+
  theme_bw() + 
  labs(x=NULL)+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.ticks = element_line(linewidth =0.2, color="black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  stat_compare_means(method="t.test",hide.ns = F,comparisons =my_comparisons,label="p.signif")

ggplot(data = shuju,aes(x = `冠心病`, 
                        y = `METS-IR`, 
                        fill = `冠心病`))+ 
  scale_fill_manual(values = c("#CB997E","#6B705C")) +
  geom_violin(alpha = 0.4, position = position_dodge(width = .75),
              size = 0.8, color="black") +
  geom_boxplot(notch = TRUE, outlier.size = -1, 
               color="black", lwd=0.8, alpha = 0.7) +
  geom_point(shape = 21, size=2, 
             position = position_jitterdodge(), 
             color="black", alpha = 1) +
  scale_x_discrete(labels = c("1"="冠心病组","0"="非冠心病组"))+
  theme_bw() + 
  labs(x=NULL)+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.ticks = element_line(linewidth =0.2, color="black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  stat_compare_means(method="t.test",hide.ns = F,comparisons =my_comparisons,label="p.signif")
#########logistic########
library(broom)
library(epiDisplay)
library(tidyverse)
library(table1)
options(scipen = 200)#不使用科学计数法
uni_glm_model <- 
  function(x){
    FML <- as.formula(paste0("冠心病==1~",x))
    glm1 <- glm(FML,data = shuju,family = binomial)
    tidy <- tidy(glm1) 
    tidy <- mutate(tidy,OR=round(exp(coef(glm1)),3)) 
    tidy <- mutate(tidy,CI=paste0(round(exp(coef(glm1)-1.96*tidy$std.error),3),'-',round(exp(coef(glm1)+1.96*tidy$std.error),3))) %>% 
      dplyr::select(!statistic)
    tidy <- tidy[-1,]
    tidy$estimate <- round(tidy$estimate,3)
    tidy$std.error <- round(tidy$std.error,3)
    tidy$p.value <- format.pval(tidy$p.value,digits = 3,eps = 0.0001)
    su <- as.data.frame(summary(glm1)[["coefficients"]])[-1,]
    tidy <- tidy %>% 
      mutate(wald=as.numeric(su$`z value`*su$`z value`))
    return(tidy)  
  }
#########基本资料和临床检查#########
x.names <- c("年龄+
             体重+
             吸烟+
             饮酒+
             高血压+
             中性粒细胞计数+
             单核细胞计数+
             嗜酸性细胞计数+
             白细胞计数+
             血红蛋白计数+
             血小板计数+
             尿素+
             门冬氨酸转移酶+
            丙氨酸氨基转移酶+
              碱性磷酸酶+
              谷氨酰转肽酶+
              丙氨酸氨基转移酶")

#变量带入循环函数
uni_glm <- lapply(x.names,uni_glm_model)
#批量输出结果合并
library(plyr)
excel <- ldply(uni_glm,data.frame)
write.xlsx(excel,file = "第一个logistic.xlsx")
#森林图
library(forestplot)
excel <- excel[excel$term%in%c("年龄","吸烟1","高血压1","嗜酸性细胞计数","白细胞计数","血红蛋白计数","门冬氨酸转移酶","丙氨酸氨基转移酶","碱性磷酸酶","谷氨酰转肽酶"),]
excel$term <- c("年龄","吸烟（是）","高血压（是）","嗜酸性细胞计数","白细胞计数","血红蛋白计数","门冬氨酸转移酶","丙氨酸氨基转移酶","碱性磷酸酶","谷氨酰转肽酶")
excel$p.value <- c("<0.001","0.035","<0.001","0.034","0.018","<0.001","<0.001","0.020","0.049","0.004")
a <- as.data.frame(str_split_fixed(excel$CI,"-",2))
excel <- excel %>% 
  mutate(HR.95L=a[,1],
         HR.95H=a[,2],
         ORCI=paste0(OR," ","(",HR.95L,"-",HR.95H,")"))

excelx <- rbind(c(NA,NA,NA,"P值",NA,NA,NA,NA,NA,NA,"OR (95%CI)"),excel)
labeltext <- as.matrix(excelx[,c(1,4,11)])
forestplot(labeltext,  # 图形文本部分 
           mean = excelx$OR,  # 图形 HR 部分 
           lower = excelx$HR.95L, # 95%CI下限           
           upper = excelx$HR.95H, # 95%CI上限
           zero = 1.0, lwd.zero = 2, # 设置无效线的横坐标和线条宽度
           xticks = c(0,1.0,2,3),  # 设置x轴刻度ticks
           lwd.xaxis = 2, # 设置x轴的宽度
           boxsize = 0.1, # 设置Box的大小，保持大小一样
           lty.ci = 1, lwd.ci = 2, # 森林图置信区间的线条类型和宽度
           ci.vertices = TRUE,  # 森林图置信区间两端添加小竖线，默认FALSE
           ci.vertices.height = 0.1, # 设置森林图置信区间两端小竖线的高度，默认0.1
           align = "l",  # 文字对齐方式，"l"、"r"和"c"分别为左对齐，右对齐和居中对齐
           # 向量长度等于图表行数，TRUE为行加粗，且该行下添加一条直线，但在未设置颜色时不显示。
           txt_gp = fpTxtGp(label = gpar(cex = 1), # 设置文本字体大小
                            ticks = gpar(cex = 1), # 设置坐标轴刻度大小
                            xlab = gpar(cex = 0.5)),# 设置坐标轴标题大小
           colgap=unit(0.1,"cm"))
#######模型1#######
x.names <- c("性别+
              年龄+
              身高+
             体重+
             吸烟+
             饮酒+
             高血压+
             TyG")
x.names <- c("性别+
              年龄+
              身高+
             体重+
             吸烟+
             饮酒+
             高血压+
             `TG/HDL-C`")
x.names <- c("性别+
              年龄+
              身高+
             体重+
             吸烟+
             饮酒+
             高血压+
             `METS-IR`")

#变量带入循环函数
uni_glm <- lapply(x.names,uni_glm_model)
#批量输出结果合并
library(plyr)
excel <- ldply(uni_glm,data.frame)
#######模型2##########
x.names <- c("性别+
              年龄+
              身高+
             体重+
             吸烟+
             饮酒+
             高血压+
             中性粒细胞计数+
             淋巴细胞计数+
             单核细胞计数+
             嗜酸性细胞计数+
             红细胞计数+
             白细胞计数+
             血红蛋白计数+
             血小板计数+
             尿素+
             肌酐+
             尿酸+
             总胆红素+
             门冬氨酸转移酶+
            丙氨酸氨基转移酶+
              碱性磷酸酶+
              谷氨酰转肽酶+
              丙氨酸氨基转移酶+
             TyG")

x.names <- c("性别+
              年龄+
              身高+
             体重+
             吸烟+
             饮酒+
             高血压+
             中性粒细胞计数+
             淋巴细胞计数+
             单核细胞计数+
             嗜酸性细胞计数+
             红细胞计数+
             白细胞计数+
             血红蛋白计数+
             血小板计数+
             尿素+
             肌酐+
             尿酸+
             总胆红素+
             门冬氨酸转移酶+
            丙氨酸氨基转移酶+
              碱性磷酸酶+
              谷氨酰转肽酶+
              丙氨酸氨基转移酶+
             `TG/HDL-C`")

x.names <- c("性别+
              年龄+
              身高+
             体重+
             吸烟+
             饮酒+
             高血压+
             中性粒细胞计数+
             淋巴细胞计数+
             单核细胞计数+
             嗜酸性细胞计数+
             红细胞计数+
             白细胞计数+
             血红蛋白计数+
             血小板计数+
             尿素+
             肌酐+
             尿酸+
             总胆红素+
             门冬氨酸转移酶+
            丙氨酸氨基转移酶+
              碱性磷酸酶+
              谷氨酰转肽酶+
             `METS-IR`")
#变量带入循环函数
uni_glm <- lapply(x.names,uni_glm_model)
#批量输出结果合并
library(plyr)
excel <- ldply(uni_glm,data.frame)
write.xlsx(shuju,file = "最终数据.xlsx")

#########3个指标ROC曲线########
library(pROC)
library(showtext)
library(caret)
showtext_auto()
tygroc <- roc(factor(shuju$冠心病,levels = c("0","1")),shuju$TyG,auc=TRUE,ci=TRUE)
tyg <- factor(if_else(shuju$TyG>8.783,"1","0"),levels = c("0","1"))
tygmatrix <- confusionMatrix(tyg,shuju$冠心病)
plot(tygroc,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="skyblue",
     main='TyG')

tygroc <- roc(factor(shuju$冠心病,levels = c("0","1")),shuju$`TG/HDL-C`,auc=TRUE,ci=TRUE)
tg <- factor(if_else(shuju$`TG/HDL-C`>2.952,"1","0"),levels = c("0","1"))
tgmatrix <- confusionMatrix(tg,shuju$冠心病)
plot(tygroc,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="skyblue",
     main='TG/HDL-C')

tygroc <- roc(factor(shuju$冠心病,levels = c("0","1")),shuju$`METS-IR`,auc=TRUE,ci=TRUE)
met <- factor(if_else(shuju$`METS-IR`>41.351,"1","0"),levels = c("0","1"))
metmatrix <- confusionMatrix(met,shuju$冠心病)
plot(tygroc,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="skyblue",
     main='METS-IR')


###########严重程度##########
shuju_2 <- read_excel("数据2.xlsx") %>% 
  na.omit() %>% 
  mutate(`年龄`=as.numeric(str_sub(`年龄`,1,2))) %>% 
  filter(冠心病=="1")
for (i in c(1,3,7:9)) {
  shuju_2[[i]] <- as.factor(shuju_2[[i]])
}
shuju_2$冠心病 <- factor(shuju_2$冠心病,levels = c("1","0"))
shuju_2$高血压 <- recode(shuju_2$高血压,"2"="0")
shuju_2$吸烟 <- recode(shuju_2$吸烟,"2"="0")
shuju_2$饮酒 <- recode(shuju_2$饮酒,"2"="0")
shuju_2$性别 <- recode(shuju_2$性别,"女"="0","男"="1")
shuju_2$高血压 <- factor(shuju_2$高血压,levels = c("0","1"))
shuju_2$吸烟 <- factor(shuju_2$吸烟,levels = c("0","1"))
shuju_2$饮酒 <- factor(shuju_2$饮酒,levels = c("0","1"))
shuju_2$性别 <- factor(shuju_2$性别,levels = c("0","1"))
shuju_2$Gensini评分 <- as.numeric(shuju_2$Gensini评分)

cor.test(shuju_2$Gensini评分,shuju_2$TyG)
cor.test(shuju_2$Gensini评分,shuju_2$`TG/HDL-C`)
cor.test(shuju_2$Gensini评分,shuju_2$`METS-IR`)

p1 <- ggplot(data = shuju_2,aes(x = `性别`, 
                        y = `Gensini评分`, 
                        fill = `性别`))+ 
  scale_fill_manual(values = c("#FCA311","#13213C")) +
  #geom_violin(alpha = 0.4, position = position_dodge(width = .75),
              #size = 0.8, color="black") +
  geom_boxplot(notch = F, outlier.size = -1, 
               color="black", lwd=0.8, alpha = 0.7) +
  geom_point(shape = 21, size=2, 
             position = position_jitterdodge(), 
             color="black", alpha = 1) +
  scale_x_discrete(labels = c("1"="男性","0"="女性"))+
  theme_bw() + 
  labs(x=NULL)+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.ticks = element_line(linewidth =0.2, color="black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  stat_compare_means(method="t.test",hide.ns = F,comparisons =my_comparisons,label="p.signif")


p2 <- ggplot(data = shuju_2,aes(x = `吸烟`, 
                                y = `Gensini评分`, 
                                fill = `吸烟`))+ 
  scale_fill_manual(values = c("#FCA311","#13213C")) +
  #geom_violin(alpha = 0.4, position = position_dodge(width = .75),
  #size = 0.8, color="black") +
  geom_boxplot(notch = F, outlier.size = -1, 
               color="black", lwd=0.8, alpha = 0.7) +
  geom_point(shape = 21, size=2, 
             position = position_jitterdodge(), 
             color="black", alpha = 1) +
  scale_x_discrete(labels = c("1"="吸烟","0"="不吸烟"))+
  theme_bw() + 
  labs(x=NULL)+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.ticks = element_line(linewidth =0.2, color="black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  stat_compare_means(method="t.test",hide.ns = F,comparisons =my_comparisons,label="p.signif")

p3 <- ggplot(data = shuju_2,aes(x = `饮酒`, 
                                y = `Gensini评分`, 
                                fill = `饮酒`))+ 
  scale_fill_manual(values = c("#FCA311","#13213C")) +
  #geom_violin(alpha = 0.4, position = position_dodge(width = .75),
  #size = 0.8, color="black") +
  geom_boxplot(notch = F, outlier.size = -1, 
               color="black", lwd=0.8, alpha = 0.7) +
  geom_point(shape = 21, size=2, 
             position = position_jitterdodge(), 
             color="black", alpha = 1) +
  scale_x_discrete(labels = c("1"="饮酒","0"="不饮酒"))+
  theme_bw() + 
  labs(x=NULL)+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.ticks = element_line(linewidth =0.2, color="black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  stat_compare_means(method="t.test",hide.ns = F,comparisons =my_comparisons,label="p.signif")

p4 <- ggplot(data = shuju_2,aes(x = `高血压`, 
                                y = `Gensini评分`, 
                                fill = `高血压`))+ 
  scale_fill_manual(values = c("#FCA311","#13213C")) +
  #geom_violin(alpha = 0.4, position = position_dodge(width = .75),
  #size = 0.8, color="black") +
  geom_boxplot(notch = F, outlier.size = -1, 
               color="black", lwd=0.8, alpha = 0.7) +
  geom_point(shape = 21, size=2, 
             position = position_jitterdodge(), 
             color="black", alpha = 1) +
  scale_x_discrete(labels = c("1"="患高血压","0"="不患高血压"))+
  theme_bw() + 
  labs(x=NULL)+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.ticks = element_line(linewidth =0.2, color="black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  stat_compare_means(method="t.test",hide.ns = F,comparisons =my_comparisons,label="p.signif")
p4
library(cowplot)
cowplot::plot_grid(p1,p2,p3,p4,ncol = 2)

#########相关性热图#########
library(corrplot)
td <- shuju_2 %>% 
  dplyr::select(!c("冠心病","性别","吸烟","饮酒","高血压","冠心病分组"))
tdc <- cor(td, method="pearson")
testRes <- cor.mtest(td, method="pearson",conf.level = 0.95)
addcol <- colorRampPalette(rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')))
addcol <- colorRampPalette(rev(c("red", "white", "blue")))
corrplot(tdc, method = "color", col = addcol(100), 
         tl.col = "black", tl.cex = 0.8, tl.srt = 45,tl.pos = "lt",
         p.mat = testRes$p, diag = T, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 1,
         insig = 'label_sig', pch.col = 'grey20', order = 'original')
corrplot(tdc, method = "number", type = "lower", col=addcol(100),
         tl.col = "n", tl.cex = 0.8, tl.pos = "n",number.font = 0.5,number.cex = 0.5,
         add = T)
dev.off()
#######相关性散点图########
p1 <- ggplot(shuju_2,aes(x=Gensini评分,y=TyG))+
  geom_jitter(color="#7FB5E0")+
  geom_smooth(method="lm", se=T)+
  stat_cor(data=shuju_2,aes(x=Gensini评分,y=TyG),method = "pearson")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x="Gensini评分")

p2 <- ggplot(shuju_2,aes(x=Gensini评分,y=`METS-IR`))+
  geom_jitter(color="#90D8F4")+
  geom_smooth(method="lm", se=T)+
  stat_cor(data=shuju_2,aes(x=Gensini评分,y=`METS-IR`),method = "pearson")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x="Gensini评分")
cowplot::plot_grid(p1,p2,ncol = 2)
devtools::install_github("Github-Yilei/ggcor")

library(ggcor)
quickcor(td, cor.test = TRUE) +
  geom_square(data = get_data(type = "lower", show.diag = FALSE),colour=addcol(253)) +
  geom_mark(data = get_data(type = "upper", show.diag = FALSE), size = 2,fontface="bold") +
  geom_abline(slope = -1, intercept = 24)









