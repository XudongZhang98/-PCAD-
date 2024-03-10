library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3measures)
library(mlr3viz)
library(precrec)
library(mlr3extralearners)
lrn()
########变量筛选#######
shuju_model <- shuju %>% 
  as.data.frame %>% 
  select(c("冠心病","年龄","吸烟","高血压","嗜酸性细胞计数","白细胞计数","血红蛋白计数","门冬氨酸转移酶","碱性磷酸酶","谷氨酰转肽酶","丙氨酸氨基转移酶"))

for (i in 1:ncol(shuju_model)) {
  shuju_model[[i]] <- as.numeric(shuju_model[[i]])
}
shuju_scale <- apply(shuju_model, 2, function(x){x=(x-min(x))/(max(x)-min(x))})
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3measures)
library(mlr3viz)
library(precrec)
library(mlr3extralearners)
lrn()
tsk_vte=as_task_classif(shuju,target = '冠心病',positive = "1")
library(glmnet)
shuju_model <- as.matrix(shuju_model)
alpha1_fit <- glmnet(shuju_scale[,-1],shuju_scale[,1],alpha=1,family="binomial")#连续性变量"gaussian"
plot(alpha1_fit,xvar="lambda",label=TRUE)
#交叉验证

alpha1.fit <- cv.glmnet(shuju_scale[,-1],shuju_scale[,1],alpha=1,family="binomial",
                        type.measure = "deviance")#连续型变量用“mes”
plot(alpha1.fit)
print(alpha1.fit)
coef(alpha1_fit,s=alpha1.fit$lambda.1se)
tidy_df <- broom::tidy(alpha1_fit) %>% 
  filter(!term=="(Intercept)")
tidy_cvdf <- broom::tidy(alpha1.fit)
library(ggplot2)
library(RColorBrewer)

#随便定义几个颜色，多找几个，防止不够用
mypalette <- viridis(10, alpha = 1, begin = 0, end = 1, direction = 1, option = "B")

ggplot(tidy_df, aes(log(lambda), estimate, group = term,color=term)) +
  geom_line(size=0.8)+
  geom_hline(yintercept = 0)+
  ylab("Coefficients")+
  scale_color_manual(name="变量",values = mypalette)+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.text = element_text(size=12))
shuju_model <- shuju %>% 
  as.data.frame() %>% 
  select(c("冠心病","年龄","吸烟","高血压","嗜酸性细胞计数","白细胞计数","血红蛋白计数","门冬氨酸转移酶","碱性磷酸酶","谷氨酰转肽酶","TyG","TG/HDL-C","METS-IR")) %>% 
  `names<-`(c('y',paste0("x",c(1:12)))) 
for (i in 1:ncol(shuju_model)) {
  shuju_model[[i]] <- as.numeric(shuju_model[[i]])
}
shuju_model <- apply(shuju_model, 2, function(x){x=(x-min(x))/(max(x)-min(x))}) %>% 
  as.data.frame()
for (i in c(1,3,4)) {
  shuju_model[[i]] <-make.names(shuju_model[[i]])
  shuju_model[[i]] <- as.factor(shuju_model[[i]])
}
#######创建任务
tsk_gxb <- as_task_classif(shuju_model,target = 'y',positive = "X1")
tsk_gxb
set.seed(123456)
split <-  partition(tsk_gxb, ratio = 0.8)
########随机森林#######
rf=lrn('classif.randomForest',predict_type='prob')
set.seed(123456)
rf$param_set
rf = lrn("classif.randomForest",
         ntree = to_tune(1,500),
         mtry = to_tune(1,12),
         nodesize = to_tune(1,10),
              predict_type='prob'
)
#定义超参数调整的策略，评价方式等
rf_tune <- ti(
  task = tsk_gxb,
  learner = rf,
  resampling = rsmp("cv",folds=10),
  measures = msr("classif.auc"),
  terminator = trm("run_time", secs =60)) #trm设置停止条件，secs为时间到了就停
# trm()
tuner=mlr3tuning::tnr("grid_search",resolution = 10,batch_size=10)

set.seed(123456)
#进行优化
tuner$optimize(rf_tune)
autoplot(rf_tune) #可视化结果
rf_tune$archive #调参过程的结果存放仓库#调参过程的结果存放仓库
#将参数值传递给模型
rf=lrn('classif.randomForest',predict_type='prob')
rf$param_set$values=rf_tune$result_learner_param_vals
#用调整好的参数进行训练
set.seed(123456)
rf$train(tsk_gxb,split$train)
#训练集混淆矩阵
rfpredictiontrain <- rf$predict(tsk_gxb,split$train)
rfcontrain <- confusionMatrix(rfpredictiontrain$confusion)
#测试集混淆矩阵
rfpredictiontest <- rf$predict(tsk_gxb,split$test)
rfcontest <- confusionMatrix(rfpredictiontest$confusion)
#训练集auc
rfroctrain <- roc(rfpredictiontrain$truth,rfpredictiontrain$prob[,1],auc=TRUE,ci=TRUE,levels=c("X0", "X1"))
#测试集AUC
rfroctest <- roc(rfpredictiontest$truth,rfpredictiontest$prob[,1],auc=TRUE,ci=TRUE,levels=c("X0", "X1"))
#####roc曲线
plot(rfroctrain,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#4EAB90",
     main='随机森林')

plot(rfroctest,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#4EAB90",
     main='随机森林')

#######xgboost#######
shuju_xgb <- shuju %>% 
  as.data.frame() %>% 
  select(c("冠心病","年龄","吸烟","高血压","嗜酸性细胞计数","白细胞计数","血红蛋白计数","门冬氨酸转移酶","碱性磷酸酶","谷氨酰转肽酶","TyG","TG/HDL-C","METS-IR")) %>% 
  `names<-`(c('y',paste0("x",c(1:12)))) 
for (i in 1:ncol(shuju_xgb)) {
  shuju_xgb[[i]] <- as.numeric(shuju_xgb[[i]])
}
shuju_xgb <- apply(shuju_xgb, 2, function(x){x=(x-min(x))/(max(x)-min(x))}) %>% 
  as.data.frame()
tsk_xgb <- as_task_classif(shuju_xgb,target = 'y',positive = "1")
xg = lrn("classif.xgboost",
            eta               = to_tune(1e-4, 1),
            nrounds           = to_tune(1, 5000),
            max_depth         = to_tune(1, 20),
            colsample_bytree  = to_tune(1e-1, 1),
            colsample_bylevel = to_tune(1e-1, 1),
            lambda            = to_tune(1e-3, 1e3, logscale = TRUE),
            alpha             = to_tune(1e-3, 1e3, logscale = TRUE),
            subsample         = to_tune(1e-1, 1),
            predict_type="prob")
#定义超参数调整的策略，评价方式等
xg_tune <- ti(
  task = tsk_xgb,
  learner = xg,
  resampling = rsmp("cv",folds=10),
  measures = msr("classif.auc"),
  terminator = trm("run_time", secs =200)) #trm设置停止条件，secs为时间到了就停
# trm()
tuner=mlr3tuning::tnr("grid_search",resolution = 10,batch_size=10)

set.seed(123456)
#进行优化
tuner$optimize(xg_tune)
autoplot(xg_tune) #可视化结果
xg_tune$archive #调参过程的结果存放仓库#调参过程的结果存放仓库
#将参数值传递给模型
xg=lrn('classif.xgboost',predict_type='prob')
xg$param_set$values=xg_tune$result_learner_param_vals
#用调整好的参数进行训练
set.seed(123456)
xg$train(tsk_xgb,split$train)
#训练集混淆矩阵
xgpredictiontrain <- xg$predict(tsk_xgb,split$train)
xgcontrain <- confusionMatrix(xgpredictiontrain$confusion)
#测试集混淆矩阵
xgpredictiontest <- xg$predict(tsk_xgb,split$test)
xgcontest <- confusionMatrix(xgpredictiontest$confusion)
#训练集auc
xgroctrain <- roc(xgpredictiontrain$truth,xgpredictiontrain$prob[,1],auc=TRUE,ci=TRUE,levels=c("0", "1"))
#测试集AUC
xgroctest <- roc(xgpredictiontest$truth,xgpredictiontest$prob[,1],auc=TRUE,ci=TRUE,levels=c("0", "1"))

#roc曲线
plot(xgroctrain,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#8EB69C",
     main='极度梯度提升树')

plot(xgroctest,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#8EB69C",
     main='极度梯度提升树')
#########支持向量机#########
svm=lrn('classif.svm',predict_type='prob')
set.seed(123456)
svm$param_set
svm = lrn('classif.svm',predict_type="prob",
         cost = to_tune(1e-5, 1e5, logscale = TRUE),
         gamma = to_tune(1e-5, 1e5, logscale = TRUE),
         kernel = "radial",
         type = "C-classification")
#定义超参数调整的策略，评价方式等
svm_tune <- ti(task = tsk_xgb,
              learner = svm,
              resampling = rsmp('cv',folds=10),
              measures = msr('classif.auc'),
              terminator = trm('run_time',secs=1500)) #trm设置停止条件，secs为时间到了就停
# trm()
tuner=mlr3tuning::tnr("grid_search",resolution = 10,batch_size=10)

set.seed(123456)
#进行优化
tuner$optimize(svm_tune)
autoplot(svm_tune) #可视化结果
svm_tune$archive #调参过程的结果存放仓库#调参过程的结果存放仓库
#将参数值传递给模型
svm=lrn('classif.svm',predict_type='prob')
svm$param_set$values=svm_tune$result_learner_param_vals
#用调整好的参数进行训练
set.seed(123456)
svm$train(tsk_xgb,split$train)
#训练集混淆矩阵
svmpredictiontrain <- svm$predict(tsk_xgb,split$train)
svmcontrain <- confusionMatrix(svmpredictiontrain$confusion)
#测试集混淆矩阵
svmpredictiontest <- svm$predict(tsk_xgb,split$test)
svmcontest <- confusionMatrix(svmpredictiontest$confusion)
#训练集auc
svmroctrain <- roc(svmpredictiontrain$truth,svmpredictiontrain$prob[,1],auc=TRUE,ci=TRUE,levels=c("0", "1"))
#测试集AUC
svmroctest <- roc(svmpredictiontest$truth,svmpredictiontest$prob[,1],auc=TRUE,ci=TRUE,levels=c("0", "1"))
#roc曲线
plot(svmroctrain,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#EDDDC3",
     main='支持向量机')

plot(svmroctest,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#EDDDC3",
     main='支持向量机')
#########LightGBM########
gbm=lrn('classif.lightgbm',predict_type='prob')
set.seed(123456)
gbm = lrn('classif.lightgbm',predict_type="prob",
          num_iterations = to_tune(1,100, logscale = F),
          learning_rate = to_tune(0.1, 0.5, logscale = F),
          num_leaves = to_tune(2, 100, logscale = F),
          feature_fraction_bynode=to_tune(0.1, 1, logscale = F),
          objective="binary")
#定义超参数调整的策略，评价方式等
gbm_tune <- ti(task = tsk_gxb,
               learner = gbm,
               resampling = rsmp('cv',folds=10),
               measures = msr('classif.auc'),
               terminator = trm('run_time',secs=1500)) #trm设置停止条件，secs为时间到了就停
# trm()
tuner=mlr3tuning::tnr("grid_search",resolution = 10,batch_size=10)

set.seed(123456)
#进行优化
tuner$optimize(gbm_tune)
autoplot(gbm_tune) #可视化结果
gbm_tune$archive #调参过程的结果存放仓库#调参过程的结果存放仓库
#将参数值传递给模型
gbm=lrn('classif.lightgbm',predict_type='prob')
gbm$param_set$values=gbm_tune$result_learner_param_vals
#用调整好的参数进行训练
set.seed(123456)
gbm$train(tsk_gxb,split$train)
#训练集混淆矩阵
gbmpredictiontrain <- gbm$predict(tsk_gxb,split$train)
gbmcontrain <- confusionMatrix(gbmpredictiontrain$confusion)
#测试集混淆矩阵
gbmpredictiontest <- gbm$predict(tsk_gxb,split$test)
gbmcontest <- confusionMatrix(gbmpredictiontest$confusion)
#训练集auc
gbmroctrain <- roc(gbmpredictiontrain$truth,gbmpredictiontrain$prob[,1],auc=TRUE,ci=TRUE,levels=c("X0", "X1"))
#测试集AUC
gbmroctest <- roc(gbmpredictiontest$truth,gbmpredictiontest$prob[,1],auc=TRUE,ci=TRUE,levels=c("X0", "X1"))
#roc曲线
plot(gbmroctrain,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#EEBF6D",
     main='轻度提升树')
plot(gbmroctest,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#EEBF6D",
     main='轻度提升树')
########knn#######
knn=lrn('classif.kknn',predict_type='prob',
              k=to_tune(lower = 3,upper = 200))
set.seed(123456)
knn_tune=tune(task = tsk_gxb,
                  learner = knn,
                  measures = msr('classif.auc'),
                  resampling = rsmp('cv',folds=5),
                  tuner = mlr3tuning::tnr('grid_search',resolution=10),
                  terminator = trm('run_time',secs=1500))
autoplot(knn_tune)
knn_tune$archive #调参过程的结果存放仓库#调参过程的结果存放仓库
#将参数值传递给模型
knn=lrn('classif.kknn',predict_type='prob')
knn$param_set$values=knn_tune$result_learner_param_vals
#用调整好的参数进行训练
set.seed(123456)
knn$train(tsk_gxb,split$train)
#训练集混淆矩阵
knnpredictiontrain <- knn$predict(tsk_gxb,split$train)
knncontrain <- confusionMatrix(knnpredictiontrain$confusion)
#测试集混淆矩阵
knnpredictiontest <- knn$predict(tsk_gxb,split$test)
knncontest <- confusionMatrix(knnpredictiontest$confusion)
#训练集auc
knnroctrain <- roc(knnpredictiontrain$truth,knnpredictiontrain$prob[,1],auc=TRUE,ci=TRUE,levels=c("X0", "X1"))
#测试集AUC
knnroctest <- roc(knnpredictiontest$truth,knnpredictiontest$prob[,1],auc=TRUE,ci=TRUE,levels=c("X0", "X1"))
#roc曲线
plot(knnroctrain,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#D94F33",
     main='k临近算法')

plot(knnroctest,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#D94F33",
     main='k临近算法')
#######单层神经网络########
net=lrn('classif.nnet',predict_type='prob')
net$param_set
net = lrn('classif.nnet',predict_type="prob",
          MaxNWts = to_tune(1000,1000000, logscale = F),
          #maxit = to_tune(1, 200, logscale = F),
          size = to_tune(0, 5, logscale = F))
#定义超参数调整的策略，评价方式等
net_tune <- ti(task = tsk_gxb,
               learner = net,
               resampling = rsmp('cv',folds=10),
               measures = msr('classif.auc'),
               terminator = trm('run_time',secs=1500)) #trm设置停止条件，secs为时间到了就停
# trm()
tuner=mlr3tuning::tnr("grid_search",resolution = 10,batch_size=10)

set.seed(123456)
#进行优化
tuner$optimize(net_tune)
autoplot(net_tune) #可视化结果
net_tune$archive #调参过程的结果存放仓库#调参过程的结果存放仓库
#将参数值传递给模型
net=lrn('classif.nnet',predict_type='prob')
net$param_set$values=net_tune$result_learner_param_vals
#用调整好的参数进行训练
set.seed(123456)
net$train(tsk_gxb,split$train)
#训练集混淆矩阵
netpredictiontrain <- net$predict(tsk_gxb,split$train)
netcontrain <- confusionMatrix(netpredictiontrain$confusion)
#测试集混淆矩阵
netpredictiontest <- net$predict(tsk_gxb,split$test)
netcontest <- confusionMatrix(netpredictiontest$confusion)
#训练集auc
netroctrain <- roc(netpredictiontrain$truth,netpredictiontrain$prob[,1],auc=TRUE,ci=TRUE,levels=c("X0", "X1"))
#测试集AUC
netroctest <- roc(netpredictiontest$truth,netpredictiontest$prob[,1],auc=TRUE,ci=TRUE,levels=c("X0", "X1"))
#roc曲线
plot(netroctrain,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#834026",
     main='单层神经网络')

plot(netroctest,
     legacy.axes=TRUE,
     xlim=c(1, 0),
     ylim=c(0, 1),
     xlab= "1-特异度",
     ylab="灵敏度",
     asp=1,
     print.auc.x=0.5,
     print.auc.y=0.2,
     #print.thres=TRUE,
     mar=c(4, 4, 2, 2),
     mgp=c(2.5, 1, 0),
     print.auc=TRUE,
     auc.polygon=TRUE,
     #grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,
     auc.polygon.col="#834026",
     main='单层神经网络')
#########多模型热图########
#训练集
library(readxl)
xunlian_1 <- read_excel("预测模型.xlsx") %>% 
  column_to_rownames(var="...1")
xunlian_2 <- cbind(xunlian_1,"总计"=rowSums(xunlian_1)) %>% as.matrix()
xunlian_gg <- reshape2::melt(xunlian_2)
# 使用ggplot2绘制热图
xunlian_gg<- xunlian_gg %>%
  mutate(fill_color = if_else(value>=0 & value<=1,value,NA))
p1 <- ggplot(data = xunlian_gg, aes(x = Var1, y = Var2, fill = fill_color)) +
  geom_tile(width = 0.95, height = 0.95,color = "black", linetype = "solid") +
  scale_fill_gradientn(colors=brewer.pal(6,"YlGnBu"),limits=c(0.6,1)) +#设置颜色变化范围
  geom_text(aes(label =value),size=4,colour="black")+#显示标签
  labs(x=NULL,y=NULL)+
  theme_minimal()+#设置为简洁主题
  theme(panel.grid=element_blank(),
        legend.title = element_blank(),
        axis.text=element_text(size = 12,face = "bold",color="black"),
        legend.text =element_text(size = 10,face = "bold"))+
  scale_y_discrete(limits = rev(c("AUC","准确率","灵敏度","特异度","精确率","召回率","F1评分","总计")))
#用ggtree做聚类
library(ggtree)
library(aplot)
phc <- hclust(dist(xunlian_1)) %>% 
  ggtree(hang=-1)+layout_dendrogram()
#合并
p1 %>%
  insert_top(phc,height=.1)
#测试集
xunlian_1 <- read_excel("测试集.xlsx") %>% 
  column_to_rownames(var="...1")
xunlian_2 <- cbind(xunlian_1,"总计"=rowSums(xunlian_1)) %>% as.matrix()
xunlian_gg <- reshape2::melt(xunlian_2)
# 使用ggplot2绘制热图
xunlian_gg<- xunlian_gg %>%
  mutate(fill_color = if_else(value>=0 & value<=1,value,NA))
p1 <- ggplot(data = xunlian_gg, aes(x = Var1, y = Var2, fill = fill_color)) +
  geom_tile(width = 0.95, height = 0.95,color = "black", linetype = "solid") +
  scale_fill_gradientn(colors=brewer.pal(6,"YlGnBu"),limits=c(0.6,1)) +#设置颜色变化范围
  geom_text(aes(label =value),size=4,colour="black")+#显示标签
  labs(x=NULL,y=NULL)+
  theme_minimal()+#设置为简洁主题
  theme(panel.grid=element_blank(),
        legend.title = element_blank(),
        axis.text=element_text(size = 12,face = "bold",color="black"),
        legend.text =element_text(size = 10,face = "bold"))+
  scale_y_discrete(limits = rev(c("AUC","准确率","灵敏度","特异度","精确率","召回率","F1评分","总计")))
#用ggtree做聚类
library(ggtree)
library(aplot)
phc <- hclust(dist(xunlian_1)) %>% 
  ggtree(hang=-1)+layout_dendrogram()
#合并
p1 %>%
  insert_top(phc,height=.1)





