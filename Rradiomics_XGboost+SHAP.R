#########################设置工作路径和数据读取#################
#设置工作路径
getwd()
setwd("D:/R work")

##一、数据准备
#1.数据读取（读进来）
#install.packages("readr")

# 导入必要的包,没有安装的可以先安装一下
library(readr)
library(dplyr) #数据处理使用
library(data.table) #数据读取使用
library(xgboost) #模型使用
library(Matrix) #模型数据处理使用
library(caret) # 调参和计算模型评价参数使用
library(pROC) #绘图使用
library(ggplot2) #绘图使用
library(ggpubr) #绘图使用
library(ggprism) #绘图使用
library(tidyverse) #数据处理
library(data.table) #读取数据
library(skimr) #数据概况
library(DataExplorer)#数据探索
library(GGally)#数据探索
library(caTools)

#rm(list=ls()) # 清除当前工作环境中所有对象的命令

# 读取数据
mydata <- read.csv("pydatas4.csv")
View(mydata)          #查看数据
str(mydata)           #查看数据的类型，非常重要
summary(mydata)       #数据进行简单描述
mydata<-na.omit(mydata)   #删除缺失数据
names(mydata)                #查看mydata的变量列表


#因变量deteriorate因子化
mydata$deteriorate <- as.factor(mydata$deteriorate)
skim(mydata)#查看因变量分布：结局变量比例至少2:1，样本平衡，模型效果较好
table(mydata$deteriorate)#本例后续使用data_d数据集
data <- mydata

#step2.构建RF模型
#设置随机种子，结果可复现
set.seed(123)
#createDataPartition()函数：分层抽样机制
train_index <- createDataPartition(y=mydata$deteriorate, p=0.7, list=F)
train_data <- mydata[train_index,]#训练集
test_data <- mydata[-train_index,]#验证集
#查看划分数据集的因变量分布情况
table(train_data$deteriorate)
# 0   1 
#51  25 
table(test_data$deteriorate)
# 0   1 
#21  10 

#定义训练集特征，分类变量转换为哑变量，构建稀疏矩阵
train_x <-  sparse.model.matrix(deteriorate ~ ., data = train_data)[,-1]
#显示稀疏矩阵前几行
head(train_x)
#定义训练集因变量，标签转换为数值向量， xgboost默认类别标签从0开始，减去1
train_y <-  as.numeric(train_data$deteriorate)-1
#使用xgb.DMatrix函数将特征和目标变量转换为DMatrix矩阵格式
#dtrain 包含了训练集的特征矩阵和对应的标签，是 xgboost 训练过程中所需的输入数据结构
dtrain <- xgb.DMatrix(data = train_x, label = train_y)

#训练集构建XGBoost初始模型
#设置XGBoost默认参数
#使用参数
#objective = "binary:logistic": 训练一个 binary classification model；
#max.depth = 2: trees 不会很深，因为我们的案例很简单；
#nthread = 2: 使用的 CPU 线程数；
#nrounds = 2: 将对数据进行两次传递，第二次传递将通过进一步减少 ground truth 和 prediction 之间的差异来增强模型。
params0 <- list(objective = "binary:logistic",                 
                booster="gbtree",               
                eval_metric = "logloss",          
                eta = 0.3,            
                gamma=0,             
                max_depth = 6,         
                min_child_weight = 1,         
                subsample = 1,          
                colsample_bytree = 1)

#训练XGBoost初始模型
set.seed(123)
xgb_model0 <- xgb.train(data=dtrain,   
                        params = params0,  
                        nrounds = 100,#迭代轮数（树的数量）  
                        verbose = 1)
#查看模型概况
xgb_model0 
#niter为迭代次数，nfeatures为训练数据中的特征数量
#使用getinfo获取其中的元素
head(getinfo(dtrain, "label"))

#训练集初始模型评估
#训练集预测概率
predict_initial <- predict(xgb_model0,newdata =dtrain)
range(predict_initial )
#XGBoost使用label向量构建回归模型，预测结果是概率。
#对于二分类任务，需要设置规则，将预测概率转换为预测类别。如果特定数据的概率 > 0.5，则观察被分类为 1，否则为 0，也可以当作超参数调整
predict_initial <- ifelse(predict_initial > 0.5,1,0)

#快速计算准确率
acc_initial <- mean(predict_initial == train_y)
acc_initial 
print(paste("训练集准确率:", acc_initial )) #"训练集准确率: 1"
#输出初始模型整体评价指标：混淆矩阵
train_matrix0 <- confusionMatrix(as.factor(predict_initial),   
                                 train_data$deteriorate)
train_matrix0 
#Confusion Matrix and Statistics
#Reference
#Prediction  0  1
#0 51  0
#1  0 25
#Accuracy : 1          
#95% CI : (0.9526, 1)
#No Information Rate : 0.6711     
#P-Value [Acc > NIR] : 6.816e-14  
#Kappa : 1          
#Mcnemar's Test P-Value : NA         
#            Sensitivity : 1.0000     
#            Specificity : 1.0000     
#         Pos Pred Value : 1.0000     
#         Neg Pred Value : 1.0000     
#             Prevalence : 0.6711     
#         Detection Rate : 0.6711     
#   Detection Prevalence : 0.6711     
#      Balanced Accuracy : 1.0000     
#       'Positive' Class : 0 

#step3.调整参数
#使用xgboost:xgb.cv进行交叉验证
set.seed(123)
xgb_model_cv <- xgb.cv(data=dtrain,  
                       objective="binary:logistic",    
                       booster="gbtree",    
                       eval_metric = "logloss",  
                       eta=0.3,  
                       max_depth=6,  
                       min_child_weight=1, 
                       gamma=0,     
                       subsample=1,   
                       colsample_bytree=1,
                       scale_pos_weight=1,   #阴性样本总数/阳性样本总数 
                       nrounds=1000, 
                       nfold=10,     
                       metrics=list("error","auc"),  
                       early_stopping_rounds=50)
#本次最佳迭代次数为67次。如果该值过大，可以提高学习率值并重新运行命令，减少树的数量
#Stopping. Best iteration:[17]	train-logloss:0.102362+0.003852	train-error:0.000000+0.000000	train-auc:1.000000+0.000000	
#test-logloss:0.692908+0.255895	test-error:0.299802+0.151501	test-auc:0.706726+0.242467

#拟合模型
set.seed(123)
xgb_model_cv.bst <- xgb.train(data=dtrain,
                              objective="binary:logistic", 
                              booster="gbtree",    
                              eval_metric = "logloss",      
                              eta=0.3,                  
                              max_depth=6,  
                              min_child_weight=1,     
                              gamma=0,          
                              subsample=1,    
                              colsample_bytree=1,       
                              scale_pos_weight=1,#阴性样本总数/阳性样本总数   
                              nrounds=17)
#输出最终模型
print(xgb_model_cv.bst)
#模型准确率快速计算
predict_cv.bst   <- predict(xgb_model_cv.bst,,newdata =dtrain)
cv.fit <- ifelse(predict_cv.bst > 0.5,1,0)
acc_cv <- mean(cv.fit == train_y)
acc_cv
                       
#使用caret包进行网格搜索
#设定trainControl参数，该对象能保存使用方法以训练调优参数
caret.Control <- trainControl(method = "adaptive_cv",   # 交叉验证
                              number = 10,     # 10折交叉验证 
                              repeats=5, 
                              verboseIter = FALSE,   
                              returnData = FALSE, 
                              selectionFunction="best",  
                              returnResamp = "final",   
                              search = "grid",       
                              seeds=set.seed(123))#缩短计算时间采用自适应重采样（Adaptive Resampling）

#设置参数网格
##expand.grid {base}:Create a Data Frame from All Combinations of Factor Variables
#创建需要考察的参数组合。组合越多，需要的算力越大
caret.grid <- expand.grid(nrounds = c(100,200,500), # 迭代轮数（nrounds） 
                          max_depth = c(3,5,6), # 最大树深度（max_depth）
                          eta = c(0.01, 0.1, 0.3), # 学习率（eta）
                          gamma = c(0,0.1,0.5), # 树分裂所需的最小损失减少值 
                          colsample_bytree = c(0.8,1), # 特征子采样比例（colsample_bytree）
                          min_child_weight = c(1,3), # 叶子节点的最小权重和（min_child_weight）
                          subsample = c(0.8,1)) # 和样本子采样比例（subsample）

#使用train()函数进行参数调优
library(plyr)
library(dplyr)
set.seed(123)
xgb_model_caret <- train(deteriorate~.,data = train_data,  
                         method = "xgbTree", 
                         trControl = caret.Control,  
                         tuneGrid = caret.grid,    
                         verbose=FALSE,            
                         verbosity=0#消除xgboost警告  
                         )#耗时很长，耐心等待
#输出最优超参数
print(xgb_model_caret$bestTune)
#    nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
#105     500         5 0.01   0.1              0.8                3       0.8
#可视化探索
#通过plot()查看不同参数值与模型性能评估指标间的变化关系
plot(xgb_model_caret)

#设置最佳XGBoost参数
params.caret.bst <- list(objective = "binary:logistic",
                         booster = "gbtree", 
                         eval_metric = "logloss", 
                         max_depth = 5, 
                         eta = 0.01,       
                         gamma = 0.1,             
                         colsample_bytree = 0.8,     
                         min_child_weight = 3,           
                         subsample = 0.8)
#拟合模型
set.seed(123)
xgb_model_caret.bst <- xgb.train(data=dtrain,
                                 params = params.caret.bst,  
                                 nrounds = 500)
#输出最终模型
print(xgb_model_caret.bst)
#模型准确率快速计算
predict_caret   <- predict(xgb_model_caret.bst,,newdata =dtrain)
caret.fit <- ifelse(predict_caret > 0.5,1,0)
acc_caret <- mean(caret.fit == y_train)
acc_caret  # 0.9210526

#step4.参数配置
#选取xgb_model_caret.bst作为最终模型
#保存加载模型
#保存加载训练好的模型：
#保存
xgb.save(xgb_model_caret.bst, "xgboost.model")
#加载
getwd()
setwd("D:/R work")
xgb.load("xgboost.model")

#特征重要性排序/变量重要性
#每个变量的重要性评估指标：Gain，Cover，frequency

importance <- xgb.importance(feature_names = colnames(dtrain), 
                             model = xgb_model_caret.bst)
head(importance)
#普通可视化
xgb.plot.importance(importance_matrix = importance,measure = "Cover")
#ggplot可视化排名前10变量
library(Ckmeans.1d.dp)
xgb.ggplot.importance(importance_matrix = importance, top_n = 10)

#查看模型中树的信息
xgb.plot.tree(model = xgb_model_caret.bst,
               trees = 0#显示模型中的第1棵
              )
#trees：整数向量指示绘制第几棵树模型，如不设置，所有的树模型都会被绘制
#xgboost模型中的树索引是从0开始的，比如trees=0:2表示模型中前3棵树

#多棵树展示在一起
xgb.plot.multi.trees(model =  xgb_model_caret.bst,
                     features_keep = 2, 
                     fill=TRUE)
#这幅图就是把上面那张图的信息整合到了一起

#查看树的深度来检查树的结构
xgb.plot.deepness(model = xgb_model_caret.bst)
#这两幅图的横坐标都是树的深度，上面的图纵坐标是叶子的数量，展示了每层深度中的叶子数量。下面的图纵坐标是每片叶子的归一化之后的加权覆盖
#从图中可以看出树的深度在6之后，叶子的数量就很多了，这提示我们为了防止过拟合，可以把树的深度控制在6以上

#训练集模型预测：混淆矩阵和ROC曲线
#训练集预测概率
predict_train <- predict(xgb_model_caret.bst, newdata = dtrain)
predict_train <- as.factor(ifelse(predict_train >0.5,1,0))
#训练集预测结果
matrix_train <- table(train_data$deteriorate,predict_train)
#训练集混淆矩阵
confusionMatrix_train <- confusionMatrix(data = predict_train, 
                                         reference = train_data$deteriorate,
                                         positive = "1", 
                                         mode = "everything")
#输出训练集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_train)
#Confusion Matrix and Statistics
#Reference
#Prediction  0  1
#0 49  4
#1  2 21
#Accuracy : 0.9211         
#95% CI : (0.836, 0.9705)
#No Information Rate : 0.6711         
#P-Value [Acc > NIR] : 2.48e-07       
#Kappa : 0.8175         
#Mcnemar's Test  P-Value : 0.6831         
#            Sensitivity : 0.8400         
#            Specificity : 0.9608         
#         Pos Pred Value : 0.9130         
#         Neg Pred Value : 0.9245         
#              Precision : 0.9130         
#                 Recall : 0.8400         
#                     F1 : 0.8750         
#             Prevalence : 0.3289         
#         Detection Rate : 0.2763         
#   Detection Prevalence : 0.3026         
#      Balanced Accuracy : 0.9004         
#       'Positive' Class : 1   

#绘制混淆矩阵
#本例以训练集混淆矩阵进行示例，验证集方法相同，替换即可#混淆矩阵转换为数据框
confusion_matrix_df2 <- as.data.frame.matrix(confusionMatrix_train$table)
colnames(confusion_matrix_df2) <- c("sensoring","terminal event")
rownames(confusion_matrix_df2) <- c("sensoring","terminal event")
draw_data2 <- round(confusion_matrix_df2 / rowSums(confusion_matrix_df2),2)
draw_data2$real <- rownames(draw_data2)
draw_data2 <- melt(draw_data2)
#绘制训练集混淆矩阵热图
ggplot(draw_data2, aes(real,variable, fill = value)) +  
  geom_tile() +  
  geom_text(aes(label = scales::percent(value))) +  
  scale_fill_gradient(low = "#ECA9B0", high = "#81D8D0") +  
  labs(x = "True", y = "Predicted", title = "Confusion matrix of train set") +  
  theme_prism(border = T)+  
  theme(panel.border = element_blank(),        
        axis.ticks.y = element_blank(),        
        axis.ticks.x = element_blank(),        
        legend.position="none")

#绘制训练集ROC曲线
#训练集预测概率
train_predprob <- predict(xgb_model_caret.bst,newdata = dtrain,type="prob")
#AUC
train_roc <- roc(response = train_data$deteriorate, predictor = train_predprob)
train_roc
#截断值
train_roc_best=coords(train_roc, "best",best.method = c("youden"), 
                      ret=c("threshold","sensitivity", "specificity"))
train_roc_best
#计算训练集ROC曲线的参数
train_roc_obj <- train_roc 
train_roc_auc <- auc(train_roc_obj)
#将ROC对象转换为数据框
train_roc_data <- data.frame(1-train_roc_obj$specificities, train_roc_obj$sensitivities)

#绘制ROC曲线
#基本ROC曲线
plot(train_roc,     
     print.auc=TRUE,
     auc.polygon=TRUE, 
     grid=T,    
     max.auc.polygon=T, 
     auc.polygon.col="skyblue", 
     print.thres=T,   
     legacy.axes=T,   
     bty="l")

#ggplot2包美化ROC
ggplot(train_roc_data, aes(x = 1-train_roc_obj$specificities, y = train_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(train_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.35, y = 0.85, label = paste("Cutoff =", round(train_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  
  theme_pubr() +  labs(x = "1-Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of train set") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+  
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),color="#5d6174",linetype="dashed") + 
  theme_prism(border = T)+ 
  geom_point(x= 1-train_roc_best$specificity ,y=train_roc_best$sensitivity,color="#D2431C",size=3)+ 
  theme(axis.text = element_text (size = 10))+ 
  theme(axis.title.x=element_text(vjust=2, size=15,face = "plain"))+
  theme(axis.title.y=element_text(vjust=2, size=15,face = "plain"))

#绘制训练集校准曲线
#使用rms包中的val.prob()函数
library(rms)
train_cv <- val.prob(train_predprob,
                     as.numeric(train_data$deteriorate)-1, 
                     logistic.cal = FALSE,    
                     statloc = F,    
                     riskdist = c("predicted","calibrated"),  
                     legendloc = c(0.8,0.25))
title(main = "calibration curve of train")

#使用predtools包
#install.packages("predtools")
library(predtools)
#模型对训练集的预测概率写入训练集
train_data$P.pred<-train_predprob 
#因变量需要是0/1型的数值型
train_data$obs<- as.numeric(train_data$deteriorate)-1
calibration_plot(data=train_data,
                 obs="obs", 
                 pred="P.pred", 
                 title="Calibration Curves for Trainset", 
                 x_lim=c(0,1),
                 y_lim=c(0,1), 
                 xlab="Prediction Probability",
                 ylab="Observation Probability") 

#使用probably包
#install.packages("probably")
library(probably)
library(dplyr)
#在数据集trainset中增加一列名称为“.pred_1”，取值是模型的预测概率。
#cal_plot_breaks函数的参数estimate名称必须是“.pred_X”，这里的X对应的是结局变量中感兴趣的水平值
#本例感兴趣的是结局变量d.unfav（6个月内是否出现不利结局(0/1)）的出现不利结局（赋值1），因此新变量名称是.pred_1
train_data2<-mutate(train_data,.pred_1=P.pred) 
cal_plot_breaks(train_data2,  
                truth=obs,     
                estimate=.pred_1,   
                num_breaks=10,conf_level=0.95,event_level="auto") #训练集校准曲线

#决策曲线
#使用rmda包
library(rmda)
DCA_train<-decision_curve(obs~P.pred,
                          data=train_data, 
                          policy='opt-in',  
                          fitted.risk=TRUE,   
                          thresholds=seq(0,1,by= 0.01))

plot_decision_curve(DCA_train, 
                    curve.names='DCA.trainset', 
                    standardize=FALSE, 
                    confidence.intervals=FALSE,  
                    col='blue')

#使用dcruves包
library(dcurves)
dca(obs~P.pred, data=train_data,thresholds=seq(0,1,by= 0.1))%>% 
  plot(smooth = TRUE,loess.span=0.1)

#Step5.应用于验证集
#step5.应用于验证集
#验证集变量格式转换
library(Matrix)
val_x <-  sparse.model.matrix(deteriorate ~ ., data = test_data)[,-1]
val_y <-  as.numeric(test_data$deteriorate)-1
library(xgboost)
dval <- xgb.DMatrix(data = val_x, label = val_y)
#使用最终模型预测验证集#验证集预测概率
predict_val <- predict(xgb_model_caret.bst,newdata = dval)
predict_val <- as.factor(ifelse(predict_val >0.5,1,0))
#验证集预测结果
matrix_val <- table(test_data$deteriorate,predict_val)
#验证集混淆矩阵
library(caret)
confusionMatrix_val <- confusionMatrix(data = predict_val, 
                                       reference = test_data$deteriorate,
                                       positive = "1", 
                                       mode = "everything")
#输出验证集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_val)
#Confusion Matrix and Statistics
#Reference
#Prediction  0  1
#0 21  4
#1  0  6
#Accuracy : 0.871           
#95% CI : (0.7017, 0.9637)
#No Information Rate : 0.6774          
#P-Value [Acc > NIR] : 0.0127          
#Kappa : 0.6702          
#Mcnemar's Test P-Value : 0.1336          
#            Sensitivity : 0.6000          
#            Specificity : 1.0000          
#         Pos Pred Value : 1.0000          
#         Neg Pred Value : 0.8400          
#              Precision : 1.0000          
#                 Recall : 0.6000          
#                     F1 : 0.7500          
#             Prevalence : 0.3226          
#         Detection Rate : 0.1935          
#   Detection Prevalence : 0.1935          
#      Balanced Accuracy : 0.8000          
#       'Positive' Class : 1

#绘制验证集混淆矩阵
#混淆矩阵转换为数据框
library(reshape2)
confusion_matrix_df3 <- as.data.frame.matrix(confusionMatrix_val$table)
colnames(confusion_matrix_df3) <- c("sensoring","terminal event")
rownames(confusion_matrix_df3) <- c("sensoring","terminal event")
draw_data3 <- round(confusion_matrix_df3 / rowSums(confusion_matrix_df3),2)
draw_data3$real <- rownames(draw_data3)
draw_data3 <- melt(draw_data3)
#绘制验证集混淆矩阵热图
ggplot(draw_data3, aes(real,variable, fill = value)) +  
  geom_tile() +  
  geom_text(aes(label = scales::percent(value))) +  
  scale_fill_gradient(low = "#ECA9B0", high = "#81D8D0") +  
  labs(x = "True", y = "Predicted", title = "Confusion matrix of valid set") +  
  theme_prism(border = T)+  
  theme(panel.border = element_blank(),        
        axis.ticks.y = element_blank(),    
        axis.ticks.x = element_blank(),    
        legend.position="none")

#绘制验证集ROC曲线
#验证集预测概率
val_predprob <- predict(xgb_model_caret.bst, newdata = dval, type="prob")
#AUC
val_roc <- roc(response = test_data$deteriorate, predictor = val_predprob)
val_roc
#截断值
val_roc_best=coords(val_roc, "best",best.method = c("youden"),  
                    ret=c("threshold","sensitivity", "specificity"))
val_roc_best
#计算验证集ROC曲线的参数
val_roc_obj <- val_roc 
val_roc_auc <- auc(val_roc_obj)
#将ROC对象转换为数据框
val_roc_data <- data.frame(1-val_roc_obj$specificities, val_roc_obj$sensitivities)

#ggplot2包美化ROC
ggplot(val_roc_data, aes(x = 1-val_roc_obj$specificities, y = val_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(val_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.1, y = 0.6, label = paste("Cutoff =", round(val_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
  theme_pubr() + 
  labs(x = "1-Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of val set") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ 
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),color="#5d6174",linetype="dashed") + 
  theme_prism(border = T)+ 
  geom_point(x=1-val_roc_best$specificity,y=val_roc_best$sensitivity,color="#D2431C",size=3)+ 
  theme(axis.text = element_text (size = 10))+
  theme(axis.title.x=element_text(vjust=2, size=15,face = "plain"))+ 
  theme(axis.title.y=element_text(vjust=2, size=15,face = "plain"))

#训练集与验证集ROC曲线叠加
plot(train_roc,     
     print.auc=TRUE,     
     print.thres=T,   
     legacy.axes=T,   
     print.auc.y=0.3,   
     main="ROC of XGBoost")
plot(val_roc,    
     print.auc=TRUE,  
     print.auc.y=0.2,   
     print.thres=T,     
     add=T,   
     col="red")
legend("bottomright",    
       legend=c("train","val"),     
       col = c(par("fg"),"red"),  
       lwd=2,   
       cex=0.9)

#绘制验证集校准曲线
#使用rms包中的val.prob()函数
val_cv <- val.prob(val_predprob,
                   as.numeric(test_data$deteriorate)-1, 
                   logistic.cal = FALSE,
                   statloc = F, 
                   riskdist = c("predicted","calibrated"), 
                   legendloc = c(0.8,0.25))
title(main = "calibration curve of val")
#使用predtools包
library(predtools)
#模型对验证集的预测概率写入验证集
test_data$P.pred<-val_predprob 
#因变量需要是0/1型的数值型
test_data$obs<- as.numeric(test_data$deteriorate)-1
calibration_plot(data=test_data,
                 obs="obs",
                 pred="P.pred", 
                 title="Calibration Curves for Valset",
                 x_lim=c(0,1), 
                 y_lim=c(0,1), 
                 xlab="Prediction Probability", 
                 ylab="Observation Probability") 

#使用probably包
library(probably)
val_data2<-mutate(test_data,.pred_1=P.pred) 
cal_plot_breaks(val_data2, 
                truth=obs,   
                estimate=.pred_1,   
                num_breaks=10,conf_level=0.95,event_level="auto") 

#验证集决策曲线
#使用rmda包
library(rmda)
DCA_val<-decision_curve(obs~P.pred,  
                        data=test_data,
                        policy='opt-in',
                        fitted.risk=TRUE,    
                        thresholds=seq(0,1,by= 0.01))
plot_decision_curve(DCA_val, 
                    curve.names='DCA.valset', 
                    standardize=FALSE, 
                    confidence.intervals=FALSE, 
                    col='blue')

#使用dcruves包
library(dcurves)
dca(obs~P.pred, data=test_data,thresholds=seq(0,1,by= 0.1))%>%  
  plot(smooth = TRUE,loess.span=0.1)

#Step6.XGBoost模型的解释################################################################################################
#使用xgboost中的xgb.plot.shap()函数
xgb.plot.shap(data=train_x,   
              model=xgb_model_caret.bst,   
              top_n = 5)

#使用SHAPforxgboost包
library(SHAPforxgboost)
#局部解释
#shap.value函数计算shap值
#每一个样本的每一个特征都计算了一个贡献值，每个特征的平均贡献
shap_values <- shap.values(xgb_model = xgb_model_caret.bst, X_train = as.matrix(train_x))
#The ranked features by mean SHAP
shap_values$mean_shap_score
shap_values$shap_score
#全局特征重要性
# shap.prep() returns the long-format SHAP data 
shap_long <- shap.prep(xgb_model = xgb_model_caret.bst, X_train = as.matrix(train_x))
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = as.matrix(train_x))
#可视化所有样本的SHAP值
shap.plot.summary(shap_long, scientific = TRUE)
#图中展示了训练集数据集中所有变量对于status的贡献，横坐标为SHAP值，纵坐标分别对应变量。颜色表示变量本身取值的高低
#其他图形：条形图
shap.plot.summary(shap_long, kind = "bar")

#变量依赖图
shap.plot.dependence(data_long = shap_long, 
                     x="age", #         
                     add_hist = TRUE,   
                     add_stat_cor = TRUE)
#通过散点和拟合曲线推测“age”和SHAP 值之间的关系，从而判断变量的贡献方向

#使用shapviz包
library(shapviz)
#计算shap值
shap_xgboost  <-  shapviz(xgb_model_caret.bst,
                          X_pred = as.matrix(train_x))
#绘制单个样本瀑布图
sv_waterfall(shap_xgboost,row_id = 10)
#row_id指定样本
#单个样本力图
sv_force(shap_xgboost,row_id = 10)
#绘制变量重要性蜂群图
sv_importance(shap_xgboost,kind = "beeswarm")
#去掉图片灰色背景
sv_importance(shap_xgboost,kind = "beeswarm")+theme_bw()
#变量重要性柱状图
sv_importance(shap_xgboost)+theme_bw()
#单个变量依赖图
sv_dependence(shap_xgboost, "UWDRS.N")+theme_bw()
#多个变量偏相关依赖图
sv_dependence(shap_xgboost,
              v = c("UWDRS.N","age"))+theme_bw()
#SHAP交互效应
#计算SHAP交互值
shap_int <- shap.prep.interaction(xgb_mod = xgb_model_caret.bst, X_train = as.matrix(train_x))
int1 <- shap.plot.dependence(data_long = shap_long, 
                             data_int = shap_int, 
                             x= "UWDRS.N", y = "UWDRS.N", 
                             color_feature = "UWDRS.N")
int2 <- shap.plot.dependence(data_long = shap_long,
                             data_int = shap_int, 
                             x= "UWDRS.N", y = "UWDRS.N", 
                             color_feature = "UWDRS.N")
gridExtra::grid.arrange(int1, int2, ncol=2)






