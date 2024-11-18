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
library(e1071) #SVM模型使用
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
library(kernlab)#SVM
library(rms) #绘图

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

#step2.构建SVM模型
#随机划分数据集
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
# 定义训练集特征和目标变量
X_train <- train_data[, -1]
y_train <- train_data$deteriorate

#构建变量公式
form_xy <- as.formula(  
  paste0("deteriorate~",paste(colnames(X_train),collapse = "+"))
  )
form_xy

#构建并训练SVM初始模型#初始SVM模型,本例中核函数以高斯核示例，其他函数代码替换即可
set.seed(123)
svm_model_initial <- svm(form_xy,data = train_data,probability = T,kernel="radial")
#模型概况
svm_model_initial

#训练集预测
#建模时需定义“probability = T”,后续才可输出预测结果
predict_train <- predict(svm_model_initial,newdata = X_train,probability = T)
#训练集预测结果；混淆矩阵
matrix_initial <- table(y_train,predict_train)
#快速计算准确率
acc_initial <- sum(diag(matrix_initial)/nrow(train_data))
#获得初始模型效果 0.9210526
acc_initial 

#step3.使用caret包调整参数
#超参数调优是机器学习的重点内容
#本例使用tune.svm()函数自助法调整超参数gamma和cost
set.seed(123)
tuned_model <- tune.svm(deteriorate ~ ., 
                        data = train_data,                       
                        cost = 10^(-1:3),gamma = 10^(-3:1),                       
                        tunecontrol=tune.control(sampling = "bootstrap", 
                                                 nboot = 100))
print(tuned_model)
#代码简单，但计算时间较长，需耐心等待哦～
#错误率最低是0.2224127,超参数gamma=0.001，cost=10

#step4.参数配置
#使用最佳超参数重新训练模型
svm_model_initial_final <- svm(form_xy,data = train_data,                               
                               probability = T,                               
                               kernel="radial",                               
                               gamma=0.001,                               
                               cost=10)
print(svm_model_initial_final)
#可视化决策边界
#plot(svm_model_initial_final,data=train_data,formula=age~nodes)
#训练集预测
predict_train_final <- predict(svm_model_initial_final,newdata = X_train,probability = T)
#获取样本属于各个类别的概率估计值
train_predprob <- attr(predict_train_final, "probabilities")
#训练集预测结果
matrix_initial <- table(y_train, predict_train_final)
#训练集混淆矩阵
confusionMatrix_train <- confusionMatrix(data = predict_train_final, 
                                         reference = train_data$deteriorate, 
                                         positive = "1",                                         
                                         mode = "everything")
#输出训练集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_train)
#Confusion Matrix and Statistics
#Reference
#Prediction  0  1
#0 49  8
#1  2 17
#Accuracy : 0.8684          
#95% CI : (0.7713, 0.9351)
#No Information Rate : 0.6711          
#P-Value [Acc > NIR] : 7.353e-05       
#Kappa : 0.6825          
#Mcnemar's Test P-Value : 0.1138          
#            Sensitivity : 0.6800          
#            Specificity : 0.9608          
#         Pos Pred Value : 0.8947          
#         Neg Pred Value : 0.8596          
#              Precision : 0.8947          
#                 Recall : 0.6800          
#                     F1 : 0.7727          
#             Prevalence : 0.3289          
#         Detection Rate : 0.2237          
#   Detection Prevalence : 0.2500          
#      Balanced Accuracy : 0.8204          
#       'Positive' Class : 1  

#step5.应用于验证集
#使用最佳参数配置预测验证集
val_x <- test_data[, -1]
val_y <- as.factor(test_data$deteriorate)
predict_val <- predict(svm_model_initial_final, newdata = test_data,probability = T)
val_predprob <- attr(predict_val, "probabilities")
#验证集预测结果
matrix_initial_val <- table(val_y,predict_val)
#验证集混淆矩阵
confusionMatrix_val <- confusionMatrix(data = predict_val,
                                       reference = test_data$deteriorate,
                                       positive = "1",
                                       mode = "everything")
#输出模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_val)
#Confusion Matrix and Statistics
#Reference
#Prediction  0  1
#0 20  4
#1  1  6
#Accuracy : 0.8387          
#95% CI : (0.6627, 0.9455)
#No Information Rate : 0.6774          
#P-Value [Acc > NIR] : 0.03645         
#Kappa : 0.5995          
#Mcnemar's Test P-Value : 0.37109         
#            Sensitivity : 0.6000          
#            Specificity : 0.9524          
#         Pos Pred Value : 0.8571          
#         Neg Pred Value : 0.8333          
#              Precision : 0.8571          
#                 Recall : 0.6000          
#                     F1 : 0.7059          
#             Prevalence : 0.3226          
#         Detection Rate : 0.1935          
#   Detection Prevalence : 0.2258          
#      Balanced Accuracy : 0.7762          
#       'Positive' Class : 1

#step6.绘制混淆矩阵
#step6.1 训练集混淆矩阵
#混淆矩阵转换为数据框
confusion_matrix_df <- as.data.frame.matrix(confusionMatrix_train$table)
colnames(confusion_matrix_df) <- c("sensoring","terminal event")
rownames(confusion_matrix_df) <- c("sensoring","terminal event")
draw_data <- round(confusion_matrix_df / rowSums(confusion_matrix_df),2)
draw_data$real <- rownames(draw_data)
draw_data <- melt(draw_data)
#绘制训练集混淆矩阵热图
ggplot(draw_data, aes(real,variable, fill = value)) +  
  geom_tile() +  
  geom_text(aes(label = scales::percent(value))) +  
  scale_fill_gradient(low = "#ECA9B0", high = "#81D8D0") +  
  labs(x = "True", y = "Predicted", title = "Confusion matrix of train set") +  
  theme_prism(border = T)+  
  theme(panel.border = element_blank(),        
        axis.ticks.y = element_blank(),       
        axis.ticks.x = element_blank(),        
        legend.position="none")

#step6.2 训练集混淆矩阵
#混淆矩阵转换为数据框
confusion_matrix_df <- as.data.frame.matrix(confusionMatrix_val$table)
colnames(confusion_matrix_df) <- c("sensoring","terminal event")
rownames(confusion_matrix_df) <- c("sensoring","terminal event")
draw_data <- round(confusion_matrix_df / rowSums(confusion_matrix_df),2)
draw_data$real <- rownames(draw_data)
draw_data <- melt(draw_data)
#绘制训练集混淆矩阵热图
ggplot(draw_data, aes(real,variable, fill = value)) +  
  geom_tile() +  
  geom_text(aes(label = scales::percent(value))) +  
  scale_fill_gradient(low = "#ECA9B0", high = "#81D8D0") +  
  labs(x = "True", y = "Predicted", title = "Confusion matrix of val set") +  
  theme_prism(border = T)+  
  theme(panel.border = element_blank(),        
        axis.ticks.y = element_blank(),       
        axis.ticks.x = element_blank(),        
        legend.position="none")

#step7.绘制ROC曲线
#step7.1 训练集ROC曲线绘制
#AUC
train_final_roc <- roc(response = train_data$deteriorate, predictor = train_predprob [, 2])
train_final_roc
#截断值
train_final_roc_best=coords(train_final_roc, "best",best.method = c("youden"),              
                            ret=c("threshold","sensitivity", "specificity"))
train_final_roc_best
#计算训练集ROC曲线的参数
train_final_roc_obj <- roc(response = train_data$deteriorate, predictor = train_predprob [, 2])
train_final_roc_auc <- auc(train_final_roc_obj)
#将ROC对象转换为数据框
train_final_roc_data <- data.frame(train_final_roc_obj$specificities, train_final_roc_obj$sensitivities)

#绘制ROC曲线
#ggplot2包美化ROC
ggplot(train_final_roc_data, aes(x = train_final_roc_obj$specificities, y = train_final_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.25, y = 0.25, label = paste("AUC =", round(train_final_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.7, y = 0.85, label = paste("Cutoff =", round(train_final_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(1, 0), ylim = c(0, 1)) +  
  theme_pubr() +  
  labs(x = "Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of train set") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+  
  geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1),color="#5d6174",linetype="dashed") +  
  theme_prism(border = T)+  
  geom_point(x=0.9190476,y=0.9185668,color="#D2431C",size=3)+  
  theme(axis.text = element_text (size = 10))+  
  theme(axis.title.x=element_text(vjust=2, size=15,face = "plain"))+  
  theme(axis.title.y=element_text(vjust=2, size=15,face = "plain"))

#step7.2 验证集ROC曲线绘制
#AUC
Val_final_roc <- roc(response = test_data$deteriorate, predictor = val_predprob [, 2])
Val_final_roc
#截断值
Val_final_roc_best=coords(Val_final_roc, "best",best.method = c("youden"),              
                            ret=c("threshold","sensitivity", "specificity"))
Val_final_roc_best
#计算训练集ROC曲线的参数
Val_final_roc_obj <- roc(response = test_data$deteriorate, predictor = val_predprob [, 2])
Val_final_roc_auc <- auc(Val_final_roc_obj)
#将ROC对象转换为数据框
Val_final_roc_data <- data.frame(Val_final_roc_obj$specificities, Val_final_roc_obj$sensitivities)

#绘制ROC曲线
#ggplot2包美化ROC
ggplot(Val_final_roc_data, aes(x = Val_final_roc_obj$specificities, y = Val_final_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.25, y = 0.25, label = paste("AUC =", round(Val_final_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.7, y = 0.85, label = paste("Cutoff =", round(Val_final_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(1, 0), ylim = c(0, 1)) +  
  theme_pubr() +  
  labs(x = "Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of train set") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+  
  geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1),color="#5d6174",linetype="dashed") +  
  theme_prism(border = T)+  
  geom_point(x=0.9190476,y=0.9185668,color="#D2431C",size=3)+  
  theme(axis.text = element_text (size = 10))+  
  theme(axis.title.x=element_text(vjust=2, size=15,face = "plain"))+  
  theme(axis.title.y=element_text(vjust=2, size=15,face = "plain"))

#step8.训练集与验证集ROC曲线叠加
plot(train_final_roc,     
     print.auc=TRUE,     
     print.thres=T,   
     legacy.axes=T,   
     print.auc.y=0.3,   
     main="ROC of SVM")
plot(Val_final_roc,    
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

#step9.绘制校准曲线
#训练集校准曲线
train_final_dc <- val.prob(train_predprob [, 1],
                           as.numeric(train_data$deteriorate)-1,
                           logistic.cal = FALSE,
                           statloc = F,                                     
                           riskdist = c("predicted","calibrated"),
                           legendloc = c(0.8,0.25))

#验证集集校准曲线
Val_final_dc <- val.prob(val_predprob [, 1],
                           as.numeric(test_data$deteriorate)-1,
                           logistic.cal = FALSE,
                           statloc = F,                                     
                           riskdist = c("predicted","calibrated"),
                           legendloc = c(0.8,0.25))


