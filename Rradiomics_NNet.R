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
library(nnet)
library(neuralnet)

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

#step2.构建NNET模型
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

# 3.模型构建
# 使用nnet包构建神经网络模型
# 隐藏层神经元数量为10，这里我们使用logistic激活函数，因此linear.output设置为FALSE

# 定义神经网络模型
nnet_model <- neuralnet(deteriorate ~ ., data = train_data, hidden = c(10), linear.output = FALSE)

# 4.模型调参
# 使用caret包进行模型调参
# 设置调参的参数范围
# 这里我们需要为nnet方法提供size和decay参数的网格
paramGrid <- expand.grid(size = c(1:20), decay = c(0.1, 0.01, 0.001))

# 创建训练控制对象
train_control <- trainControl(method = "cv", number = 10)

# 使用caret包的train函数进行模型训练和调参
caret_model <- train(deteriorate ~ ., data = train_data, method = "nnet",
                     trControl = train_control, tuneGrid = paramGrid,
                     trace = FALSE)

# 输出最优模型的参数
print(caret_model)
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were size = 4 and decay = 0.01.

# 5.模型预测
# 使用最优模型进行预测
predictions_train <- predict(caret_model, newdata = train_data)
predictions_test <- predict(caret_model, newdata = test_data)

# 6.模型评估
# 计算混淆矩阵
confusion_train <- confusionMatrix(predictions_train, train_data$deteriorate)
confusion_test <- confusionMatrix(predictions_test, test_data$deteriorate)

# 打印混淆矩阵
print(confusion_train)
#Confusion Matrix and Statistics
#Reference
#Prediction  0  1
#0 48  4
#1  3 21
#Accuracy : 0.9079          
#95% CI : (0.8194, 0.9622)
#No Information Rate : 0.6711          
#P-Value [Acc > NIR] : 1.261e-06       
#Kappa : 0.7892          
#Mcnemar's Test P-Value : 1               
#            Sensitivity : 0.9412          
#            Specificity : 0.8400          
#         Pos Pred Value : 0.9231          
#         Neg Pred Value : 0.8750          
#             Prevalence : 0.6711          
#         Detection Rate : 0.6316          
#   Detection Prevalence : 0.6842          
#      Balanced Accuracy : 0.8906          
#       'Positive' Class : 0 
       
print(confusion_test)
#Confusion Matrix and Statistics
#Reference
#Prediction  0  1
#0 17  4
#1  4  6
#Accuracy : 0.7419          
#95% CI : (0.5539, 0.8814)
#No Information Rate : 0.6774          
#P-Value [Acc > NIR] : 0.2879          
#Kappa : 0.4095          
#Mcnemar's Test P-Value : 1.0000          
#            Sensitivity : 0.8095          
#            Specificity : 0.6000          
#         Pos Pred Value : 0.8095          
#         Neg Pred Value : 0.6000          
#             Prevalence : 0.6774          
#         Detection Rate : 0.5484          
#   Detection Prevalence : 0.6774          
#      Balanced Accuracy : 0.7048          
#       'Positive' Class : 0          

# 计算召回率和F1值
recall_train <- confusion_train$byClass['Sensitivity']
recall_test <- confusion_test$byClass['Sensitivity']
f1_train <- 2 * (recall_train * confusion_train$byClass['Pos Pred Value']) / (recall_train + confusion_train$byClass['Pos Pred Value'])
f1_test <- 2 * (recall_test * confusion_test$byClass['Pos Pred Value']) / (recall_test + confusion_test$byClass['Pos Pred Value'])

# 打印召回率和F1值
cat("Train Recall:", recall_train, "\n")
cat("Train F1 Score:", f1_train, "\n")
cat("Test Recall:", recall_test, "\n")
cat("Test F1 Score:", f1_test, "\n")


# 7.ROC曲线和AUC值
# 计算ROC曲线和AUC值
roc_train <- roc(train_data$deteriorate, as.numeric(predictions_train))
roc_test <- roc(test_data$deteriorate, as.numeric(predictions_test))

# 打印ROC值
print(roc_train)
print(roc_test)

# 8.绘制ROC曲线
# 8.1 绘制训练集
#截断值
train_roc_best=coords(roc_train, "best",best.method = c("youden"), 
                      ret=c("threshold","sensitivity", "specificity"))
train_roc_best
#计算训练集ROC曲线的参数
train_roc_obj <- roc_train 
train_roc_auc <- auc(train_roc_obj)
#将ROC对象转换为数据框
train_roc_data <- data.frame(1-train_roc_obj$specificities, train_roc_obj$sensitivities)

#绘制ROC曲线
#基本ROC曲线
plot(roc_train,     
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

# 8.2 绘制验证集
#截断值
val_roc_best=coords(roc_test, "best",best.method = c("youden"),  
                    ret=c("threshold","sensitivity", "specificity"))
val_roc_best
#计算验证集ROC曲线的参数
val_roc_obj <- roc_test 
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
plot(roc_train,     
     print.auc=TRUE,     
     print.thres=T,   
     legacy.axes=T,   
     print.auc.y=0.3,   
     main="ROC of N-net")
plot(roc_test,    
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





















