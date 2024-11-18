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

#step2.构建LR模型
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

# 构建逻辑回归模型
model <- glm(deteriorate ~ ., data = train_data, family = binomial)
# 查看模型摘要
summary(model)

#step3.预测验证
#step3.1 训练集预测
train_predictions <- predict(model, newdata = train_data, deteriorate = "response")

# 将预测结果转化为二分类（0和1）
threshold <- 0.5 # 设置阈值
train_predicted_classes <- ifelse(train_predictions >= threshold, 1, 0)

# 模型评估
confusion_matrix <- table(train_data$deteriorate, train_predicted_classes)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ]) # 在二分类问题中，Recall和Sensitivity是相同的
f1_score <- 2 * precision * recall / (precision + recall)

# 打印评估指标
print(confusion_matrix)
cat("Accuracy: ", accuracy, "\n") #Accuracy:  0.9078947
cat("Precision: ", precision, "\n") #Precision:  0.9090909  
cat("Recall: ", recall, "\n") #Recall:  0.8
cat("F1 Score: ", f1_score, "\n") #F1 Score:  0.8510638 



#step3.2 验证集预测概率
predictions <- predict(model, newdata = test_data, deteriorate = "response")
predicted_classes <- ifelse(predictions >= 0.5, 1, 0)

# 将预测结果转化为二分类（0和1）
threshold <- 0.5 # 设置阈值
predicted_classes <- ifelse(predictions >= threshold, 1, 0)

# 模型评估
confusion_matrix <- table(test_data$deteriorate, predicted_classes)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ]) # 在二分类问题中，Recall和Sensitivity是相同的
f1_score <- 2 * precision * recall / (precision + recall)

# 打印评估指标
print(confusion_matrix)
cat("Accuracy: ", accuracy, "\n") #Accuracy:  0.7096774
cat("Precision: ", precision, "\n") #Precision:  0.5384615 
cat("Recall: ", recall, "\n") #Recall:  0.7
cat("F1 Score: ", f1_score, "\n") #F1 Score:  0.6086957 

#Step4. 训练集和测试集的ROC曲线
# 计算训练集ROC曲线
train_roc_obj <- roc(train_data$deteriorate, train_predictions)
train_roc_auc <- auc(train_roc_obj)

#截断值
train_roc_best=coords(train_roc_obj, "best",best.method = c("youden"),                     
                     ret=c("threshold","sensitivity", "specificity"))
train_roc_best

# 将ROC对象转换为数据框
train_roc_data <- data.frame(1 - train_roc_obj$specificities, train_roc_obj$sensitivities)

# 绘制ROC曲线
ggplot(train_roc_data, aes(x = 1 - train_roc_obj$specificities, y = train_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.35, y = 0.85, label = paste("Cutoff =", round(train_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  
  theme_minimal() +  # 使用一个基本主题
  labs(x = "1-Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of test set") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +  
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color="#5d6174", linetype="dashed") +  
  theme_prism(border = TRUE) +  # 假设theme_prism是自定义或来自另一个包
  geom_point(x = 1 - train_roc_best$specificity, y = train_roc_best$sensitivity, color = "#D2431C", size = 3) + 
  theme(axis.text = element_text(size = 10)) +  
  theme(axis.title.x = element_text(vjust = 2, size = 15, face = "plain")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15, face = "plain"))


# 计算测试集ROC曲线指标
test_roc_obj <- roc(test_data$deteriorate, predictions)
test_roc_auc <- auc(test_roc_obj)

#截断值
test_roc_best=coords(test_roc_obj, "best",best.method = c("youden"),                     
                    ret=c("threshold","sensitivity", "specificity"))
test_roc_best

# 将ROC对象转换为数据框
test_roc_data <- data.frame(1 - test_roc_obj$specificities, test_roc_obj$sensitivities)

# 绘制ROC曲线
ggplot(test_roc_data, aes(x = 1 - test_roc_obj$specificities, y = test_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.35, y = 0.85, label = paste("Cutoff =", round(test_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  
  theme_minimal() +  # 使用一个基本主题
  labs(x = "1-Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of test set") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +  
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color="#5d6174", linetype="dashed") +  
  theme_prism(border = TRUE) +  # 假设theme_prism是自定义或来自另一个包
  geom_point(x = 1 - test_roc_best$specificity, y = test_roc_best$sensitivity, color = "#D2431C", size = 3) + 
  theme(axis.text = element_text(size = 10)) +  
  theme(axis.title.x = element_text(vjust = 2, size = 15, face = "plain")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15, face = "plain"))

#训练集与验证集ROC曲线叠加
plot(train_roc_obj,     
     print.auc=TRUE,     
     print.thres=T,   
     legacy.axes=T,   
     print.auc.y=0.3,   
     main="ROC of LR")
plot(test_roc_obj,    
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















