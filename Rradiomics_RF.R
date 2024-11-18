#########################设置工作路径和数据读取#################
#设置工作路径
getwd()
setwd("D:/R work")

##一、数据准备
#1.数据读取（读进来）
#install.packages("readr")
library(readr)
mydata <- read_csv("pydatas4.csv")

# 导入必要的包,没有安装的可以先安装一下
library(dplyr) #数据处理使用
library(data.table) #数据读取使用
library(randomForest) #RF模型使用
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
# 定义训练集特征和目标变量
X_train <- train_data[, -1]
y_train <- as.factor(train_data$deteriorate)

#构建变量公式
form_xy <- as.formula(  
  paste0("deteriorate~",paste(colnames(X_train),collapse = "+"))
)
form_xy

#在训练集上构建RF初始模型
set.seed(123)
rf_model0 <- randomForest(form_xy,data = train_data,                          
                          nodesize=1,                          
                          importance=TRUE,                          
                          proximity=TRUE)
#模型概况
rf_model0

#可视化整体错误率与树的数量之间的关系
plot(rf_model0,main = "ERROR & TREES")
legend("top",       
       legend=colnames(rf_model0$err.rate),       
       lty = 1:3,       
       col = 1:3,       
       horiz = TRUE)
#随着树的数量增加错误率逐渐降低并渐趋平稳。中间的黑色线条是整体的错误率，上下两条是结果变量中两个类别的错误率。
#训练集初始预测结果；混淆矩阵
predict_initial <- predict(rf_model0,newdata = train_data)
matrix0 <- table(y_train,predict_initial)
#快速计算准确率
acc_initial <- sum(diag(matrix0)/nrow(train_data))
#获得初始模型效果 =1（祖传过拟合）
acc_initial 

#step3.调整参数
#主要参数：mtry和ntree
#方法一：使用tuneRF()函数调整参数mtry
set.seed(123)
tuned_mtry <- tuneRF(X_train, y_train, 
                     stepFactor = 2, 
                     improve = 0.05, 
                     trace = TRUE,  
                     plot = TRUE)#mtry=5时,OOB error最小23.68%

#方法二：使用遍历法选择最优mtry参数值
set.seed(123)
errRate <- c(1)
for (i in 1:11) {  
  m <- randomForest(deteriorate~.,data = train_data,mtry=i,proximity=TRUE)  
  err <- mean(m$err.rate)  
  errRate[i] <- err}
print(errRate)
which.min(errRate)#mtry=4时,OOB error最小0.2920143

#方法三：使用caret包调整参数mtry 27
#创建训练控制对象
set.seed(123)
trainControl <- trainControl(method = "cv",  
                             summaryFunction=twoClassSummary,
                             number = 10,     
                             classProbs = TRUE,    
                             savePredictions = TRUE) #使用5折/10折交叉验证
rfFit <- train(deteriorate~.,data=train_data,method="rf",  
               trContro=trainControl )
rfFit 

#调整参数ntree
#查看整体错误率最小时有几棵树
which.min(rf_model0$err.rate[,1])
#34

#可视化树的节点数
hist(treesize(rf_model0), 
     main="No. of Nodes for Trees", 
     col="#C49488")

#手动调整参数ntree
#使用最佳mtry值固定mtry
ctrl <- trainControl(method = "cv", number = 10)
bestgrid <- expand.grid(mtry = c(27))  
# 每棵树中用于分裂的特征数量
#定义模型列表，存储每一个模型评估结果
modellist <- list()
#调整的参数是决策树的数量
for (ntree in c(100,200,300,400,500)) {  
  set.seed(123)  
  fit <- train(x =X_train, y = y_train, method="rf", 
               metric="Accuracy", tuneGrid=bestgrid,   
               trControl=ctrl, ntree=ntree)  
  key <- toString(ntree)  
  modellist[[key]] <- fit
  }
# compare results
results <- resamples(modellist)
# 输出最佳模型和参数
summary(results)
# mtry=4, ntree = 500, accuracy=0.5714286;
# mtry=5, ntree = 500, accuracy=0.6666667;
# mtry=27, ntree = 100, accuracy=0.5;

#step4.参数配置
#使用最佳超参数重新训练模型
#本例设置mtry=5，ntree=500示例运行
set.seed(123)
rf_model2 <- randomForest(x = X_train, y = y_train,                          
                          mtry = 5,                          
                          ntree = 500,                          
                          nodesize=1,                          
                          importance=TRUE,                          
                          proximity=TRUE)
# 输出最终模型
print(rf_model2)
#绘制袋外均方误差：OOB误差 
plot(rf_model2,lwd=2,main="OOB MSE")  #OOB estimate of  error rate: 26.32%

#查看各个变量的重要性
importance(rf_model2)
#MeanDecreaseGini越大说明变量越重要

#可视化变量重要性
varImpPlot(rf_model2,           
           main = "varImpPlot",           
           n.var=11,#Top变量个数           
           scale=T,#显示横坐标           
           cex = 1#字体大小
           )
#分别输出
#type=1是以准确率递减方法得到维度重要性值，type=2为基尼系数方法
varImpPlot(rf_model2, main = "variable importance",type=1)
varImpPlot(rf_model2, main = "variable importance",type=2)
#根据变量重要性排序可选择变量。通常选择前5个，前10个，或者大于所有变量性平均值(中位数，百分位数等)的变量等

#绘制直方图，查看系数重要性
#variable importance plot-PSA score
#使用ggRandomForests包美化图形
library(ggRandomForests)
gg_rf2 <- gg_vimp(rf_model2)
plot(gg_rf2) #MeanDecreaseGini

#使用vip包绘图
library(vip)
vip(rf_model2)#MeanDecreaseAccuracy

#美化变量重要性
#绘图数据
#获取特征重要性并排序
importance_scores <- importance(rf_model2)
feature_importance <- data.frame(variable = rownames(importance_scores), Importance = importance_scores[, "MeanDecreaseGini"])
ordered_features <- feature_importance[order(-feature_importance$Importance), ]
#设置渐变颜色范围
color_range <- c("#C8EBF6", "#154778")
#绘制横向柱状图
ggplot(ordered_features, aes(x = reorder(variable, Importance), y = Importance, fill = Importance, label = round(Importance, 2))) +  
  geom_bar(stat = "identity") +  
  geom_text(size = 3, position = position_stack(vjust = 0.5), color = "black") + # 添加标签  
  scale_fill_gradient(low = color_range[1], high = color_range[2]) + # 使用渐变填充 
  theme(    
    plot.title = element_text(hjust = 0.5, size = 16),     
    plot.caption = element_text(size = 12),     
    axis.text = element_text(size = 12),     
    axis.title = element_text(size = 15)  
    ) +  
  labs(title = "Important Variables", x = "Variables", y = "Importance") +  
  coord_flip() 

#绘制环状图
color_range <- c("#EE9C4B", "#EA5514")
ggplot(ordered_features, aes(x = reorder(variable, Importance), y = Importance, fill = Importance, label = round(Importance, 2))) +  
  geom_bar(stat = "identity") +  
  ylim(-200,220) +  
  geom_text(size = 5, position = position_stack(vjust = 0.5), color = "black") +   
  scale_fill_gradient(low = color_range[1], high = color_range[2]) +   
  theme(    
    plot.title = element_text(hjust = 0.5, size = 16),     
    plot.caption = element_text(size = 12),     
    axis.text = element_text(size = 8),     
    axis.title = element_text(size = 15)  ) +  
  labs(title = "Important Variables", x = "Variables", y = "Importance") +  
  coord_polar()

#绘制偏相关图
partialPlot(x=rf_model2,            
            pred.data = train_data,            
            main = "Patial Dependence Plot of rf_model2",            
            x.var = deteriorate,            
            which.class = "1",            
            ylab = "Yes")
prop.table(table(train_data$deteriorate,train_data$deteriorate),margin=1)

#提取某一棵树
getTree(rf_model2,k=6)
#输出这颗树在分支时的各种细节
#观察模型中变量具体去做权重

#邻近矩阵的多维图
MDSplot(rf_model2,train_data$deteriorate,        
        main="Multi-dimensional Scaling Plot of Proximity Matrix_RF_model0")

##交叉验证变量筛选
#使用randomForest包中的rfcv()函数进行交叉验证递归特征消除
#十折交叉验证
set.seed(123)
train.rfcv <- rfcv(trainx=train_data[,-1],trainy=train_data$deteriorate,                 
                   cv.fold=10,                 
                   recursive = TRUE)
train.rfcv$n.var
train.rfcv$error.cv
with(train.rfcv,plot(n.var,error.cv,type="o",lwd=2))

#重复5次
train.re.rfcv <- replicate(5, rfcv(trainx=train_data[,-1], train_data$deteriorate,                           
                                   cv.fold=10,step=1.5), simplify=FALSE)
train.re.rfcv <- data.frame(sapply(train.re.rfcv, '[[', 'error.cv'))
train.re.rfcv$variables <- rownames(train.re.rfcv)
train.re.rfcv <- reshape2::melt(train.re.rfcv, id = 'variables')
train.re.rfcv$variables <- as.numeric(as.character(train.re.rfcv$variables))
plot.rfcv <- ggplot(train.re.rfcv, aes(variables, value)) +  
  geom_smooth(se = FALSE)+  
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +  
  labs(title = '',x = 'Number of Variables', y = 'Cross-validation error')
plot.rfcv

#提取并保存重要变量数据
importance <- importance(rf_model2)
importance <- as.data.frame(importance)
top5_acc <- head(rownames(importance[order(-importance$MeanDecreaseAccuracy),]),5)
top5_data <- data[,top5_acc]
write.csv(top5_data,"top5_acc_data.csv")


#Step4.应用于训练集
#训练集模型预测：混淆矩阵和ROC曲线
#训练集预测概率
predict_train <- predict(rf_model2,newdata = train_data)
#训练集预测结果
matrix_train <- table(y_train,predict_train)
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
#        Pos Pred Value : 1.0000     
#         Neg Pred Value : 1.0000     
#              Precision : 1.0000     
#                 Recall : 1.0000     
#                     F1 : 1.0000     
#             Prevalence : 0.3289     
#         Detection Rate : 0.3289     
#   Detection Prevalence : 0.3289     
#      Balanced Accuracy : 1.0000     
#       'Positive' Class : 1  

#绘制混淆矩阵
#本例以训练集混淆矩阵进行示例，验证集方法相同，替换即可
#混淆矩阵转换为数据框
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
train_predprob <- predict(rf_model2,newdata = train_data,type="prob")
#AUC
train_roc <- roc(response = train_data$deteriorate, predictor = train_predprob[,2])
train_roc
#截断值
train_roc_best=coords(train_roc, "best",best.method = c("youden"),                       
                      ret=c("threshold","sensitivity", "specificity"))
train_roc_best
#计算训练集ROC曲线的参数
train_roc_obj <- train_roc 
train_roc_auc <- auc(train_roc_obj)
#将ROC对象转换为数据框
train_roc_data <- data.frame(train_roc_obj$specificities, train_roc_obj$sensitivities)

#绘制ROC曲线
#ggplot2包美化ROC
ggplot(train_roc_data, aes(x = train_roc_obj$specificities, y = train_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.25, y = 0.25, label = paste("AUC =", round(train_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.7, y = 0.85, label = paste("Cutoff =", round(train_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(1, 0), ylim = c(0, 1)) +  
  theme_pubr() +  
  labs(x = "Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of train set") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+  
  geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1),color="#5d6174",linetype="dashed") +  
  theme_prism(border = T)+  
  geom_point(x= train_roc_best$specificity ,y=train_roc_best$sensitivity,color="#D2431C",size=3)+  
  theme(axis.text = element_text (size = 10))+  
  theme(axis.title.x=element_text(vjust=2, size=15,face = "plain"))+  
  theme(axis.title.y=element_text(vjust=2, size=15,face = "plain"))

#绘制训练集校准曲线
#本例使用rms包中的val.prob()函数
train_cv <- val.prob(train_predprob [, 2],                    
                     as.numeric(train_data$deteriorate)-1,                               
                     logistic.cal = FALSE,                               
                     statloc = F,                               
                     riskdist = c("predicted","calibrated"),                     
                     legendloc = c(0.8,0.25))
title(main = "calibration curve of train")

#Step5.应用于验证集
#使用最终模型预测验证集
val_x <- test_data[, -1]
val_y <- test_data$deteriorate
#验证集预测概率
predict_val <- predict(rf_model2,newdata = val_x)
#验证集预测结果
matrix_val <- table(val_y,predict_val)
#验证集混淆矩阵
confusionMatrix_val <- confusionMatrix(data = predict_val,
                                       reference = test_data$deteriorate,
                                       positive = "1",
                                       mode = "everything")
#输出训练集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
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
confusion_matrix_df3 <- as.data.frame.matrix(confusionMatrix_val$table)
colnames(confusion_matrix_df3) <- c("sensoring","terminal event")
rownames(confusion_matrix_df3) <- c("sensoring","terminal event")
draw_data3 <- round(confusion_matrix_df3 / rowSums(confusion_matrix_df3),2)
draw_data3$real <- rownames(draw_data3)
draw_data3 <- melt(draw_data3)
#绘制训练集混淆矩阵热图
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
val_predprob <- predict(rf_model2,newdata = test_data,type="prob")
#AUC
val_roc <- roc(response = test_data$deteriorate, predictor = val_predprob[,2])
val_roc
#截断值
val_roc_best=coords(val_roc, "best",best.method = c("youden"), 
                    ret=c("threshold","sensitivity", "specificity"))
val_roc_best
#计算验证集ROC曲线的参数
val_roc_obj <- val_roc 
val_roc_auc <- auc(val_roc_obj)
#将ROC对象转换为数据框
val_roc_data <- data.frame(val_roc_obj$specificities, val_roc_obj$sensitivities)

#绘制验证集ROC曲线
#ggplot2包美化ROC
ggplot(val_roc_data, aes(x = val_roc_obj$specificities, y = val_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.25, y = 0.25, label = paste("AUC =", round(val_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.7, y = 0.85, label = paste("Cutoff =", round(val_roc_best[,1], 2))), size = 6, color = "#D2431C") + 
  coord_cartesian(xlim = c(1, 0), ylim = c(0, 1)) + 
  theme_pubr() + 
  labs(x = "Specificity", y = "Sensitivity") + 
  ggtitle("ROC Curve of train set") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ 
  geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1),color="#5d6174",linetype="dashed") + 
  theme_prism(border = T)+ 
  geom_point(x=val_roc_best$specificity,y=val_roc_best$sensitivity,color="#D2431C",size=3)+ 
  theme(axis.text = element_text (size = 10))+ 
  theme(axis.title.x=element_text(vjust=2, size=15,face = "plain"))+ 
  theme(axis.title.y=element_text(vjust=2, size=15,face = "plain"))

#绘制验证集校准曲线
val_cv <- val.prob(val_predprob [, 2],   
                   as.numeric(test_data$deteriorate)-1,   
                   logistic.cal = FALSE,       
                   statloc = F,                
                   riskdist = c("predicted","calibrated"),     
                   legendloc = c(0.8,0.25))
title(main = "calibration curve of val")

#训练集与验证集ROC曲线叠加
plot(train_roc,     
     print.auc=TRUE,     
     print.thres=T,   
     legacy.axes=T,   
     print.auc.y=0.3,   
     main="ROC of RF")
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

#step6.模型解释
#随机森林模型的解释
#使用randomForestExplainer包对随机森林模型的结果进行解释
library(randomForestExplainer)
library(randomForest)
set.seed(123)
forest <- randomForest(status ~ ., data = train_data, localImp = TRUE)
forest
#最小深度分布
min_depth_frame <- min_depth_distribution(forest)
head(min_depth_frame, n = 10)
#查看分布图时需要综合平均值和分布
plot_min_depth_distribution(min_depth_frame)

#变量重要性
#同时计算多种衡量变量重要性的指标
importance_forest <- measure_importance(forest)
importance_forest

#可视化两个变量重要性指标mean_min_depth和times_a_root,同时把点的大小映射给no_of_nodes,相当于展示了3个指标
plot_multi_way_importance(importance_forest, size_measure = "no_of_nodes")
#通过x_measure/y_measure指定不同的指标
plot_multi_way_importance(importance_forest, 
                          x_measure = "accuracy_decrease",  
                          y_measure = "gini_decrease",      
                          size_measure = "p_value",     
                          no_of_labels = 5)

#使用GGally包中的ggpairs函数比较多个变量重要性指标的关系,即计算相关系数并进行可视化
plot_importance_ggpairs(importance_forest)
#更改图形的分布形式,展示不同指标的排名
plot_importance_rankings(importance_forest)

#交互作用解释
#根据mean_min_depth和no_of_trees选择前5个最重要的变量
(vars <- important_variables(importance_forest, k = 5, 
                             measures = c("mean_min_depth", "no_of_trees")))
#探索交互作用，很慢
interactions_forest <- min_depth_interactions(forest, vars)
#输出每个变量和其他变量交互作用的最小深度等信息，occurrences是交互作用出现的次数
head(interactions_forest[order(interactions_forest$occurrences, 
                               decreasing = TRUE), ])
#结果可视化
plot_min_depth_interactions(interactions_forest)
#图中横坐标是变量间的交互作用，按照交互作用出现次数递减排序。
#本例最前面的是nodes:age，其mean_min_depth也是最小的。

#选择连续变量探索交互作用对预测结果的影响plot_predict_interaction(forest, train_data, "age", "nodes")
#生成报告explain_forest(forest, interactions = TRUE, data = train_data)

#step7.SHAP分析
library(fastshap)
#使用ranger()函数建立随机森林模型
library(ranger)
set.seed(123)  
# for reproducibility
(rfo <- ranger(status ~ ., data = train_data, probability = TRUE))

#局部解释
#建立新观测值
new <- data.frame(rx=factor("Lev+5FU",levels = c("Obs","Lev","Lev+5FU")),
                  sex=0L, 
                  age=40,     
                  obstruct=0L,    
                  perfor=0L,      
                  adhere=0L,              
                  nodes=6,        
                  #status=0L,   
                  differ=1L,    
                  extent=1L,      
                  surg=1L,         
                  node4=0L  
                  )
#自定义预测函数，用于直接返回数值（回归任务）或者类别概率（分类任务）
pfun <- function(object, newdata) {  # prediction wrapper  
  unname(predict(object, data = newdata)$predictions[, 1])
  }
#计算观测new的概率
(new.prob <- pfun(rfo, newdata = new))
#计算train_data所有观测的平均概率
(baseline <- mean(pfun(rfo, newdata = train_data)))  
#观测new与平均的差值
(difference <- new.prob - baseline)

#建立解释器
set.seed(123)
(new.ex <- explain(rfo,X=train_data[,-8],nsim=10, 
                   pred_wrapper = pfun,newdata = new))

#fastshap包使用高效版本的蒙特卡洛（Monte-Carlo，MC）算法，应多次计算特征贡献，并将结果取平均值，以保证稳定性和准确性
#设置参数nsim为较大值（默认是1）
#本例计算new的1000个基于Shapley的特征贡献，并获取平均结果
set.seed(123)
(new.exn <- explain(rfo, X=train_data[,-8], pred_wrapper = pfun, 
                    newdata = new,    
                    nsim = 1000))
#explain()函数中设置adjust = TRUE
#fastshap使用的MC方法计算的Shapley值的加和不会等于相应预测和基线（即平均预测值）之间的差值,基于回归的调整校正总和
set.seed(2133)  
(new.exn.adj <- explain(rfo, X = train_data[,-8], pred_wrapper = pfun,  
                        newdata = new,     
                        nsim = 1000, adjust = TRUE))
sum(new.exn.adj) 

#使用shapviz包进行可视化
#创建瀑布图可视化特征对预测概率的影响
library(shapviz)
shapv <- shapviz(new.exn.adj, X = new, baseline = baseline)
sv_waterfall(shapv)
sv_force(shapv)

#全局解释
#对数据集中所有的观测都进行一遍SHAP解释，聚合结果后可得到全局SHAP解释
#使用1000次MC重复计算训练数据中每个观测的Shapley解释，并将生成的矩阵强制转换为tibble（以便更好地打印）
#设置shap_only=FALSE可以方便shapviz使用
library(fastshap)
library(ranger)
set.seed(123) 
ex.total<- explain(rfo, X = train_data[,-8], pred_wrapper = pfun, nsim = 100, adjust = TRUE,
                   shap_only = FALSE)
tibble::as_tibble(ex.total$shapley_values)

#可视化全局变量重要性（注意和基于重排的变量重要性区分）
#各个变量的多个Shapley值绝对值的平均值
shapv.global <- shapviz(ex.total)
#变量重要性图
sv_importance(shapv.global)  
#变量依赖蜂窝图
sv_importance(shapv.global, kind = "beeswarm")
#查看特征贡献age对其输入值的依赖性
sv_dependence(shapv.global, v = "age")
