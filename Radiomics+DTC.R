

#########################设置工作路径和数据读取#################
#设置工作路径
getwd()
setwd("D:/R work")

##一、数据准备
#1.数据读取（读进来）
#install.packages("readr")
library(readr)
mydata <- read_csv("pydatas4.csv")

#删除有缺失值的行
mydata<-na.omit(mydata)
View(mydata)


###########################################基于两独立样本参数/非参数检验+lasso+rmse的特征筛选###################################################
# 加载所需的库
library(readr)
library(dplyr)
library(MASS)
library(nortest)
library(rstatix)
library(rlang)

#设置工作路径
getwd()
setwd("D:/R work")

# 读取CSV文件
data <- read_csv("pydatas2.csv",col_names = TRUE)
View(data)
#print(names(data))

# 定义一个函数来执行正态性检验和相应的T检验或非参数检验
analyze_data <- function(data, deteriorate) {
  results <- list()  # 创建一个列表来存储结果
  
  # 迭代每一列，除了分组列
  for (col in 2:1051) {
    value_column <- names(data)[col]
    
    # 分别对两个分组进行正态性检验
    group1_data <- data %>% filter(deteriorate == 0) %>% pull(!!sym(value_column))
    group2_data <- data %>% filter(deteriorate == 1) %>% pull(!!sym(value_column))
    
    normality_test_group1 <- ad.test(group1_data)
    normality_test_group2 <- ad.test(group2_data)
    
    # 根据正态性检验结果选择检验方法
    if (normality_test_group1[["p.value"]] > 0.05 && normality_test_group2[["p.value"]] > 0.05) {
      # 正态分布，使用两独立样本T检验
      test_result <- t.test(group1_data, group2_data, var.equal = TRUE)
    } else {
      # 非正态分布，使用Wilcoxon秩和检验
      test_result <- wilcox.test(group1_data, group2_data)
    }
    
    # 存储结果
    results[[value_column]] <- test_result
  }
  
  return(results)
}

# 应用函数并输出P值小于0.05的结果
results <- analyze_data(data, "deteriorate")

# 打印P值小于0.05的结果
p_value_threshold <- 0.05
significant_results <- names(results)[sapply(results, function(res) !is.null(res$p.value) && res$p.value < p_value_threshold)]
if (length(significant_results) > 0) {
  print("P值小于0.05的数据集列名:")
  print(significant_results)
  
  #################################
  # 将显著结果转换为数据框
  #  significant_results_df <- data.frame(Column = significant_results,
  #                                       TTestPValue = sapply(results[significant_results], function(res) res$p.value),
  #                                       WilcoxPValue = sapply(results[significant_results], function(res) ifelse(!is.null(res$statistic), NA, res$p.value)),
  #                                       TestType = sapply(results[significant_results], function(res) ifelse(!is.null(res$statistic), "Wilcoxon", "T-Test")))
  
  # 写入CSV文件
  #  write.csv(significant_results_df, "significant_results.csv", row.names = FALSE)
  #} else {
  #  print("没有找到P值小于0.05的数据集列。")
  #}
  #################################
  ################################# 
  
  # 筛选显著列
  significant_data <- data %>% select(all_of(c("deteriorate", significant_results)))
  
  # 写入新的CSV文件
  write_csv(significant_data, "pydatas3.csv")
} else {
  print("没有找到P值小于0.05的数据集列。")
}

#2. 基于随机森林的递归特征消除
# Load necessary libraries
library(caret)
library(pROC)
library(ggplot2)
library(randomForest)

#设置工作路径
getwd()
setwd("D:/R work")

##一、数据准备
#1.数据读取（读进来）
#install.packages("readr")
library(readr)
mydata <- read_csv("pydatas3.csv")

#删除有缺失值的行
mydata<-na.omit(mydata)
View(mydata)

# 设置 RFE 控制函数
control <- rfeControl( 
  functions = rfFuncs,  # 使用随机森林 
  method = "cv",        # 交叉验证 
  number = 10           # 交叉验证的重复次数
)

# 利用多核加速加速
library(future)
plan("multisession",workers=8)


# 指定 RFE 过程中考虑的特征子集大小
sizes <- c(2:ncol(mydata))  # 考虑所有可能的特征子集大小

# 执行 RFE
set.seed(123)

rfe_results <- rfe(     #rfe：函数用来执行递归特征消除，输入包括特征集 x（除去目标变量的所有列），目标变量 y，要考虑的特征子集大小 sizes，以及控制参数
  x = mydata[, -1],     #x = Variables：特征数量用作x轴
  y = mydata$deteriorate,                   #y = Accuracy：准确率用作y轴
  sizes = sizes,                         #size = AccuracySD：准确率的标准偏差用来定义气泡的大小，表示模型在不同特征子集上的稳定性
  rfeControl = control  #rfeControl：设置了使用随机森林模型的 RFE 控制函数，指定了交叉验证的方法和重复次数。
)

# 查看 RFE 结果
print(rfe_results)

# 将RFE结果提取到数据框中
results_df <- rfe_results$results
results_df

# 查看 RFE 结果
plot(rfe_results, type = c("g", "o"))
# 提取选择的特征
selected_features <- predictors(rfe_results)
print(selected_features)
write.csv(selected_features,"RMSE_selected_features.csv")

# 训练最终的随机森林模型
final_model <- randomForest(mydata$deteriorate ~ ., data = mydata[, selected_features], ntree = 500, importance = TRUE)

# 获取特征重要性并排序
importance_scores <- importance(final_model)
print(importance_scores)
# 创建一个数据框，包含特征名称和它们的重要性指标
feature_importance <- data.frame(
  feature = rownames(importance_scores),
  IncMSE = importance_scores[, "%IncMSE"],
  IncNodePurity = importance_scores[, "IncNodePurity"]
)

# 根据%IncMSE的值对特征进行降序排序
ordered_features <- feature_importance[order(-feature_importance$IncMSE), ]

# 根据%IncMSE和IncNodePurity的值对特征进行降序排序
ordered_features <- feature_importance[order(-feature_importance$IncMSE, -feature_importance$IncNodePurity), ]
print(ordered_features)

# 绘制柱状图
library(ggplot2)
library(ggthemes)

color_range <- c("#D8BFD8", "#8B008B")
ggplot(ordered_features, aes(x = reorder(feature, IncMSE), y = IncMSE, fill = IncMSE)) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = color_range[1], high = color_range[2]) + # 使用渐变填充 
  theme_few() +  
  theme( 
    plot.title = element_text(hjust = 0.5, size = 16),  
    plot.caption = element_text(size = 12),  
    axis.text = element_text(size = 12),  
    axis.title = element_text(size = 15) 
    ) + 
  labs(title = "Feature Importance(IncMSE)", x = "feature_name", y = "IncMSE") + 
  coord_flip() # 横向柱状图更易读


color_range <- c("#D8BFD8", "#8B008B")
ggplot(ordered_features, aes(x = reorder(feature, IncNodePurity), y = IncNodePurity, fill = IncNodePurity)) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = color_range[1], high = color_range[2]) + # 使用渐变填充 
  theme_few() +  
  theme( 
    plot.title = element_text(hjust = 0.5, size = 16),  
    plot.caption = element_text(size = 12),  
    axis.text = element_text(size = 12),  
    axis.title = element_text(size = 15) 
  ) + 
  labs(title = "Feature Importance(IncNodePurity)", x = "feature_name", y = "IncNodePurity") + 
  coord_flip() # 横向柱状图更易读

library(pROC)
# 确定阈值
threshold <- 0.5 # 这个阈值需要根据实际情况来确定

# 预测回归值
predictions <- predict(final_model, newdata = mydata)

# 将回归预测转换为分类预测
binary_predictions <- ifelse(predictions > threshold, 1, 0)

# 计算ROC曲线
roc_obj <- roc(response = mydata$deteriorate, predictor = as.numeric(binary_predictions))

# 绘制ROC曲线
plot(roc_obj, print.auc=TRUE, print.thres=TRUE,main = "ROC CURVE", col= "black",print.thres.col="black",identity.col="black",
     identity.lty=1,identity.lwd=1)

# 计算AUC
auc_value <- auc(roc_obj)
print(auc_value)

saveRDS(featsweep,file = 'rfe_results.rds') #保存 rds
featsweep <-readRDS('rfe_results.rds') # 读取 rd

#2.基于Lasso回归实现特征筛选
library(glmnet)
library(Matrix)
library(rms)
library(foreign)
library(readr)
setwd("D:/R work")

mydata <- read_csv("pydatas3.csv")

mydata<-na.omit(mydata)

library(psych)

describe(mydata)
str(mydata)
View(mydata)

x <- as.matrix(data.frame(mydata[,c(2:48)]))
#write.csv(x,file='xdata.csv')
y <- as.matrix(mydata[,1])
fit<-glmnet(x,y,alpha=0.5,family='binomial')

plot(fit)

plot(fit, xvar = "lambda", label = TRUE)
plot(fit, xvar = "lambda", label = FALSE)

#abline(v=log(c(cv.fit$lambda.min,cv.fit$lambda.1se)),lty=2)  #运行CVlasso后可以运行
print(fit)

#可以直接用Lasso模型进行预测
mydata$lassopred <-predict(fit,type="response",newx=x[1:107,],s=0.000026)  #x[1:685,]685代表咱dev中的样本量或记录数

View(mydata)

#3.CVlasso交叉验证#目的：(1)筛选变量，找到最优模型，(2)利用最优模型预测概率
set.seed(123)
cv.fit <- cv.glmnet(x,y,alpha=1,nfolds = 10)
plot(cv.fit)
abline(v=log(c(cv.fit$lambda.min,cv.fit$lambda.1se)),lty=2)

#?cv.glmnet
#?glmnet
#如果取最小值时 # 0.02188684 AUC=0.8915
cv.fit$lambda.min
Coefficients <- coef(fit, s = cv.fit$lambda.min)
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index]
Active.Index
Active.Coefficients
row.names(Coefficients)[Active.Index]
mydata$minlassopred <-predict(fit,type="response",newx=x[1:107,],s=cv.fit$lambda.min)  #x[1:685,]685代表咱dev中的样本量或记录数

#如果取1倍标准误 # 0.06683924 AUC=0.8788
cv.fit$lambda.1se
Coefficients <- coef(fit, s = cv.fit$lambda.1se)
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index]
Active.Index
Active.Coefficients
row.names(Coefficients)[Active.Index]
mydata$selassopred <-predict(fit,type="response",newx=x[1:107,],s=cv.fit$lambda.1se)

#4.lasso回归最优模型选择
#交叉验证与Bootstrap验证
getwd()
setwd("D:/R work")

library(readr)
mydata <- read_csv("pydatas6.csv")

#删除有缺失值的行
mydata<-na.omit(mydata)
View(mydata)

names(mydata)

#必须做，了解变量类型非常重要
str(mydata)

mydata$deteriorate<-factor(mydata$deteriorate,levels=c(0,1),labels=c("no","yes"))
mydata$ALT<-factor(mydata$ALT,levels=c(0,1),labels=c("no","yes"))
dev = mydata[mydata$dataset==1,]
vad = mydata[mydata$dataset==0,]

#install.packages("caret")
library(caret)
library(ggplot2)
library(lattice)

formula1<-as.formula(deteriorate ~ DependenceNonUniformity.Brian_stem + 
                       GrayLevelVariance.Brian_stem + 
                       LargeDependenceHighGrayLevelEmphasis.Brian_stem + 
                       LongRunLowGrayLevelEmphasis.Brian_stem + 
                       SizeZoneNonUniformity.Brian_stem + 
                       SizeZoneNonUniformityNormalized.Brian_stem + 
                       SmallAreaEmphasis.Brian_stem + 
                       Contrast.1.Brian_stem + 
                       SmallDependenceEmphasis.CC_Central + 
                       SmallDependenceHighGrayLevelEmphasis.CC_Central + 
                       GrayLevelVariance.2.CC_Central + 
                       ZonePercentage.CC_Central + 
                       Busyness.Left.Caudate + 
                       Strength.Left.Caudate + 
                       SmallAreaLowGrayLevelEmphasis.Right.Caudate + 
                       ZoneEntropy.Right.Caudate + 
                       GrayLevelNonUniformityNormalized.1.Right.Putamen + 
                       Contrast.1.Right.Thalamus)
formula1

formula2<-as.formula(deteriorate ~ DependenceNonUniformity.Brian_stem + 
                       LargeDependenceHighGrayLevelEmphasis.Brian_stem + 
                       LongRunHighGrayLevelEmphasis.Brian_stem + 
                       SizeZoneNonUniformity.Brian_stem + 
                       SizeZoneNonUniformityNormalized.Brian_stem + 
                       SmallAreaEmphasis.Brian_stem + 
                       SmallDependenceEmphasis.CC_Central + 
                       SmallDependenceHighGrayLevelEmphasis.CC_Central + 
                       GrayLevelVariance.2.CC_Central + 
                       ZonePercentage.CC_Central + 
                       Busyness.Left.Caudate + 
                       Strength.Left.Caudate + 
                       LowGrayLevelZoneEmphasis.Right.Caudate + 
                       SmallAreaLowGrayLevelEmphasis.Right.Caudate + 
                       ZoneEntropy.Right.Caudate + 
                       GrayLevelNonUniformityNormalized.1.Right.Putamen + 
                       LongRunHighGrayLevelEmphasis.Right.Thalamus + 
                       Contrast.1.Right.Thalamus)
formula2

# 生成预测概率
modelA <- glm(deteriorate ~ DependenceNonUniformity.Brian_stem + 
                GrayLevelVariance.Brian_stem + 
                LargeDependenceHighGrayLevelEmphasis.Brian_stem + 
                LongRunLowGrayLevelEmphasis.Brian_stem + 
                SizeZoneNonUniformity.Brian_stem + 
                SizeZoneNonUniformityNormalized.Brian_stem + 
                SmallAreaEmphasis.Brian_stem + 
                Contrast.1.Brian_stem + 
                SmallDependenceEmphasis.CC_Central + 
                SmallDependenceHighGrayLevelEmphasis.CC_Central + 
                GrayLevelVariance.2.CC_Central + 
                ZonePercentage.CC_Central + 
                Busyness.Left.Caudate + 
                Strength.Left.Caudate + 
                SmallAreaLowGrayLevelEmphasis.Right.Caudate + 
                ZoneEntropy.Right.Caudate + 
                GrayLevelNonUniformityNormalized.1.Right.Putamen + 
                Contrast.1.Right.Thalamus ,data=mydata, 
              family = binomial(link="logit"),x=TRUE)

modelB <- glm(deteriorate ~ DependenceNonUniformity.Brian_stem + 
                LargeDependenceHighGrayLevelEmphasis.Brian_stem + 
                LongRunHighGrayLevelEmphasis.Brian_stem + 
                SizeZoneNonUniformity.Brian_stem + 
                SizeZoneNonUniformityNormalized.Brian_stem + 
                SmallAreaEmphasis.Brian_stem + 
                SmallDependenceEmphasis.CC_Central + 
                SmallDependenceHighGrayLevelEmphasis.CC_Central + 
                GrayLevelVariance.2.CC_Central + 
                ZonePercentage.CC_Central + 
                Busyness.Left.Caudate + 
                Strength.Left.Caudate + 
                LowGrayLevelZoneEmphasis.Right.Caudate + 
                SmallAreaLowGrayLevelEmphasis.Right.Caudate + 
                ZoneEntropy.Right.Caudate + 
                GrayLevelNonUniformityNormalized.1.Right.Putamen + 
                LongRunHighGrayLevelEmphasis.Right.Thalamus + 
                Contrast.1.Right.Thalamus ,data=mydata, 
              family = binomial(link="logit"),x=TRUE)

mydata$predmodelA<- predict(newdata=mydata,modelA,"response")
mydata$predmodelB<- predict(newdata=mydata,modelB,"response")

#5. ROC找截点
library(pROC)

#在建模人群中绘制分别二条ROC曲线并给出阈值和ROC曲线下面积。
#建模集，model8的auc与roc分析
devmodelA <- roc(deteriorate~predmodelA, data = mydata,smooth=F)
devmodelA # 0.8915

devmodelB <- roc(deteriorate~predmodelB, data = mydata,smooth=F)
devmodelB # 0.8788


#ROC画图方法一(灵敏度与特异度ROC，非标准；但会给出AUC，阈值及对应的灵敏度和特异度)
plot(devmodelA, print.auc=TRUE, print.thres=TRUE,main = "ROC CURVE", col= "blue",print.thres.col="blue",identity.col="blue",
     identity.lty=1,identity.lwd=1)

plot(devmodelB, print.auc=TRUE, print.thres=TRUE,main = "ROC CURVE", col= "red",print.thres.col="red",identity.col="red",
     identity.lty=1,identity.lwd=1)

#绘制多条ROC


###################################影像组学特征筛选结束##############################

####################################临床一般资料比较####################
#设置工作路径
getwd()
setwd("D:/R work")

##一、数据准备
#1.数据读取（读进来）
#install.packages("readr")
library(readr)
mydata <- read_csv("pydatas5.csv")

#删除有缺失值的行
mydata<-na.omit(mydata)
View(mydata)

names(mydata)

#必须做，了解变量类型非常重要
str(mydata)

# 加载所需的库
library(readr)
library(dplyr)
library(MASS)
library(nortest)
library(rstatix)
library(rlang)

# 读取CSV文件
data <- read_csv("pydatas5.csv",col_names = TRUE)
View(data)
#print(names(data))

#1.定义一个函数来执行正态性检验和相应的T检验或非参数检验
analyze_data <- function(data, deteriorate) {
  results <- list()  # 创建一个列表来存储结果
  
  # 迭代每一列，除了分组列
  for (col in 3:8) {
    value_column <- names(data)[col]
    
    # 分别对两个分组进行正态性检验
    group1_data <- data %>% filter(deteriorate == 0) %>% pull(!!sym(value_column))
    group2_data <- data %>% filter(deteriorate == 1) %>% pull(!!sym(value_column))
    
    normality_test_group1 <- ad.test(group1_data)
    normality_test_group2 <- ad.test(group2_data)
    
    # 根据正态性检验结果选择检验方法
    if (normality_test_group1[["p.value"]] > 0.05 && normality_test_group2[["p.value"]] > 0.05) {
      # 正态分布，使用两独立样本T检验
      test_result <- t.test(group1_data, group2_data, var.equal = TRUE)
    } else {
      # 非正态分布，使用Wilcoxon秩和检验
      test_result <- wilcox.test(group1_data, group2_data)
    }
    
    # 存储结果
    results[[value_column]] <- test_result
  }
  
  return(results)
}

# 应用函数并输出P值小于0.05的结果
results <- analyze_data(data, "DWI")
print(results)

# 定义一个函数来执行正态性检验和相应的T检验或非参数检验
analyze_data <- function(data, DWI) {
  results <- list()  # 创建一个列表来存储结果
  
  # 迭代每一列，除了分组列
  for (col in 3:8) {
    value_column <- names(data)[col]
    
    # 分别对两个分组进行正态性检验
    group1_data <- data %>% filter(DWI == 0) %>% pull(!!sym(value_column))
    group2_data <- data %>% filter(DWI == 1) %>% pull(!!sym(value_column))
    
    normality_test_group1 <- ad.test(group1_data)
    normality_test_group2 <- ad.test(group2_data)
    
    # 根据正态性检验结果选择检验方法
    if (normality_test_group1[["p.value"]] > 0.05 && normality_test_group2[["p.value"]] > 0.05) {
      # 正态分布，使用两独立样本T检验
      test_result <- t.test(group1_data, group2_data, var.equal = TRUE)
    } else {
      # 非正态分布，使用Wilcoxon秩和检验
      test_result <- wilcox.test(group1_data, group2_data)
    }
    
    # 存储结果
    results[[value_column]] <- test_result
  }
  
  return(results)
}

# 应用函数并输出P值小于0.05的结果
results <- analyze_data(data, "DWI")
print(results)

#2.一般资料的卡方检验及fisher精确检验
# 加载必要的库
library(dplyr)

# 读取CSV文件
data <- read.csv("pydatas5.csv")

# 定义一个函数来执行卡方检验或Fisher精确检验
perform_chi_square_or_fisher <- function(data, row_var, col_vars) {
  results <- list()  # 创建一个列表来存储结果
  
  # 迭代每一列
  for (col in col_vars) {
    table_data <- table(data[[row_var]], data[[col]])
    
    # 检查期望频数是否满足卡方检验的期望条件
    if (all(table_data >= 5)) {
      # 进行卡方检验
      chi_square_result <- chisq.test(table_data)
      results[[col]] <- chi_square_result
    } else {
      # 进行Fisher精确检验
      fisher_result <- fisher.test(table_data)
      results[[col]] <- fisher_result
    }
  }
  
  return(results)
}

# 定义感兴趣的列
group_var <- "deteriorate"
col_vars <- c("sex", "Putamen", "brainstem", "thalamus", "callosum", "caudatum")

# 应用函数并输出P值小于0.05的结果
results <- perform_chi_square_or_fisher(data, group_var, col_vars)
print(results)

# 打印所有统计值
for (col in col_vars) {
  test_result <- results[[col]]
  cat("Test for variable:", col, "\n")
  print(summary(test_result))
  cat("\n")
}

# 如果需要，可以将结果写入CSV文件
# create_data_frame_for_csv <- function(results, col_vars) {
#   result_df <- data.frame()
#   for (col in col_vars) {
#     test_result <- results[[col]]
#     result_df <- rbind(result_df, data.frame(
#       Variable = col,
#       P_Value = pvalue(test_result),
#       Method = if (is.na(test_result$p.value)) "Fisher's Exact Test" else "Chi-squared Test"
#     ))
#   }
#   return(result_df)
# }
# result_df <- create_data_frame_for_csv(results, col_vars)
# write.csv(result_df, "test_results.csv")

# 定义感兴趣的列
group_var <- "DWI"
col_vars <- c("sex", "Putamen", "brainstem", "thalamus", "callosum")

# 应用函数并输出P值小于0.05的结果
results <- perform_chi_square_or_fisher(data, group_var, col_vars)
print(results)

#3.
#设置工作路径
getwd()
setwd("D:/R work")

install.packages("ggplot2")
install.packages("reshape2")
install.packages("pheatmap")

# 加载所需的库
library(readr)
library(dplyr)
library(MASS)
library(nortest)
library(rstatix)
library(rlang)
library(ggplot2)
library(reshape2)
library(pheatmap)

# 读取CSV文件
data <- read_csv("pydatas6.csv", col_names = TRUE)
View(data)

# 定义一个函数来执行正态性检验和相应的T检验或非参数检验
analyze_data <- function(data, deteriorate) {
  results <- list()  # 创建一个列表来存储结果
  
  # 迭代每一列，除了分组列
  for (col in 3:1052) {
    value_column <- names(data)[col]
    
    # 分别对两个分组进行正态性检验
    group1_data <- data %>% filter(!!sym(deteriorate) == 0) %>% pull(!!sym(value_column))
    group2_data <- data %>% filter(!!sym(deteriorate) == 1) %>% pull(!!sym(value_column))
    
    normality_test_group1 <- ad.test(group1_data)
    normality_test_group2 <- ad.test(group2_data)
    
    # 根据正态性检验结果选择检验方法
    if (normality_test_group1[["p.value"]] > 0.05 && normality_test_group2[["p.value"]] > 0.05) {
      # 正态分布，使用两独立样本T检验
      test_result <- t.test(group1_data, group2_data, var.equal = TRUE)
    } else {
      # 非正态分布，使用Wilcoxon秩和检验
      test_result <- wilcox.test(group1_data, group2_data)
    }
    
    # 存储结果
    results[[value_column]] <- test_result
  }
  
  return(results)
}

# 应用函数并输出P值小于0.05的结果
results <- analyze_data(data, "deteriorate")

# 打印P值小于0.05的结果
p_value_threshold <- 0.05
significant_results <- names(results)[sapply(results, function(res) !is.null(res$p.value) && res$p.value < p_value_threshold)]
if (length(significant_results) > 0) {
  print("P值小于0.05的数据集列名:")
  print(significant_results)
  
  # 将显著结果转换为数据框
  significant_results_df <- data.frame(Column = significant_results,
                                       TTestPValue = sapply(results[significant_results], function(res) res$p.value),
                                       WilcoxPValue = sapply(results[significant_results], function(res) ifelse(!is.null(res$statistic), NA, res$p.value)),
                                       TestType = sapply(results[significant_results], function(res) ifelse(!is.null(res$statistic), "Wilcoxon", "T-Test")))
  
  # 写入CSV文件
  write.csv(significant_results_df, "significant_results.csv", row.names = FALSE)
  
  # 筛选显著列
  significant_data <- data %>% select(deteriorate, all_of(significant_results))
  
  # 写入新的CSV文件
  write_csv(significant_data, "pydatas3.csv")
  
  # 确保value列是数值类型
  melted_data$value <- as.numeric(as.character(melted_data$value))
  
  ggplot(melted_data, aes(x = Column, y = variable, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.05, space = "Lab") +
    theme_minimal() +
    labs(x = "Column", y = "Test Type", title = "Significant Results Heatmap", fill = "P-Value") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
} else {
  print("没有找到P值小于0.05的数据集列。")
}

# 定义一个函数来格式化统计值
format_statistic <- function(res) {
  if (!is.null(res$statistic)) {
    # 如果存在统计量，则假定为T检验
    stat_type <- "T值"
    stat_value <- res$statistic
  } else if (!is.null(res$estimate) && !is.null(res$conf.low) && !is.null(res$conf.high)) {
    # 如果存在估计值和置信区间，则假定为Wilcoxon秩和检验
    stat_type <- "Z值"
    stat_value <- res$estimate
  }
  return(paste(stat_type, ":", round(stat_value, 3), "P值:", format.pval(res$p.value, digits = 3)))
}

# 初始化一个空的数据框来存储结果
results_df <- data.frame(Column = character(), Statistic = character(), P_Value = character(), stringsAsFactors = FALSE)

# 遍历results列表并收集统计值
for (col_name in names(results)) {
  res <- results[[col_name]]
  if (!is.null(res$p.value) && res$p.value < p_value_threshold) {
    stat_info <- format_statistic(res)
    results_df <- rbind(results_df, data.frame(Column = col_name, Statistic = stat_info, P_Value = format.pval(res$p.value, digits = 3), stringsAsFactors = FALSE))
  }
}

# 写入CSV文件
write.csv(results_df, "feature_result.csv", row.names = FALSE, quote = FALSE)

# 加载程序包
library(pheatmap)

# 读取CSV文件
data <- read.csv("feature_result.csv", stringsAsFactors = FALSE)

# 将Statistic和P_Value转换为数值类型
data$Statistic <- as.numeric(gsub("Statistic_z: ", "", data$Statistic))
data$P_Value <- as.numeric(gsub("P_Value: ", "", data$P_Value))

# 检查数据
if (length(data$P_Value) != length(data$Column)) {
  stop("P值的数量与列名的数量不匹配")
}

# 提取P值和对应的列名
p_values <- matrix(data$P_Value, nrow = length(data$Column), ncol = 1)
rownames(p_values) <- data$Column

# 绘制热图
pheatmap(p_values, 
         display_numbers = TRUE, 
         number_format = "%.3f", 
         color = colorRampPalette(c("blue", "white", "red"))(100), 
         main = "Significant Results Heatmap", 
         fontsize_number = 10,
         cluster_rows = FALSE, # 关闭行聚类
         cluster_cols = FALSE) # 关闭列聚类

# 加载所需的库
library(ggplot2)

# 读取CSV文件
data <- read.csv(text = "Column,Statistic_z,P_Value
Contrast.Brian_stem,1631.5,0.0138
DifferenceAverage.Brian_stem,1624.5,0.0156
DifferenceEntropy.Brian_stem,1632.5,0.0135
DifferenceVariance.Brian_stem,1637.5,0.0123
Id.Brian_stem,893.5,0.0151
Idm.Brian_stem,899.5,0.0168
Idn.Brian_stem,963.5,0.0494
InverseVariance.Brian_stem,1627.5,0.0148
JointEntropy.Brian_stem,1639.5,0.0118
SumEntropy.Brian_stem,1594.5,0.0266
SumSquares.Brian_stem,1573.5,0.0377
DependanceEntropy.Brian_stem,1630.5,0.014
DependanceNonUniformity.Brian_stem,-2.113,0.037
DependanceNonUniformityNormalized.Brian_stem,926.5,0.027
GrayLevelNonUniformity.Brian_stem,-2.052,0.0427
GrayLevelVariance.Brian_stem,1583.5,0.032
HighGrayLevelEmphasis.Brian_stem,954.5,0.0428
LargeDependenceEmphasis.Brian_stem,889.5,0.014
LargeDependenceHighGrayLevelEmphasis.Brian_stem,846.5,0.0061
LowGrayLevelEmphasis.Brian_stem,1577.5,0.0353
LongRunEmphasis.Brian_stem,875.5,0.0108
LongRunHighGrayLevelEmphasis.Brian_stem,851.5,0.00674
LongRunLowGrayLevelEmphasis.Brian_stem,916.5,0.0228
RunPercentage.Brian_stem,1626.5,0.0151
RunVariance.Brian_stem,880.5,0.0118
LargeAreaHighGrayLevelEmphasis.Brian_stem,-2.01,0.047
SizeZoneNonUniformity.Brian_stem,883.5,0.0125
SizeZoneNonUniformityNormalized.Brian_stem,-2.463,0.0154
SmallAreaEmphasis.Brian_stem,-2.442,0.0163
Contrast.1.Brian_stem,1561.5,0.0456
SmallDependenceEmphasis.CC_Central,1569.5,0.0402
SmallDependenceHighGrayLevelEmphasis.CC_Central,1600.5,0.024
GrayLevelVariance.2.CC_Central,1563.5,0.0442
LargeAreaEmphasis.CC_Central,957.5,0.0449
LargeAreaLowGrayLevelEmphasis.CC_Central,898.5,0.0165
ZonePercentage.CC_Central,2.422,0.0172
ClusterShade.Left.Caudate,962.5,0.0486
Busyness.Left.Caudate,885.5,0.013
Strength.Left.Caudate,1589.5,0.0289
LowGrayLevelZoneEmphasis.Right.Caudate,1575,0.0368
SmallAreaLowGrayLevelEmphasis.Right.Caudate,1591.5,0.028
ZoneEntropy.Right.Caudate,-2.367,0.0198
GrayLevelNonUniformity.2.Right.Putamen,888.5,0.0137
GrayLevelNonUniformityNormalized.1.Right.Putamen,907.5,0.0194
LargeDependenceHighGrayLevelEmphasis.Right.Thalamus,946.5,0.0377
LongRunHighGrayLevelEmphasis.Right.Thalamus,949.5,0.0395
Contrast.1.Right.Thalamus,1564.5,0.0435", header = TRUE, stringsAsFactors = FALSE)

# 定义显著性颜色
significant_color <- "red" # 显著的P值用红色表示
nonsignificant_color <- "grey" # 不显著的P值用灰色表示

# 绘制箱式图
p <- ggplot(data, aes(x = Column, y = Statistic_z)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Feature", y = "Statistic Z", title = "Boxplot of Statistical Values")

# 标注P值，并根据P值上色
p <- p + geom_text(aes(label = sprintf("%.4f", P_Value), color = ifelse(P_Value < 0.05, significant_color, nonsignificant_color)), 
                   vjust = -1, 
                   size = 3)

# 显示图形
print(p)

# 如果你想要保存图像
ggsave("boxplot_with_pvalues.pdf", p, width = 8, height = 6)

############################################一般资料分析结束##############################

###################################影像组学特征与临床特征相关性分析######################
#设置工作路径
getwd()
setwd("D:/R work")

##一、数据准备
#1.数据读取（读进来）
#install.packages("readr")

library(corrplot)
library(ggplot2)
library(ggpubr)
library(pheatmap)
library(readr)
library(data.table) # 数据读取
library(RColorBrewer) #颜色
library(circlize) #颜色
library(readr)

mydata <- read_csv("pydatas4.csv") # 数据读取

#删除有缺失值的行
mydata<-na.omit(mydata)
View(mydata)

library(ggplot2)
library(dplyr)
head(mydata)

library(psych)
library(reshape2)
states_cor <- psych::corr.test(mydata, adjust = "none")
states_cor
corr_r <- states_cor$r %>% reshape2::melt(value.name = "r.value")
corr_p <- states_cor$p %>% reshape2::melt(value.name = "p.value")
corr_df <- corr_r %>% left_join(corr_p)
head(corr_df)

##标注显著性符号及去除对角线r值和p值
library(insight)
corr_dta <- corr_df %>% mutate(sig = insight::format_p(p.value, stars_only = TRUE)) %>% 
  mutate(r.value = ifelse(Var1 == Var2, NA, r.value)) %>% 
  mutate(r_label = paste0(sprintf("%.3f", r.value), sig)) %>% 
  mutate(r_label2 = ifelse(Var1 == Var2, NA, r_label)) 
head(corr_dta)

ggplot(corr_dta, aes(Var1, Var2, fill = r.value)) + 
  geom_tile(color = "grey90") +
  geom_text(aes(label = r_label2) , fontface = "bold" ) + 
  scale_fill_distiller(palette = "RdBu", limits = c(-1, 1), na.value = "grey95") + 
  guides(size = "none") + 
  theme_minimal(base_size = 17, base_family = "serif") + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(colour = "black"))


##############################################决策树分类器###############################################
# 安装并加载必要的包
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("party")
install.packages("mlbench")

library(rpart)#决策树模型
library(rpart.plot)#决策树模型可视化
library(tidyverse) #数据处理
library(data.table) #读取数据
library(skimr) #数据概况
library(DataExplorer)#数据探索
library(GGally)#数据探索
library(caret) # 调参和计算模型评价参数
library(pROC) #绘制ROC曲线
library(dplyr) #数据处理
library(ggplot2) #绘图

#设置工作路径
getwd()
setwd("D:/R work")

#rm(list=ls()) # 清除当前工作环境中所有对象的命令

# 读取数据
mydata <- read.csv("pydatas4.csv")
View(mydata)          #查看数据
str(mydata)           #查看数据的类型，非常重要
summary(mydata)       #数据进行简单描述
mydata<-na.omit(mydata)   #删除缺失数据
names(mydata)                #查看mydata的变量列表

# 数据预处理（如果需要）
#mydata$deteriorate <- factor(mydata$deteriorate, levels = 0:1, labels = c("no", "yes"))
mydata$lenticula <- factor(mydata$lenticula, levels = 0:1, labels = c("no", "yes"))
mydata$brainstem <- factor(mydata$brainstem , levels = 0:1, labels = c("no", "yes"))
mydata$thalamus <- factor(mydata$thalamus , levels = 0:1, labels = c("no", "yes"))
mydata$callosum <- factor(mydata$callosum , levels = 0:1, labels = c("no", "yes"))
mydata$sex <- factor(mydata$sex , levels = 1:2, labels = c("man", "female"))

#因变量deteriorate因子化
mydata$deteriorate <- as.factor(mydata$deteriorate)
skim(mydata)#查看因变量分布：结局变量比例至少2:1，样本平衡，模型效果较好
table(mydata$deteriorate)#本例后续使用data_d数据集
data <- mydata

#使用GGally包查看变量分布
library("GGally")
p_ <- GGally::print_if_interactive
p_(ggbivariate(data, outcome ="deteriorate") +     
     scale_fill_brewer(type = "qual"))

#step2.构建决策树模型
#随机划分数据集
#设置随机种子，结果可复现
set.seed(123)
#createDataPartition()函数：分层抽样机制
train_index <- createDataPartition(y=data$deteriorate, p=0.7, list=F)
#训练集
train_data <- data[train_index, ]
#验证集
val_data <- data[-train_index,]
#查看划分数据集的因变量分布情况
table(train_data$deteriorate)
# 0   1 
#51  25 
table(val_data$deteriorate)
# 0   1 
#21  10 
#训练集构建决策树初始模型
#在训练集上使用默认参数建立决策树初始模型
set.seed(123)
tree_model0 <- rpart(deteriorate~.,data=train_data,method = "class")
#默认参数：minsplit：这是父节点可以进一步分割的最小观测数。默认值为20。
#maxdepth：这个参数防止树生长超过一定的深度/高度。默认值为30，超过这个值可能会导致32位机器上出现不良结果。
#cp（复杂度参数）：这是在每个节点所需的模型改进的最小值。它基于模型的成本复杂性定义。
#cp值是一个停止参数，有助于加快寻找分割的速度，因为它可以识别不符合此标准的分割并在走得太远之前进行剪枝。
#默认值通常为0.01，但如果构建非常深的树，这个默认值可能过于限制性。
#minbucket：这是任何终端节点中的最小观测数。默认值通常与minsplit相关，但具体默认值未在搜索结果中明确提及。
#split：用于选择分割的准则，默认使用基尼不纯度（Gini impurity），但也可以通过parms参数设置为信息增益（information gain）

#查看模型概况
tree_model0 
#输出一颗建立好的决策树及其每次分支的标准

#可视化
#plot绘图
par(xpd = TRUE)
plot(tree_model0,compress = TRUE)
text(tree_model0,use.n = TRUE)
#rpart.plot绘图
rpart.plot(tree_model0)

#训练集初始模型评估
#训练集初始模型预测结果
predict_initial <- predict(tree_model0,newdata = train_data,type = "class")
#计算混淆矩阵
caret::confusionMatrix(train_data$deteriorate, predict_initial)
#Confusion Matrix and Statistics
#Reference
#Prediction no yes
#no  43   8
#yes  2  23
#Accuracy : 0.8684          
#95% CI : (0.7713, 0.9351)
#No Information Rate : 0.5921          
#P-Value [Acc > NIR] : 1.466e-07       
#Kappa : 0.7191          
#Mcnemar's Test P-Value : 0.1138          
#            Sensitivity : 0.9556          
#            Specificity : 0.7419          
#         Pos Pred Value : 0.8431          
#         Neg Pred Value : 0.9200          
#             Prevalence : 0.5921          
#         Detection Rate : 0.5658          
#   Detection Prevalence : 0.6711          
#      Balanced Accuracy : 0.8487          
#       'Positive' Class : no       

##############################step3.调整参数#################################################
#方法一：通过cp参数进行后剪枝
#通过printcp函数查看复杂度和误差之间的数值关系
#输出参数复杂性值
printcp(tree_model0)

#可视化参数复杂性图像进行参数选择
plotcp(tree_model0,upper="splits")

#根据CP进行剪枝，通常选择xerror最小时的CP值，或1倍标准差法等
cp <- tree_model0$cptable[which.min(tree_model0$cptable[,"xerror"]), "CP"]
cp
tree_cp <- prune(tree_model0, cp = cp)
tree_cp
#画图展示剪枝后的树
rpart.plot(tree_cp)

#再次评估修剪后的树
pred_minsplit <- predict(tree_cp , newdata = train_data, type = "class")
caret::confusionMatrix(train_data$deteriorate, pred_minsplit)
#通过后剪枝的准确度Accuracy= 0.8026，结果不如修剪之间的树
#Confusion Matrix and Statistics
#Reference
#Prediction no yes
#no  49   2
#yes 13  12
#Accuracy : 0.8026          
#95% CI : (0.6954, 0.8851)
#No Information Rate : 0.8158          
#P-Value [Acc > NIR] : 0.680300        
#Kappa : 0.4965          
#Mcnemar's Test P-Value : 0.009823        
#            Sensitivity : 0.7903          
#            Specificity : 0.8571          
#         Pos Pred Value : 0.9608          
#         Neg Pred Value : 0.4800          
#             Prevalence : 0.8158          
#         Detection Rate : 0.6447          
#   Detection Prevalence : 0.6711          
#      Balanced Accuracy : 0.8237          
#       'Positive' Class : no    

#方法二：通过学习曲线调整超参数进行预剪枝
#使用学习曲线对单个超参数进行调优，以调整minsplit为例
res <- list()
for(i in 2:300){
  f <- rpart(deteriorate ~ ., data = train_data,
             control = rpart.control(minsplit = i ) 
             )  
  pred <- predict(f, newdata = train_data,type = "class")  
  acc <- caret::confusionMatrix(train_data$deteriorate, pred)[["overall"]][["Accuracy"]]  
  df <- data.frame(minsplit = i, accuracy = acc)  
  res[[i-1]] <- df
  }

acc.res <- do.call(rbind, res)

ggplot(acc.res, aes(minsplit, accuracy))+  
  geom_point()+  geom_line()+  
  geom_vline(xintercept = c(60,150),linetype = 2, color = "red")+  
  theme_bw()
#minsplit的学习曲线:minsplit在大约0~60之间，准确度accuracy达到了最大，大概是0.79以上
#输出准确度最高的minsplit值
acc.res[which.max(acc.res$accuracy),]
View(acc.res)

#minsplit=2，重新建立模型
set.seed(123)
tree_minsplit <- rpart(deteriorate ~ ., data = train_data,               
                       control = rpart.control(minsplit = 2))
rpart.plot(tree_minsplit)
#再次评估修剪后的树
pred_minsplit <- predict(tree_minsplit , newdata = train_data, type = "class")
caret::confusionMatrix(train_data$deteriorate, pred_minsplit)
#通过后剪枝的准确度Accuracy= 1
#Confusion Matrix and Statistics
#Reference
#Prediction no yes
#no  51   0
#yes  0  25
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
#       'Positive' Class : no  

#方法三：网格搜索
library(e1071)
#默认10折交叉验证
tune_obj <- tune.rpart(deteriorate ~ ., data = train_data,                       
                       minsplit = seq(50,100,5),                       
                       cp = c(0.0001,0.001,0.01,0.1,1)
                       ) 

tune_obj
#minsplit=70，重新建立模型
set.seed(123)
tree_tune <- rpart(deteriorate ~ ., data = train_data,                       
                   control = rpart.control(minsplit = 70)
                   )
rpart.plot(tree_tune)
#再次评估修剪后的树
pred_tune <- predict(tree_tune , newdata = train_data, type = "class")
caret::confusionMatrix(train_data$deteriorate, pred_tune)
#通过后剪枝的准确度Accuracy= 0.7237
#Confusion Matrix and Statistics
#Reference
#Prediction no yes
#no  33  18
#yes  3  22
#Accuracy : 0.7237          
#95% CI : (0.6091, 0.8201)
#No Information Rate : 0.5263          
#P-Value [Acc > NIR] : 0.0003461       
#Kappa : 0.4571          
#Mcnemar's Test P-Value : 0.0022502       
#            Sensitivity : 0.9167          
#            Specificity : 0.5500          
#         Pos Pred Value : 0.6471          
#         Neg Pred Value : 0.8800          
#             Prevalence : 0.4737          
#         Detection Rate : 0.4342          
#   Detection Prevalence : 0.6711          
#      Balanced Accuracy : 0.7333          
#       'Positive' Class : no 

#方法四：使用caret包进行交叉验证
library(caret)
ctrl <- trainControl(method = "cv", number = 10)
cv <- train(deteriorate ~ ., data = train_data, method = "rpart", trControl = ctrl)
print(cv)

#cp = 0.01791531，重新建立模型
set.seed(123)
tree_cv <- rpart(deteriorate ~ ., data = train_data,                   
                 control = rpart.control(cp = 0.01791531)
                 )
rpart.plot(tree_cv)
#再次评估修剪后的树
pred_cv <- predict(tree_cv , newdata = train_data, type = "class")
caret::confusionMatrix(train_data$deteriorate, pred_cv)
#通过后剪枝的准确度Accuracy= 0.8684，和原始模型一样

#################################step4-1.初始模型配置###########################################
# 本研究中初始模型已经为最优模型，故我们选择初始模型进行后续示例展示
#step4-1.参数配置
#本例以初始模型为最终模型
#特征重要性排序/变量重要性
tree_model0$variable.importance

#barplot
barplot(tree_model0$variable.importance)

#ggplot美化
import_train <- data.frame(importance=tree_model0$variable.importance)
ggplot(import_train,       
       aes(x=as_factor(rownames(import_train)),y=importance))+    
  geom_col()+    
  labs(x="variables")+    
  theme_classic()+    
  theme(axis.text.x = element_text(angle=15,hjust=1))    

#树形图
prp(tree_model0,type=2,    
    extra = 104,    
    tweak = 1,    
    fallen.leaves = TRUE,    
    main="Decision Tree")

#训练集模型预测：混淆矩阵和ROC曲线
#训练集预测概率
predict_train <- predict(tree_model0, newdata = train_data, type = "class")

#训练集混淆矩阵
confusionMatrix_train <- confusionMatrix(data = predict_train,
                                         reference = train_data$deteriorate, 
                                         positive = "yes",
                                         mode = "everything")
#输出训练集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_train)
#Confusion Matrix and Statistics
#Reference
#Prediction no yes
#no  43   2
#yes  8  23
#Accuracy : 0.8684          
#95% CI : (0.7713, 0.9351)
#No Information Rate : 0.6711          
#P-Value [Acc > NIR] : 7.353e-05       
#Kappa : 0.7191          
#Mcnemar's Test P-Value : 0.1138          
#            Sensitivity : 0.9200          
#            Specificity : 0.8431          
#         Pos Pred Value : 0.7419          
#         Neg Pred Value : 0.9556          
#              Precision : 0.7419          
#                 Recall : 0.9200          
#                     F1 : 0.8214          
#             Prevalence : 0.3289          
#         Detection Rate : 0.3026          
#   Detection Prevalence : 0.4079          
#      Balanced Accuracy : 0.8816          
#       'Positive' Class : yes

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
library(ggprism)
ggplot(draw_data2, aes(real,variable, fill = value)) +  
  geom_tile() +  
  geom_text(aes(label = scales::percent(value))) +  
  scale_fill_gradient(low = "#ECA9B0", high = "#81D8D0") +  
  labs(x = "True", y = "Predicted", title = "Confusion matrix of train set") +  
  theme_prism(border = T)+  theme(panel.border = element_blank(),        
                                  axis.ticks.y = element_blank(),        
                                  axis.ticks.x = element_blank(),        
                                  legend.position="none")

#绘制ROC曲线
#训练集集预测概率
library(pROC)
train_predprob <- predict(tree_model0,newdata = train_data, type="prob")
#AUC
train_roc <- roc(response = train_data$deteriorate, predictor = train_predprob[,2])
train_roc
#截断值
train_roc_best=coords(train_roc, "best", best.method = c("youden"), 
                      ret=c("threshold","sensitivity", "specificity"))
train_roc_best
#计算训练集ROC曲线的参数
train_roc_obj <- train_roc 
train_roc_auc <- auc(train_roc_obj)
#将ROC对象转换为数据框
train_roc_data <- data.frame(1-train_roc_obj$specificities, train_roc_obj$sensitivities)

#ggplot2包美化ROC
ggplot(train_roc_data, aes(x = 1 - train_roc_obj$specificities, y = train_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(train_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.35, y = 0.85, label = paste("Cutoff =", round(train_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  
  theme_minimal() +  # 使用一个基本主题
  labs(x = "1-Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of train set") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +  
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color="#5d6174", linetype="dashed") +  
  theme_prism(border = TRUE) +  # 假设theme_prism是自定义或来自另一个包
  geom_point(x = 1 - train_roc_best$specificity, y = train_roc_best$sensitivity, color = "#D2431C", size = 3) + 
  theme(axis.text = element_text(size = 10)) +  
  theme(axis.title.x = element_text(vjust = 2, size = 15, face = "plain")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15, face = "plain"))

#step4-2.原始模型应用于验证集
#验证集预测概率
predict_val <- predict(tree_model0,newdata = val_data,type = "class")
#验证集混淆矩阵
confusionMatrix_val <- confusionMatrix(data = predict_val,                                       
                                       reference = val_data$deteriorate,                                       
                                       positive = "yes",                                       
                                       mode = "everything")
#输出验证集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_val)
#Confusion Matrix and Statistics
#Reference
#Prediction no yes
#no  17   2
#yes  4   8
#Accuracy : 0.8065          
#95% CI : (0.6253, 0.9255)
#No Information Rate : 0.6774          
#P-Value [Acc > NIR] : 0.08547         
#Kappa : 0.5792          
#Mcnemar's Test P-Value : 0.68309         
#            Sensitivity : 0.8000          
#            Specificity : 0.8095          
#         Pos Pred Value : 0.6667          
#         Neg Pred Value : 0.8947          
#              Precision : 0.6667          
#                 Recall : 0.8000          
#                     F1 : 0.7273          
#             Prevalence : 0.3226          
#         Detection Rate : 0.2581          
#   Detection Prevalence : 0.3871          
#      Balanced Accuracy : 0.8048          
#       'Positive' Class : yes        


#绘制验证集混淆矩阵#混淆矩阵转换为数据框
# 安装并加载 reshape2 包
install.packages("reshape2")
library(reshape2)
confusion_matrix_df3 <- as.data.frame.matrix(confusionMatrix_val$table)
colnames(confusion_matrix_df3) <- c("sensoring","terminal event")
rownames(confusion_matrix_df3) <- c("sensoring","terminal event")
draw_data3 <- round(confusion_matrix_df3 / rowSums(confusion_matrix_df3),2)
draw_data3$real <- rownames(draw_data3)
draw_data3 <- melt(draw_data3)
#绘制验证集混淆矩阵热图
library(ggprism)
library(ggplot2)
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
library(pROC)
val_predprob <- predict(tree_model0,newdata = val_data,type="prob")
#AUC
val_roc <- roc(response = val_data$deteriorate, predictor = val_predprob[,2])
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

#绘制验证集ROC曲线
#ggplot2包美化ROC
library(ggplot2)
ggplot(val_roc_data, aes(x = 1-val_roc_obj$specificities, y = val_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(val_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.1, y = 0.6, label = paste("Cutoff =", round(val_roc_best[,1], 2))), size = 6, color = "#D2431C") + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
  theme_minimal() +  # 使用一个基本主题 
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
     main="ROC of Decision Tree")
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
#############################################初始模型end#################################
##################################step4-2.cp参数预剪枝模型配置#################################
# 本例以cp参数预剪枝模型进行
# 特征重要性排序/变量重要性
tree_cp$variable.importance

#barplot
barplot(tree_cp$variable.importance)

#ggplot美化
import_train <- data.frame(importance=tree_cp$variable.importance)
library(ggplot2)
library(dplyr)
ggplot(import_train,       
       aes(x=as_factor(rownames(import_train)),y=importance))+    
  geom_col()+    
  labs(x="variables")+    
  theme_classic()+    
  theme(axis.text.x = element_text(angle=15,hjust=1))    

#树形图
prp(tree_cp,type=2,    
    extra = 104,    
    tweak = 1,    
    fallen.leaves = TRUE,    
    main="Decision Tree")

#训练集模型预测：混淆矩阵和ROC曲线
#训练集预测概率
predict_train <- predict(tree_cp, newdata = train_data, type = "class")

#训练集混淆矩阵
library(caret)
confusionMatrix_train <- confusionMatrix(data = predict_train,
                                         reference = train_data$deteriorate, 
                                         positive = "yes",
                                         mode = "everything")
#输出训练集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_train)
#Confusion Matrix and Statistics
#Reference
#Prediction no yes
#no  49  13
#yes  2  12
#Accuracy : 0.8026          
#95% CI : (0.6954, 0.8851)
#No Information Rate : 0.6711          
#P-Value [Acc > NIR] : 0.008173        
#Kappa : 0.4965          
#Mcnemar's Test P-Value : 0.009823        
#            Sensitivity : 0.4800          
#            Specificity : 0.9608          
#         Pos Pred Value : 0.8571          
#         Neg Pred Value : 0.7903          
#              Precision : 0.8571          
#                 Recall : 0.4800          
#                     F1 : 0.6154          
#             Prevalence : 0.3289          
#         Detection Rate : 0.1579          
#   Detection Prevalence : 0.1842          
#      Balanced Accuracy : 0.7204          
#       'Positive' Class : yes  

#绘制混淆矩阵
#本例以训练集混淆矩阵进行示例，验证集方法相同，替换即可
#混淆矩阵转换为数据框
library(reshape2)
confusion_matrix_df2 <- as.data.frame.matrix(confusionMatrix_train$table)
colnames(confusion_matrix_df2) <- c("sensoring","terminal event")
rownames(confusion_matrix_df2) <- c("sensoring","terminal event")
draw_data2 <- round(confusion_matrix_df2 / rowSums(confusion_matrix_df2),2)
draw_data2$real <- rownames(draw_data2)
draw_data2 <- melt(draw_data2)
#绘制训练集混淆矩阵热图
library(ggprism)
ggplot(draw_data2, aes(real,variable, fill = value)) +  
  geom_tile() +  
  geom_text(aes(label = scales::percent(value))) +  
  scale_fill_gradient(low = "#ECA9B0", high = "#81D8D0") +  
  labs(x = "True", y = "Predicted", title = "Confusion matrix of train set") +  
  theme_prism(border = T)+  theme(panel.border = element_blank(),        
                                  axis.ticks.y = element_blank(),        
                                  axis.ticks.x = element_blank(),        
                                  legend.position="none")

#绘制ROC曲线
#验证集预测概率
library(pROC)
train_predprob <- predict(tree_cp,newdata = train_data,type="prob")
#AUC
train_roc <- roc(response = train_data$deteriorate, predictor = train_predprob[,2])
train_roc
#截断值
train_roc_best=coords(train_roc, "best",best.method = c("youden"),                     
                      ret=c("threshold","sensitivity", "specificity"))
train_roc_best
#计算验证集ROC曲线的参数
train_roc_obj <- train_roc 
train_roc_auc <- auc(train_roc_obj)
#将ROC对象转换为数据框
train_roc_data <- data.frame(1-train_roc_obj$specificities, train_roc_obj$sensitivities)

#ggplot2包美化ROC
library(ggplot2)
ggplot(train_roc_data, aes(x = 1 - train_roc_obj$specificities, y = train_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(train_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.35, y = 0.85, label = paste("Cutoff =", round(train_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  
  theme_minimal() +  # 使用一个基本主题
  labs(x = "1-Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of train set") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +  
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color="#5d6174", linetype="dashed") +  
  theme_prism(border = TRUE) +  # 假设theme_prism是自定义或来自另一个包
  geom_point(x = 1 - train_roc_best$specificity, y = train_roc_best$sensitivity, color = "#D2431C", size = 3) + 
  theme(axis.text = element_text(size = 10)) +  
  theme(axis.title.x = element_text(vjust = 2, size = 15, face = "plain")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15, face = "plain"))

#tree_cp模型应用于验证集
#验证集预测概率
predict_val <- predict(tree_cp,newdata = val_data,type = "class")
#验证集混淆矩阵
confusionMatrix_val <- confusionMatrix(data = predict_val,                                       
                                       reference = val_data$deteriorate,                                       
                                       positive = "yes",                                       
                                       mode = "everything")
#输出验证集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_val)

#绘制验证集混淆矩阵#混淆矩阵转换为数据框
confusion_matrix_df3 <- as.data.frame.matrix(confusionMatrix_val$table)
colnames(confusion_matrix_df3) <- c("sensoring","terminal event")
rownames(confusion_matrix_df3) <- c("sensoring","terminal event")
draw_data3 <- round(confusion_matrix_df3 / rowSums(confusion_matrix_df3),2)
draw_data3$real <- rownames(draw_data3)draw_data3 <- melt(draw_data3)
#绘制验证集混淆矩阵热图
library(ggprism)
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
val_predprob <- predict(tree_cp,newdata = val_data,type="prob")
#AUC
val_roc <- roc(response = val_data$deteriorate, predictor = val_predprob[,2])
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

#绘制验证集ROC曲线
#ggplot2包美化ROC
library(ggplot2)
library(ggpubr)
ggplot(val_roc_data, aes(x = 1-val_roc_obj$specificities, y = val_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(val_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.1, y = 0.6, label = paste("Cutoff =", round(val_roc_best[,1], 2))), size = 6, color = "#D2431C") + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
  theme_pubr() +  labs(x = "1-Specificity", y = "Sensitivity") +
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
     main="ROC of TREE_CV")
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
#######################################cp参数预剪枝模型配置end#############################
##################################step4-3.学习曲线调整超参数模型配置#################################
#本例以minsplit = 2模型为最终模型
#特征重要性排序/变量重要性
tree_minsplit$variable.importance

#barplot
barplot(tree_minsplit$variable.importance)

#ggplot美化
library(ggplot2)
library(dplyr)
import_train <- data.frame(importance=tree_minsplit$variable.importance)
ggplot(import_train,       
       aes(x=as_factor(rownames(import_train)),y=importance))+    
  geom_col()+    
  labs(x="variables")+    
  theme_classic()+    
  theme(axis.text.x = element_text(angle=15,hjust=1))    

#树形图
library(rpart.plot)
prp(tree_minsplit,type=2,    
    extra = 104,    
    tweak = 1,    
    fallen.leaves = TRUE,    
    main="Decision Tree")

#训练集模型预测：混淆矩阵和ROC曲线
#训练集预测概率
predict_train <- predict(tree_minsplit, newdata = train_data, type = "class")

#训练集混淆矩阵
library(caret)
confusionMatrix_train <- confusionMatrix(data = predict_train,
                                         reference = train_data$deteriorate, 
                                         positive = "yes",
                                         mode = "everything")
#输出训练集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_train)
#Confusion Matrix and Statistics
#Reference
#Prediction no yes
#no  51   0
#yes  0  25
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
#              Precision : 1.0000     
#                 Recall : 1.0000     
#                     F1 : 1.0000     
#             Prevalence : 0.3289     
#         Detection Rate : 0.3289     
#   Detection Prevalence : 0.3289     
#      Balanced Accuracy : 1.0000     
#       'Positive' Class : yes  

#绘制混淆矩阵
#本例以训练集混淆矩阵进行示例，验证集方法相同，替换即可
#混淆矩阵转换为数据框
library(reshape2)
confusion_matrix_df2 <- as.data.frame.matrix(confusionMatrix_train$table)
colnames(confusion_matrix_df2) <- c("sensoring","terminal event")
rownames(confusion_matrix_df2) <- c("sensoring","terminal event")
draw_data2 <- round(confusion_matrix_df2 / rowSums(confusion_matrix_df2),2)
draw_data2$real <- rownames(draw_data2)
draw_data2 <- melt(draw_data2)
#绘制训练集混淆矩阵热图
library(ggprism)
ggplot(draw_data2, aes(real,variable, fill = value)) +  
  geom_tile() +  
  geom_text(aes(label = scales::percent(value))) +  
  scale_fill_gradient(low = "#ECA9B0", high = "#81D8D0") +  
  labs(x = "True", y = "Predicted", title = "Confusion matrix of train set") +  
  theme_prism(border = T)+  theme(panel.border = element_blank(),        
                                  axis.ticks.y = element_blank(),        
                                  axis.ticks.x = element_blank(),        
                                  legend.position="none")

#绘制ROC曲线
#验证集预测概率
library(pROC)
train_predprob <- predict(tree_minsplit,newdata = train_data,type="prob")
#AUC
train_roc <- roc(response = train_data$deteriorate, predictor = train_predprob[,2])
train_roc
#截断值
train_roc_best=coords(train_roc, "best",best.method = c("youden"),                     
                    ret=c("threshold","sensitivity", "specificity"))
train_roc_best
#计算验证集ROC曲线的参数
train_roc_obj <- train_roc 
train_roc_auc <- auc(train_roc_obj)
#将ROC对象转换为数据框
train_roc_data <- data.frame(1-train_roc_obj$specificities, train_roc_obj$sensitivities)

#ggplot2包美化ROC
library(ggplot2)
ggplot(train_roc_data, aes(x = 1 - train_roc_obj$specificities, y = train_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(train_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.35, y = 0.85, label = paste("Cutoff =", round(train_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  
  theme_minimal() +  # 使用一个基本主题
  labs(x = "1-Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of train set") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +  
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color="#5d6174", linetype="dashed") +  
  theme_prism(border = TRUE) +  # 假设theme_prism是自定义或来自另一个包
  geom_point(x = 1 - train_roc_best$specificity, y = train_roc_best$sensitivity, color = "#D2431C", size = 3) + 
  theme(axis.text = element_text(size = 10)) +  
  theme(axis.title.x = element_text(vjust = 2, size = 15, face = "plain")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15, face = "plain"))

#step5-2.minsplit = 2模型应用于验证集#验证集预测概率
predict_val <- predict(tree_minsplit,newdata = val_data,type = "class")
#验证集混淆矩阵
confusionMatrix_val <- confusionMatrix(data = predict_val,                                       
                                       reference = val_data$deteriorate,                                       
                                       positive = "yes",                                       
                                       mode = "everything")
#输出验证集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_val)

#绘制验证集混淆矩阵#混淆矩阵转换为数据框
confusion_matrix_df3 <- as.data.frame.matrix(confusionMatrix_val$table)
colnames(confusion_matrix_df3) <- c("sensoring","terminal event")
rownames(confusion_matrix_df3) <- c("sensoring","terminal event")
draw_data3 <- round(confusion_matrix_df3 / rowSums(confusion_matrix_df3),2)
draw_data3$real <- rownames(draw_data3)draw_data3 <- melt(draw_data3)
#绘制验证集混淆矩阵热图
library(ggprism)
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
val_predprob <- predict(tree_minsplit,newdata = val_data,type="prob")
#AUC
val_roc <- roc(response = val_data$deteriorate, predictor = val_predprob[,2])
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

#绘制验证集ROC曲线
#ggplot2包美化ROC
library(ggplot2)
ggplot(val_roc_data, aes(x = 1-val_roc_obj$specificities, y = val_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(val_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.1, y = 0.6, label = paste("Cutoff =", round(val_roc_best[,1], 2))), size = 6, color = "#D2431C") + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
  theme_pubr() +  labs(x = "1-Specificity", y = "Sensitivity") +
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
########################################学习曲线调整超参数模型end#################
##################################step4-4.调整网格模型配置#################################
#本例以tree_tune模型为最终模型
#特征重要性排序/变量重要性
tree_tune$variable.importance

#barplot
barplot(tree_tune$variable.importance)

#ggplot美化
library(ggplot2)
library(dplyr)
import_train <- data.frame(importance=tree_tune$variable.importance)
ggplot(import_train,       
       aes(x=as_factor(rownames(import_train)),y=importance))+    
  geom_col()+    
  labs(x="variables")+    
  theme_classic()+    
  theme(axis.text.x = element_text(angle=15,hjust=1))    

#树形图
library(rpart.plot)
prp(tree_tune,type=2,    
    extra = 104,    
    tweak = 1,    
    fallen.leaves = TRUE,    
    main="Decision Tree")

#训练集模型预测：混淆矩阵和ROC曲线
#训练集预测概率
predict_train <- predict(tree_tune, newdata = train_data, type = "class")

#训练集混淆矩阵
library(caret)
confusionMatrix_train <- confusionMatrix(data = predict_train,
                                         reference = train_data$deteriorate, 
                                         positive = "yes",
                                         mode = "everything")
#输出训练集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_train)
# Confusion Matrix and Statistics
#Reference
#Prediction no yes
#no  33   3
#yes 18  22
#Accuracy : 0.7237          
#95% CI : (0.6091, 0.8201)
#No Information Rate : 0.6711          
#P-Value [Acc > NIR] : 0.19751         
#Kappa : 0.4571          
#Mcnemar's Test P-Value : 0.00225         
#            Sensitivity : 0.8800          
#           Specificity : 0.6471          
#         Pos Pred Value : 0.5500          
#         Neg Pred Value : 0.9167          
#              Precision : 0.5500          
#                 Recall : 0.8800          
#                     F1 : 0.6769          
#             Prevalence : 0.3289          
#         Detection Rate : 0.2895          
#   Detection Prevalence : 0.5263          
#      Balanced Accuracy : 0.7635          
#       'Positive' Class : yes   

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
library(ggprism)
ggplot(draw_data2, aes(real,variable, fill = value)) +  
  geom_tile() +  
  geom_text(aes(label = scales::percent(value))) +  
  scale_fill_gradient(low = "#ECA9B0", high = "#81D8D0") +  
  labs(x = "True", y = "Predicted", title = "Confusion matrix of train set") +  
  theme_prism(border = T)+  theme(panel.border = element_blank(),        
                                  axis.ticks.y = element_blank(),        
                                  axis.ticks.x = element_blank(),        
                                  legend.position="none")

#绘制ROC曲线
#验证集预测概率
library(pROC)
train_predprob <- predict(tree_tune,newdata = train_data,type="prob")
#AUC
train_roc <- roc(response = train_data$deteriorate, predictor = train_predprob[,2])
train_roc
#截断值
train_roc_best=coords(train_roc, "best",best.method = c("youden"),                     
                      ret=c("threshold","sensitivity", "specificity"))
train_roc_best
#计算验证集ROC曲线的参数
train_roc_obj <- train_roc 
train_roc_auc <- auc(train_roc_obj)
#将ROC对象转换为数据框
train_roc_data <- data.frame(1-train_roc_obj$specificities, train_roc_obj$sensitivities)

#ggplot2包美化ROC
library(ggplot2)
ggplot(train_roc_data, aes(x = 1 - train_roc_obj$specificities, y = train_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(train_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.35, y = 0.85, label = paste("Cutoff =", round(train_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  
  theme_minimal() +  # 使用一个基本主题
  labs(x = "1-Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of train set") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +  
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color="#5d6174", linetype="dashed") +  
  theme_prism(border = TRUE) +  # 假设theme_prism是自定义或来自另一个包
  geom_point(x = 1 - train_roc_best$specificity, y = train_roc_best$sensitivity, color = "#D2431C", size = 3) + 
  theme(axis.text = element_text(size = 10)) +  
  theme(axis.title.x = element_text(vjust = 2, size = 15, face = "plain")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15, face = "plain"))

#step5-2.tree_tune模型应用于验证集#验证集预测概率
predict_val <- predict(tree_tune,newdata = val_data,type = "class")
#验证集混淆矩阵
confusionMatrix_val <- confusionMatrix(data = predict_val,                                       
                                       reference = val_data$deteriorate,                                       
                                       positive = "yes",                                       
                                       mode = "everything")
#输出验证集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_val)
#Confusion Matrix and Statistics
#Reference
#Prediction no yes
#no  17   2
#yes  4   8
#Accuracy : 0.8065          
#95% CI : (0.6253, 0.9255)
#No Information Rate : 0.6774          
#P-Value [Acc > NIR] : 0.08547         
#Kappa : 0.5792          
#Mcnemar's Test P-Value : 0.68309         
#            Sensitivity : 0.8000          
#            Specificity : 0.8095          
#         Pos Pred Value : 0.6667          
#         Neg Pred Value : 0.8947          
#              Precision : 0.6667          
#                 Recall : 0.8000          
#                     F1 : 0.7273          
#             Prevalence : 0.3226          
#         Detection Rate : 0.2581          
#   Detection Prevalence : 0.3871          
#      Balanced Accuracy : 0.8048          
#       'Positive' Class : yes        


#绘制验证集混淆矩阵#混淆矩阵转换为数据框
confusion_matrix_df3 <- as.data.frame.matrix(confusionMatrix_val$table)
colnames(confusion_matrix_df3) <- c("sensoring","terminal event")
rownames(confusion_matrix_df3) <- c("sensoring","terminal event")
draw_data3 <- round(confusion_matrix_df3 / rowSums(confusion_matrix_df3),2)
draw_data3$real <- rownames(draw_data3)draw_data3 <- melt(draw_data3)
#绘制验证集混淆矩阵热图
library(ggprism)
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
val_predprob <- predict(tree_tune,newdata = val_data,type="prob")
#AUC
val_roc <- roc(response = val_data$deteriorate, predictor = val_predprob[,2])
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

#绘制验证集ROC曲线
#ggplot2包美化ROC
library(ggplot2)
ggplot(val_roc_data, aes(x = 1-val_roc_obj$specificities, y = val_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(val_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.1, y = 0.6, label = paste("Cutoff =", round(val_roc_best[,1], 2))), size = 6, color = "#D2431C") + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
  theme_pubr() +  labs(x = "1-Specificity", y = "Sensitivity") +
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
########################################调整网格模型模型end#################
##################################step4-5.10折交叉模型配置#################################
#本例以tree_cv模型为最终模型
#特征重要性排序/变量重要性
tree_cv$variable.importance

#barplot
library(ggplot2)
library(dplyr)
barplot(tree_cv$variable.importance)

#ggplot美化
library(rpart.plot)
import_train <- data.frame(importance=tree_cv$variable.importance)
ggplot(import_train,       
       aes(x=as_factor(rownames(import_train)),y=importance))+    
  geom_col()+    
  labs(x="variables")+    
  theme_classic()+    
  theme(axis.text.x = element_text(angle=15,hjust=1))    

#树形图
prp(tree_cv,type=2,    
    extra = 104,    
    tweak = 1,    
    fallen.leaves = TRUE,    
    main="Decision Tree")

#训练集模型预测：混淆矩阵和ROC曲线
#训练集预测概率
predict_train <- predict(tree_cv, newdata = train_data, type = "class")

#训练集混淆矩阵
library(caret)
confusionMatrix_train <- confusionMatrix(data = predict_train,
                                         reference = train_data$deteriorate, 
                                         positive = "yes",
                                         mode = "everything")
#输出训练集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_train)
#Confusion Matrix and Statistics
#Reference
#Prediction no yes
#no  44   3
#yes  7  22
#Accuracy : 0.8684          
#95% CI : (0.7713, 0.9351)
#No Information Rate : 0.6711          
#P-Value [Acc > NIR] : 7.353e-05       
#Kappa : 0.7136          
#Mcnemar's Test P-Value : 0.3428          
#            Sensitivity : 0.8800          
#            Specificity : 0.8627          
#         Pos Pred Value : 0.7586          
#         Neg Pred Value : 0.9362          
#              Precision : 0.7586          
#                 Recall : 0.8800          
#                     F1 : 0.8148          
#             Prevalence : 0.3289          
#         Detection Rate : 0.2895          
#   Detection Prevalence : 0.3816          
#      Balanced Accuracy : 0.8714          
#       'Positive' Class : yes

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
library(ggprism)
ggplot(draw_data2, aes(real,variable, fill = value)) +  
  geom_tile() +  
  geom_text(aes(label = scales::percent(value))) +  
  scale_fill_gradient(low = "#ECA9B0", high = "#81D8D0") +  
  labs(x = "True", y = "Predicted", title = "Confusion matrix of train set") +  
  theme_prism(border = T)+  theme(panel.border = element_blank(),        
                                  axis.ticks.y = element_blank(),        
                                  axis.ticks.x = element_blank(),        
                                  legend.position="none")

#绘制ROC曲线
#验证集预测概率
library(pROC)
train_predprob <- predict(tree_cv,newdata = train_data,type="prob")
#AUC
train_roc <- roc(response = train_data$deteriorate, predictor = train_predprob[,2])
train_roc
#截断值
train_roc_best=coords(train_roc, "best",best.method = c("youden"),                     
                      ret=c("threshold","sensitivity", "specificity"))
train_roc_best
#计算验证集ROC曲线的参数
train_roc_obj <- train_roc 
train_roc_auc <- auc(train_roc_obj)
#将ROC对象转换为数据框
train_roc_data <- data.frame(1-train_roc_obj$specificities, train_roc_obj$sensitivities)

#ggplot2包美化ROC
library(ggplot2)
ggplot(train_roc_data, aes(x = 1 - train_roc_obj$specificities, y = train_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(train_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.35, y = 0.85, label = paste("Cutoff =", round(train_roc_best[,1], 2))), size = 6, color = "#D2431C") +  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  
  theme_minimal() +  # 使用一个基本主题
  labs(x = "1-Specificity", y = "Sensitivity") +  
  ggtitle("ROC Curve of train set") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +  
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color="#5d6174", linetype="dashed") +  
  theme_prism(border = TRUE) +  # 假设theme_prism是自定义或来自另一个包
  geom_point(x = 1 - train_roc_best$specificity, y = train_roc_best$sensitivity, color = "#D2431C", size = 3) + 
  theme(axis.text = element_text(size = 10)) +  
  theme(axis.title.x = element_text(vjust = 2, size = 15, face = "plain")) +
  theme(axis.title.y = element_text(vjust = 2, size = 15, face = "plain"))

#step5-2.tree_cv模型应用于验证集#验证集预测概率
predict_val <- predict(tree_cv,newdata = val_data,type = "class")
#验证集混淆矩阵
confusionMatrix_val <- confusionMatrix(data = predict_val,                                       
                                       reference = val_data$deteriorate,                                       
                                       positive = "yes",                                       
                                       mode = "everything")
#输出验证集模型评价指标：准确率(accuracy),精确率(precision),召回率(recall),F1值
print(confusionMatrix_val)

#绘制验证集混淆矩阵#混淆矩阵转换为数据框
confusion_matrix_df3 <- as.data.frame.matrix(confusionMatrix_val$table)
colnames(confusion_matrix_df3) <- c("sensoring","terminal event")
rownames(confusion_matrix_df3) <- c("sensoring","terminal event")
draw_data3 <- round(confusion_matrix_df3 / rowSums(confusion_matrix_df3),2)
draw_data3$real <- rownames(draw_data3)draw_data3 <- melt(draw_data3)
#绘制验证集混淆矩阵热图
library(ggprism)
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
val_predprob <- predict(tree_cv,newdata = val_data,type="prob")
#AUC
val_roc <- roc(response = val_data$deteriorate, predictor = val_predprob[,2])
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

#绘制验证集ROC曲线
#ggplot2包美化ROC
library(ggplot2)
ggplot(val_roc_data, aes(x = 1-val_roc_obj$specificities, y = val_roc_obj$sensitivities)) +  
  geom_line(color = "#2089CB", size = 1) +  
  geom_text(aes(x = 0.75, y = 0.25, label = paste("AUC =", round(val_roc_auc, 2))), size = 8, color = "#345D82") +  
  geom_text(aes(x = 0.1, y = 0.6, label = paste("Cutoff =", round(val_roc_best[,1], 2))), size = 6, color = "#D2431C") + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
  theme_pubr() +  labs(x = "1-Specificity", y = "Sensitivity") +
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
########################################10折交叉模型模型end#################
##########################################Step6.决策树模型的可视化####################
#使用rpart.plot函数绘图
rpart.plot(tree_model0)

#6种展示风格
par(mfrow = c(3, 2))
rpart.plot(tree_model0, type = 0)
rpart.plot(tree_model0, type = 1)
rpart.plot(tree_model0, type = 2) #默认
rpart.plot(tree_model0, type = 3)
rpart.plot(tree_model0, type = 4)
rpart.plot(tree_model0, type = 5)
par(mfrow = c(1, 1))
#添加额外的信息
rpart.plot(tree_model0,       
           extra = 101,#添加样本量     
           digits = 2,#调整小数点保留的位数    
           box.palette = list("lightgreen","pink"),#调节节点颜色    
           sub="Decision Tree"#添加标题
           )

#使用partykit进行可视化
#install.packages("partykit")
library(partykit)
plot(as.party(tree_model0))

#使用rattle包
library(rattle)
fancyRpartPlot(tree_model0,main="Decision Tree",type=5,
               palettes=c("Oranges","Purples")#调整节点颜色
               )

#使用treeheatr包
library(treeheatr)

#rx字符型变量报错，省略数据转换，移除rx
heat_tree(train_data[,-1], target_lab = 'status')

#7.最终模型验证的校准曲线和临床决策曲线
#install.packages("calibrate")
library(calibrate)
library(MASS)
#install.packages("rms")
library(rms)

# 在建模人群中绘制Calibration plot
dd <- datadist(train_data )
options(datadist = "dd")
fit <- lrm(deteriorate ~ age + UWDRS.N + GrayLevelVariance.Brian_stem  + ratio.for.Blood.to.urine.copper
           , data = train_data , x = TRUE, y = TRUE)
cal <- calibrate(fit, u = 0:0.2/0.05, method = "boot", B = 1000)
plot(cal)
# n=76   Mean absolute error=0.054   Mean squared error=0.00383 0.9 Quantile of absolute error=0.098

# 在训练集人群中绘制Calibration plot
dd <- datadist(val_data )
options(datadist = "dd")
fit <- lrm(deteriorate ~ age + UWDRS.N + GrayLevelVariance.Brian_stem  + ratio.for.Blood.to.urine.copper
           , data = val_data , x = TRUE, y = TRUE)
cal <- calibrate(fit, u = 0:0.2/0.05, method = "boot", B = 1000)
plot(cal)
# n=31   Mean absolute error=0.082   Mean squared error=0.0138 0.9 Quantile of absolute error=0.216






















