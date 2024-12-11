#### 1.设置工作路径(数据存储和结果输出的文件夹)
setwd("D:/R work")

#### 2.安装和加载包
library(pbapply)
library(rlang)
library(tidyverse)
library(reshape2)
library(openxlsx)
library(DALEX)
library(readr)
library(gbm)
library(dplyr)
library(caret)
library(ggplot2)
library(pROC)
library(rms)
library(rmda)
library(dcurves)
library(Hmisc)
library(ResourceSelection)
library(DynNom)
library(survey)
library(caret)
library(foreign)
library(plotROC)
library(survival)
library(shapper)
library(iml)
library(e1071)
library(ROCR)
library(corrplot)
library(lattice)
library(Formula)
library(SparseM)
library(survival)
library(riskRegression)
# install.packages("pheatmap")
library(pheatmap)
library(fastshap)
library(naivebayes)
library(ingredients)
library(mlr3)
# install.packages("table1")
# install.packages("adabag")
library(table1)
library(tableone)
library(adabag)
library(RColorBrewer)
library(VIM)
library(mice)
library(autoReg)
# install.packages("cvms")
library(cvms)
library(tibble)
library(plotROC)
library(pROC)
library(ggplot2)
library(cvms)
library(tibble)
library(corrplot)
library(data.table)
library(pheatmap)
# install.packages("ComplexHeatmap")
# devtools::install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(ROSE)
# install.packages("DMwR")
# devtools::install_github("cran/DMwR")
library(DMwR)
library(scales)
library(catboost)
library(lightgbm)
library(plotROC)
library(pROC)
library(ggplot2)
library(kernelshap)
library(shapviz)

#### 3.读取数据
data = read.csv("pydatas4.csv",
                header = T,      # 第一行作为列名
                encoding = "GBK") # 文件编码为GBK

# 显示数据框的列名
colnames(data)

# 以数据表的形式查看数据
View(data)

# 显示数据的结构信息，包括每列的数据类型
str(data)

#### 4.分类变量因子化并替换为原始值
# 变量因子化，只有分类进行因子化(设置哑变量)，修改自变量名称
# deteriorate为分类结局
data$Result = factor(data$deteriorate,levels = c(0,1),labels = c('No','Yes'))    
View(data)
str(data)

#### 5.划分训练集和测试集
set.seed(52) # 设置随机种子，保证每次使用的训练集和测试集分割一致

# 使用createDataPartition函数修改因变量名称，划分训练集，设置训练集的比例为0.7
inTrain = createDataPartition(y=data[,"deteriorate"], 
                              p=0.7,  # 训练集所占比例
                              list=F) # 返回矩阵格式而非列表格式

# 提取训练集数据
traindata = data[inTrain,]

# 提取验证集数据
testdata = data[-inTrain,]

# 保存训练集数据到"dev.csv"文件，不保存行名
write.csv(traindata,
          "dev.csv",
          row.names = F)

# 保存验证集数据到"vad.csv"文件，不保存行名
write.csv(testdata,
          "vad.csv",
          row.names = F)

#### 6.训练集基线表格制作
# 把训练集赋值给x
x = traindata   

# 连续性自变量
x1 = colnames(x[,2:ncol(x)])# 从2列以后为连续变量

# 分类型自变量
#x2 = colnames(x[,2:15]) # 这里分类自变量为2到15列

# 生成训练集总体的基线表
CreateTableOne(data=x)

# 获取变量名称，排除第一列（通常是因变量）
myVars = colnames(x[,2:ncol(x)])

# 修改分类自变量列数，指定分类变量
#catVars = colnames(x[,2:15]) # 假设前15列为分类变量

# 将总体的基线资料表格储存在tab2方便保存
tab2 = CreateTableOne(vars = myVars,  # 指定要包含的变量
                      data = x)        # 数据框
                      #factorVars = catVars) # 指定分类变量

# 打印基线表格，设置格式选项
print(tab2, format0ptions=list(big.showAllLevels=TRUE,  # 显示所有分类水平
                         mark=',')) # 使用逗号作为千位分隔符

# 将表格转换为矩阵格式，设置打印选项
tab2Mat = print(tab2,quote = FALSE,  # 不显示引号
                noSpaces = TRUE, # 不显示空格
                printToggle = FALSE) # 不切换打印模式

# 将基线表格保存为CSV文件，文件名为'训练集Tableone.csv'
write.csv(tab2Mat, file = '训练集Tableone.csv')


#### 7.训练集差异性分析/阳性事件基线
# 创建基线表，按因变量'结果'分层
tab3 = CreateTableOne(vars = myVars,     # 指定要包含的变量
                      strata = 'Result',  # strata指定因变量
                      data = x)           # 数据框
                      #factorVars = catVars) # 指定分类变量

# 打印差异表格，显示所有分类水平
print(tab3,
      showAllLevels=TRUE,  # 显示所有分类水平
      format0ptions=list(big.mark=',')) # 使用逗号作为千位分隔符

# 整理输出为矩阵格式
tab3Mat = print(tab3,
                quote = FALSE,  # 不显示引号
                noSpaces = TRUE, # 不显示空格
                printToggle = FALSE) # 不切换打印模式

# 将单因素分析结果保存为CSV文件，文件名为'训练集单因素分析结果.csv'
write.csv(tab3Mat, file = '训练集单因素分析结果.csv')

#### 8.训练集单因素logistic回归
# install.packages("autoReg")#装包
# 使用Logistic回归模型，因变量为'Result'，自变量为数据框中的所有变量
overall.log = glm(deteriorate ~ .,             # 修改因变量名称进行Logistic回归
                  data=x,                # 数据框
                  family=binomial)      # 指定为二项分布

# 输出模型的摘要信息
summary(overall.log)

# “~”前为因变量，“~”后的.代表除了因变量之后所有变量
# 注意若此处产生NA，则需要合并分类或者重新选择变量

# 进行单因素和多因素Logistic回归结果分析
model3 = autoReg(overall.log,
                 uni=TRUE,            # 启用单因素分析
                 milti=TRUE,          # 启用多因素分析
                 threshold=0.05)      # 设置显著性阈值

# 输出模型结果
model3

# 将单因素和多因素Logistic回归分析结果保存为CSV文件
# 文件名为'训练集单因素多因素Logistic回归.csv'
write.csv(model3, 
          "训练集单因素多因素Logistic回归.csv",
          row.names = F) # 不保存行名

#### 9.选中Y和特征筛选选中的变量X
colnames(x) # 读取变量名

# 指定多因素分析中有意义的变量
var=c("deteriorate",# 修改因变量
      "SizeZoneNonUniformity.Brian_stem","SizeZoneNonUniformityNormalized.Brian_stem","SmallAreaEmphasis.Brian_stem",
      "GrayLevelVariance.2.CC_Central","ZonePercentage.CC_Central","Busyness.Left.Caudate","Strength.Left.Caudate",
      "SmallAreaLowGrayLevelEmphasis.Right.Caudate","ZoneEntropy.Right.Caudate")# 修改多因素Logistic有意义的变量
# 性别 序列 分期 年龄 尺寸

# 重新读入数据，确保数据为数值状态
data = read.csv("pydatas6.csv",
                header = T,      # 第一行作为列名
                encoding = "GBK") # 文件编码为GBK
colnames(data)
# 只将结局变量因子化
data$deteriorate = factor(data$deteriorate,levels = c(0,1),labels = c('No','Yes'))

# 设置随机种子，确保每次分割一致
set.seed(52)

# 创建训练集和测试集，训练集比例为0.7
inTrain = createDataPartition(y=data[,"deteriorate"], p=0.7, list=F)
traindata = data[inTrain,]   # 提取训练集数据
testdata = data[-inTrain,]   # 提取测试集数据

# 定义训练集和验证集
dev = traindata
vad = testdata

# 提取有意义的变量
dev = dev[, var]  # 从训练集中提取指定变量
vad = vad[, var]  # 从测试集中提取指定变量

# 将因变量因子化，确保数据格式正确
dev$deteriorate = factor(as.character(dev$deteriorate)) # 修改因变量名称把因变量因子化


#### 10.分类多模型比较
# 定义模型列表
models = c("glm", "svmRadial", "gbm",  # 模型参数
           "nnet", "extraTrees", "xgbTree",
           "kknn", "AdaBoost.M1")

# 模型名称映射
models_names = list(Logistic = "glm",            # 逻辑回归
                    SVM = "svmRadial",          # 支持向量机
                    GBM = "gbm",                # 广义增强模型
                    NeuralNetwork = "nnet",     # 神经网络
                    RandomForest = "extraTrees", # 随机森林
                    Xgboost = "xgbTree",       # XGBoost
                    KNN = "kknn",               # K近邻
                    Adaboost = "AdaBoost.M1")   # 自适应提升

# 各模型的参数设置
glm.tune.grid = NULL  # 逻辑回归无参数设置
svm.tune.grid = expand.grid(sigma = 0.001,         # SVM参数设置
                            C = 0.09)
gbm.tune.grid = expand.grid(n.trees = 100,         # GBM参数设置
                            interaction.depth = 5,
                            shrinkage = 0.1, 
                            n.minobsinnode = 10)
nnet.tune.grid = expand.grid(size = 6,              # 神经网络参数设置
                             decay = 0.6)
rf.tune.grid = expand.grid(mtry = 11,               # 随机森林参数设置
                           numRandomCuts = 3)
xgb.tune.grid = expand.grid(nrounds = 10,          # XGBoost参数设置
                            max_depth = 3,
                            eta = 0.001,
                            gamma = 0.5,
                            colsample_bytree = 0.5,
                            min_child_weight = 1,
                            subsample = 0.6)
knn.tune.grid <- expand.grid(kmax = 12,            # KNN参数设置
                             distance = 1,
                             kernel = "optimal")
ada.tune.grid <- expand.grid(mfinal = 2,           # AdaBoost参数设置
                             maxdepth = 2,
                             coeflearn = "Zhu")

# 将所有模型的参数设置汇总
Tune_table = list(glm = glm.tune.grid,
                  svmRadial = svm.tune.grid,
                  gbm = gbm.tune.grid,
                  nnet = nnet.tune.grid,
                  extraTrees = rf.tune.grid,
                  xgbTree = xgb.tune.grid,
                  kknn = knn.tune.grid,
                  AdaBoost.M1 = ada.tune.grid)

# 创建预测值结果数据框
train_probe = data.frame(deteriorate = dev$deteriorate)  # 训练集结果
test_probe = data.frame(deteriorate = vad$deteriorate)    # 测试集结果

# 初始化变量重要性列表
importance = list()

# 各模型的列表
ML_calss_model = list()

# 设置随机种子
set.seed(520)

# 定义训练控制参数
train.control <- trainControl(method = 'repeatedcv',  # 重复交叉验证
                              number = 10,          # 折数
                              repeats = 5,          # 重复次数
                              classProbs = TRUE,    # 计算类概率
                              summaryFunction = twoClassSummary) # 总结函数

# 创建进度条
pb = txtProgressBar(min = 0, 
                    max = length(models),  # 设置进度条的最大值为模型数量
                    style = 3)

# 循环遍历每个模型
for (i in seq_along(models)) {
  model <- models[i]  # 当前模型
  model_name <- names(models_names)[which(models_names == model)]  # 获取模型名称
  
  set.seed(52)  # 设置随机种子，确保结果可重复
  
  # 训练模型
  fit = train(deteriorate ~ .,                   # 使用训练集数据
              data = dev,
              tuneGrid = Tune_table[[model]],  # 使用相应的调优参数
              metric = 'ROC',                  # 评估指标为ROC
              method = model,                  # 模型类型
              trControl = train.control)       # 训练控制参数
  
  # 预测训练集和测试集的概率
  train_Pro = predict(fit, newdata = dev, type = 'prob')  # 训练集预测
  test_Pro = predict(fit, newdata = vad, type = 'prob')    # 测试集预测
  
  # 将预测结果存入数据框
  train_probe[[model_name]] <- train_Pro$Yes  # 存储训练集的预测概率
  test_probe[[model_name]] <- test_Pro$Yes    # 存储测试集的预测概率
  
  # 存储训练好的模型和变量重要性
  ML_calss_model[[model_name]] = fit  # 存储模型
  importance[[model_name]] = varImp(fit, scale = TRUE)  # 存储变量重要性
  
  setTxtProgressBar(pb, i)  # 更新进度条
}

# 关闭进度条
close(pb)  


# 额外添加模型
# 9.LightGBM
# 将训练集命名为train
train = dev
# 将结果变量转为数值型，"Yes"为1，"No"为0
train$deteriorate = ifelse(train$deteriorate == "Yes", 1, 0)

# 创建LightGBM数据集
dtrain = lgb.Dataset(as.matrix(train[2:ncol(train)]), label = train$deteriorate)

# 提取测试集，并将结果变量转为数值型
test = vad[, var]
test$deteriorate = ifelse(test$deteriorate == "Yes", 1, 0)

# 创建测试集的LightGBM数据集
dtest = lgb.Dataset.create.valid(dtrain, as.matrix(test[2:ncol(test)]), label = test$deteriorate)

# 设置LightGBM参数
params = list(
  objective = "binary",         # 二分类目标
  metric = "auc",               # 使用AUC作为评估指标
  min_data = 1L,               # 最小数据量
  learning_rate = 1.0,          # 学习率
  num_threads = 2L,             # 线程数
  force_col_wise = TRUE         # 强制按列进行计算
)

# 定义验证集
valids = list(test = dtest)

# 训练LightGBM模型
lightgbm_model = lgb.train(params = params, data = dtrain,
                           nrounds = 5L,                    # 迭代次数
                           valids = valids,                # 验证集
                           early_stopping_rounds = 3L)      # 提前停止的轮次

# 预测训练集和测试集的概率
train_probe$LightGBM = predict(lightgbm_model,
                               newdata = as.matrix(dev[2:ncol(dev)]), 
                               type = 'prob')  # 训练集预测

test_probe$LightGBM = predict(lightgbm_model,
                              newdata = as.matrix(vad[2:ncol(vad)]),
                              type = 'prob')  # 测试集预测

# 获取特征重要性
lightGBM_Imp = lgb.importance(lightgbm_model, 
                              percentage = TRUE)

# 将特征重要性结果保存为CSV文件
write.csv(lightGBM_Imp,
          "LightGBM_important.csv",
          row.names = FALSE)  # 不保存行名

# 重要性绘图
# 读取LightGBM特征重要性数据
rt=read.csv("LightGBM_important.csv", 
            header=T,
            check.names=F)# 不检查列名的有效性

# 使用ggplot2绘图
g <- ggplot(rt, 
            aes(x=Gain, # x轴为Gain（重要性得分）
                y=reorder(Feature,Gain)))  # y轴为Feature（特征），按Gain重新排序

# 绘制条形图
p2 = g + geom_bar(aes(fill = Gain),      # 用Gain填充颜色
                  stat = "identity",     # 统计方式为直接使用y值
                  width = 0.6,           # 条形宽度
                  position = position_stack(reverse = TRUE),  # 堆叠反向
                  size = 1) +            # 条形边框大小
  theme_classic() +                        # 使用经典主题
  scale_fill_gradient() +                  # 使用渐变色
  theme(plot.title = element_text(hjust = 0.5, size = 16),  # 标题居中，字体大小
        legend.position = "none",          # 不显示图例
        axis.text = element_text(size = 10, face = "bold", color = "black"),  # 坐标轴文字设置
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),  # x轴标题设置
        axis.title.y = element_text(size = 12, face = "bold", color = "black"),  # y轴标题设置
        legend.title = element_text(size = 12),  # 图例标题大小
        legend.text = element_text(size = 12)) + # 图例文字大小
  labs(x = "Importance Scores",             # x轴标签
       y = "Features",                       # y轴标签
       title = "LightGBM")                  # 图表标题

# 显示图形
p2

# 将图形保存为PDF文件
pdf("LightGBM_importance.pdf", 7, 5, family = "serif")  # 设置PDF文件的大小
p2  # 绘制图形到PDF文件
dev.off()  # 关闭设备

# 10.CatBoost
# 将训练集命名为train
train = dev
# 将结果变量转为数值型，"Yes"为1，"No"为0
train$deteriorate = ifelse(train$deteriorate == "Yes", 1, 0)

# 将训练集中所有整数型变量转换为数值型
train <- as.data.frame(lapply(train, function(x) {
  if (is.integer(x)) {
    return(as.numeric(x))  # 如果是整数型，则转换为数值型
  }
  return(x)  # 否则返回原变量
}))

# 创建CatBoost训练数据池
train_pool = catboost.load_pool(as.matrix(train[2:ncol(train)]),  # 取训练集的特征
                                label = train$deteriorate)  # 设置标签为结果变量

# 提取测试集，并将结果变量转为数值型
test = vad[, var]
test$deteriorate = ifelse(test$deteriorate == "Yes", 1, 0)

# 将测试集中所有整数型变量转换为数值型
test <- as.data.frame(lapply(test, function(x) {
  if (is.integer(x)) {
    return(as.numeric(x))  # 如果是整数型，则转换为数值型
  }
  return(x)  # 否则返回原变量
}))

# 创建CatBoost测试数据池
test_pool = catboost.load_pool(as.matrix(test[2:ncol(test)]),  # 取测试集的特征
                               label = test$deteriorate)  # 设置标签为结果变量

# 设置CatBoost模型参数
fit_params = list(
  iterations = 100,              # 迭代次数
  use_best_model = TRUE,         # 使用最佳模型
  eval_metric = 'AUC',           # 评估指标为AUC
  ignored_features = c(4, 9),    # 忽略的特征列
  border_count = 32,             # 分割边界数量
  depth = 5,                     # 树的深度
  learning_rate = 0.03,          # 学习率
  random_seed = 123              # 随机种子
)

# 训练CatBoost模型
Catboost_model = catboost.train(train_pool, 
                                test_pool, 
                                fit_params)

# 输出训练好的模型
Catboost_model

# 预测训练集和测试集的概率
train_probe$CatBoost = catboost.predict(Catboost_model, 
                                        train_pool, 
                                        prediction_type = 'Probability')  # 训练集预测

test_probe$CatBoost = catboost.predict(Catboost_model, 
                                       test_pool, 
                                       prediction_type = 'Probability')  # 测试集预测

# 获取特征重要性
Catboost_Imp = catboost.get_feature_importance(Catboost_model)

# 将特征重要性结果转换为数据框
Catboost_Imp = data.frame(Feature = colnames(dev)[2:ncol(dev)],  # 特征名称
                          Overall = Catboost_Imp)  # 特征重要性得分

# 将特征重要性结果保存为CSV文件
write.csv(Catboost_Imp,
          "CatBoost_important.csv",
          row.names = FALSE)  # 不保存行名

# 重要性绘图
# 读取特征重要性数据
rt = read.csv("CatBoost_important.csv", 
              header = TRUE,          # 读取表头
              check.names = FALSE)   # 不检查列名

# 使用ggplot2绘图
g <- ggplot(rt, 
            aes(x = Overall,                # x轴为重要性得分
                y = reorder(Feature, Overall))) # y轴为特征名，按重要性得分重排序

# 创建条形图
p2 = g + geom_bar(aes(fill = Overall),    # 填充颜色为重要性得分
                  stat = "identity",       # 统计方式为身份
                  width = 0.6,             # 条形宽度
                  position = position_stack(reverse = TRUE), # 反向堆叠
                  size = 1)                 # 条形边框大小

# 设置主题和样式
p2 = p2 + theme_classic() +  # 使用经典主题
  scale_fill_gradient() +     # 填充颜色渐变
  theme(plot.title = element_text(hjust = 0.5, size = 16),  # 标题居中，字号16
        legend.position = "none",  # 不显示图例
        axis.text = element_text(size = 10, face = "bold", color = "black"),  # 坐标轴文字设置
        axis.title.x = element_text(size = 12, face = "bold", color = "black"), # x轴标题设置
        axis.title.y = element_text(size = 12, face = "bold", color = "black"), # y轴标题设置
        legend.title = element_text(size = 12),  # 图例标题设置
        legend.text = element_text(size = 12)) + # 图例文字设置
  labs(x = "Importance Scores",    # x轴标签
       y = "Features",              # y轴标签
       title = "CatBoost")          # 图表标题

# 显示绘图
p2

# 保存图形为PDF文件
pdf("CatBoost_importance.pdf", 7, 5, family = "serif")  # 设置PDF文件名和尺寸
p2  # 绘制图形
dev.off()  # 关闭PDF设备


# 训练集变量重要性绘图
# 遍历模型名称列表
for(model_name in names(models_names)){
  # 获取该模型的变量重要性
  imp = importance[[model_name]]
  
  # 将结果转换为数据框
  imp_table <- as.data.frame(imp$importance)
  imp_table$Features <- rownames(imp_table)  # 添加特征名列

  # 确定绘图所需的填充列
  if ("Yes" %in% colnames(imp_table)) {
    fill_col <- "Yes"  # 如果存在 "Yes" 列，则使用
  } else if ("Overall" %in% colnames(imp_table)) {
    fill_col <- "Overall"  # 否则使用 "Overall" 列
  } else {
    stop("Neither 'Yes' nor 'Overall' column found in importance table.")  # 如果两者都不存在，则停止
  }
  
  print(fill_col)

  # 使用 ggplot2 进行绘图
  g = ggplot(imp_table, 
             aes(x = !!sym(fill_col),  # x轴为填充列
                 y = reorder(Features, 
                             !!sym(fill_col))))  # y轴为特征名，按填充列重排序
  
  # 创建条形图
  p2 = g + geom_bar(aes(fill = !!sym(fill_col)),  # 填充颜色为填充列
                    stat = "identity",  # 统计方式为身份
                    width = 0.6,  # 条形宽度
                    position = position_stack(reverse = TRUE),  # 反向堆叠
                    size = 1) +  # 条形边框大小
    theme_classic() +  # 使用经典主题
    scale_fill_gradient() +  # 填充颜色渐变
    theme(plot.title = element_text(hjust = 0.5, size = 16),  # 标题居中，字号16
          legend.position = "none",  # 不显示图例
          axis.text = element_text(size = 10, face = "bold", color = "black"),  # 坐标轴文字设置
          axis.title.x = element_text(size = 12, face = "bold", color = "black"),  # x轴标题设置
          axis.title.y = element_text(size = 12, face = "bold", color = "black"),  # y轴标题设置
          legend.title = element_text(size = 12),  # 图例标题设置
          legend.text = element_text(size = 12)) +  # 图例文字设置
    labs(x = "Importance Scores",  # x轴标签
         y = "Features",  # y轴标签
         title = paste0(model_name, " "))  # 图表标题
  
  # 保存图形为PDF文件
  pdf(paste0(model_name, "_important.pdf"), 7, 5, family = "serif")  # 设置PDF文件名和尺寸
  print(p2)  # 绘制图形
  dev.off()  # 关闭PDF设备
}

# 定义模型名称及其对应的函数
models_names = list(Logistic = "glm",  # 逻辑回归模型
                    SVM = "svmRadial",  # 支持向量机模型
                    GBM = "gbm",  # 梯度提升机模型
                    NeuralNetwork = "nnet",  # 神经网络模型
                    RandomForest = "extraTrees",  # 随机森林模型
                    Xgboost = "xgbTree",  # XGBoost模型
                    KNN = "kknn",  # K近邻模型
                    Adaboost = "AdaBoost.M1",  # AdaBoost模型
                    LightGBM = "LightGBM",  # LightGBM模型
                    CatBoost = "CatBoost")  # CatBoost模型

# 创建用于存储训练集和测试集预测结果的对象
Train = train_probe  # 训练集预测结果
Test = test_probe    # 测试集预测结果

# 将训练集和测试集数据放入列表中
datalist = list(Train = train_probe,  # 训练集数据
                Test = test_probe)    # 测试集数据

for (newdata_tt in names(datalist)) {
  
  newdata = datalist[[newdata_tt]]
  # 训练集校准曲线
  #构建公式
  formula = as.formula(paste0("deteriorate ~ ", paste(colnames(newdata)[2:ncol(newdata)], collapse = " + ")))
  trellis.par.set(caretTheme())
  cal_obj = calibration(formula, data = newdata, class = 'Yes',cuts = 3)
  caldata=as.data.frame(cal_obj$data)
  caldata=na.omit(caldata)
  #通过ggplot2美化Calibration校准曲线
  Calibrat_plot=ggplot(data = caldata,aes(x=midpoint,
                            y=Percent,
                            group = calibModelVar,
                            color=calibModelVar))+
    geom_point(size=1)+
    geom_line(linewidth=0.65)+
    geom_abline(slope = 1, intercept = 0 ,color="black",linetype = 'dotdash')+
    xlab("Bin Midpoint")+
    ylab("Observed Event Percentage")+#纵坐标名称
    theme_bw() +#去掉背景灰色
    theme(
      plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
      axis.text=element_text(size=12,face="bold"),
      legend.position.inside=c(0.9,0.3),
      legend.background = element_blank(),
      axis.title.y = element_text(size=12,face="bold"),
      axis.title.x = element_text(size=12,face="bold"),
      panel.border = element_rect(color="black",size=1),
      panel.background = element_blank())+
    scale_color_discrete(name = "Model")
  pdf(paste0(newdata_tt,"Calibration.pdf"),5,5,family = "serif")
  print(Calibrat_plot)
  dev.off()
  
  
  ROC_list = list()
  ROC_label = list()
  AUC_metrics = data.frame()
  Evaluation_metrics = data.frame(Model = NA,Threshold=NA,Accuracy=NA,Sensitivity=NA,Specificity=NA,Precision=NA,F1=NA)
  #绘制训练集ROC曲线
  for (model_name in names(models_names)) {
    
    ROC = roc(response=newdata$deteriorate,predictor=newdata[,model_name])
    AUC = round(auc(ROC),3)
    CI = ci.auc(ROC)
    label = paste0(model_name," (AUC=",sprintf("%0.3f", AUC),",95%CI:",sprintf("%0.3f", CI[1]),"-",sprintf("%0.3f", CI[3]),")")
    
    bestp = ROC$thresholds[
      which.max(ROC$sensitivities+ROC$specificities-1)
    ]
    
    predlab = as.factor(ifelse(newdata[,model_name] > bestp,"Yes","No"))
    
    index_table = confusionMatrix(data = predlab,
                                  reference = newdata$deteriorate,
                                  positive = "Yes",
                                  mode="everything")
    
    mydata = data.frame(reference=newdata$deteriorate,prediction=predlab)
    mytibble =  as_tibble(table(mydata))
    
    confusion_plot = plot_confusion_matrix(mytibble,
                                           target_col = "reference",
                                           prediction_col = "prediction",
                                           counts_col = "n",
                                           sub_col = NULL,
                                           class_order = NULL,
                                           add_sums = FALSE,
                                           add_counts = TRUE,
                                           add_normalized = TRUE,
                                           add_row_percentages = F,
                                           add_col_percentages = F,
                                           diag_percentages_only = FALSE,
                                           rm_zero_percentages = TRUE,
                                           rm_zero_text = TRUE,
                                           add_zero_shading = TRUE,
                                           amount_3d_effect = 1,
                                           add_arrows = TRUE,
                                           counts_on_top = FALSE,
                                           palette = "Blues",
                                           intensity_by = "counts",
                                           intensity_lims = NULL,
                                           intensity_beyond_lims = "truncate",
                                           theme_fn = ggplot2::theme_minimal,
                                           place_x_axis_above = TRUE,
                                           rotate_y_text = TRUE,
                                           digits = 1,
                                           font_counts = font(),
                                           font_normalized = font(),
                                           font_row_percentages = font(),
                                           font_col_percentages = font(),
                                           arrow_size = 0.048,
                                           arrow_nudge_from_text = 0.065,
                                           tile_border_color = NA,
                                           tile_border_size = 0.1,
                                           tile_border_linetype = "solid",
                                           sums_settings = sum_tile_settings(),
                                           darkness = 0.8)
    
    pdf(paste0(newdata_tt,model_name,"_cm_plot.pdf"),5,5,family = "serif")
    print(confusion_plot)
    dev.off()
    Evaluation_metrics = rbind(Evaluation_metrics,c(Model = model_name, Threshold = bestp,  
                                                    Accuracy = sprintf("%0.3f",index_table[["overall"]][["Accuracy"]]),
                                                    Sensitivity = sprintf("%0.3f",index_table[["byClass"]][["Sensitivity"]]),
                                                    Specificity = sprintf("%0.3f",index_table[["byClass"]][["Specificity"]]),
                                                    Precision = sprintf("%0.3f",index_table[["byClass"]][["Precision"]]),
                                                    F1 = sprintf("%0.3f",index_table[["byClass"]][["F1"]])  )
    )
    
    ROC_label[[model_name]] = label
    ROC_list[[model_name]] = ROC
    
  }
  write.csv(Evaluation_metrics,paste0(newdata_tt,"_Evaluation_metrics.csv"),row.names = F)
  

  
  ROC_plot=pROC::ggroc(ROC_list,size=1.5,legacy.axes = T)+theme_bw()+
    labs(title = ' ROC curve')+
    theme(plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
          axis.text=element_text(size=12,face="bold"),
          legend.title = element_blank(),
          legend.text = element_text(size=12,face="bold"),
          legend.position.inside=c(0.7,0.25),#前面调整模型名称左右位置，后面调整模型名称上下位置
          legend.background = element_blank(),
          axis.title.y = element_text(size=12,face="bold"),#element_blank()
          axis.title.x = element_text(size=12,face="bold"),
          panel.border = element_rect(color="black",linewidth=1),
          panel.background = element_blank())+
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),colour='grey',linetype = 'dotdash')+
    scale_colour_discrete(
      breaks=c(names(models_names)),
      labels=c(ROC_label))
  pdf(paste0(newdata_tt,"_ROC.pdf"),7,7,family = "serif")
  print(ROC_plot)
  dev.off()
  
  dca_data = newdata
  dca_data$deteriorate=ifelse(dca_data$deteriorate=="Yes",1,0)
  
  DCA_list = list()
  for (model_name in names(models_names)) {
    dca_formula = as.formula(paste("deteriorate ~", model_name))
    set.seed(123)
    dca_curvers = decision_curve(dca_formula, #修改因变量名称
                                 data = dca_data, 
                                 study.design = "cohort", 
                                 bootstraps = 50 
    )
    DCA_list[[model_name]] = dca_curvers
  }
  
  dca = setNames(DCA_list, names(DCA_list))
  #绘制模型的DCA曲线
  pdf(paste0(newdata_tt,"DCA.pdf"),7,7,family = "serif")
  plot_decision_curve(dca, curve.names = c(names(models_names)),
                      cost.benefit.axis = F, 
                      confidence.intervals = "none" ,
                      lwd = 2,
                      legend.position ="topright")+theme(
                        plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
                        axis.text=element_text(size=12,face="bold"),
                        legend.title = element_blank(),
                        legend.text = element_text(size=12,face="bold"),
                        #legend.position="topright",
                        legend.background = element_blank(),
                        axis.title.y = element_text(size=12,face="bold"),#element_blank()
                        axis.title.x = element_text(size=12,face="bold"),
                        panel.border = element_rect(color="black",linewidth=1),
                        panel.background = element_blank())
  dev.off()
  write.csv(dca_data,paste0(newdata_tt,"_PRplot.csv"),row.names = F)
}


#### 11.SHAP解释模型
ML_calss_model$LightGBM = lightgbm_model
ML_calss_model$CatBoost = Catboost_model


n_train = 100  #使用数据量(按照自己需要求更改)
n_test = 100   #使用数据量

names(models_names)

#最优模型(目前暂不支持Catboost)
best_Model = "RandomForest"
# 若最优模型是其他模型，替换其中的模型即可
explain_kernel = kernelshap(ML_calss_model[[best_Model]], 
                            dev[1:n_train,-1], 
                            bg_X = vad[1:n_test,-1])  

shap_value = shapviz(explain_kernel,
                     X_pred = dev[1:n_train,-1], 
                     interactions = TRUE) 
#单样本特征
pdf(paste0("SHAP_",best_Model,"_sv_force.pdf"),7,5)
sv_force(shap_value$Yes, 
         row_id = 12,
         size = 9)+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  color = "black"))
dev.off()
# 特征重要性蜂群图
pdf(paste0("SHAP_",
           best_Model,
           "_importance_beeswarm.pdf"),7,5)
sv_importance(shap_value$Yes, 
              kind = "beeswarm", 
              viridis_args = list(begin = 0.25, 
                                  end = 0.85, 
                                  option = "B"),#A-H
              show_numbers = F)+
  ggtitle(label = paste0("",best_Model))+
  theme_bw()+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  color = "black"))
dev.off()

#特征重要性条形图
pdf(paste0("SHAP_",best_Model,"_importance_bar.pdf"),7,5)
sv_importance(shap_value$Yes, 
              kind = "bar", 
              show_numbers = F,
              fill = "#fca50a",#修改颜色
              class = "Yes")+
  theme_bw()+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  color = "black"))
dev.off()

colnames(dev)
#偏相关依赖图
pdf(paste0("SHAP_",best_Model,"_dependence.pdf"),5,5)
sv_dependence(shap_value$Yes,
              v = "Age",#指定第一个变量自己需要绘制的变量
              color = "#3b528b",
              color_var = "Size",#指定第二个变量
)+
  theme_bw()+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black"))
dev.off()

#瀑布图
pdf(paste0("SHAP_",best_Model,"_waterfall.pdf"),5,5)
sv_waterfall(shap_value$Yes, row_id = 12,
             fill_colors = c("#f7d13d", "#a52c60"))+
  theme_bw()+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black"))
dev.off()

sv_interaction(shap_value$Yes,
               kind = "beeswarm",
               max_display = 5,
               alpha = 0.3,
               bee_width = 0.3,
               bee_adjust = 0.5,
               viridis_args = getOption("shapviz.viridis_args"),
               color_bar_title = "Row feature value")

