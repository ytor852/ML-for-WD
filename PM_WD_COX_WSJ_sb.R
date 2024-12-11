


#######################################################################
##############------------预后预测模型R语言实战--------------##########
###############################生存分析统计############################

#1.数据读取

#1.1读取
setwd("D:/R work")     #设置工作路径

library(foreign)       #调用foreign包

rawdata<-read.csv("WD_COX.csv")        #读取R work中的

View(rawdata)          #查看数据

str(rawdata)           #查看数据的类型，非常重要

summary(rawdata)       #数据进行简单描述
summary(rawdata) 

rawdata<-na.omit(rawdata)   #删除缺失数据


#数据集区分
dev = rawdata[rawdata$group==1,]     #提取dataset=1为训练集
vad = rawdata[rawdata$group==0,]     #提取dataset=0为验证集


#1.2数据解读
names(rawdata)                #查看rawdata的变量列表
#-----------------------------------------------
#这是一个肝癌死亡数据
#ID     编号
#Sex    性别     1=male    0=female
#Age    年龄     连续性变量（实际年龄）
#age66  >66岁    1=是      0=否
#HBV    HBV抗体  1=是      0=否
#HCV    HCV抗体  1=是      0=否
#Alc    谷丙     1=是      0=否
#burden 疾病负担 1-2-3-4
#CTP    评分     0-1-2
#ps     评分     1-2-3-4
#NewAFP 甲胎蛋白 1=是      0=否
#DM     糖尿病   1=是      0=否
#GFR60  滤过率   1=是      0=否
#group  分组     1=dev     0=vad
#dead   肝癌死亡 1=是      0=删失
#time   时间     连续性
#-----------------------------------------------


#2.数据预处理

#############################################
#3.预后模型构建
#3.0软件包准备

#install.packages("survival")      #安装生存分析包
library(survival)                  #加载生存分析包

#############################################################################
########coxLasso####先用Lasso回归对变量进行降维##############################
#install.packages("foreign")
library(foreign)       #调用foreign包

rawdata<-read.csv("WD_COX.csv")        #读取R work中的

View(rawdata)          #查看数据

str(rawdata)           #查看数据的类型，非常重要

summary(rawdata)       #数据进行简单描述

rawdata<-na.omit(rawdata)

?glmnet
#数据集区分
dev = rawdata[rawdata$group==1,]
vad = rawdata[rawdata$group==0,]
#install.packages("glmnet")
#install.packages("Matrix")
library(glmnet)
library(survival)

dev<-dev[dev$time != 0,]   #切记，time不可以出现等于0的case，否则后续不能运行

View(dev)
x<-as.matrix(dev[,c(6:47)])

y<-data.matrix(Surv(dev$time,dev$group))
y

fit<-glmnet(x,y,family = "cox")

#X轴为lnlambda
plot(fit,xvar="lambda",label=F)

#X轴为L1 norm
plot(fit,xvar="norm",label=TRUE)

#X轴为lnlambda
plot(fit,xvar="dev",label=TRUE)


print(fit)

lasso.coef<-coef(fit,s=0.00118)   #s=0.00118根据右边窗口的值进行修改

lasso.coef       #在最小lambda时，lasso回归个变量系数

##################################################################
####cvlasso#######################################################
cv.fit<-cv.glmnet(x,y,family="cox")
plot(cv.fit)
abline(v=log(c(cv.fit$lambda.min,cv.fit$lambda.1se)),lty=2,lwd=1.5)

#如果取最小值时
cv.fit$lambda.min
Coefficients <- coef(fit, s = cv.fit$lambda.min)
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index]
Active.Index
Active.Coefficients
row.names(Coefficients)[Active.Index]

#如果取1倍标准误
cv.fit$lambda.1se
Coefficients <- coef(fit, s = cv.fit$lambda.1se)
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index]
Active.Index
Active.Coefficients
row.names(Coefficients)[Active.Index]

#最小值或取1倍标准误的结果都可以，可以依据临床经验或一些其他文献的证据
######################################################################
#3.1先单因素分析

fit<-coxph(Surv(time,group==1)~Sex,data=dev)
fit

fit<-coxph(Surv(time,group==1)~Age,data = dev)
fit

fit<-coxph(Surv(time,group==1)~age66,data = dev)
fit

#3.2后多因素分析
fit<-coxph(Surv(time,dead==1)~Sex + Age + age66,data = dev)
fit
summary(fit)

######## 3.3批量单因素分析#############

uni_cox_model<-function(x){
  FML<-as.formula(paste0("Surv(time,dead==1)~",x))  #dead==1，指的是1为目标结局事件
  cox1<-coxph(FML,data = dev)
  cox2<-summary(cox1)
  
  #计算我们所要的指标
  HR<-round(cox2$conf.int[,2],3)
  CI<-paste0(round(cox2$conf.int[,3:4],3),collapse = '-')
  P<-round(cox2$coefficients[,5],3)
  
  #将计算出来的指标制作为数据框
  uni_cox_model<-data.frame('characteristics'=x,
                            'HR'=HR,
                            'CI'=CI,
                            'P'=P)
  return(uni_cox_model)
}

#指定参与分析的若干自变量X
variable.names<-colnames(dev)[c(6,15,17:19,22,25,31,38,39,41:42,44)]  #要核实这里的X对应的列是否对？若分开的可以这样[c(3:18,20:40)]
variable.names

#运行上面自定义批量执行函数
uni_cox<-lapply(variable.names,uni_cox_model)
uni_cox


#install.packages("plyr")
library(plyr)

#生成单变量分析的综合结果
uni_cox<-ldply(uni_cox,data.frame)

#看下结果是啥样子的
uni_cox

View(uni_cox)


write.csv(uni_cox, "unicox.csv")

#3.4多因素分析
#直接将P<0.1的变量的characteristics提取出来
uni_cox$characteristics[uni_cox$P<= 0.1]


fml<- as.formula(paste0('Surv(time,dead==1)~',paste0(uni_cox$characteristics[uni_cox$P<0.1],collapse = '+'))) 
fml

#Surv(time, dead == 1) ~ sex + age + diseaseonset + treatment + 
#complication + Treatmethods + CPT + PLR + ALB + CHE + INR + 
#  NH3 + Cu + urineCu


#enter回归
mcox<-coxph(fml,data=dev)
summary(mcox)


#cbind(coef= coef(mcox),confint(mcox))
#exp(cbind(OR= coef(mcox),confint(mcox)))

#install.packages("MASS")
library(MASS)

#向前（代码有问题）
#fmcox<-stepAIC(mcox,direction ="forward",data=dev)
#summary(fmcox)

#向后法
bmcox<-stepAIC(mcox,direction = "backward",data=dev)
summary(bmcox)

#逐步法
dbmcox<-stepAIC(mcox,direction = "both",data=dev)

summary(dbmcox)
#Surv(time, dead == 1) ~ sex + diseaseonset + CPT

#展示各模型的AIC
AIC(mcox,bmcox,dbmcox)

#各模型AIC比较
#anova(mcox,fmcox)
anova(mcox,bmcox)
anova(mcox,dbmcox)
anova(mcox,bmcox)
anova(mcox,dbmcox)
anova(bmcox,dbmcox)


#选择最终的结果
msumcox<-summary(bmcox)
msumcox

#多因素结果制表
names(dev)

multinames<-as.character(colnames(dev)[c(6,17,18,38,39,42,44)])
MHR<-round(msumcox$coefficients[,2],2)
MPV<-round(msumcox$coefficients[,5],3)
MCIL<-round(msumcox$conf.int[,3],2)
MCIU<-round(msumcox$conf.int[,4],2)
MCI<-paste0(MCIL,'-',MCIU)

mulcox<-data.frame('characteristics'=multinames, 'Hazard Ratio'= MHR, 'CI95'= MCI, 'P value' = MPV)
mulcox
#整合表格
Final<-merge.data.frame(uni_cox,mulcox,by ='characteristics',
                        all=T,sort = T)


View(Final)


#保存结果
write.csv(Final, "Finalcox.csv")

###################---至此，最终模型确定完毕----------####################


#units(dev$time)<-"Month"


############################################################################
#4.0模型验证-区分度
############################################################################

#----------------------------------------------------------------
##C-index计算方法一（rms包，只适合训练集，无法可信区间)

#install.packages("rms")
library(rms)

#rms包函数一般都需运行这两行代码
dd<- datadist(dev)
options(datadist='dd')

#重写一遍最终模型，注意与coxph模型模型表达不一样，主要为了方便nomo
fcox<-cph(Surv(time,group)~ CPT + diseaseonset,
          surv=T,x=T,y=T,data = dev)

fcox

#通过如下公式计算C指数
#C=Dxy/2+0.5
#C=0.511/2+0.5

#-----------------------------------------------------------------------
#方法二：采用coxph（可以计算C_index的95%CI，可以做训练集和验证集）
library(survival)

#训练集C_index和95%CI
model1<-coxph(Surv(time,dead==1)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu, data=dev)
summary(model1)
AIC(model1)

model2<-coxph(Surv(time,dead==1)~ age +  N.109L. + PTA.. + INR + urineCu, data=dev)
summary(model2)
AIC(model2)

model3<-coxph(Surv(time,dead==1)~ age +  WBC.109L. + N.109L. + PTA.. + INR + urineCu + TBIL, data=dev)
summary(model3)
AIC(model3)

model4<-coxph(Surv(time,dead==1)~ age +  WBC.109L. + N.109L. + PTA.. + INR + urineCu + ALB, data=dev)
summary(model4)
AIC(model4)

model5<-coxph(Surv(time,dead==1)~ age +  WBC.109L. + N.109L. + PTA.. + INR + urineCu + PT, data=dev)
summary(model5)
AIC(model5)

model6<-coxph(Surv(time,dead==1)~ age +  WBC.109L. + N.109L. + PTA.. + INR + urineCu + NH3, data=dev)
summary(model6)
AIC(model6)

model7<-coxph(Surv(time,dead==1)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu + TBIL + NH3, data=dev)
summary(model7)
AIC(model7)

#Concordance= 0.798  (se = 0.031 )
#95%CI:C+/-1.96*se
L<-0.799-1.96*0.031
L
U<-0.799+1.96*0.031
U


#验证集C_index和95%CI
model2<-coxph(Surv(time,dead==1)~predict(model7,newdata=vad),data=vad)
summary(model2)

#Concordance= 0.838 (se = 0.062 )
#95%CI:C+/-1.96*se
L<-0.838-1.96*0.062
L
U<-0.838+1.96*0.062
U

##--------------------------------------------------------------------
#区分度之：时点ROC

setwd("D:/R work")     #设置工作路径
library(foreign)       #调用foreign包
rawdata<-read.csv("WD_Cox.csv")        #读取R work中的
View(rawdata)          #查看数据
str(rawdata)           #查看数据的类型，非常重要
summary(rawdata)       #数据进行简单描述
rawdata<-na.omit(rawdata)

#数据集区分
dev = rawdata[rawdata$group==1,]
vad = rawdata[rawdata$group==0,]

library(survival)
coxmod<-coxph(Surv(time,dead==1)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu, data=dev)

#以死亡率为连续变量进行ROC曲线分析
#训练集6年和18年死亡概率
dev$six.years.death.Probability = c(1- (summary(survfit(coxmod,newdata=dev),times=72)$surv))  #计算72月死亡率
dev$twelve.years.death.Probability = c(1- (summary(survfit(coxmod,newdata=dev),times=144)$surv))  #计算144月死亡率
dev$eighteen.years.death.Probability = c(1- (summary(survfit(coxmod,newdata=dev),times=216)$surv))  #计算216月死亡率

#验证集6年和18年死亡概率
vad$six.years.death.Probability = c(1- (summary(survfit(coxmod,newdata=vad),times=72)$surv))  #计算72月死亡率
vad$twelve.years.death.Probability = c(1- (summary(survfit(coxmod,newdata=vad),times=144)$surv))  #计算144月死亡率
vad$eighteen.years.death.Probability = c(1- (summary(survfit(coxmod,newdata=vad),times=216)$surv))  #计算216月死亡率

#安装survivalROC包
#install.packages("survivalROC")
library(survivalROC)

#---<----------------------------------------------------------------
#训练集用"KM"法拟合6年时点生存ROC曲线 
SROC= survivalROC(Stime = dev$time, status = dev$dead,
                  marker = dev$six.years.death.Probability,
                  predict.time = 72, method= "KM" )       #构建生存函数

cut.op= SROC$cut.values[which.max(SROC$TP-SROC$FP)]      #计算最佳截断值
cut.op      # 输出最佳截断值，注意这里是6年死亡率

#绘制训练集6年时点生存ROC曲线
plot(SROC$FP,SROC$TP, type="l", xlim=c(0,1), ylim=c(0,1),  
     xlab = paste( "FP","\n", "AUC = ",round(SROC$AUC,3)),
     ylab = "TP", col="black")

abline(0,1)
legend("bottomright",c("six year ROC "),col="black",lty=c(1,1))


#---<----------------------------------------------------------------
#训练集用"KM"法拟合12年时点生存ROC曲线 
SROC= survivalROC(Stime = dev$time, status = dev$dead,
                  marker = dev$twelve.years.death.Probability,
                  predict.time = 144, method= "KM" )       #构建生存函数

cut.op= SROC$cut.values[which.max(SROC$TP-SROC$FP)]      #计算最佳截断值
cut.op      # 输出最佳截断值，注意这里是6年死亡率

#绘制训练集6年时点生存ROC曲线
plot(SROC$FP,SROC$TP, type="l", xlim=c(0,1), ylim=c(0,1),  
     xlab = paste( "FP","\n", "AUC = ",round(SROC$AUC,3)),
     ylab = "TP", col="black")

abline(0,1)
legend("bottomright",c("twelve year ROC "),col="black",lty=c(1,1))

#---------------------------------------------
#训练集用"KM"法拟合18年生存ROC曲线 
SROC= survivalROC(Stime = dev$time, status = dev$dead, 
                  marker = dev$eighteen.years.death.Probability,
                  predict.time = 216, method= "KM") #构建生存函数

cut.op= SROC$cut.values[which.max(SROC$TP-SROC$FP)]  # 计算最佳截断值
cut.op # 输出最佳截断值，注意这里是18年的死亡率

#绘制训练集18年时点生存ROC曲线
plot(SROC$FP,SROC$TP, type="l", xlim=c(0,1), ylim=c(0,1),  
     xlab = paste( "FP","\n", "AUC = ",round(SROC$AUC,3)),
     ylab = "TP", col="black")
abline(0,1)
legend("bottomright",c("eight year ROC"),col="black",lty=c(1,1))

#---------------------------------------------------------
#训练集6年、12年和18年绘制在一张图
SROC1 = survivalROC(Stime = dev$time, status = dev$dead,
                   marker = dev$six.years.death.Probability,
                   predict.time = 72, method= "KM" )       #构建生存函数

cut.op= SROC1$cut.values[which.max(SROC1$TP-SROC1$FP)]      #计算最佳截断值
cut.op      # 输出最佳截断值，注意这里是6年死亡率
#绘制训练集6年时点生存ROC曲线
plot(SROC1$FP,SROC1$TP, type="l", xlim=c(0,1), ylim=c(0,1),  
     xlab = "FP",
     ylab = "TP", col="red")

#绘制训练集12年时点生存ROC曲线
SROC2= survivalROC(Stime = dev$time, status = dev$dead,
                   marker = dev$twelve.years.death.Probability,
                   predict.time = 144, method= "KM" )       #构建生存函数

cut.op= SROC2$cut.values[which.max(SROC2$TP-SROC2$FP)]      #计算最佳截断值
cut.op      # 输出最佳截断值，注意这里是12年死亡率
lines(SROC2$FP,SROC2$TP, type="l", xlim=c(0,1), ylim=c(0,1),  
      xlab = "FP",
      ylab = "TP", col="green",add=TRUE)

#绘制训练集18年时点生存ROC曲线
SROC2= survivalROC(Stime = dev$time, status = dev$dead,
                   marker = dev$eighteen.years.death.Probability,
                   predict.time = 216, method= "KM" )       #构建生存函数
cut.op= SROC2$cut.values[which.max(SROC2$TP-SROC1$FP)]      #计算最佳截断值
cut.op      # 输出最佳截断值，注意这里是18年死亡率
lines(SROC2$FP,SROC2$TP, type="l", xlim=c(0,1), ylim=c(0,1),  
      xlab = "FP",
      ylab = "TP", col="blue",add=TRUE)

#添加对角线和图例
abline(0,1)
legend("bottomright",c("6year AUC=0.808","12year AUC=0.788","18year AUC=0.874"),col=c("red","green","blue"),lty=c(1,1))


###############################验证集#####################################################
#验证集6年和18年
#----------------------------------------------------------------
#验证集用"KM"法拟合6年时点生存ROC曲线 
SROC= survivalROC(Stime = vad$time, status = vad$dead,
                  marker = vad$six.years.death.Probability,
                  predict.time = 72, method= "KM" )       #构建生存函数

cut.op= SROC$cut.values[which.max(SROC$TP-SROC$FP)]      #计算最佳截断值
cut.op      # 输出最佳截断值，注意这里是36月死亡率

#绘制验证集6年时点生存ROC曲线
plot(SROC$FP,SROC$TP, type="l", xlim=c(0,1), ylim=c(0,1),  
     xlab = paste( "FP","\n", "AUC = ",round(SROC$AUC,3)),
     ylab = "TP", col="black")

abline(0,1)
legend("bottomright",c("six year ROC"),col="black",lty=c(1,1))


#验证集用"KM"法拟合12年时点生存ROC曲线 
SROC= survivalROC(Stime = vad$time, status = vad$dead,
                  marker = vad$twelve.years.death.Probability,
                  predict.time = 144, method= "KM" )       #构建生存函数

cut.op= SROC$cut.values[which.max(SROC$TP-SROC$FP)]      #计算最佳截断值
cut.op      # 输出最佳截断值，注意这里是144月死亡率

#绘制验证集6年时点生存ROC曲线
plot(SROC$FP,SROC$TP, type="l", xlim=c(0,1), ylim=c(0,1),  
     xlab = paste( "FP","\n", "AUC = ",round(SROC$AUC,3)),
     ylab = "TP", col="black")

abline(0,1)
legend("bottomright",c("twelve year ROC"),col="black",lty=c(1,1))

#-----------------------------------------------------------------
#验证集用"KM"法拟合18年时点生存ROC曲线 

SROC= survivalROC(Stime = vad$time, status = vad$dead,
                  marker = vad$eighteen.years.death.Probability,
                  predict.time = 216, method= "KM" )       #构建生存函数

cut.op= SROC$cut.values[which.max(SROC$TP-SROC$FP)]      #计算最佳截断值
cut.op      # 输出最佳截断值，注意这里是3月死亡率


#绘制验证集18年时点生存ROC曲线
plot(SROC$FP,SROC$TP, type="l", xlim=c(0,1), ylim=c(0,1),  
     xlab = paste( "FP","\n", "AUC = ",round(SROC$AUC,3)),
     ylab = "TP", col="black")

abline(0,1)

legend("bottomright",c("eight year ROC"),col="black",lty=c(1,1))


#---------------------------------------------------------------------
##验证集6年和18年时点ROC绘制在一张图
#验证集6年
SROC6= survivalROC(Stime = vad$time, status = vad$dead,
                   marker = vad$six.years.death.Probability,
                   predict.time = 72, method= "KM" )       #构建生存函数

cut.op= SROC6$cut.values[which.max(SROC6$TP-SROC6$FP)]      #计算最佳截断值
cut.op      # 输出最佳截断值，注意这里是72月死亡率

#绘制验证集6年时点生存ROC曲线
plot(SROC6$FP,SROC6$TP, type="l", xlim=c(0,1), ylim=c(0,1), lwd=1, 
     xlab ="FP",
     ylab = "TP", col="red")

#验证集12年
SROC12= survivalROC(Stime = vad$time, status = vad$dead,
                   marker = vad$twelve.years.death.Probability,
                   predict.time = 144, method= "KM" )       #构建生存函数

cut.op= SROC12$cut.values[which.max(SROC12$TP-SROC12$FP)]      #计算最佳截断值
cut.op      # 输出最佳截断值，注意这里是144月死亡率

#绘制验证集144月时点生存ROC曲线
lines(SROC12$FP,SROC12$TP, type="l", xlim=c(0,1), ylim=c(0,1), lwd=1, 
      xlab ="FP",
      ylab = "TP", col="green")

#验证集18年
SROC18= survivalROC(Stime = vad$time, status = vad$dead,
                   marker = vad$eighteen.years.death.Probability,
                   predict.time = 216, method= "KM" )       #构建生存函数

cut.op= SROC18$cut.values[which.max(SROC18$TP-SROC18$FP)]      #计算最佳截断值
cut.op      # 输出最佳截断值，注意这里是216月死亡率

#绘制验证集6月时点生存ROC曲线
lines(SROC18$FP,SROC18$TP, type="l", xlim=c(0,1), ylim=c(0,1), lwd=1, 
      xlab ="FP",
      ylab = "TP", col="blue")


#添加对角线和图例
abline(0,1,lwd=1)
legend("bottomright",c("6year AUC=0.509","12year AUC=0.795","18year AUC=0.842"),col=c("red","green","blue"),lty=c(1,1))


#################################################################################
##### 绘制整个模型的time-AUC，展示整个模型的区分度###############################
setwd("D:/R work")     #设置工作路径

library(foreign)       #调用foreign包


rawdata<-read.csv("WD_Cox.csv")        #读取R work中的

View(rawdata)          #查看数据

str(rawdata)           #查看数据的类型，非常重要

summary(rawdata)       #数据进行简单描述

rawdata<-na.omit(rawdata)


#数据集区分
dev = rawdata[rawdata$group==1,]
vad = rawdata[rawdata$group==0,]

#TIme-AUC-----------------------------------
#install.packages("pec")
library(pec)
library(rms)
library(prodlim)
library(survival)

fcox<-cph(Surv(time,dead)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu, 
          surv=T,x=T,y=T,data = dev)

#Time-Cindex
C_index1<-cindex(list("model final"=fcox),
                 eval.times=seq(0,588,12),
                 cens.dodel="cox",
                 keep.pvalue=T,
                 confint=T,
                 confLevel=0.95
)
C_index1


plot(C_index1,
     xlim = c(0,588),  #设置x轴范围
     legend.x = 1,     #图例的位置
     legend.y = 1,     #图例的位置
     legend.cex = 0.8, #图例的文字大小
     col = "red")      #线条颜色设置为红色


#Time-Cindex with Bootstrap

C_index2<-cindex(list("model Booststrap"=fcox),
                 eval.times=seq(0,588,12),
                 cens.dodel="cox",
                 keep.pvalue=T,
                 confint=T,
                 confLevel=0.95,
                 splitMethod = "bootcv",
                 B = 500)
C_index2


plot(C_index2,
     xlim = c(0,588),  #设置x轴范围
     legend.x = 1,     #图例的位置
     legend.y = 1,     #图例的位置
     legend.cex = 0.8, #图例的文字大小
     col = "blue")      #线条颜色设置为红色


###合并在一张图中(原始数据C_index和Bootstrap C_index)

plot(C_index1,
     xlim = c(0,588),  #设置x轴范围
     legend=FALSE,
     legend.x = 1,     #图例的位置
     legend.y = 1,     #图例的位置
     legend.cex = 0.8, #图例的文字大小
     col = "red")      #线条颜色设置为红色



plot(C_index2,
     xlim = c(0,588),  #设置x轴范围
     legend.x = 1,     #图例的位置
     legend.y = 0.8,     #图例的位置
     legend.cex = 0.8, #图例的文字大小
     col = "blue",
     add=TRUE)      #线条颜色设置为红色


legend("topright",c("model final","model Bootstrap"),
       lty = c(1,1,1),
       lwd = c(2,2,2),
       col = c("red","blue"))

#或者指定图例的位置
legend(6,1,c("model final","model Bootstrap"),
       lty = c(1,1,1),
       lwd = c(2,2,2),
       col = c("red","blue"))


##############################################################    
#5.0模型验证-校准度

library(rms)
library(survival)

#设置时间单位为month
units(dev$time)<-"month"

##训练集6年校准曲线
#重写一遍模型
fcox<-cph(Surv(time,dead)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu,
          surv=T,x=T,y=T,
          time.inc = 72,      #6年时的校准曲线
          data = dev)

#拟合calibrate
cal1<-calibrate(fcox,
                cmethod = 'KM',
                method = 'boot',
                u = 72,        #需要与上面的time.inc一致
                m = 25,       #大约为样本量除以3
                B = 500)
#绘制校准曲线
plot(cal1,
     lwd = 2,
     lty = 1,
     errbar.col = "blue",
     xlim=c(0,1),
     ylim = c(0,1),
     xlab = "Nomogram-Predicted Probability of 72-month OS",
     ylab = "Actual 72-month OS(proportion)",
     subtitles = T)


#设置中间连接线的类型
lines(cal1[,c("mean.predicted","KM")],type = "b",lwd = 2, col = "red",pch=16)

#参考线类型
abline(0,1,lty=3,lwd=2,col="black")

######################################################
##训练集18年校准曲线
#重写一遍模型
fcox6<-cph(Surv(time,dead)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu,
           surv=T,x=T,y=T,
           time.inc = 216,      #18年时的校准曲线
           data = dev)

#拟合calibrate
cal2<-calibrate(fcox6,
                cmethod = 'KM',
                method = 'boot',
                u = 216,        #需要与上面的time.inc一致
                m = 25,       #大约为样本量除以3
                B = 500)
#绘制校准曲线
plot(cal2,
     lwd = 2,
     lty = 1,
     errbar.col = "blue",
     xlim=c(0,1),
     ylim = c(0,1),
     xlab = "Nomogram-Predicted Probability of 216-month OS",
     ylab = "Actual 216-month OS(proportion)",
     subtitles = T)


#设置中间连接线的类型
lines(cal2[,c("mean.predicted","KM")],type = "b",lwd = 2, col = "red",pch=16)

#参考线类型
abline(0,1,lty=3,lwd=2,col="black")

########################################################################
##验证集72个月校准曲线
#重写一遍模型
vadfcox<-cph(Surv(time,dead)~predict(fcox,newdata = vad),
             surv=T,x=T,y=T,
             time.inc = 72,      #72月时的校准曲线
             data = vad)


#拟合calibrate
vadcal1<-calibrate(vadfcox,
                   cmethod = 'KM',
                   method = 'boot',
                   u = 72,        #需要与上面的time.inc一致
                   m = 15,       #大约为样本量除以3
                   B = 500)
#绘制校准曲线
plot(vadcal1,
     lwd = 2,
     lty = 1,
     errbar.col = "blue",
     xlim=c(0,1),
     ylim = c(0,1),
     xlab = "Predicted Probability of 72-month OS",
     ylab = "Actual 72-month OS(proportion)",
     subtitles = T)


#设置中间连接线的类型
lines(vadcal1[,c("mean.predicted","KM")],type = "b",lwd = 2, col = "red",pch=16)

#参考线类型
abline(0,1,lty=3,lwd=2,col="black")

####################################################################
##验证集216个月校准曲线
#重写一遍模型
vadfcox6<-cph(Surv(time,dead)~predict(fcox6,newdata=vad),
              surv=T,x=T,y=T,
              time.inc = 216,      #18年的校准曲线
              data = vad)


#拟合calibrate
vadcal6<-calibrate(vadfcox6,
                   cmethod = 'KM',
                   method = 'boot',
                   u = 216,        #需要与上面的time.inc一致
                   m = 15,       #大约为样本量除以3
                   B = 500)
#绘制校准曲线
plot(vadcal6,
     lwd = 2,
     lty = 1,
     errbar.col = "blue",
     xlim=c(0,1),
     ylim = c(0,1),
     xlab = "Nomogram-Predicted Probability of 216-month OS",
     ylab = "Actual 216-month OS(proportion)",
     subtitles = T)


#设置中间连接线的类型
lines(vadcal6[,c("mean.predicted","KM")],type = "b",lwd = 2, col = "red",pch=16)

#参考线类型
abline(0,1,lty=3,lwd=2,col="black")

##其他时间点如此类推即可


#################################################################
#####6.0模型验证-临床适用度
################################################################

setwd("D:/R work")     #设置工作路径

library(foreign)       #调用foreign包


rawdata<-read.csv("WD_Cox.csv")        #读取R work中的

View(rawdata)          #查看数据

str(rawdata)           #查看数据的类型，非常重要

summary(rawdata)       #数据进行简单描述

rawdata<-na.omit(rawdata)
#数据集区分
dev = rawdata[rawdata$group==1,]
vad = rawdata[rawdata$group==0,]

#install.packages("devtools")
#options(unzip ='internal')
#devtools::install_github('yikeshu0611/ggDCA')

library(survival)
library(rms)
source("stdca.R")

#重写一遍模型
fcox<-coxph(Surv(time,dead)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu, data = dev)

#模型预测6，18年死亡概率
dev$pr_failure6<-c(1-summary(survfit(fcox,newdata=dev),times = 72)$surv)
dev$pr_failure18<-c(1-summary(survfit(fcox,newdata=dev),times = 216)$surv)

##决策曲线分析
#训练集6-18year-succeed
#dev<-as.data.frame(dev)

#View(dev)
#??stdca
dca6<-stdca(data = dev,
            outcome = "dead",
            ttoutcome = "time",
            timepoint = 72,
            predictors = "pr_failure6",
            xstop = 0.6,        #如果出图有问题，调整这里的界值为0.5，然后再调整
            smooth = TRUE)

dca6


dca18<-stdca(data = dev,
            outcome = "dead",
            ttoutcome = "time",
            timepoint = 216,
            predictors = "pr_failure18",
            xstop = 0.7,            #0.9不能出图
            smooth = TRUE)
dca18



#####验证集6-18year决策曲线
#模型预测6，18year死亡概率
vad$pr_failure6<-c(1-summary(survfit(fcox,newdata=vad),times = 72)$surv)
vad$pr_failure18<-c(1-summary(survfit(fcox,newdata=vad),times = 216)$surv)

#决策曲线分析
vdca6<-stdca(data = vad,
             outcome = "dead",
             ttoutcome = "time",
             timepoint = 72,
             predictors = "pr_failure6",
             xstop = 0.5,
             smooth = TRUE)
vdca6

vdca18<-stdca(data = vad,
             outcome = "dead",
             ttoutcome = "time",
             timepoint = 216,
             predictors = "pr_failure18",
             xstop = 0.5,
             smooth = TRUE)
vdca18



##如何将多条曲线放入一张图
#构建fcoxA模型
fcoxA<-coxph(Surv(time,dead)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu,
             data = dev)

#模型预测6year死亡概率
dev$pr_failure6A<-c(1-summary(survfit(fcoxA,newdata=dev),times = 72)$surv)


dca6A<-stdca(data = dev,
             outcome = "dead",
             ttoutcome = "time",
             timepoint = 72,
             predictors = "pr_failure6A",
             xstop = 0.8,
             smooth = TRUE)

dca6A


#构建fcoxB模型
fcoxB<-coxph(Surv(time,dead)~age + WBC.109L. + N.109L. + PTA.. + INR + urineCu,data = dev)

#模型预测18year死亡概率
dev$pr_failure6B<-c(1-summary(survfit(fcoxB,newdata=dev),times = 216)$surv)

dca6B<-stdca(data = dev,
             outcome = "dead",
             ttoutcome = "time",
             timepoint = 216,
             predictors = "pr_failure6B",
             xstop = 0.8,
             smooth = TRUE)
dca6B

#曲线叠加
######################suceed####

plot(dca6A$net.benefit.threshold,dca6A$net.benefit.none,type = "l",lwd=2,xlim = c(0,.80),ylim = c(-.05,.80),xlab = "Threshold Probability",ylab = "Net benefit")

lines(dca6A$net.benefit$threshold,dca6A$net.benefit$all,type = "l",col="red",lwd=2)

lines(dca6A$net.benefit$threshold,dca6A$net.benefit$none,type = "l",col="red",lwd=2,lty=2)

lines(dca6A$net.benefit$threshold,dca6A$net.benefit$pr_failure6A,type = "l",lwd=2,col="blue")

lines(dca6B$net.benefit$threshold,dca6B$net.benefit$pr_failure6B,type = "l",lwd=2,col="green",lty=2)

legend("topright",cex = 0.8,legend = c("ALL","fCOXA","fcoxB","None"),col = c("red","blue","green","red"),lwd=c(2,2,2,2),lty = c(1,1,2,2))

#######################################################################
#交叉验证：mboost-CoxBoost
#Boosting in Cox regression: a comparison between the likelihood-based and the model-based approaches with focus on the R-packages CoxBoost and mboost
#1.下载CoxBoost安装包
#2.安装，如果不成功，看是否缺少Rtools；
#3.下载Rtools网址：https://mirrors.tuna.tsinghua.edu.cn/CRAN/bin/windows/Rtools/history.html
#4.安装Rtools
#5.重启Rstudio，输入代码运行以下：writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
#6.然后在安装CoxBoost包，即可安装成功！

#install.packages("mboost")
#install.packages("CoxBoost")
library(CoxBoost)

setwd("D:/R work")     #设置工作路径

library(foreign)       #调用foreign包

rawdata<-read.csv("WD_Cox.csv")        #读取R work中的

#数据集区分
dev = rawdata[rawdata$group==1,]
vad = rawdata[rawdata$group==0,]

View(dev)

time<-dev$time
status<-dev$dead
x<-as.matrix(dev[,2:13])

cbfit<-CoxBoost(time = time,
                status = status,
                x=x,              #x必须为矩阵格式
                stepno = 100,     #设置Bootstrap次数，最大为500次，预设后期要改
                penalty = 50)     #设置惩罚值为50，预设后期要改

summary(cbfit)

#如下不可以含有缺失值
optim.res<-optimCoxBoostPenalty(time = time,
                                status = status,
                                x=x,
                                trace = FALSE,       #不显示迭代过程
                                start.penalty = 10)  #设置惩罚值起点，过程较长，需要等待一段时间


optim.res$penalty


#10重交叉验证
set.seed(123)
cv.res<-cv.CoxBoost(time = time,
                    status = status,
                    x=x,                      #x必须为矩阵
                    maxstepno = 500,          #设置Bootstrap最大次数为500
                    K=10,                     #设置10折交叉验证
                    type = "verweij",         #计算带外数据的偏似然贡献值
                    penalty=optim.res$penalty)#设置最优惩罚值

plot(cv.res$mean.logplik)

cv.res$optimal.step

#重新建立模型

#cbfit<-CoxBoost(time = time,
#                status = status,
#               x=x,
#              maxstepno = cv.res$optimal.step,     #设置Bootstrap最优迭代次数
#             K=10,                              #设置10折交叉验证
#            type = "verweij",                  #计算带外数据的偏似然贡献值
#           penalty=optim.res$penalty)         #设置最优惩罚值

cbfit<-CoxBoost(time = time,
                status = status,
                x=x,
                stepno = cv.res$optimal.step,     #设置Bootstrap次数
                penalty = optim.res$penalty)      #设置惩罚值为50

summary(cbfit)

names<-cbfit$xnames[which(cbfit$coefficients[51,]!=0)]     #此处51，因为前面我们最优迭代为50，所以51行为最终结果

coef<-cbfit$coefficients[51,][which(cbfit$coefficients[51,]!=0)]
cbind(names,coef)


?CoxBoost
#########################################################################
#7.0模型展示-Nomogram
##方法一：普通列线图

library(foreign)     #读取SPSS文件
library(rms)         #回归及绘制列线图
library(survival)    #生存分析包

rawdata<-read.csv("WD_Cox.csv")        #读取R work中的

rawdata<-as.data.frame(rawdata)

#数据集区分
dev = rawdata[rawdata$group==1,]
vad = rawdata[rawdata$group==0,]

dd<-datadist(dev)
options(datadist = "dd")


fcox<-cph(Surv(time,group)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu,x=T,y=T,surv = T,
          data = dev)

#设置生存函数
surv<-Survival(fcox)

#
surv1<-function(x)surv(1*72,lp=x)   #6year生存函数
surv2<-function(x)surv(1*144,lp=x)   #12year生存函数
surv3<-function(x)surv(1*216,lp=x)  #18year生存函数

nomocox<-nomogram(fcox,
                  fun=list(surv1,surv2,surv3),
                  lp=FALSE,
                  funlabel = c("6-year Survival prob","12-year survival Prob","18-year survival prob"),
                  maxscale = 100,   #将points刻度设置为100或者10
                  fun.at = c("0.9","0.85","0.80","0.70","0.60","0.50","0.40","0.30","0.20","0.10"))


plot(nomocox)


###########################################
##方法二：动态列线图
###########################################
library(shiny)
library(DynNom)
library(magrittr)

DynNom(fcox,DNtitle="Nomogram",DNxlab="probability",data = dev)



#####################################################################
######8.0其他重要技术
######8.1riskplot绘制（风险得分图绘制）（ggrisk在Rversion4.1测试成功，3.6不行）

#install.packages("ggrisk")
#install.packages("rms")

library(ggrisk)
library(rms)

#library(foreign)     #读取SPSS文件
#library(rms)         #回归及绘制列线图
#library(survival)    #生存分析包

rawdata<-read.csv("WD_COX.csv")        #读取R work中的

#数据集区分
dev = rawdata[rawdata$group==1,]
vad = rawdata[rawdata$group==0,]


fcox<-cph(Surv(time,dead)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu, x=T,y=T,surv = T,
          data = dev)
#采用coxph函数也可以
#fcox<-coxph(Surv(time,dead)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu,data = dev)

#指定median为切点
ggrisk(fcox,           #模型名称
       cutoff.value = "median",
       cutoff.x=145,   #cutoff标签位置
       cutoff.y=-0.8)  #cutoff标签位置
#采用roc找切点
ggrisk(fcox,           #模型名称
       cutoff.value = "roc",
       cutoff.x=145,   #cutoff标签位置
       cutoff.y=-0.8)  #cutoff标签位置

#指定热图指标
ggrisk(fcox,
       heatmap.genes=c('age','WBC.109L.','N.109L.','PTA..','INR','urineCu'))
ggrisk

#############################################
###8.2 NRI指数

library(survival)

#install.packages("nricens")
library(nricens)
fit1<-coxph(Surv(time,dead)~ age + WBC.109L. + N.109L. + PTA.. + INR + urineCu,x=T,data = dev)
fit2<-coxph(Surv(time,dead)~ age + N.109L. + PTA.. + INR + urineCu,x=T,data = dev)

#分类NRI
set.seed(123)                  #设定种子，保证大家结果一致
nricens(mdl.std=fit1,
        mdl.new=fit2,
        t0=12,                 #设置要比较的时间点
        cut = c(0.2,0.4),      #设置分类界值，这个一定要结合临床实际确定，此处为演示
        updown = "category",   #指定为分类NRI
        niter = 100)           #设定迭代次数，方便计算可信区间

#连续NRI
set.seed(123)
nricens(mdl.std=fit1,
        mdl.new=fit2,
        t0=12,                 #设置要比较的时间点
        cut = 0,               #设置0界值，这个一定要结合临床实际确定，此处为演示
        updown = "diff",       #指定为updown=iiff,代表连续性NRI
        niter = 100)           #设定迭代次数，方便计算可信区间


###8.3 IDI指数
#install.packages("survIDINRI")

library(survival)
library(survIDINRI)
library(survC1)

#读取数据
rawdata<-read.csv("WD_Cox.csv")        #读取R work中的


#数据集区分
dev = rawdata[rawdata$group==1,]
vad = rawdata[rawdata$group==0,]

y<-dev[,c("time","dead")]

old<-dev[,c('age','WBC.109L.','N.109L.','PTA..','INR','urineCu')]
new<-dev[,c('age','N.109L.','PTA..','INR','urineCu')]


x<-IDI.INF(y,old,new,t0=12,npert = 200)

IDI.INF.OUT(x)    #M1即IDI

IDI.INF.GRAPH(x)

?IDI.INF          #可以看具体参数解释


###################################################
##------风险分组再验证----------------------------------------

library(foreign)       #调用foreign包

rawdata<-read.csv("WD_Cox.csv")        #读取R work中的

View(rawdata)          #查看数据

str(rawdata)           #查看数据的类型，非常重要

summary(rawdata)       #数据进行简单描述

rawdata<-na.omit(rawdata)

#数据集区分
dev = rawdata[rawdata$group==1,]
vad = rawdata[rawdata$group==0,]


#构建的最终COX模型
multiCox=coxph(Surv(time,dead)~age + WBC.109L. + N.109L. + PTA.. + INR + urineCu,data = dev)

#预测训练集的riskscore
riskScore=predict(multiCox,type = "risk",newdata = dev)   #type="lp"

#将riskscore按照中位数分为高低风险组
risk=as.vector(ifelse(riskScore>median(riskScore),"high","low"))

#预测验证集的riskscore
vadriskScore=predict(multiCox,type = "risk",newdata = vad)

#将riskscore按照中位数分为高低风险组
vadrisk=as.vector(ifelse(vadriskScore>median(riskScore),"high","low"))

#---训练集-----------------------
fit1 <- survfit(Surv(time,dead) ~ risk, data = dev)
#install.packages("survminer")
#install.packages("ggplot2")
library(ggplot2)
library(survminer)
library(survival)

ggsurvplot(fit1,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF")
)

plot(fit1, xlab="Survival Time in Months",
     ylab="% Surviving", yscale=100,col=c("red","blue"),
     main="Survival Distributions by risk")
legend("topright", title="Risk",c("High risk", "Low risk"),
       fill=c("red", "blue")) # 绘图

#-验证集--------------------------------------------------

fit2 <- survfit(Surv(time,dead) ~ vadrisk, data = vad)

ggsurvplot(fit2,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF")
)


plot(fit2, xlab="Survival Time in Months",
     ylab="% Surviving", yscale=100,col=c("red","blue"),
     main="Survival Distributions by Risk")
legend("topright", title="Risk",c("High risk", "Low risk"),
       fill=c("red", "blue")) # 绘图

#####This is the end of COX predict model################
#########################################################



