#设置工作路径
getwd()
setwd("D:/R work")

########################################################################
#install.package('meta')
#install.packages("remotes")
#remotes::install_github("guido-s/meta", ref = "R-book-first-edition")
library('meta')
md = read.csv('肝肾阴虚.csv')
#data<-read.csv('E:/R 湿热内蕴',sep=",",header=TRUE)
View(md)

#在数据集data，产生率（没有经过转化），5种方法，选择最接近正太分布的那种
#单个率资料的Meta分析要求率的分布应该尽量服从正态分布，
#如原始率不服从正态分布，如原始率不服从正态分布，可经过转换使其服从或接近正态分布，从而提高合并结果的可靠性。
#命令metaprop()进提供了5种样本率的估计方法，根据样本率的分布决定使用哪种合并方法，
#五种估计方法如下：“PRAW”(没有转换的原始率), “PLN”(对数转换), “PLOGIT”(logit转换), “PAS”(反正弦转换),“PFT”(Freeman-Tukey双重反正弦转换),
#在进行Meta分析之前，对原始率及按四种估计方法进行转换后的率进行正态性检验，根据检验结果选择最接近正态分布的方法。
#PRAW”(没有转换的原始率) 痰瘀互结
rate<-transform(md,p=Events/Total)
shapiro.test(rate$p)

#“PLN”(对数转换)
rate<-transform(md,log=log(Events/Total))
shapiro.test(rate$log)

#“PLOGIT”(logit转换)
rate<-transform(md,logit=log((Events/Total)/(1-Events/Total)))
shapiro.test(rate$logit)

#“PAS”(反正弦转换) 湿热内蕴
rate<-transform(md,arcsin.size=asin(sqrt(Events/(Total+1))))
shapiro.test(rate$arcsin)

#“PFT”(Freeman-Tukey双重反正弦转换) 肝肾阴虚
rate<-transform(md,darcsin=0.5*(asin(sqrt(Events/(Total+1)))+asin((sqrt(Events+1)/(Total+1)))))
shapiro.test(rate$darcsin)

#合并效应量
meta1<-metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PFT")
meta1

#绘制森林图
forest(meta1)

#发表偏倚
funnel(meta1)

#Egger检验
metabias(meta1, k.min = 32)

#####################################湿热内蕴########################
library('meta')
md = read.csv('湿热内蕴.csv')
View(md)

#在数据集data，产生率（没有经过转化），5种方法，选择最接近正太分布的那种
#单个率资料的Meta分析要求率的分布应该尽量服从正态分布，
#如原始率不服从正态分布，如原始率不服从正态分布，可经过转换使其服从或接近正态分布，从而提高合并结果的可靠性。
#命令metaprop()进提供了5种样本率的估计方法，根据样本率的分布决定使用哪种合并方法，
#五种估计方法如下：“PRAW”(没有转换的原始率), “PLN”(对数转换), “PLOGIT”(logit转换), “PAS”(反正弦转换),“PFT”(Freeman-Tukey双重反正弦转换),
#在进行Meta分析之前，对原始率及按四种估计方法进行转换后的率进行正态性检验，根据检验结果选择最接近正态分布的方法。
#PRAW”(没有转换的原始率) 痰瘀互结
rate<-transform(md,p=Events/Total)
shapiro.test(rate$p)

#“PLN”(对数转换)
rate<-transform(md,log=log(Events/Total))
shapiro.test(rate$log)

#“PLOGIT”(logit转换)
rate<-transform(md,logit=log((Events/Total)/(1-Events/Total)))
shapiro.test(rate$logit)

#“PAS”(反正弦转换) 湿热内蕴
rate<-transform(md,arcsin.size=asin(sqrt(Events/(Total+1))))
shapiro.test(rate$arcsin)

#“PFT”(Freeman-Tukey双重反正弦转换) 肝肾阴虚
rate<-transform(md,darcsin=0.5*(asin(sqrt(Events/(Total+1)))+asin((sqrt(Events+1)/(Total+1)))))
shapiro.test(rate$darcsin)

#合并效应量
meta1<-metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PLOGIT")
meta1

#绘制森林图
forest(meta1)

#发表偏倚
funnel(meta1)

#Egger检验
metabias(meta1, k.min = 32)

###################################痰瘀互结######################
library('meta')
md = read.csv('痰瘀互结.csv')
View(md)

#在数据集data，产生率（没有经过转化），5种方法，选择最接近正太分布的那种
#单个率资料的Meta分析要求率的分布应该尽量服从正态分布，
#如原始率不服从正态分布，如原始率不服从正态分布，可经过转换使其服从或接近正态分布，从而提高合并结果的可靠性。
#命令metaprop()进提供了5种样本率的估计方法，根据样本率的分布决定使用哪种合并方法，
#五种估计方法如下：“PRAW”(没有转换的原始率), “PLN”(对数转换), “PLOGIT”(logit转换), “PAS”(反正弦转换),“PFT”(Freeman-Tukey双重反正弦转换),
#在进行Meta分析之前，对原始率及按四种估计方法进行转换后的率进行正态性检验，根据检验结果选择最接近正态分布的方法。
#PRAW”(没有转换的原始率) 痰瘀互结
rate<-transform(md,p=Events/Total)
shapiro.test(rate$p)

#“PLN”(对数转换)
rate<-transform(md,log=log(Events/Total))
shapiro.test(rate$log)

#“PLOGIT”(logit转换)
rate<-transform(md,logit=log((Events/Total)/(1-Events/Total)))
shapiro.test(rate$logit)

#“PAS”(反正弦转换) 湿热内蕴
rate<-transform(md,arcsin.size=asin(sqrt(Events/(Total+1))))
shapiro.test(rate$arcsin)

#“PFT”(Freeman-Tukey双重反正弦转换) 肝肾阴虚
rate<-transform(md,darcsin=0.5*(asin(sqrt(Events/(Total+1)))+asin((sqrt(Events+1)/(Total+1)))))
shapiro.test(rate$darcsin)

#合并效应量
meta1<-metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PRAW")
meta1

#绘制森林图
forest(meta1)

#发表偏倚
funnel(meta1)

#Egger检验
metabias(meta1, k.min = 22)

#Trim剪补法矫正数据后进行发表偏倚检验



##################################脾肾阳虚#############################
library('meta')
md = read.csv('脾肾阳虚.csv')
View(md)

#在数据集data，产生率（没有经过转化），5种方法，选择最接近正太分布的那种
#单个率资料的Meta分析要求率的分布应该尽量服从正态分布，
#如原始率不服从正态分布，如原始率不服从正态分布，可经过转换使其服从或接近正态分布，从而提高合并结果的可靠性。
#命令metaprop()进提供了5种样本率的估计方法，根据样本率的分布决定使用哪种合并方法，
#五种估计方法如下：“PRAW”(没有转换的原始率), “PLN”(对数转换), “PLOGIT”(logit转换), “PAS”(反正弦转换),“PFT”(Freeman-Tukey双重反正弦转换),
#在进行Meta分析之前，对原始率及按四种估计方法进行转换后的率进行正态性检验，根据检验结果选择最接近正态分布的方法。
#PRAW”(没有转换的原始率) 痰瘀互结
rate<-transform(md,p=Events/Total)
shapiro.test(rate$p)

#“PLN”(对数转换)
rate<-transform(md,log=log(Events/Total))
shapiro.test(rate$log)

#“PLOGIT”(logit转换)
rate<-transform(md,logit=log((Events/Total)/(1-Events/Total)))
shapiro.test(rate$logit)

#“PAS”(反正弦转换) 湿热内蕴
rate<-transform(md,arcsin.size=asin(sqrt(Events/(Total+1))))
shapiro.test(rate$arcsin)

#“PFT”(Freeman-Tukey双重反正弦转换) 肝肾阴虚
rate<-transform(md,darcsin=0.5*(asin(sqrt(Events/(Total+1)))+asin((sqrt(Events+1)/(Total+1)))))
shapiro.test(rate$darcsin)

#合并效应量
meta1<-metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PFT")
meta1

#绘制森林图
forest(meta1)

#发表偏倚
funnel(meta1)

#Egger检验
metabias(meta1, k.min = 19)








###############肝肾阴虚亚组分析######################################
#设置工作路径
getwd()
setwd("D:/R work")
md2 = read.csv('肝肾阴虚.csv')
md2<-na.omit(md2)
View(md2)

mod3 = metaprop(Events,Total,data=md2,studlab=paste(study.author),sm="PFT",
                subgroup = NOS
)

summary(mod3)
forest(mod3)

mod4 = metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PFT",
                subgroup = criteria 
)

summary(mod4)
forest(mod4)

mod5 = metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PFT",
                subgroup = method
)

summary(mod5)
forest(mod5)


###############湿热内蕴亚组分析######################################
#设置工作路径
getwd()
setwd("D:/R work")
#湿热内蕴
md2 = read.csv('湿热内蕴.csv')
md2<-na.omit(md2)
View(md2)

mod3 = metaprop(Events,Total,data=md2,studlab=paste(study.author),sm="PLOGIT",
                subgroup = NOS
)

summary(mod3)
forest(mod3)

mod4 = metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PLOGIT",
                subgroup = criteria 
)

summary(mod4)
forest(mod4)

mod5 = metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PLOGIT",
                subgroup = method
)

summary(mod5)
forest(mod5)


###############痰瘀互结亚组分析######################################
#设置工作路径
getwd()
setwd("D:/R work")
#痰瘀互结
md2 = read.csv('痰瘀互结.csv')
md2<-na.omit(md2)
View(md2)

mod3 = metaprop(Events,Total,data=md2,studlab=paste(study.author),sm="PRAW",
                subgroup = NOS
)

summary(mod3)
forest(mod3)

mod4 = metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PRAW",
                subgroup = criteria 
)

summary(mod4)
forest(mod4)

mod5 = metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PRAW",
                subgroup = method
)

summary(mod5)
forest(mod5)


###############脾肾阳虚亚组分析######################################
#设置工作路径
getwd()
setwd("D:/R work")
#脾肾阳虚
md2 = read.csv('脾肾阳虚.csv')
md2<-na.omit(md2)
View(md2)

mod3 = metaprop(Events,Total,data=md2,studlab=paste(study.author),sm="PFT",
                subgroup = NOS
)

summary(mod3)
forest(mod3)

mod4 = metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PFT",
                subgroup = criteria 
)

summary(mod4)
forest(mod4)

mod5 = metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PFT",
                subgroup = method
)

summary(mod5)
forest(mod5)

################################END#######################################

mod6 = metaprop(Events,Total,data=md,studlab=paste(study.author),sm="PAS",
                subgroup = AHRQ1
)

summary(mod6)
forest(mod6)

#meta回归具体命令
metareg(meta1, criteria + method + NOS1)
metareg(meta1, method + AHRQ)


# 数据来源于<艾灸治疗脑卒中后尿失禁临床效果的Meta分析>
md = read.csv('肝肾阴虚.csv')

mod = metacont(n1,mean1,sd1,n2,mean2,sd2,sm='SMD',
               studlab = paste(study, year),
               data=md
)
#sm用于指定效应量，这里选择SMD，stulab指定文献的标签，
#就是study和year。data指定数据来源，我们之前将数据命名为md

summary(mod) #总结并输出结果

funnel(mod)#漏斗图

#漏斗图是图示法，我们还可以用量化的方法检验下，下面的代码默认使用"Egger"检验方法
metabias(mod, k.min = 3)

#假设各研究之间的异质性比较高，此时可以视情况选择做meta回归、亚组分析或者敏感性分析。
#先介绍敏感性分析的做法：
#1，敏感性分析：
#敏感性分析的方法之一，就是逐项排除某些文献，比如说3个研究，A,B,C，如果三项合并异质性过高，可以先删A，看看效果，再删B，看看效果，删除某项研究或某几项研究后，剩余的研究之间异质性大幅减少，说明被删除的研究就大概率是造成异质性的来源。敏感性分析可以通过下面几行代码实现。
mod2 = metacont(n1,mean1,sd1,n2,mean2,sd2,sm='SMD',
                studlab = paste(study, year),
                data=md,
                exclude = grep("liu", study)
)
summary(mod2)

metainf(m1)#自动进行敏感性分析

#2，亚组分析
#亚组分析
md2 = read.csv('222.csv')

mod3 = metacont(n1,mean1,sd1,n2,mean2,sd2,sm='SMD',
                studlab = paste(study, year),data=md2,
                subgroup = time
)
summary(mod3)

#3,森林图
#install.packages('forest')
library(forest)
forest(md,digits=2) #digits=2意思是只显示2位小数

#meta分析回归
#导入数据，5项研究
data(Fleiss1993cont)  

#手动添加年龄
Fleiss1993cont$age <- c(55, 65, 55, 65, 55)  

#手动添加地区
Fleiss1993cont$region <- c("Europe", "Europe", 
                           "Asia", "Asia", "Europe")  

# 进行meta分析，效应量选择“MD”
m1 <- metacont(n.psyc, mean.psyc, sd.psyc, n.cont, 
               mean.cont, sd.cont,
               data = Fleiss1993cont, sm = "MD")

#以年龄、地区为自变量，以各研究的效应量为因变量，执行meta回归
metareg(m1, ~region + age)

forest(metareg(m1, ~region + age))

bubble(metareg(m1, ~age)) #气泡图，一次只能一个变量
bubble(metareg(m1, ~region))
