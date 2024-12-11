#设置工作路径
getwd()
setwd("D:/R work")

#install.packages("readr")
library(readr)
mydata <- read_csv("WD_data.csv")

mydata<-na.omit(mydata)
View(mydata)
names(mydata)

str(mydata)
#attach(mydata)

head(mydata)
#head(mydata,6)

# chisq.test
#######################################################################################
##############################Table 2 Part I General information for chisq.test########
# group with sex
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
sex <- c(rep("M",63),rep("F",33),rep("M",14),rep("none",8))
isex <- as.factor(sex)
data1 <- data.frame(ID,group,sex)
table(mydata$group, mydata$sex)
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$sex, expected = F, chisq = T, fisher = F, mcnemar = F, format = "SPSS")

# group with Hepatic subtype/neurologic subtype
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
type <- c(rep("H",10),rep("N",86),rep("H",4),rep("N",18))
itype <- as.factor(type)
data1 <- data.frame(ID,group,type)
table(mydata$group, mydata$type)
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$type, expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with ADL(able/disable)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
ADL <- c(rep("able",85),rep("disable",11),rep("able",16),rep("disable",6))
iADL <- as.factor(ADL)
data2 <- data.frame(ID,group,ADL)
table(mydata$group, mydata$ADL)
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$ADL, expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with Deterioration(yse/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
Deterioration <- c(rep("yes",38),rep("no",58),rep("yes",7),rep("no",15))
iDeterioration <- as.factor(Deterioration)
data3 <- data.frame(ID,group,Deterioration)
table(mydata$group, mydata$Deterioration)
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$Deterioration, expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with Whether to be readmitted to hospital within 6 months (yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'sixmonths' <- c(rep("yes",19),rep("no",77),rep("yes",9),rep("no",13))
'isixmonths' <- as.factor('sixmoth')
data4 <- data.frame(ID,group,'sixmoth')
table(mydata$group, mydata$'sixmoth')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'sixmoth', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with Whether to be readmitted to hospital within a year (yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
ayear <- c(rep("yes",38),rep("no",58),rep("yes",16),rep("no",6))
iayear <- as.factor(ayear)
data5 <- data.frame(ID,group,ayear)
table(mydata$group, mydata$ayear)
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$ayear, expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

###################################################################################
#######################Table Part II Cranial injury sites##########################
# group with Putamen(yes/no)
ID <- seq(1,114)
group <- c(rep("referral",92),rep("new",22))
group <- factor(group)
Putamen <- c(rep("yes",81),rep("no",11),rep("yes",18),rep("no",4))
iPutamen <- as.factor(Putamen)
data6 <- data.frame(ID,group,Putamen)
table(mydata$group, mydata$Putamen)
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$Putamen, expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with globus pallidus (yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'globus pallidus' <- c(rep("yes",38),rep("no",58),rep("yes",10),rep("no",12))
'iglobus pallidus' <- as.factor('globus')
data7 <- data.frame(ID,group,'globus')
table(mydata$group, mydata$'globus')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'globus', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with caudate nucleus(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'caudate nucleus' <- c(rep("yes",26),rep("no",70),rep("yes",8),rep("no",14))
'icaudate nucleus' <- as.factor('caudate')
data8 <- data.frame(ID,group,'caudate')
table(mydata$group, mydata$'caudate')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'caudate', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with Inner capsule(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'Inner capsule' <- c(rep("yes",6),rep("no",90),rep("yes",1),rep("no",21))
'iInner capsule' <- as.factor('Inner')
data9 <- data.frame(ID,group,'Inner')
table(mydata$group, mydata$'Inner')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'Inner', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with thalamus(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'thalamus' <- c(rep("yes",39),rep("no",57),rep("yes",11),rep("no",11))
'ithalamus' <- as.factor('thalamus')
data10 <- data.frame(ID,group,'thalamus')
table(mydata$group, mydata$'thalamus')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'thalamus', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with midbrain(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'midbrain' <- c(rep("yes",64),rep("no",32),rep("yes",17),rep("no",5))
'imidbrain' <- as.factor('midbrain')
data11 <- data.frame(ID,group,'midbrain')
table(mydata$group, mydata$'midbrain')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'midbrain', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with Pons(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'Pons' <- c(rep("yes",51),rep("no",45),rep("yes",16),rep("no",6))
'iPons' <- as.factor('Pons')
data12 <- data.frame(ID,group,'Pons')
table(mydata$group, mydata$'Pons')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'Pons', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with cerebellum(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'cerebellum' <- c(rep("yes",38),rep("no",58),rep("yes",10),rep("no",12))
'icerebellum' <- as.factor('cerebellum')
data13 <- data.frame(ID,group,'cerebellum')
table(mydata$group, mydata$'cerebellum')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'cerebellum', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with medulla(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'medulla' <- c(rep("yes",1),rep("no",95),rep("yes",0),rep("no",22))
'imedulla' <- as.factor('medulla')
data14 <- data.frame(ID,group,'medulla')
table(mydata$group, mydata$'medulla')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'medulla', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with cortex(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'cortex' <- c(rep("yes",17),rep("no",79),rep("yes",5),rep("no",17))
'icortex' <- as.factor('cortex')
data15 <- data.frame(ID,group,'cortex')
table(mydata$group, mydata$'cortex')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'cortex', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with corpus callosum(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'corpus callosum' <- c(rep("yes",17),rep("no",79),rep("yes",2),rep("no",20))
'icorpus callosum' <- as.factor('corpus')
data1 <- data.frame(ID,group,'corpus')
table(mydata$group, mydata$'corpus')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'corpus', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

# group with Brain atrophy(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)
'Brain atrophy' <- c(rep("yes",86),rep("no",10),rep("yes",20),rep("no",2))
'iBrain atrophy' <- as.factor('atrophy')
data1 <- data.frame(ID,group,'atrophy')
table(mydata$group, mydata$'atrophy')
#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'atrophy', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")


#####################################################################
#######table Part III DWI hyper-intensity for injury sites###########
# 批量执行 group with DWI hyper-intensity(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)

# Putamen DWI hyper-intensity (yes/no)
PutamenDWI<- c(rep("yes",7),rep("no",89),rep("yes",6),rep("no",16))
iPutamenDWI <- as.factor('PutamenDWI')
data1 <- data.frame(ID,group,'PutamenDWI')
table(mydata$group, mydata$'PutamenDWI')

#globus pallidus DWI hyper-intensity (yes/no)
globuspallidusDWI <- c(rep("yes",1),rep("no",95),rep("yes",1),rep("no",21))
iglobuspallidusDWI <- as.factor('globusDWI')
data2 <- data.frame(ID,group,'globusDWI')
table(mydata$group, mydata$'globusDWI')

#caudate nucleus DWI hyper-intensity  (yes/no)
caudatenucleusDWI <- c(rep("yes",2),rep("no",94),rep("yes",3),rep("no",19))
icaudatenucleusDWI <- as.factor('caudateDWI')
data3 <- data.frame(ID,group,'caudateDWI')
table(mydata$group, mydata$'caudateDWI')

# Inner capsule DWI hyper-intensity (yes/no)
InnercapsuleDWI <- c(rep("yes",1),rep("no",95),rep("yes",1),rep("no",21))
iInnercapsuleDWI <- as.factor('InnerDWI')
data4 <- data.frame(ID,group,'InnerDWI')
table(mydata$group, mydata$'InnerDWI')

# Thalamus DWI hyper-intensity (yes/no)
ThalamusDWI <- c(rep("yes",5),rep("no",91),rep("yes",6),rep("no",16))
iInnercapsuleDWI <- as.factor('ThalamusDWI')
data5 <- data.frame(ID,group,'ThalamusDWI')
table(mydata$group, mydata$'ThalamusDWI')

# Midbrain DWI hyper-intensity (yes/no)
MidbrainDWI <- c(rep("yes",6),rep("no",90),rep("yes",6),rep("no",16))
iMidbrainDWI <- as.factor('MidbrainDWI')
data6 <- data.frame(ID,group,'MidbrainDWI')
table(mydata$group, mydata$'MidbrainDWI')

# Pons DWI hyper-intensity (yes/no)
PonsDWI <- c(rep("yes",2),rep("no",94),rep("yes",4),rep("no",18))
iPonsDWI <- as.factor('PonsDWI')
data7 <- data.frame(ID,group,'PonsDWI')
table(mydata$group, mydata$'PonsDWI')

# Cerebellum DWI hyper-intensity (yes/no)
CerebellumDWI <- c(rep("yes",2),rep("no",94),rep("yes",4),rep("no",18))
iCerebellumDWI <- as.factor('CerebellumDWI')
data8 <- data.frame(ID,group,'CerebellumDWI')
table(mydata$group, mydata$'CerebellumDWI')

# Medulla DWI hyper-intensity (yes/no)
MedullaDWI <- c(rep("yes",1),rep("no",95),rep("yes",0),rep("no",22))
iMedullaDWI <- as.factor('MedullaDWI')
data9 <- data.frame(ID,group,'MedullaDWI')
table(mydata$group, mydata$'MedullaDWI')

# Cortex DWI hyper-intensity (yes/no)
CortexDWI <- c(rep("yes",1),rep("no",95),rep("yes",1),rep("no",21))
iCortexDWI <- as.factor('CortexDWI')
data10 <- data.frame(ID,group,'CortexDWI')
table(mydata$group, mydata$'CortexDWI')

# corpus callosum DWI hyper-intensity (yes/no)
'corpus callosumDWI' <- c(rep("yes",23),rep("no",73),rep("yes",2),rep("no",20))
'icorpus callosumDWI' <- as.factor('corpusDWI')
data11 <- data.frame(ID,group,'corpusDWI')
table(mydata$group, mydata$'corpusDWI')

#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'PutamenDWI', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'globusDWI', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'caudateDWI', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'InnerDWI', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'ThalamusDWI', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'MidbrainDWI', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'PonsDWI', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'CerebellumDWI', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'MedullaDWI', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'CortexDWI', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'corpusDWI', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

##############################################################################################
### 3.2 Difference in symptoms between newly diagnosed patients and patients referred with WD#####
# group with symptoms(yes/no)
ID <- seq(1,118)
group <- c(rep("referral",96),rep("new",22))
group <- factor(group)

# dysarthria (yes/no)
'dysarthria'<- c(rep("yes",74),rep("no",22),rep("yes",17),rep("no",5))
'idysarthria' <- as.factor('dysarthria')
data1 <- data.frame(ID,group,'dysarthria')
table(mydata$group, mydata$'dysarthria')

#difficulty swallowing (yes/no)
'swallow'<- c(rep("yes",35),rep("no",61),rep("yes",9),rep("no",13))
'iswallow' <- as.factor('swallow')
data2 <- data.frame(ID,group,'swallow')
table(mydata$group, mydata$'swallow')

# tremor (yes/no)
'tremor'<- c(rep("yes",55),rep("no",41),rep("yes",12),rep("no",10))
'itremor' <- as.factor('tremor')
data3 <- data.frame(ID,group,'tremor')
table(mydata$group, mydata$'tremor')

# gait disturbance (yes/no)
'gait' <- c(rep("yes",43),rep("no",53),rep("yes",10),rep("no",12))
'igait' <- as.factor('gait')
data4 <- data.frame(ID,group,'gait')
table(mydata$group, mydata$'gait')

# choreoathetosis (yes/no)
'chore' <- c(rep("yes",2),rep("no",94),rep("yes",4),rep("no",18))
'ichore' <- as.factor('chore')
data5 <- data.frame(ID,group,'chore')
table(mydata$group, mydata$'chore')

# torsion spasms (yes/no)
'torsion' <- c(rep("yes",11),rep("no",85),rep("yes",7),rep("no",15))
'itorsion' <- as.factor('torsion')
data6 <- data.frame(ID,group,'torsion')
table(mydata$group, mydata$'torsion')

# salivation (yes/no)
'salivation' <- c(rep("yes",25),rep("no",71),rep("yes",10),rep("no",12))
'isalivation' <- as.factor('salivation')
data7 <- data.frame(ID,group,'salivation')
table(mydata$group, mydata$'salivation')

# dystonia (yes/no)
'dystonia' <- c(rep("yes",46),rep("no",50),rep("yes",10),rep("no",12))
'idystonia' <- as.factor('dystonia')
data8 <- data.frame(ID,group,'dystonia')
table(mydata$group, mydata$'dystonia')

# bradykinesia and rigidity (yes/no)
'rigidity' <- c(rep("yes",14),rep("no",82),rep("yes",2),rep("no",20))
'rigidity' <- as.factor('rigidity')
data9 <- data.frame(ID,group,'rigidity')
table(mydata$group, mydata$'rigidity')

# ataxia (yes/no)
'ataxia' <- c(rep("yes",30),rep("no",66),rep("yes",8),rep("no",14))
'iataxia' <- as.factor('ataxia')
data10 <- data.frame(ID,group,'ataxia')
table(mydata$group, mydata$'ataxia')

# mental disorders (yes/no)
'dismental' <- c(rep("yes",38),rep("no",58),rep("yes",8),rep("no",14))
'idismental' <- as.factor('dismental')
data11 <- data.frame(ID,group,'dismental')
table(mydata$group, mydata$'dismental')

# epilepsy (yes/no)
'epilepsy' <- c(rep("yes",4),rep("no",92),rep("yes",2),rep("no",20))
'iepilepsy' <- as.factor('epilepsy')
data12 <- data.frame(ID,group,'epilepsy')
table(mydata$group, mydata$'epilepsy')

#install.packages('gmodels')
library(gmodels)
CrossTable(mydata$group, mydata$'dysarthria', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'swallow', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'tremor', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'gait', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'chore', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'torsion', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'salivation', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'dystonia', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'rigidity', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'ataxia', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'dismental', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
CrossTable(mydata$group, mydata$'epilepsy', expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")


##############################################################################
#####Table2 Part I General information nomormal test##########################
#3.2 正态性检验+非参数检验 

getwd()
setwd("D:/R work")

##一、数据准备
#1.数据读取（读进来）
#install.packages("readr")
library(readr)
mydata <- read_csv("WD_data.csv")

#删除有缺失值的行
mydata<-na.omit(mydata)
View(mydata)

names(mydata)

#install.packages("rio")
#install_formats('arrow')
#install_formats('feather')
#install_formats('fst')
#install_formats('hexView')
#install_formats('pzfx')
#install_formats('readODS')
#install_formats('rmatio')
library(rio)
mydata$group<-factor(mydata$group, levels = c(0:1), 
                  labels = c("referral", "new"))
str(mydata)
###Age of onset###
shapiro.test(mydata$onsetage)
by(mydata$onsetage, mydata$group, shapiro.test) #这里就替换onsetage就行了
wilcox.test(onsetage ~ group, data = mydata) 
AA <- wilcox.test(onsetage ~ group, data = mydata) 
z <- qnorm(AA$p.value/2)

###UWDRS-I###
shapiro.test(mydata$UWDRSone)
by(mydata$UWDRSone, mydata$group, shapiro.test) #这里就替换onsetage就行了
wilcox.test(UWDRSone ~ group, data = mydata) 
AA <- wilcox.test(UWDRSone ~ group, data = mydata) 
z <- qnorm(AA$p.value/2)

###UWDRS-II###
shapiro.test(mydata$UWDRStwo)
by(mydata$UWDRStwo, mydata$group, shapiro.test) #这里就替换onsetage就行了
wilcox.test(UWDRStwo ~ group, data = mydata) 
AA <- wilcox.test(UWDRStwo ~ group, data = mydata) 
z <- qnorm(AA$p.value/2)

###UWDRS-III###
shapiro.test(mydata$UWDRSthree)
by(mydata$UWDRSthree, mydata$group, shapiro.test) #这里就替换onsetage就行了
wilcox.test(UWDRSthree ~ group, data = mydata) 
AA <- wilcox.test(UWDRSthree ~ group, data = mydata) 
z <- qnorm(AA$p.value/2)

###UWDRS###
shapiro.test(mydata$UWDRS)
by(mydata$UWDRS, mydata$group, shapiro.test) #这里就替换onsetage就行了
wilcox.test(UWDRS ~ group, data = mydata) 
AA <- wilcox.test(UWDRS ~ group, data = mydata) 
z <- qnorm(AA$p.value/2)

###Urine copper before treatment###
shapiro.test(mydata$UrineCu)
by(mydata$UrineCu, mydata$group, shapiro.test) #这里就替换onsetage就行了
wilcox.test(UrineCu ~ group, data = mydata) 
AA <- wilcox.test(UrineCu ~ group, data = mydata) 
z <- qnorm(AA$p.value/2)

###Serum copper###
shapiro.test(mydata$SerumCu)
by(mydata$SerumCu, mydata$group, shapiro.test) #这里就替换onsetage就行了
wilcox.test(SerumCu ~ group, data = mydata) 
AA <- wilcox.test(SerumCu ~ group, data = mydata) 
z <- qnorm(AA$p.value/2)

###ceruloplasmin###
shapiro.test(mydata$CP)
by(mydata$CP, mydata$group, shapiro.test) #这里就替换onsetage就行了
wilcox.test(CP ~ group, data = mydata) 
AA <- wilcox.test(CP ~ group, data = mydata) 
z <- qnorm(AA$p.value/2)

###Total score of imaging###
shapiro.test(mydata$MRscore)
by(mydata$MRscore, mydata$group, shapiro.test) #这里就替换onsetage就行了
wilcox.test(MRscore ~ group, data = mydata) 
AA <- wilcox.test(MRscore ~ group, data = mydata) 
z <- qnorm(AA$p.value/2)

###Highest urine copper###
shapiro.test(mydata$HighestUC)
by(mydata$HighestUC, mydata$group, shapiro.test) #这里就替换onsetage就行了
wilcox.test(HighestUC ~ group, data = mydata) 
AA <- wilcox.test(HighestUC ~ group, data = mydata) 
z <- qnorm(AA$p.value/2)

###################################################################################
########### 3.4 Correlations between the cranial injury site and symptoms##########
#fml<- as.formula(paste0('hypoglycemia==1~',paste0(uni_glm$characteristics[uni_glm$P<0.1],collapse = '+'))) #P<0.05也是可以的
#fml

fml<-as.formula(dismental == 1 ~ Putamen + globus + caudate + Inner + thalamus +
                midbrain + Pons + cerebellum + cortex + corpus + atrophy)
fml
#dysarthria/swallow/tremor/gait/chore/torsion/salivation/dystonia/rigidity/ataxia/dismental/epilepsy///==1


#enter
modelA<-glm(fml,data = mydata,family=binomial)

modelA             #只能拿到模型的系数
summary(modelA)    #可以拿到模型概要

#-backward
modelC<-step(modelA,direction ="backward")
summary(modelC)

modelD<-step(modelA,direction = "backward")
modelD
glm3<-summary(modelD)
glm3

#看模型的系数及95%CI
cbind(coef=coef(modelC),confint(modelC))
#看模型的OR及95%CI
exp(cbind(OR=coef(modelC),confint(modelC)))

glm3$coefficients

OR<-round(exp(glm3$coefficients[,1]),2)
OR

SE<-round(glm3$coefficients[,2],3)
CI2.5<-round(exp(coef(modelD)-1.96*SE),2)
CI97.5<-round(exp(coef(modelD)+1.96*SE),2)
CI<-paste0(CI2.5,'-',CI97.5)
B<-round(glm3$coefficients[,1],3)
Z<-round(glm3$coefficients[,3],3)
P<-round(glm3$coefficients[,4],3)

#制作数据框#'characteristics'=multiname,
mlogit<-data.frame( 
  'B'=B,
  'SE'=SE,
  'OR'=OR,
  'CI'=CI,
  'Z' =Z,
  'P'=P)[-1,]   #-1是指删除常数项
mlogit
View(mlogit)

##################################################################################################################
#3.5 Risk prediction of DWI hyperintensity at different sites for deterioration or not during treatment###########
#####Deterioration#####
fml<-as.formula(Deterioration == 1 ~ PutamenDWI + caudateDWI + ThalamusDWI+ PonsDWI +
                  CerebellumDWI + MidbrainDWI  + corpusDWI)
fml
#Deterioration/ayear/sixmoth//==1

#enter
modelA<-glm(fml,data = mydata,family=binomial)

modelA             #只能拿到模型的系数
summary(modelA)    #可以拿到模型概要

#-backward
modelC<-step(modelA,direction ="backward")
summary(modelC)

modelD<-step(modelA,direction = "backward")
modelD
glm3<-summary(modelD)
glm3

#看模型的系数及95%CI
cbind(coef=coef(modelC),confint(modelC))
#看模型的OR及95%CI
exp(cbind(OR=coef(modelC),confint(modelC)))

glm3$coefficients

OR<-round(exp(glm3$coefficients[,1]),2)
OR

SE<-round(glm3$coefficients[,2],3)
CI2.5<-round(exp(coef(modelD)-1.96*SE),2)
CI97.5<-round(exp(coef(modelD)+1.96*SE),2)
CI<-paste0(CI2.5,'-',CI97.5)
B<-round(glm3$coefficients[,1],3)
Z<-round(glm3$coefficients[,3],3)
P<-round(glm3$coefficients[,4],3)

#制作数据框#'characteristics'=multiname,
mlogit<-data.frame( 
  'B'=B,
  'SE'=SE,
  'OR'=OR,
  'CI'=CI,
  'Z' =Z,
  'P'=P)[-1,]   #-1是指删除常数项
mlogit
View(mlogit)

#####ayear###########################
fml<-as.formula(ayear == 0 ~ PutamenDWI + caudateDWI + ThalamusDWI+ PonsDWI +
                  CerebellumDWI + MidbrainDWI  + corpusDWI)
fml
#Deterioration/ayear/sixmoth//==1

#enter
modelA<-glm(fml,data = mydata,family=binomial)

modelA             #只能拿到模型的系数
summary(modelA)    #可以拿到模型概要

#-backward
modelC<-step(modelA,direction ="backward")
summary(modelC)

modelD<-step(modelA,direction = "backward")
modelD
glm3<-summary(modelD)
glm3

#看模型的系数及95%CI
cbind(coef=coef(modelC),confint(modelC))
#看模型的OR及95%CI
exp(cbind(OR=coef(modelC),confint(modelC)))

glm3$coefficients

OR<-round(exp(glm3$coefficients[,1]),2)
OR

SE<-round(glm3$coefficients[,2],3)
CI2.5<-round(exp(coef(modelD)-1.96*SE),2)
CI97.5<-round(exp(coef(modelD)+1.96*SE),2)
CI<-paste0(CI2.5,'-',CI97.5)
B<-round(glm3$coefficients[,1],3)
Z<-round(glm3$coefficients[,3],3)
P<-round(glm3$coefficients[,4],3)

#制作数据框#'characteristics'=multiname,
mlogit<-data.frame( 
  'B'=B,
  'SE'=SE,
  'OR'=OR,
  'CI'=CI,
  'Z' =Z,
  'P'=P)[-1,]   #-1是指删除常数项
mlogit
View(mlogit)

#####sixmoth#####
fml<-as.formula(sixmoth == 1 ~ PutamenDWI + caudateDWI + ThalamusDWI+ PonsDWI +
                  CerebellumDWI + MidbrainDWI  + corpusDWI)
fml
#Deterioration/ayear/sixmoth//==1

#enter
modelA<-glm(fml,data = mydata,family=binomial)

modelA             #只能拿到模型的系数
summary(modelA)    #可以拿到模型概要

#-backward
modelC<-step(modelA,direction ="backward")
summary(modelC)

modelD<-step(modelA,direction = "backward")
modelD
glm3<-summary(modelD)
glm3

#看模型的系数及95%CI
cbind(coef=coef(modelC),confint(modelC))
#看模型的OR及95%CI
exp(cbind(OR=coef(modelC),confint(modelC)))

glm3$coefficients

OR<-round(exp(glm3$coefficients[,1]),2)
OR

SE<-round(glm3$coefficients[,2],3)
CI2.5<-round(exp(coef(modelD)-1.96*SE),2)
CI97.5<-round(exp(coef(modelD)+1.96*SE),2)
CI<-paste0(CI2.5,'-',CI97.5)
B<-round(glm3$coefficients[,1],3)
Z<-round(glm3$coefficients[,3],3)
P<-round(glm3$coefficients[,4],3)

#制作数据框#'characteristics'=multiname,
mlogit<-data.frame( 
  'B'=B,
  'SE'=SE,
  'OR'=OR,
  'CI'=CI,
  'Z' =Z,
  'P'=P)[-1,]   #-1是指删除常数项
mlogit
View(mlogit)

##########################################################################################
##### 3.6 Optimization of the semi-quantitative scoring system for cranial MR in WD#######

#设置工作路径
getwd()
setwd("D:/R work")

#install.packages("readr")
library(readr)
mydata <- read_csv("WD_data.csv")

mydata<-na.omit(mydata)
View(mydata)
names(mydata)

# Shapiro-Wilk normality test for mpg
shapiro.test(mydata$MRscore) # => p = 0.1229
shapiro.test(mydata$NMRscore) # => p = 0.1229
# Shapiro-Wilk normality test for wt
shapiro.test(mydata$UWDRS) # => p = 0.09
shapiro.test(mydata$SerumCu) # => p = 0.09
shapiro.test(mydata$CP)


#pearson
res1 <-cor.test(mydata$MRscore, mydata$UWDRS,  method = "pearson")
res1
res2 <-cor.test(mydata$MRscore, mydata$SerumCu,  method = "pearson")
res2
res3 <-cor.test(mydata$MRscore, mydata$CP,  method = "pearson")
res3
res5 <-cor.test(mydata$MRscore, mydata$adl,  method = "pearson")
res5

res6 <-cor.test(mydata$NMRscore, mydata$UWDRS,  method = "pearson")
res6
res7 <-cor.test(mydata$NMRscore, mydata$SerumCu,  method = "pearson")
res7
res8 <-cor.test(mydata$NMRscore, mydata$CP,  method = "pearson")
res8
res10 <-cor.test(mydata$NMRscore, mydata$adl,  method = "pearson")
res10

#设置工作路径
getwd()
setwd("D:/R work")

#install.packages("readr")
library(readr)
mydata <- read_csv("WD_data.csv")

mydata<-na.omit(mydata)
View(mydata)
names(mydata)

shapiro.test(mydata$UrineCu)
res4 <-cor.test(mydata$MRscore, mydata$UrineCu,  method = "pearson")
res4
res9 <-cor.test(mydata$NMRscore, mydata$UrineCu,  method = "pearson")
res9

###########3.6 Optimization of the semi-quantitative scoring system for cranial MR in WD#######
#设置工作路径
getwd()
setwd("D:/R work")

#install.packages("readr")
library(readr)
mydata <- read_csv("WD_data.csv")

mydata<-na.omit(mydata)
View(mydata)
names(mydata)

uni_glm_model<-function(x){
  FML<-as.formula(paste0("sixmoth==1~",x))  #Deterioration/ayear/sixmoth/==1，您以后就改绿色的变量即可
  glm1<-glm(FML,data = mydata,family = binomial)
  glm2<-summary(glm1)
  
  #计算我们所要的指标
  OR<-round(exp(coef(glm1)),5)
  SE<-round(glm2$coefficients[,2],5)  
  CI2.5<-round(exp(coef(glm1)-1.96*SE),5)
  CI97.5<-round(exp(coef(glm1)+1.96*SE),5)
  CI<-paste0(CI2.5,'-',CI97.5)
  B<-round(glm2$coefficients[,1],3)
  Z<-round(glm2$coefficients[,3],3)
  P<-round(glm2$coefficients[,4],3)
  
  #将计算出来的指标制作为数据框
  uni_glm_model<-data.frame('characteristics'=x,
                            'B'=B,
                            'SE'=SE,
                            'OR'=OR,
                            'CI'=CI,
                            'Z' =Z,
                            'P'=P)[-1,]
  
  return(uni_glm_model)
}

#指定参与分析的若干自变量X
variable.names<-colnames(mydata)[c(11:12)]  #要核实这里的X对应的列是否对？若分开的可以这样[c(3:18,20:40)]
variable.names
#运行上面自定义批量执行函数
uni_glm<-lapply(variable.names,uni_glm_model)
uni_glm
#install.packages("plyr")
library(plyr)
#生成单变量分析的综合结果
uni_glm<-ldply(uni_glm,data.frame)
#看下结果是啥样子的
uni_glm
View(uni_glm)













