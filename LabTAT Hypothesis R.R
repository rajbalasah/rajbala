############## 1 sample z test ##########
LabTAT=read.csv(file.choose())
LabTAT
##### Normality Test##################
library(nortest)
ad.test(LabTAT$Laboratory.1)      ###Anderson-Darling test
ad.test(LabTAT$Laboratory.2)
ad.test(LabTAT$Laboratory.3)
ad.test(LabTAT$Laboratory.4)

########### 1 sample t-test ##########
t.test(LabTAT$Laboratory.1, mu = 0, alternative = "two.sided")
t.test(LabTAT$Laboratory.2, mu = 0, alternative = "two.sided")
t.test(LabTAT$Laboratory.3, mu = 0, alternative = "two.sided")
t.test(LabTAT$Laboratory.4, mu = 0, alternative = "two.sided")
############2 sample T Test ##################
LabTAT<- read_excel(file.choose())    # LabTAT.xlsx
attach(LabTAT)
colnames(LabTAT)<-c("Laboratory.1","Laboratory.2","Laboratory.3","Laboratory.4")
# Changing column names
View(LabTAT)
attach(LabTAT)

#############Variance test###############

var.test(Laboratory.3,Laboratory.4)#variance test

############2 sample T Test ##################

t.test(Laboratory.3,Laboratory.4,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
?t.test
t.test(Laboratory.3,Laboratory.4,alternative = "greater",var.equal = T)

########## Mann-Whitney Test ########
wilcox.test(LabTAT$Laboratory.1,LabTAT$Laboratory.2,LabTAT$Laboratory.3,LabTAT$Laboratory.4,mu=0,alt="two.sided",conf.int=T,conf.level=0.95,paired=T,exact=F,correct=T)
