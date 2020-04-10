############## 1 sample z test ##########
Cutlets= read.csv(file.choose())
Cutlets
##### Normality Test##################
library(nortest)
ad.test(Cutlets$Unit.A)      ###Anderson-Darling test
ad.test(Cutlets$Unit.B)
########### 1 sample z-test ##########
install.packages("TeachingDemos")
library(TeachingDemos)
z.test(Cutlets$Unit.A, mu = 0, stdev = 4, n=25, alternative = "two.sided", conf.level = 0.95)
z.test(Cutlets$Unit.B, mu = 0, stdev = 4, n=25, alternative ="greater", conf.level = 0.95 )
######## Mann-Whitney Test ########
wilcox.test(Cutlets$Unit.A,Cutlets$Unit.B,mu=0,alt="two.sided",conf.int=T,conf.level=0.95,paired=F,exact=T,correct=T)

