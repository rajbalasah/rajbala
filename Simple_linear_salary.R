
salary=read.csv("E://Hanika//Excelr//Assignment//Simple Linear Regression//Salary_Data.csv")
View(salary)

##EDA
summary(salary)
plot(salary)
boxplot(salary$YearsExperience,ylab="years")
        boxplot(salary$Salary,ylab="salary")
#hist(salary)
 ###Verify correlation among two
cor(salary)
##Positve correlation

colnames(salary)<-c("years","sal")
attach(salary)
###Verify linear regrission
salarylm=lm(sal~years)
abline(salarylm)
summary(salarylm)

sum(salarylm$residuals)
mean(salarylm$residuals)
sqrt(mean(salarylm$residuals)^2)
###predict 

pred=predict(salarylm)

##add predict column to table
pred_add <- cbind(salary,pred)
View(pred_add)
