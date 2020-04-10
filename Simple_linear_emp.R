
emp=read.csv("E://Rajbala//Excelr//Assignment//Simple Linear Regression//emp_Data.csv")
View(emp)

##EDA
summary(emp)
plot(emp)
  boxplot(emp$Salary_hike,ylab="salary")
boxplot(emp$Churn_out_rate,ylab="Churn_out_rate")

###Verify correlation among two
cor(emp)

colnames(emp)<-c("hike","rate")
attach(emp)
###Verify linear regrission
emplm=lm(rate~hike)
summary(emplm)

###predict 

pred=predict(emplm)

##add predict column to table
pred_add <- cbind(emp,pred)
View(pred_add)
sum(emplm$residuals)

mean(emplm$residuals)

sqrt(mean(salarylm$residuals)^2)
# Logrithamic Model

# x = log(hike); y = Rate

plot(log(hike), rate)
cor(log(hike), rate)

reg_log <- lm(rate ~ log(hike))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(emp))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = hike and y = log(rate)

plot(hike, log(rate))

cor(hike, log(rate))

reg_exp <- lm(log(rate) ~ hike)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

lograte <- predict(reg_exp)
at <- exp(lograte)

error = emp$rate - at
error

sqrt(sum(error^2)/nrow(wc_at))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")
pred<-predict(reg_exp)
data_pred<- cbind(emp,pred)
View(data_pred)
