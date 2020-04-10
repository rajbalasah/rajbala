
delivery=read.csv("E://Rajbala//Excelr//Assignment//Simple Linear Regression//delivery_time.csv")
View(delivery)

##EDA
summary(delivery)
plot(delivery)
boxplot(delivery)
###Verify correlation among two
cor(delivery)

colnames(delivery)<-c("dtime","sorting")
attach(delivery)
###Verify linear regrission
deliverylm=lm(dtime~sorting)
summary(deliverylm)
plot(dtime~sorting,main="time")
abline(deliverylm,col="red")

par(mfrow=c(2,2))
plot(deliverylm)

# Logrithamic Model

# x = log(sortig); y = delivery

plot(log(sorting), dtime)
cor(log(sorting), dtime)

reg_log <- lm(dtime ~ log(sorting))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

mean(reg_log$residuals)
mean(reg_log)
sqrt(sum(reg_log$residuals^2)/nrow(delivery))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = sorting and y = log(dtime)

plot(sorting, log(dtime))

cor(sorting, log(dtime))

reg_exp <- lm(log(dtime) ~ sorting)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

lograte <- predict(reg_exp)
at <- exp(lograte)
# Poly Model  with  2 degree

# x = sorting and y = (dtime)

plot(sorting*sorting, (dtime))

cor(sorting*sorting, (dtime))

reg_poly <- lm(dtime ~ sorting*sorting)  #lm((Y) ~ X)

summary(reg_poly)

mean(reg_poly$residuals)
mean(reg_log)
sqrt(sum(reg_log$residuals^2)/nrow(delivery))  #RMSE

###predict 

pred=predict(reg_exp)

##add predict column to table
pred_add_delivery <- cbind(delivery,pred)
View(pred_add_delivery)
