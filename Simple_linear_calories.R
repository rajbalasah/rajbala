Install(corrplot,moments)
library(corrplot)
library(moments)
calories_w <-read.csv(file.choose())
#calories_w <-read.csv("E://Rajbala//Excelr//Assignment//Simple Linear Regression//calories_consumed.csv")
View(calories_w)
attach(calories_w)
colnames(calories_w) <- c("Calories","Weight")

#EDA
#Scatter plot
plot(calories_w)
boxplot(calories_w)
summary(calories_w)

var(calories_w)
skewness(calories_w)
kurtosis(calories_w)
###Verify correlation among two

colnames(calories_w)
cor(calories_w$Calories,calories_w$Weight)
#corrplot(cor,type="lower")
###Verify linear regrission
cal <- lm(calories_w$Weight~calories_w$Calories)
summary(cal)
#(bestfitline)
plot(calories_w)
abline(cal,col="red")
###predict 
confint(cal,level=0.95)
pred=predict(cal)

##add predict column to table
pred_add <- cbind(calories_w,pred)
View(pred_add)

cal$residuals
summary(cal$residuals)
plot(calories_w$Weight,type="l",lty=1.8,col="green")
lines(pred,type="l",col="blue")

# Logrithamic Model

# x = log(calories); y = Weight

plot(log(Calories), Weight)
cor(log(Calories), Weight)

reg_log <- lm(Weight~ log(Calories))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(calories_w))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = calories and y = log(Weight)

plot(Calories, log(Weight))

cor(Calories, log(Weight))

reg_exp <- lm(log(Weight) ~ Calories)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))
