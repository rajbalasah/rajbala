install.packages("varhandle","dummies")
computer <-read.csv(file.choose())
View(computer)
str(computer)
#BAsed on data 3 column data is categorical
library(varhandle,dummies)
library(dummy)
attach(computer)
unique(cd)
unique(multi)
unique(premium)
#Convert cd,mulit,premium to dummy
#cd_dummy <-to.dummy(cd,"cd")
c_dummy <-dummy.data.frame(computer,names=c("cd","multi","premium"),sep='')
colnames(c_dummy)
colnames(computer)

#computer_c<-cbind(computer,c_dummy[,-1])
#head(computer_c)
#View(computer_c)
#str(computer_c)

computer_com<-cbind(c_dummy[,-14:-21])
str(computer_com)
# Verify corelation and scatter plot

head(computer_com)
str(computer_com)
cor(computer_com)
pairs(computer_com)
plot(computer_com)

#build model with all variable
model1 <-lm(price~.,data=computer_com)
summary(model1)
#Verify relation of individual variable with price

speedlm <-lm(price~speed,data=computer_com)
summary(speedlm)
hdlm <-lm(price~hd,data=computer_com)
summary(hdlm)
ramlm <-lm(price~ram,data=computer_com)
summary(ramlm)
screenlm <-lm(price~screen,data=computer_com)
summary(screenlm)
multinolm <-lm(price~multino,data=computer_com)
summary(multinolm)
multiyeslm <-lm(price~multiyes,data=computer_com)
summary(multiyeslm)
premiumyeslm <-lm(price~premiumyes,data=computer_com)
summary(premiumyeslm)
premiumnolm <-lm(price~premiumno,data=computer_com)
summary(premiumnolm)
adslm <-lm(price~ads,data=computer_com)
summary(adslm)
trendlm <-lm(price~trend,data=computer_com)
summary(trendlm)
cdyeslm <-lm(price~cd.yes,data=computer_com)
summary(cdyeslm)
cdnolm <-lm(price~cd.no,data=computer_com)
summary(cdnolm)

#removing variable which have less impact as per r2 of individual contribution
#remvoing multino
model2 <-lm(price ~ hd+speed+ram+screen+multiyes+premiumyes+premiumno+ads+trend+cdno+cdyes,data=computer_com)
summary(model2)
#removing premiumno
model3 <-lm(price ~ hd+speed+ram+screen+multiyes+premiumyes++ads+trend+cdno+cdyes,data=computer_com)
summary(model3)
#corelation&RMSE
predict_com_price <-predict(model3)
sum(model3$residuals)
mean(model3$residuals)
sqrt(mean(model3$residuals^2))
cor(computer_com$price,predict_com_price)#actual&predictedprice

#Addinglog
model4<-lm(price~log(hd)+log(speed)+log(ram)+screen+multiyes+premiumyes++ads+trend+cdno+cdyes,data=computer_com)
summary(model4)
#r2value  reduce
#corelation&RMSE
predict_log_price <-predict(model4)
sum(model4$residuals)
mean(model4$residuals)
sqrt(mean(model4$residuals^2))
cor(computer_com$price,predict_log_price)#actual&predictedprice

#VIF&outlier
#Diagnostic Plots:   #Residual Plots, QQ-Plos, Std. Residuals vs Fitted 
library(car)
influenceIndexPlot(model3)
plot(model3)
#Asperplot  1441&1701 observation as  outlier
cmd_del<-lm(price~.,data=computer_com[-c(1441,1701)])
summary(cmd_del)

#remove variable  with  no value
model5 <-lm(price ~ hd+speed+ram+screen+multiyes+premiumyes++ads+trend+cdyes,data=computer_com[-c(1441,1701)])
summary(model5)
model6 <-lm(price ~ hd+speed+ram+screen+ads+trend+cdyes,data=computer_com)
summary(model6)
#r2valueis  descreasing
#predicate  value with  model5
#car::vif(model3)
predictprice<-predict(model5)
plot(computer_com$price,predictprice)

abline(computer_com$price,predictprice)
computer_price<-cbind(computer_com,predictprice)
View(computer_price)
