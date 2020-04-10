#read.xls("E://Hanika//Excelr//Assignment//toyata coralla.xlxs")
library(xlsx)
library(readxl)

toyata_coralla<-read.csv(file.choose())
summary (toyata_coralla)
View(toyata_coralla)
colnames (toyata_coralla)
cor(toyata_coralla)
summary(toyata_coralla)
#remove column door
toyata <- toyata_coralla[,(-6)] 
attach(toyata)

#Correlation Matrix: 
cor (toyata)
library(corpcor)
cor2pcor(cor(toyata))

# scater plot
pairs(toyata)

#Regression Model and Summary
carprice <- lm(Price ~ .,data=toyata)
summary (carprice)

#lets find out individual impact 
cor(Age_08_04,Price)
agelm <-lm(Price~Age_08_04)
summary(agelm)

cor(KM,Price)
kmlm <-lm(Price~KM)
summary(kmlm)

cor(HP,Price)
hplm <- lm(Price~HP)
summary(hplm)

cor(cc,Price)
cclm<- lm(Price~cc)
summary(cclm)

cor(Gears,Price)
gearlm <-lm(Price~Gears)
summary(gearlm)

cor(Quarterly_Tax,Price)
taxlm <-lm(Price~Quarterly_Tax)
summary(taxlm)

cor(Weight,Price)
weightlm <-lm(Price~Weight)
summary(weightlm)

#LM Model based on removing variable one bye one
#remove cc
removecclm <-lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+
               Weight)
summary(removecclm)
mean(removecclm$residuals)
sqrt(mean(removecclm$residuals^2))
#remove gears
removegearslm <-lm(Price~Age_08_04+KM+HP+Quarterly_Tax+
                  Weight)
summary(removegearslm)
mean(removegearslm$residuals)
sqrt(mean(removegearslm$residuals^2))
#after removing gears price significance is reduced
#remove tax and add gears
removetaxlm <-lm(Price~Age_08_04+KM+HP+Gears+
                  Weight)
summary(removetaxlm)
mean(removetaxlm$residuals)
sqrt(mean(removetaxlm$residuals^2))

#Verify VIF
car::vif(carprice)
mean(carprice$residuals)
sqrt(mean(carprice$residuals^2))
summary(carprice$residuals)
#Diagnostic Plots:   #Residual Plots, QQ-Plos, Std. Residuals vs Fitted 

plot(carprice)  


residuals(carprice)

#Residuals vs Regressors residualPlots
residualPlots(carprice,tests=F) 

#Added Variable Plots 
library(car)
library(carData)
car::avPlots(carprice) 

#QQ plots of studentized residuals 

qqPlot(carprice) 


influence.measures(carprice)

car::influenceIndexPlot(carprice)

#Deletion Diagnostics 

carprice <- lm(Price ~ .,data=toyata[-c(222),])
summary (carprice)

carprice <- lm(Price ~ .,data=toyata[-c(81),])
summary (carprice)

carprice <- lm(Price ~ .,data=toyata[-c(602),])
summary (carprice)
carprice$residuals

mean(carprice$residuals)
sqrt(mean(carprice$residuals^2))  # RMSE

predictprice <-predict(carprice)
toyata <-cbind(toyata[-c(222,81,602)],predictprice)
View(toyata)
