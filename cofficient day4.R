
dotchart(WC_AT$Waist,labels = row.names(WC_AT$Waist))
dotchart(WC_AT$AT,labels = row.names(WC_AT$AT))
attach(WC_AT)
boxplot(AT,col = "dodgerblue4")
boxplot(Waist,col="blue")

plot(Waist,AT,main="scatter plot",
     col="blue",
     col.main="blue",
     col.lab="blue",
    xlab ="Daily circulations",
    ylab ="sunday circulations",pch=20)
    


    
    
reg.model<-lm(AT~Waist,WC_AT)
summary(reg.model)

##plotting best fit line
plot(Waist,AT,main="line of best fit",col="dodgerblue4",col.main="dodgerblue4")
abline(reg.model,col="red")
segments(Waist,AT,Waist,predict(reg.model))
predict(reg.model,newdata = data.frame(Waist=c(50,200)))

min(WC_AT$Waist)
max(WC_AT$Waist)


#multiplae linear regration model
#scatter plot matrix
pairs(Cars1)
#correlation matrix
cor(Cars1)
#regration model and mummary
model.car<-lm(MPG~.,data=Cars1)
summary(model.car)

###### experiment###########
reg_vol<-lm(MPG~VOL,data = Cars1)
summary(reg_vol)
reg_wt<-lm(MPG~WT,data = Cars1)
summary(reg_wt)
reg_wt_vol<-lm(MPG~WT+VOL,data = Cars1)
summary(reg_wt_vol)

#regration model and summary
model.car<-lm(MPG~.,data = Cars)
summary(model.car)

#multi_colinearity
install.packages("car")
library(car)
car::vif(model.car)

#subset selection
library(MASS)
stepAIC(model.car)

#model building
#regration model and summary
model.car <- lm(MPG~.,data = Cars)
summary(model.car)
#dignostic plots:
#resedual plots qq plot
plot(model.car)
#residual vs regressor
library(car)

residualPlots(model.car)
#added variable plots
avPlots(model.car)
#QQ plots of studentized residual
qqPlot(model.car)
#deletion diagnostics
influenceIndexPlot(model.car)#index plot of influncer measures
### iterations 1
#remove 77th observation
cars1<-Cars[-77,]
model1<-lm(cars1$MPG~.,data = cars1)
car::vif(model1)
#resedual plots qq plot
plot(model1)


residualPlots(model1)
#added variable plots
avPlots(model1)
#QQ plots of studentized residual
qqPlot(model1)
#deletion diagnostics
influenceIndexPlot(model1)


#Iteration 2
cars2<-Cars[-c(77,79),]
model2<-lm(cars2$MPG~.,data = cars2)
plot(model2)
#resedual plots qq plot
residualPlots(model2)
#added variable plots
avPlots(model2)
#QQ plots of studentized residual
qqPlot(model2)
#deletion diagnostics
influenceIndexPlot(model2)



##A large Toyota car dealership offers purchasers of new Toyota cars the 	option to buy their used car as part of trade-in. In particular, a new 	promotion offers to pay high prices for used Toyota cars for purchasers of a 	new car. The dealer then sells for a small profit. To ensure a reasonable 	profit, the dealer needs to be able to predict the price that the dealership 	will get for used cars. 

##For this reason, data were collected on all previous sales of used Toyota 	cars at the dealership. We have a dataset with 1000 cars.
#Model Building
#Regression Model and Summary
model.car<-lm(MPG~.,data = Toyoto_Corrola)
summary(model.car)
#Diagnostic Plots:
#Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(model.car) 
#Residuals vs Regressors
library(Toyoto_Corrola)
residualPlots(model.car)
#Added Variable Plots
avPlots(model.car)
#QQ plots of studentized residuals
qqPlot(model.car)
#Deletion Diagnostics
influenceIndexPlot(model.car) # Index Plots of the influence measures
####Iteration 1 
#Remove 77th observation
Cars1<-Toyoto_Corrola[,-c(1,2,8)]
model1<-lm(Cars1$Price~.,data = Cars1)
summary(model1)
car::vif(model1)

plot(model1) 
#Residuals vs Regressors
#library(Toyoto_Corrola)
residualPlots(model1)
#Added Variable Plots
avPlots(model1)
#QQ plots of studentized residuals
qqPlot(model1)
#Deletion Diagnostics
influenceIndexPlot(model1) # Index Plots of the influence measures


####Iteration 1 
cars2<-Cars1[-222,]
model3<-lm(cars2$Price~.,data = cars2)
summary(model3)
car::vif(model3)
plot(model3) 
#Residuals vs Regressors
residualPlots(model3)
#Added Variable Plots
avPlots(model3)
#QQ plots of studentized residuals
qqPlot(model3)
#Deletion Diagnostics
influenceIndexPlot(model3)
#iteration 2
cars3<-Cars1[-c(222,961),]
model4<-lm(cars3$Price~.,data = cars3)
summary(model4)
car::vif(model4)
plot(model4) 
#Residuals vs Regressors
residualPlots(model4)
#Added Variable Plots
avPlots(model4)
#QQ plots of studentized residuals
qqPlot(model4)
#Deletion Diagnostics
influenceIndexPlot(model4)

cars4<-Cars1[-c(222,961,602,957,992),]
model5<-lm(cars4$Price~.,data = cars4)
summary(model5)
