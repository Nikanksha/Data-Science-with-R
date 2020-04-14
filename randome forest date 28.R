install.packages("randomForest")
library(randomForest)


model<-randomForest(iris$Species~.,data=iris,ntree=1000)

# View the forest results.
print(model)
#Imoporantce of the variable - Lower Gini
print(importance(model))
#Prediction
pred<- predict(model,iris[,-5])
table(pred,iris$Species)


#install.packages("randomForest")
#library(randomForest)


#Problem Statement:
#A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 
#Approach - A Random Forest can be built with target variable Sales (we will first convert it in categorical variable) & all other variable will be independent in the analysis.  

model2<-randomForest(Company_Data$Sales~.,data = Company_Data,ntree=1000)

# View the forest results.
print(model2)
#Imoporantce of the variable - Lower Gini
print(importance(model2))
#Prediction
pred<- predict(model2,Company_Data[,-1])
table(pred,Company_Data$Sales)

#Use Random Forest to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others 
#are "Good"

model1<-randomForest(Fraud_check$Taxable.Income~.,data = Fraud_check,ntree=1000)

# View the forest results.
print(model1)
#Imoporantce of the variable - Lower Gini
print(importance(model1))
#Prediction
pred<- predict(model1,Fraud_check[,-3])
table(pred,Fraud_check$Taxable.Income)

