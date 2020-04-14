data("iris")
install.packages("caret")
install.packages("C50")
library(caret)
library(C50)
inTraininglocal<-createDataPartition(iris$Species,p=.70,list = F)
training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]
#model building
model<-C5.0(training$Species~.,data=training)


#generate the model summary
summary(model)
#predict for test data
pred<-predict.C5.0(model,testing[,-5])
a<-table(testing$Species,pred)
sum(diag(a))/sum(a)



#####bagging 

acc<-c()
for (i in 1:100)
  {
print(i)
#data partition
inTraininglocal<-createDataPartition(iris$Species,p=.70,list = F)
training1<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]

#model building
fittree<-C5.0(training1$Species~.,data=training1)

#predict for test data
pred<-predict.C5.0(fittree,testing[,-5])
a<-table(testing$Species,pred)

#Accuracy
acc<-c(acc,sum(diag(a))/sum(a))
}
#generate the model summary
summary(acc)
boxplot(acc)

#boosting Model
inTraininglocal<-createDataPartition(iris$Species,p=.70,list = F)
training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]
#model building
model<-C5.0(training$Species~.,data=training,trials=25)


#generate the model summary
summary(model)
#predict for test data
pred<-predict.C5.0(model,testing[,-5])
a<-table(testing$Species,pred)
sum(diag(a))/sum(a)


#Bagging and bosting

acc<-c()
for (i in 1:100)
{
  print(i)
  #data partition
  inTraininglocal<-createDataPartition(iris$Species,p=.70,list = F)
  training1<-iris[inTraininglocal,]
  testing<-iris[-inTraininglocal,]
  
  #model building
  fittree<-C5.0(training1$Species~.,data=training1,trials=20)
  
  #predict for test data
  pred<-predict.C5.0(fittree,testing[,-5])
  a<-table(testing$Species,pred)
  
  #Accuracy
  acc<-c(acc,sum(diag(a))/sum(a))
}
#generate the model summary
summary(acc)
boxplot(acc)


#stacking
wbcd<-read.csv("/Volumes/Data/Course Content/DS content/NB and Knn/KNN.csv")
wbcd<-wbcd[,-1]
# table of diagnosis
table(wbcd$diagnosis)
# table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
 # normalize the wbcd data
  wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_nl<-cbind(wbcd_n,wbcd$diagnosis)
# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
# create labels for training and test data
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
#---- Training a model on the data ----
# load the "class" library
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=20)
 ##--------Evaluating model performance ----
# load the "gmodels" library
library(gmodels)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)
#############################################################
## Improving model performance ----
# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))
# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)
# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

