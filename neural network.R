# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# apply normalization to entire data frame
concrete<-read.csv("C:/Users/Nik/Downloads/concrete.csv")
concrete_norm <- as.data.frame(lapply(concrete, normalize))
# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]
## Training a model on the data ----
# train the neuralnet model
library(neuralnet)
# simple ANN with only a single hidden neuron
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = concrete_train)
# visualize the network topology
windows()
plot(concrete_model)
## Evaluating model performance ----
# obtain model results
model_results <- compute(concrete_model, concrete_test[1:8])
# obtain predicted strength values
predicted_strength <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)
## Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
concrete_model2 <- neuralnet(strength ~ cement + slag +
ash + water + superplastic + coarseagg + fineagg + age,data = concrete_train,concrete_model2 <- neuralnet(strength ~ cement + slag +
ash + water + superplastic + coarseagg + fineagg + age,data = concrete_train, hidden =c(5,2)))
# plot the network
windows()
plot(concrete_model2)                            # evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
                             

#######svm
#letterdata<-read.csv("/Volumes/Data/Course Content/DS content/SVM and NN/letterdata.csv")
# divide into training and test data
letterdata<-letterdata
letters_train <- letterdata[1:16000, ]
letters_test  <- letterdata[16001:20000, ]
##Training a model on the data ----
# begin by training a simple linear SVM
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")
## Evaluating model performance ----
# predictions on testing dataset
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)
#table(letter_predictions, letters_test$letter)
agreement <- letter_predictions == letters_test$letter
prop.table(table(agreement))
## Improving model performance ----
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)
head(letter_predictions_rbf)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

