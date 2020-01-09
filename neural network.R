library(readr)
Insurance_Dataset_ <- read_csv("D:/new/Insurance Dataset .csv")
#View(Insurance_Dataset_)
Insurance_Dataset_ <- Insurance_Dataset_[,-c(1:5,7,11,14:19,21,26:27,31:32)]
sum(is.na(Insurance_Dataset_))
summary(Insurance_Dataset_)
library(Hmisc)
Insurance_Dataset_["Description_illness"] <- with(Insurance_Dataset_,impute(Insurance_Dataset_$Description_illness,mode))
Insurance_Dataset_["Mortality_risk"] <- with(Insurance_Dataset_,impute(Insurance_Dataset_$Mortality_risk,mode))
data_factor <- as.data.frame(lapply(Insurance_Dataset_[,-c(12,15:16)],factor))
str(data_factor)
library(correlationfunnel)
data_factor <- binarize(data_factor)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
data_norm<-as.data.frame(lapply(Insurance_Dataset_[,c(12,15)],FUN=normalize))


final_data <- data.frame(Insurance_Dataset_[,16],data_factor,data_norm)
final_data <- final_data[1:500,]
sum(is.na(final_data))

set.seed(3)
final_data_1<- final_data[sample(nrow(final_data)),]
train <- final_data_1[1:as.integer(0.70*nrow(final_data)),]
test <- final_data_1[-c(1:as.integer(0.70*nrow(final_data))),]

library(neuralnet)

# Building model
formula_nn <- paste("Result",paste(colnames(final_data[,-1]),collapse ="+"),sep="~")
insurance_model <- neuralnet(formula_nn,data=train)
str(insurance_model)
plot(insurance_model)
# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared


training_results <- predict(insurance_model,train[,-1]) #we use predict instead of compute
library(dplyr)
predict_train <- ifelse(training_results[,1] > training_results[,2],"Fraudulent","Genuine")
# model_results$neurons
mean(predict_train==train$Result) #Training Accuracy = 75% 
library(caret)
confusionMatrix(predict_train,train$Result)

testing_results <- predict(insurance_model,test[,-1]) #we use predict instead of compute
predict_test <- ifelse(testing_results[,1] > testing_results[,2],"Fraudulent","Genuine")
# model_results$neurons
mean(predict_test==test$Result) #Testing Accuracy = 74.98% 
library(caret)
confusionMatrix(predict_test,test$Result)


insurance_model_5 <- neuralnet(formula_nn,data=train,hidden = 5)
str(insurance_model_5)
plot(insurance_model_5)

training_results <- predict(insurance_model_5,train[,-1]) #we use predict instead of compute
predict_train <- ifelse(training_results[,1] > training_results[,2],"Fraudulent","Genuine")
# model_results$neurons
mean(predict_train==train$Result) #Training Accuracy = 92% 

testing_results <- predict(insurance_model_5,test[,-1]) #we use predict instead of compute
predict_test <- ifelse(testing_results[,1] > testing_results[,2],"Fraudulent","Genuine")
# model_results$neurons
mean(predict_test==test$Result) #Testing Accuracy = 67% 

# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased


model_c <- neuralnet(formula_nn,data=train,hidden = c(7,3))
plot(model_c)

training_results <- predict(model_c,train[,-1]) #we use predict instead of compute
predict_train <- ifelse(training_results[,1] > training_results[,2],"Fraudulent","Genuine")
# model_results$neurons
mean(predict_train==train$Result) #Training Accuracy = 98.57% 

testing_results <- predict(model_c,test[,-1]) #we use predict instead of compute
predict_test <- ifelse(testing_results[,1] > testing_results[,2],"Fraudulent","Genuine")
# model_results$neurons
mean(predict_test==test$Result) #Testing Accuracy = 60% 

# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased



