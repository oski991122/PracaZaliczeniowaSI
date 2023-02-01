library("caret")
library("klaR")
library("rpart")
library("gains")
library("pROC")
library("stats")
library("randomForest")
library(e1071)
library("class")
################################# setting up the data
rm(list=ls())
data <- read.csv('C:/Users/oski9/Desktop/mushrooms.csv', col.names = c("class","capshape","capsurface", "capcolor","","","","","","","","","","","","","","","","","","",""))
###deleting all of the unused fields
data$X <- NULL
data$X.1 <- NULL
data$X.2 <- NULL
data$X.3 <- NULL
data$X.4 <- NULL
data$X.5 <- NULL
data$X.6 <- NULL
data$X.7 <- NULL
data$X.8 <- NULL
data$X.9 <- NULL
data$X.10 <- NULL
data$X.11 <- NULL
data$X.12 <- NULL
data$X.13 <- NULL
data$X.14 <- NULL
data$X.15 <- NULL
data$X.16 <- NULL
data$X.17 <- NULL
data$X.18 <- NULL

###making a factor and str
data$class <- as.factor(data$class)
str(data)
set.seed(1)



############################### Naive Bayes
accuracy_all<-c()
for (i in 1:4) {
  myIndex <- createDataPartition(data$class, p=0.8, list=FALSE)
  trainSet <- data[myIndex,]
  validationSet <- data[-myIndex,]
nb_results <- naiveBayes(class ~., data = trainSet)
nb_pred <- predict(nb_results, newdata = validationSet)
cm<-confusionMatrix(nb_pred, validationSet$class, positive="e")
# Extract the values you need for the calculation
tp <- cm$table[1,1] # True positives
fp <- cm$table[2,1] # False positives
tn <- cm$table[2,2] # True negatives
fn <- cm$table[1,2] # False negatives

# Calculate accuracy
accuracy <- (tp + tn) / (tp + tn + fp + fn)
accuracy_all <- append(accuracy_all,accuracy)
}
average_accuracy <- mean(accuracy_all)
classifier_quality <- sd(accuracy_all)
cat("Total quality of the classifier:", classifier_quality, "\n")
cat("Standard deviation:", average_accuracy, "\n")
cm


############################### Random Tree
accuracy_all<-c()
for (i in 1:4) {
  myIndex <- createDataPartition(data$class, p=0.8, list=FALSE)
  trainSet <- data[myIndex,]
  validationSet <- data[-myIndex,]
  rf_results<-randomForest(class~., data=trainSet, method="class")
  rf_Pred <- predict(rf_results,validationSet)
  cm <-confusionMatrix(rf_Pred, validationSet$class, positive="e")
  
  # Extract the values you need for the calculation
  tp <- cm$table[1,1] # True positives
  fp <- cm$table[2,1] # False positives
  tn <- cm$table[2,2] # True negatives
  fn <- cm$table[1,2] # False negatives
  
  # Calculate accuracy
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  accuracy_all <- append(accuracy_all,accuracy)
}
average_accuracy <- mean(accuracy_all)
classifier_quality <- sd(accuracy_all)
cat("Total quality of the classifier:", classifier_quality, "\n")
cat("Standard deviation:", average_accuracy, "\n")
cm

############################### svm
accuracy_all<-c()
for (i in 1:2) {
  myIndex <- createDataPartition(data$class, p=0.75, list=FALSE)
  trainSet <- data[myIndex,]
  validationSet <- data[-myIndex,]
  svm_results <- svm(class ~., data = trainSet, method="class")
  svm_pred <- predict(svm_results, validationSet)
  cm <-confusionMatrix(svm_pred, validationSet$class, positive="e")
  
  # Extract the values you need for the calculation
  tp <- cm$table[1,1] # True positives
  fp <- cm$table[2,1] # False positives
  tn <- cm$table[2,2] # True negatives
  fn <- cm$table[1,2] # False negatives
  
  # Calculate accuracy
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  accuracy_all <- append(accuracy_all,accuracy)
}

average_accuracy <- mean(accuracy_all)
classifier_quality <- sd(accuracy_all)
cat("Total quality of the classifier:", classifier_quality, "\n")
cat("Standard deviation:", average_accuracy, "\n")
cm

