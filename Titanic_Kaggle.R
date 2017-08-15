library(readr)
dataset <- read.csv("train.csv")
#Summing up and forming a family!

dataset <- mutate(dataset, family = SibSp + Parch)
dataset <- select(dataset, c(-9,-11,-12))

#Age Rpart
data_rpart <- dataset[,c(3,5,6,9,10)]

data_rpart$Pclass <- as.factor(data_rpart$Pclass)
data_rpart$Sex <- factor(data_rpart$Sex,levels = c("male","female"),labels = c(0,1))
data_rpart$family <- as.factor(data_rpart$family)

clean <- data_rpart[!is.na(data_rpart$Age),]
na <- data_rpart[is.na(data_rpart$Age),]


library(randomForest)
set.seed(1)
age_predict <- randomForest(x = clean[-3],y = clean$Age, ntree = 10 )
prediction <- predict(age_predict, na[-3])
dup <- data_rpart
dup[is.na(data_rpart$Age),]$Age  <- prediction
dataset_dup <- dataset
dataset_dup$Age <- dup$Age

#Factorizing and Removing
dataset_dup <- dataset_dup[c(-1,-4)]
dataset_dup$Sex <- factor(dataset_dup$Sex,levels = c("male","female"),labels = c(0,1))
dataset_dup$Survived <- as.factor(dataset_dup$Survived)
dataset_dup$Pclass <- as.factor(dataset_dup$Pclass)
dataset_dup$alive <- ifelse(dataset_dup$Age < 15, 1,0)
dataset_dup$alive <- as.factor(dataset_dup$alive)
dataset_dup$joint <- ifelse(dataset_dup$family == 0,0,
                            (ifelse(dataset_dup$family > 0 & dataset_dup$family < 4,1,2))) 

dataset_dup$joint <- as.factor(dataset_dup$joint)
training_set <- dataset_dup[c(-4,-5,-6)]
View(training_set)
training_set <- training_set[c(-5)]
write.csv(training_set,"trained_model.csv")
#Fitting the Regression Model to the Dataset
library(mlr)
library(stringr)
library(xgboost)
library(caret)
sparse_matrix <- createDummyFeatures(training_set[-1])
sparse_matrix$Survived <- dataset$Survived
param <- list("objective" = "binary:logistic",    # binary classification 
              "eval_metric" = "rmse",    # evaluation metric 
              "nthread" = 3,   # number of threads to be used 
              "max_depth" = 15,    # maximum depth of tree 
              "eta" = 0.1,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 0.99,    # part of data instances to grow tree 
              "colsample_bytree" = 0.99,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 1,
              "seed" = 1)
xgb <- xgboost(data = data.matrix(sparse_matrix[,-12]), 
               label = data.matrix(sparse_matrix$Survived), 
               params = param,nrounds = 500)
summary(xgb)

#Importing the test set
dataset <- read.csv("test.csv")
#Summing up and forming a family!

dataset <- mutate(dataset, family = SibSp + Parch)
dataset <- select(dataset, c(-8,-10,-11))

dataset[is.na(dataset$Fare),]$Fare <- median(dataset$Fare,na.rm = T) 
#Age Rpart
data_rpart <- dataset[,c(2,4,5,8,9)]

data_rpart$Pclass <- as.factor(data_rpart$Pclass)
data_rpart$Sex <- factor(data_rpart$Sex,levels = c("male","female"),labels = c(0,1))
data_rpart$family <- as.factor(data_rpart$family)

clean <- data_rpart[!is.na(data_rpart$Age),]
na <- data_rpart[is.na(data_rpart$Age),]

library(randomForest)
set.seed(1)
age_predict <- randomForest(x = clean[-3],y = clean$Age, ntree = 10 )
prediction <- predict(age_predict, na[-3])
dup <- data_rpart
dup[is.na(data_rpart$Age),]$Age  <- prediction
dataset_dup <- dataset
dataset_dup$Age <- dup$Age

#Factorizing and Removing
dataset_dup <- dataset_dup[c(-1,-3)]
dataset_dup$Sex <- factor(dataset_dup$Sex,levels = c("male","female"),labels = c(0,1))
dataset_dup$Pclass <- as.factor(dataset_dup$Pclass)
dataset_dup$alive <- ifelse(dataset_dup$Age < 15, 1,0)
dataset_dup$alive <- as.factor(dataset_dup$alive)
dataset_dup$joint <- ifelse(dataset_dup$family == 0,0,
                            (ifelse(dataset_dup$family > 0 & dataset_dup$family < 4,1,2))) 

dataset_dup$joint <- as.factor(as.character(dataset_dup$joint))

test_set <- dataset_dup[c(-3,-4,-5,-7)]
##Predict

sparse_matrix_test <- createDummyFeatures(test_set)
y_pred = predict(xgb,  data.matrix(sparse_matrix_test))
y_pred = ifelse(y_pred >= 0.5,1,0)


model <- data.frame(PassengerId = dataset$PassengerId,Survived = y_pred)
write.csv(model,"Kaggle_Upload.csv",row.names = F)

