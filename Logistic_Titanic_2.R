library(readr)
dataset <- read.csv("train.csv")
#Summing up and forming a family!

dataset <- mutate(dataset, family = SibSp + Parch)
dataset <- select(dataset, c(-9,-11,-12))

#Age Rpart
data_rpart <- dataset[,c(3,5,6,7,8,9,10)]

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
classifier <- glm(Survived ~ ., family = binomial, training_set) 
summary(classifier)

#Importing the test set
dataset <- read.csv("test.csv")
#Summing up and forming a family!

dataset <- mutate(dataset, family = SibSp + Parch)
dataset <- select(dataset, c(-8,-10,-11))

dataset[is.na(dataset$Fare),]$Fare <- median(dataset$Fare,na.rm = T) 
#Age Rpart
data_rpart <- dataset[,c(2,4,5,6,7,8,9)]

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
y_pred <- predict(classifier,type = "response" ,test_set)
y_pred <- ifelse(y_pred > 0.5, 1, 0)
table(test_set[,3],y_pred)


model <- data.frame(dataset$PassengerId,y_pred)
write.csv(model,"Kaggle_Upload.csv")

