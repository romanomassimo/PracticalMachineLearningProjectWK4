# PracticalMachineLearningProjectWK4
Prediction Assignment

## load data
testing <- read.csv("~/Data/pml-testing.csv", header=TRUE)
training <- read.csv("~/Data/pml-training.csv", header=TRUE)

## preprocessing
    ## remove near Zero Variance
nzv <- nearZeroVar(training)
training <- training[,-nzv]
    ## remove NA variables
nacol <- !is.na(training)[1,]
training <- training[,nacol]
    ## remove identification variables
training <- training[,-c(1:6)]

## data partitioning: large number of obs, thus decided to create two partitions
inTrain <- createDataPartition(training$classe,p=0.7, list = FALSE)
train <- training[inTrain,]
test <- training[-inTrain,]

### test several models and select highest accuracy 

## linear discriminant analysis model
lda.model <- train(classe~.,data=train, method="lda")
lda.predict <- predict(lda.model, test)
## expected out of sample error is Accuracy : 0.7014  
confusionMatrix(lda.predict,test$classe)

## classification tree model
rpart.model <- train(classe~.,data=train, method="rpart")
rpart.predict <- predict(rpart.model, test)
## expected out of sample error is Accuracy :    
confusionMatrix(rpart.predict,test$classe)

## boosted model
gbm.model <- train(classe~.,data=train, method="gbm")
gbm.predict <- predict(gbm.model, test)
## expected out of sample error is Accuracy :    
confusionMatrix(gbm.predict,test$classe)

## random forest model
rf.model <- train(classe~.,data=train, method="rf")
rf.predict <- predict(rf.model, test)
## expected out of sample error is Accuracy : 0.9934   
confusionMatrix(rf.predict,test$classe)

## prediction using best out-of-sample performing model: rf
testing <- read.csv("~/J&J/Machine Learning/Applied ML in R/Project/Data/pml-testing.csv", header=TRUE)
testing <- testing[,-nzv];testing <- testing[,nacol];testing <- testing[,c(-1:6)]
testing.predict <- predict(rf.model,testing)
data.frame(testing.predict)

