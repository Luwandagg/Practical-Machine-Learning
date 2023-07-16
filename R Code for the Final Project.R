library(caret)
library(randomForest)
library(e1071)
library(caTools)
library(tidyverse)

setwd("C:/Users/aluwandagga/Desktop/R/Coursera/John Hopkins/Final Project")
TrainRaw <- read.csv("pml-training.csv")
TestRaw <- read.csv("pml-testing.csv")

## Cleaning Data Process  

#The code snippet you provided uses the nearZeroVar() function from the caret package to identify near-zero variance variables in the TrainRaw data frame. The nearZeroVar() function is typically used as a preprocessing step to remove variables that have little or no variability.

NZV <- nearZeroVar(TrainRaw, saveMetrics = TRUE)
Training <- TrainRaw[, !NZV$nzv]
Testing <- TestRaw[, !NZV$nzv]
  

#Removing columns username, raw_timestamp_part_1 which not add value to the models
Training <- Training[, -c(1:5)]
Testing <- Testing[, -c(1:5)]
dim(Training)
dim(Testing)

#The code snippet you provided is used to remove columns that contain missing values (NAs) from the Training and Testing data frames
colNA <- colSums(is.na(Training)) == 0
Training <- Training[, colNA]
Testing <- Testing[, colNA]
rm(colNA)


## Partitioning Training Set and create the Validation Data 
# Set the seed for reproducibility
set.seed(12345)
inTrain <- createDataPartition(Training$classe, p = 0.70, list = FALSE)
Training <- Training[inTrain, ]
Validation <- Training[-inTrain, ]


### Decision Tree  
set.seed(12345)
XY <- train(x = Training %>% select(-classe),
            y = Training$classe,
            method = "rpart",
            parms = list(split = 'information'),
            metric = "Accuracy"
)


# Model performance on the Validation set
Predict_DT <- predict(XY, Validation)
Validation$classe <- factor(Validation$classe, levels = levels(PredictTree))
CM_DT<-confusionMatrix(Validation$classe, PredictTree)
CM_DT


#using the naivebayes algorithm
# Set the seed for reproducibility
set.seed(12345)
NBmodel <- naiveBayes(classe ~ ., data = Training)

#predictions
pre_nb<-predict(NBmodel, newdata = Validation %>% select(-classe))

confusion_matrix <- confusionMatrix(pre_nb, Validation$classe)

confusion_matrix

#using the Knn algorithm

control=trainControl(method = "cv", number = 10)

set.seed(12345)
KNN_model <- train(
  x = Training %>% select(-classe),    
  y = Training$classe,                 
  method = "knn",                        
  trControl = control,                   
  preProcess = c("center", "scale")      
)

#prediction on the validation set

Predict_KNN <- predict(KNN_model, Validation)
CM_KNN<-confusionMatrix(Validation$classe, Predict_KNN)
CM_KNN


## Predicting the classe for the using the KNN on the testing dataset

predict(KNN_model, Testing[, -length(names(Testing))])



#random forest
# Set the seed for reproducibility
set.seed(12345)

# Train the random forest model
Model_RF1 <- train(classe ~ ., data = Training, method = "rf",
                   ntree = 100, allowParallel = FALSE)




Predict_RF <- predict(Model_RF1, Validation)
Validation$classe <- factor(Validation$classe, levels = levels(Predict_RF))
confusionMatrix(Validation$classe, Predict_RF)

#calculating the accuracy from the confusion matrix
cm <- confusionMatrix(Validation$classe, Predict_RF)
Accuracy_RF <- cm$overall["Accuracy"]
Accuracy_RF
error_RF <- 1 -  Accuracy_RF
error_RF

The accuracy of the random forest model is `r accuracy_RF` and the error rate is `r error_RF`.

#conclusion

The random forest model has more accuary than the naivebayes and the decisiontrees models


## Predicting the classe for the using the Testing dataset

predict(Model_RF1, Testing[, -length(names(Testing))])

##

