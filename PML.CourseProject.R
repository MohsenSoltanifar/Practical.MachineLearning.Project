---
title: 'Practical Machine Learning: Course Project'
subtitle: "Prediction of Exercise Types in six Participants"
author: "Mohsen Soltanifar"
date: "July 30, 2020"
output: html_document
---
  
#Introduction 
  In this project we explore  data from accelerometers on the belt,
forearm, arm, and dumbell of 6 participants to investigate how well people do
excercise activities. The appendix includes the R code for the presented analysis.

#Data Preparation
The data preparation is performed in three steps: (1) Loading the training and the test data; (2) Cleaning the loaded datasets from three types of variables: Near zero variance variables, those with minimum 90% missing values, and those intutively useless characters; (3) Partitioning the train data to 60% train data and 40% cross validation data.

```{r  simple1, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'}
defaultW <- getOption("warn") 
options(warn = -1)

library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(RGtk2)
library(rattle)
library(randomForest)

setwd("C:/Users/Mohsen/Desktop/Data.Science.Certificate.Coursera2020/Course 8. Practical Machine Learning/Week4/CourseProject/SampleSolutions")

#1. Data Loading

trainingdata <- read.csv('pml-training.csv', na.strings = c("NA", "#DIV/0!", ""))  
testdata <- read.csv('pml-testing.csv', na.strings = c("NA", "#DIV/0!", ""))

#2. Data Cleaning
# Remove variables with Nearzero variances

nearzerovarindex <- nearZeroVar(trainingdata)
trainingdata<-trainingdata[,-nearzerovarindex]
testdata<-testdata[,-nearzerovarindex]

#Remove variables more than 90% of the observation to be NA.

clnColumnIndex <- colSums(is.na(trainingdata))/nrow(trainingdata) < 0.90
cleantrainingdata <- trainingdata[,clnColumnIndex] 
cleantestdata <- testdata[,clnColumnIndex] 

#Removing variables which are non-numeric. Intuitively, these are useles variables. 

cleantrainingdata<-cleantrainingdata[,-c(1:7)]
cleantestdata<-cleantestdata[,-c(1:7)]


#3. Data Partitioning 
#Partition training data to 60% training and 40% corss vadiatin test data.

Index <- createDataPartition(cleantrainingdata$classe, p=0.60)[[1]]
trainingtrainingdata <- cleantrainingdata[Index,]
trainingcrossvaldata <- cleantrainingdata[-Index,]


options(warn = defaultW)
```



#Data Analysis
We consider three machine learning algorithms: the Decision Tree Model, the Random Forest Model and the knn.  We fit each of them on the 60% training data above and predict over the leftover 40% cross validation data to see its acuracy. The acuracy statistics were 0.459, 0.990 and 0.953 respectively. 

###The Decision Tree Model
```{r  simple2, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'}
defaultW <- getOption("warn") 
options(warn = -1)
#1. Decision Tree Model 
DTmodel <- train(classe ~ ., data = trainingtrainingdata, method="rpart")
DTPrediction <- predict(DTmodel, trainingcrossvaldata)
CM1=confusionMatrix(as.factor(trainingcrossvaldata$classe), DTPrediction)
CM1 
#0.4598522
fancyRpartPlot(DTmodel$finalModel)

options(warn = defaultW)
```

###The Random Forest Model
```{r  simple3, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'}
defaultW <- getOption("warn") 
options(warn = -1)

#2. Random Forest Model
set.seed(12345)  
RFmodel<- train(classe ~ ., data = trainingtrainingdata, method = "rf", ntree = 20)
RFPrediction <- predict(RFmodel, trainingcrossvaldata)
CM2=confusionMatrix(as.factor(trainingcrossvaldata$classe), RFPrediction)
CM2 
#0.9886566
plot(RFmodel)
#Maximum Acuracy by number of selected predictors: almost 27 predictor for AC=0.982 


options(warn = defaultW)
```

###The KNN Model
```{r  simple4, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'}
defaultW <- getOption("warn") 
options(warn = -1)

#3. KNN method
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(12345)
KNNmodel=train(classe ~ ., data = trainingtrainingdata,
               method="knn", trControl=trctrl,
               preProcess = c("center", "scale"),
               tuneLength = 10)

KNNPrediction <- predict(KNNmodel, trainingcrossvaldata)
CM3=confusionMatrix(as.factor(trainingcrossvaldata$classe), KNNPrediction)
CM3 
#0.95398
plot(KNNmodel)

options(warn = defaultW)
```

#Conclusion.
Comparing three methods of Decision Tree Model, Random Forest Model and the KNN method, the best method is the Random Forest Model with maximum Accuracy of 0.99.


#Prediction.
We predict the first 20 cases from the test using the Random Forest Model.



#Appendix
##R Code for the Analysis

###R Code for Processing Data
```{r, results="hide", message=FALSE, warnings=FALSE, eval = FALSE}
defaultW <- getOption("warn") 
options(warn = -1) 

library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(RGtk2)
library(rattle)
library(randomForest)

setwd("C:/Users/Mohsen/Desktop/Data.Science.Certificate.Coursera2020/Course 8. Practical Machine Learning/Week4/CourseProject/SampleSolutions")

#1. Data Loading

trainingdata <- read.csv('pml-training.csv', na.strings = c("NA", "#DIV/0!", ""))  
testdata <- read.csv('pml-testing.csv', na.strings = c("NA", "#DIV/0!", ""))

#2. Data Cleaning
# Remove variables with Nearzero variances

nearzerovarindex <- nearZeroVar(trainingdata)
trainingdata<-trainingdata[,-nearzerovarindex]
testdata<-testdata[,-nearzerovarindex]

#Remove variables more than 90% of the observation to be NA.

clnColumnIndex <- colSums(is.na(trainingdata))/nrow(trainingdata) < 0.90
cleantrainingdata <- trainingdata[,clnColumnIndex] 
cleantestdata <- testdata[,clnColumnIndex] 

#Removing variables which are non-numeric. Intuitively, these are useles variables. 

cleantrainingdata<-cleantrainingdata[,-c(1:7)]
cleantestdata<-cleantestdata[,-c(1:7)]


#3. Data Partitioning 
#Partition training data to 60% training and 40% corss vadiatin test data.

Index <- createDataPartition(cleantrainingdata$classe, p=0.60)[[1]]
trainingtrainingdata <- cleantrainingdata[Index,]
trainingcrossvaldata <- cleantrainingdata[-Index,]

options(warn = defaultW)
```

###R Code for 3 Machine Learning Algorithms

```{r, results="hide", message=FALSE, warnings=FALSE, eval = FALSE}
defaultW <- getOption("warn") 
options(warn = -1) 

#1. Decision Tree Model 
DTmodel <- train(classe ~ ., data = trainingtrainingdata, method="rpart")
DTPrediction <- predict(DTmodel, trainingcrossvaldata)
CM1=confusionMatrix(as.factor(trainingcrossvaldata$classe), DTPrediction)
CM1$overall
#0.4598522
#fancyRpartPlot(DTmodel$finalModel)

#2. Random Forest Model
set.seed(12345)  
RFmodel<- train(classe ~ ., data = trainingtrainingdata, method = "rf", ntree = 20)
RFPrediction <- predict(RFmodel, trainingcrossvaldata)
CM2=confusionMatrix(as.factor(trainingcrossvaldata$classe), RFPrediction)
CM2$overall
#0.9886566
#plot(RFmodel)
#Maximum Acuracy by number of selected predictors: almost 27 predictor for AC=0.982 

#3. KNN method
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(12345)
KNNmodel=train(classe ~ ., data = trainingtrainingdata,
               method="knn", trControl=trctrl,
               preProcess = c("center", "scale"),
               tuneLength = 10)

KNNPrediction <- predict(KNNmodel, trainingcrossvaldata)
CM3=confusionMatrix(as.factor(trainingcrossvaldata$classe), KNNPrediction)
CM3$overall
#0.95398
#plot(KNNmodel)

options(warn = defaultW)
```


###R Code for  Prediction for th first 20 test cases

```{r, results="hide", message=FALSE, warnings=FALSE, eval = FALSE}
defaultW <- getOption("warn") 
options(warn = -1) 

Testprediction <- predict(RFmodel, cleantestdata )
Testprediction 

options(warn = defaultW)
```


