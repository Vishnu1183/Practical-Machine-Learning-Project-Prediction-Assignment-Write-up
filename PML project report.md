1. Overview
The goal of your project is to predict the manner in which they did the exercise. 
This is the "classe" variable in the training set. You may use any of the other variables to predict with. 
You should create a report describing how you built your model, how you used cross validation, 
what you think the expected out of sample error is, and why you made the choices you did. 
You will also use your prediction model to predict 20 different test cases.

II. Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data
about personal activity relatively inexpensively. These type of devices are part of the quantified self movement 
- a group of enthusiasts who take measurements about themselves regularly to improve their health, 
to find patterns in their behavior, or because they are tech geeks. One thing that people regularly 
do is quantify how much of a particular activity they do, but they rarely quantify how well they do it.
In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.
They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 
More information is available from the website here: http://groupware.les.inf.puc-rio.br/har 
(see the section on the Weight Lifting Exercise Dataset).

Read more: http://groupware.les.inf.puc-rio.br/har#ixzz3xsbS5bVX


III. Data Loading and Data Cleaning
a) Environment Preparation

library(caret); library(rattle); library(rpart); library(rpart.plot)
library(randomForest); library(corrplot);

b)Data Loading and cleaning

training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

c) Partitioning Training set further into Train & Test Data

intrain <- createDataPartition(training$classe, p=0.7, list=FALSE)
traindata <- training[intrain, ]
testdata <- training[-intrain,]

d) Cleaning Data

Removing the dependent variables having Near Zero Variance
NZV <- nearZeroVar(traindata)
trainset <- traindata[, -NZV]
testset  <- testdata[, -NZV]

e) Removing the dependent variables having missing (NA) value

trans <- trainset[, colSums(is.na(trainset)) == 0]
tsts <- testset[, colSums(is.na(trainset)) == 0]

f) Removing unimportant variables from the cleaned data

trains <- trans[,-(1:5)]
tests<- tsts[,-(1:5)]

4) Exploratory Analysis
Plotting correlation matrix graphically

corMatrix <- cor(trains[, -54])
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))
         
5)Preprocessing & Model Fitting & Predicting

Three methods will be applied to model the regressions (in the Train dataset) and
the best one (with higher accuracy when applied to the Test dataset) will be used for the quiz predictions.
The methods are: Random Forests, Decision Tree and Generalized Boosted Model, as described below.
A Confusion Matrix is plotted at the end of each analysis to better visualize the accuracy of the models.

a) Method: Random Forest
# model fit
set.seed(12345)
controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
modFitRandForest <- train(classe ~ ., data=TrainSet, method="rf",
                          trControl=controlRF)
modFitRandForest$finalModel

# prediction on Test dataset
predictRandForest <- predict(modFitRandForest, newdata=TestSet)
confMatRandForest <- confusionMatrix(predictRandForest, TestSet$classe)
confMatRandForest

# plot matrix results
plot(confMatRandForest$table, col = confMatRandForest$byClass, 
     main = paste("Random Forest - Accuracy =",
                  round(confMatRandForest$overall['Accuracy'], 4)))
                  
 b) Method: Decision Trees
# model fit
set.seed(12345)
modFitDecTree <- rpart(classe ~ ., data=TrainSet, method="class")
fancyRpartPlot(modFitDecTree)

# prediction on Test dataset
predictDecTree <- predict(modFitDecTree, newdata=TestSet, type="class")
confMatDecTree <- confusionMatrix(predictDecTree, TestSet$classe)
confMatDecTree

# plot matrix results
plot(confMatDecTree$table, col = confMatDecTree$byClass, 
     main = paste("Decision Tree - Accuracy =",
                  round(confMatDecTree$overall['Accuracy'], 4)))
                  
 c) Method: Generalized Boosted Model
# model fit
set.seed(12345)
controlGBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
modFitGBM  <- train(classe ~ ., data=TrainSet, method = "gbm",
                    trControl = controlGBM, verbose = FALSE)
modFitGBM$finalModel

# prediction on Test dataset
predictGBM <- predict(modFitGBM, newdata=TestSet)
confMatGBM <- confusionMatrix(predictGBM, TestSet$classe)
confMatGBM

# plot matrix results
plot(confMatGBM$table, col = confMatGBM$byClass, 
     main = paste("GBM - Accuracy =", round(confMatGBM$overall['Accuracy'], 4)))
     
6)Applying the Selected Model to the Test Data     

predictTEST <- predict(modFitRandForest, newdata=testing)
predictTEST
