rm(list = ls())


list.of.packages <- c("caret","e1071","ggplot2","rpart", "rpart.plot","pROC","randomForest","caTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)
library(ggplot2)
library(rpart.plot)
library(pROC)
library(ROCR)
library(rpart)
library(randomForest)
library(caTools)

Path<-"D:/R assignments/Case Study 6"
setwd(Path)
getwd()

letters=read.csv("letters_ABPR.csv",stringsAsFactors = TRUE)
head(letters)


dim(letters)
str(letters)
summary(letters)
colSums(is.na(letters))

letters$letter<-as.factor(letters$letter)
str(letters$letter)


#----------------------Splitting the dataset into train and test data----------------------#


set.seed(1000)
spl = sample.split(letters$letter, SplitRatio = 0.7)
Train = subset(letters, spl==TRUE)
dim(Train)
str(Train)

Test = subset(letters, spl==FALSE)
dim(Test)
str(Test)
summary(Test)

paste("The accuracy of the baseline model on the testing dataset=", 241/nrow(Test))



#------------------------------Building the CART model------------------------------------#



CART1<-rpart(letter~.,data=Train, method = "class")
prp(CART1)
CART1


#------------------Checking the accuracy of the model in the train data---------------------#

predictCART <-predict(CART1, newdata=Train, type = "class")

confusionMatrix(predictCART,Train$letter)

#------------------Checking the accuracy of the model in the test data---------------------#
predictCART1<-predict(CART1, newdata=Test, type = "class")
table(Test$letter,predictCART1)

#ConfusionMatrix
confusionMatrix(predictCART1,Test$letter)


#---------------------------End of the CART model-----------------------------------------#


#-------------------------------Random Forest Model--------------------------------------#


PredictForest1<-randomForest(letter~.,data = Train, ntree = 1000)
PredictForest1



#--------------------Checking the accuracy of the model-------------------------------------------#
predForest1<-predict(PredictForest1, newdata=Test, type = "class")
Test$Y_pred<-predict(PredictForest1, newdata=Test, type = "class")

length(predForest1)
length(Test$letter)

table(Test$letter,predForest1)

#ConfusionMatrix
confusionMatrix(predForest1,Test$letter)

