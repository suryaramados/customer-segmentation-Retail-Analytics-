library(readr)
install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)
library(mlbench)
install.packages('caretEnsemble')
library(caretEnsemble)
library(randomForest)
library(kernlab)



setwd("C:/Users/Surya/Desktop")
epa <- read.csv('existingproductattributes2017.CSV')
epa
summary(epa)
str(epa)
is.na((epa$BestSellersRank))
table(is.na(epa))

epa$BestSellersRank

epa1 <- na.exclude(epa)

str(epa1)
sum(is.na(epa1))




npa <- read.csv('newproductattributes2017.CSV')
npa
summary(npa)
str(npa)
sum(is.na(npa))




#Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_model <- dummyVars(~ ., data=epa1)
dummies_model
# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
trainData_mat <- predict(dummies_model, newdata = epa1)
str(trainData_mat)

traindt <- data.frame(trainData_mat)
traindt
# # See the structure of the new dataset
str(traindt)
names(traindt)


dummies_model2 <- dummyVars(~ ., data=npa)
dummies_model2
# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
trainData_mat1 <- predict(dummies_model2, newdata = npa)
str(trainData_mat1)
# # Convert to dataframe
trainD <- data.frame(trainData_mat1)
trainD
# # See the structure of the new dataset
str(trainD)
names(trainD)

traindt$BestSellersRank <- NULL
str(traindt)
corrData <- cor(traindt)
corrData

install.packages("corrplot")

library(corrplot)

corrplot(corrData)
trainD$BestSellersRank <- NULL
trainD$x3StarReviews <- NULL
trainD$x1StarReviews <- NULL
str(trainD)


traindt$x1StarReviews <- NULL
traindt$x3StarReviews <- NULL

str(trainData1)
corrData1 <- cor(trainD)
corrData1

install.packages("corrplot")

library(corrplot)

corrplot(corrData1)

set.seed(123)
trainSize<-round(nrow(traindt)*0.7) 
trainSize
testSize<-nrow(traindt)-trainSize
testSize






training_indices<-sample(seq_len(nrow(traindt)),size =trainSize)
training_indices
trainSet<-traindt[training_indices,]
str(trainSet)
testSet<-traindt[-training_indices,] 
str(testSet)

test2 <- trainData1
str(test2)



trainindex <- createDataPartition(traindt$Volume, p=0.75, list=FALSE, times = 1)
trainindex

# Step 2: Create the training  dataset
traindt1 <- traindt[trainindex,]
str(traindt1)

# Step 3: Create the test dataset
testdt1 <- traindt[-trainindex,]
str(testdt1)












lm.fit1 <- lm(formula = Volume ~ ., data = traindt1)
lm.fit1
summary (lm.fit1)



require(e1071) #Contains the SVM 
library(caret)
fitControl <- trainControl(method="repeatedcv", number=10, repeats=1) # cross validation
svmFit1 <- train(Volume~., data=trainSet, method="svmLinear2", trControl=fitControl, tuneLength=3)
svmFit1
summary(svmFit1)

print(svmFit1)


# test with train data
predsvm <- predict(svmFit1, testSet)
predsvm
summary(predsvm)
postResample(predsvm,testSet$Volume)

test4 <- trainD

predsvmfinal <- predict(svmFit1, test4)
predsvmfinal
summary(predsvmfinal)
postResample(predsvmfinal,test4$Volume)





library(randomForest)

rfFit1 <- train(Volume~., data=trainSet, method="rf", trControl=fitControl, tuneLength=3)
summary(rfFit1)
rfFit1
predictedrf1= predict(rfFit1,testSet)
predictedrf1

summary(predictedrf1)
str(predictedrf1)
postResample(predictedrf1,testSet$Volume)


predrffinal <- predict(rfFit1, test4)
predrffinal
summary(predrffinal)
postResample(predrffinal,test4$Volume)


library(gbm)

gbmFit1 <- train(Volume~., data=traindt1, method="gbm", trControl=fitControl, verbose=FALSE, tuneLength=3)
gbmFit1

summary(gbmFit1)

predictedgbm= predict(gbmFit1,testdt1)
predictedgbm

summary(predictedgbm)
str(predictedgbm)
postResample(predictedgbm,testdt1$Volume)




predgbmfinal <- predict(gbmFit1, test4)
predgbmfinal
summary(predgbmfinal)
table(predgbmfinal,test4$Volume)
postResample(predgbmfinal, test4$Volume)

output <- test2
output$predictions <- predgbmfinal

write.csv(output, file="C6.T3output.csv", row.names = TRUE)
----------------------------------------------------------------------------




# Fitting model
rffit1<- randomForest(Volume ~ ., trainSet,ntree=500)
rffit1
summary(rffit1)
#Predict Output 
predictedrf1= predict(rffit1,testSet)
predictedrf1

summary(predictedrf1)
str(predictedrf1)
table(predictedrf1,testSet$Volume)



finalpredrf1 = predict(rffit1,trainData1)
finalpredrf1
summary(finalpredrf1)
str(finalpredrf1)
table(finalpredrf1,trainData1$Volume)




library(mlbench)
library(gbm)

gradboost=gbm(Volume ~ .,data = trainSet,distribution = "gaussian",n.trees = 10000,shrinkage = 0.01, interaction.depth = 4)
summary(gradboost)
gradboost

predgbm<-predict(gradboost,testSet,n.trees = 10000)
summary(predgbm) #dimentions of the Prediction Matrix
str(predgbm)
predgbm
table(predgbm,testSet$Volume)


finalpredgbm<-predict(gradboost,trainData1,n.trees = 10000)
summary(finalpredgbm) #dimentions of the Prediction Matrix
str(finalpredgbm)
finalpredgbm
table(finalpredgbm,trainData1$Volume)



str(npa)

output <- (npa)
output

finalpredrf1
output$predictions <- (finalpredrf1)
output$predictions


write.csv(output, file="C2.T4output.csv", row.names = TRUE)
