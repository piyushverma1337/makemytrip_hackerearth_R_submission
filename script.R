#checking train data
summary(train)

#train has NA values which is not suitable for randomForest in B and N
train[is.na(train$B), ]
train[is.na(train$N), ]

#looking at the data seems the records with N as NA 
#some will have no significant contribution so removing
rm(train.2)
train.2 <- subset(train, D!='' & E!='' & F!='' & G!='')

#remaining NA records
summary(train.2)
train.2[is.na(train.2$N),] 
train.2[is.na(train.2$B),]

#check the data with mice
library(mice)
md.pattern(train.2)

#impute data
tempData <- mice(train.2,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
completedData <- complete(tempData,1)
summary(completedData)

#featuers that we are using for training
features <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O")
rf.completedData <- completedData[1:545, features]
rf.label <- as.factor(completedData$P)

#set seed to get comparable results
set.seed(1234)
library(randomForest)
rf.completedData <- randomForest(x = rf.completedData, y = rf.label, importance = TRUE, ntree = 1000)
rf.completedData

#get accuracy plot
varImpPlot(rf.completedData)

#checking test data
summary(test)

#found missing values in test data
test[is.na(test$B), ]
test[is.na(test$N), ]

#will have to impute data here as well since random forest doesnt deal with NA
tempTestData <- mice(test,m=5,maxit=50,meth='pmm',seed=500)
summary(tempTestData)
completedTestData <- complete(tempTestData,1)
summary(completedTestData)

#levels between the to data sets dont match, randomforest requires this to happen
levels(completedTestData$D) <- levels(completedData$D)
levels(completedTestData$E) <- levels(completedData$E)
levels(completedTestData$F) <- levels(completedData$F)
levels(completedTestData$G) <- levels(completedData$G)

#prediction
rf.preds <- predict(rf.completedData, completedTestData)
table(rf.preds)

#preparing data for output
predictedData <- data.frame(id = rep(553:690), P = rf.preds)
write.csv(predictedData, file = "SUBMISSION.csv", row.names = FALSE)
