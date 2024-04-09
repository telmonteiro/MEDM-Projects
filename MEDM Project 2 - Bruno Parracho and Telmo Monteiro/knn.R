library(foreign)
higgs <- read.arff("../dataset.arff")
data_frame <- data.frame(higgs)
# sample 50000 rows from the data set
sample_indices <- sample(1:nrow(data_frame), 50000, replace = FALSE)
data_ <- data_frame[sample_indices, ]

# Define the breaks for the bins
breaks <- c(-Inf, 1, 2, Inf)
# Convert the numeric column to a factor with specified breks
data_$jet_1_b.tag <- cut(data_$jet_1_b.tag, breaks = breaks, labels = c(1, 2, 3), include.lowest = TRUE)
data_$jet_2_b.tag <- cut(data_$jet_2_b.tag, breaks = breaks, labels = c(1, 2, 3), include.lowest = TRUE)
data_$jet_3_b.tag <- cut(data_$jet_3_b.tag, breaks = breaks, labels = c(1, 2, 3), include.lowest = TRUE)
data_$jet_4_b.tag <- cut(data_$jet_4_b.tag, breaks = breaks, labels = c(1, 2, 3), include.lowest = TRUE)

#k neighbors
library(caret)
data_$target <- factor(data_$target, levels = c(0, 1))
#train 70%, test 30%
trainIndex <- createDataPartition(data_$target,times=1,p = .7,list = FALSE)
data.train <- data_[trainIndex, ]
data.test <- data_[-trainIndex, ]

#-----------------------------------------------------------------------------------
#NOT SCALING VARIABLES
#10-fold CV
fitControl <- trainControl(method = "repeatedcv", number = 10,verboseIter=TRUE)
knnModel <- train(target~., data = data.train, method = "knn", 
                  trControl = fitControl, 
                  tuneGrid = data.frame(k = c(1:60)))
plot(knnModel,main="Accuracy for training data for k between 1 and 60 without scaling")
best_model<- knn3(target~., data = data.train, k = knnModel$bestTune$k, 
                  prob=TRUE)
predictions <- predict(best_model, data.test,type = "class")
cm <- confusionMatrix(predictions, data.test$target)
cm
data.frame(Accuracy = cm$overall["Accuracy"],Sensitivity = cm$byClass["Sensitivity"],
           Specificity = cm$byClass["Specificity"])


#-----------------------------------------------------------------------------------
#SCALING VARIABLES
preProcValues <- preProcess(data.train, method = c("center", "scale"),
                            verbose=TRUE,rangeBounds=c(0, 1))
trainTransformed <- predict(preProcValues, data.train)
testTransformed <- predict(preProcValues, data.test)
#10-fold CV
fitControl <- trainControl(method = "repeatedcv", number = 10,verboseIter=TRUE)
knnModel <- train(target~., data = trainTransformed, method = "knn", 
                  trControl = fitControl, 
                  tuneGrid = data.frame(k = c(1:60)))
plot(knnModel,main="Accuracy for training data for k between 1 and 60 with scaling")
best_model<- knn3(target~., data = trainTransformed, k = knnModel$bestTune$k, 
                  prob=TRUE)
predictions <- predict(best_model, testTransformed,type = "class")
cm <- confusionMatrix(predictions, testTransformed$target)
cm
data.frame(Accuracy = cm$overall["Accuracy"],Sensitivity = cm$byClass["Sensitivity"],
           Specificity = cm$byClass["Specificity"])
