library(foreign)
library(MASS)
library(rpart)
library(caret)
library(rpart.plot)
library(vip)
library(ggplot2)

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

data_$target <- factor(data_$target, levels = c(0, 1))
#train 70%, test 30%
trainIndex <- createDataPartition(data_$target,times=1,p = .7,list = FALSE)
data.train <- data_[trainIndex, ]
data.test <- data_[-trainIndex, ]


#------------------------------------------------------------------------------------------------------

# Function to calculate accuracy for a given model
accuracy_tune <- function(fit, data.test) {
  predict_unseen <- predict(fit, data.test, type = 'class')
  table_mat <- table(data.test$target, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}
# Generate combinations of tuning parameters
param_grid <- expand.grid(minsplit = seq(1, 51, by = 5),
                          minbucket = seq(1, 31, by = 5),
                          maxdepth = seq(1, 30, by = 5),
                          cp = c(0.0005,0.001,0.005,0.01,0.05,0.1))
results <- data.frame()
# loop over parameter combinations
for (i in 1:nrow(param_grid)) {
  print(i)
  control <- rpart.control(minsplit = param_grid$minsplit[i],
                           minbucket = param_grid$minbucket[i],
                           maxdepth = param_grid$maxdepth[i],
                           cp = param_grid$cp[i])
  tune_fit <- rpart(target ~ ., data = data.train, method = 'class', control = control)
  acc <- accuracy_tune(tune_fit, data.test)
  results <- rbind(results, cbind(param_grid[i, ], accuracy = acc))}
print(results)
# find index of row with highest accuracy
max_acc_index <- which.max(results$accuracy)
#print row with the highest accuracy
print(results[max_acc_index, ])

#to visualize the tree
#minsplit minbucket maxdepth    cp  accuracy
#       1        21       16 5e-04 0.6880459
control <- rpart.control(minsplit = 1,minbucket = 21,maxdepth = 16,cp = 0.0005)
tree_fit <- rpart(target~., data = data.train, method = 'class', control = control)
prp(tree_fit,type = 4,extra = 101,under = TRUE,
  cex = 0.45,  # Adjust the text size
  box.palette = "auto",
  branch.lty = 4,  # Dashed branch lines
  tweak = 1   # Increase the space between text and nodes
  )

#rules of the tree
rules <- rpart.rules(tree_fit,roundint=TRUE)
print(rules)

#importance of variables for tree
var_importance <- vip::vip(tree_fit, num_features = 15)
print(var_importance)

#predict and print confusion matrix
prediction <- predict(tree_fit,data.test,type = "class") 
cm <- confusionMatrix(prediction, data.test$target)
cm
data.frame(Accuracy = cm$overall["Accuracy"],Sensitivity = cm$byClass["Sensitivity"],
           Specificity = cm$byClass["Specificity"])

#plot for accuracy vs complexity parameter with lines for each maxdepth
ggplot(results, aes(x = cp, y = accuracy, color = factor(maxdepth))) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy vs Complexity Parameter, colored by max depth",
       x = "Complexity Parameter (cp)",
       y = "Accuracy",
       color = "max depth") +
  theme_minimal()
#plot for accuracy vs minsplit with lines for each minbucket
ggplot(results, aes(x = minsplit, y = accuracy, color = factor(minbucket))) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy vs min split, colored by min bucket",
       x = "min split",
       y = "Accuracy",
       color = "min bucket") +
  theme_minimal()


#------------------------------------------------------------------------------------------------------
#now scaling the values
preProcValues <- preProcess(data.train, method = c("center", "scale"),verbose=TRUE,rangeBounds=c(0, 1))
trainTransformed <- predict(preProcValues, data.train)
testTransformed <- predict(preProcValues, data.test)

accuracy_tune <- function(fit, testTransformed) {
  predict_unseen <- predict(fit, testTransformed, type = 'class')
  table_mat <- table(testTransformed$target, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}
results <- data.frame()
for (i in 1:nrow(param_grid)) {
  print(i)
  control <- rpart.control(minsplit = param_grid$minsplit[i],
                           minbucket = param_grid$minbucket[i],
                           maxdepth = param_grid$maxdepth[i],
                           cp = param_grid$cp[i])
  tune_fit <- rpart(target ~ ., data = trainTransformed, method = 'class', control = control)
  acc <- accuracy_tune(tune_fit, testTransformed)
  results <- rbind(results, cbind(param_grid[i, ], accuracy = acc))}
print(results)
max_acc_index <- which.max(results$accuracy)
print(results[max_acc_index, ])

#minsplit minbucket maxdepth    cp  accuracy
#       1         31       11 0.0005 0.6850457
#to visualize the tree
control <- rpart.control(minsplit = 1,minbucket = 31,maxdepth = 11,cp = 0.0005)
tree_fit <- rpart(target~., data = data.train, method = 'class', control = control)
prp(tree_fit,type = 4,extra = 101,under = TRUE,
    cex = 0.4,  # Adjust the text size
    box.palette = "auto",
    branch.lty = 4,  # Dashed branch lines
    tweak = 1   # Increase the space between text and nodes
)

#rules of the tree
rules <- rpart.rules(tree_fit,roundint=TRUE)
print(rules)

#importance of variables for tree
var_importance <- vip::vip(tree_fit, num_features = 15)
print(var_importance)

#predict and print confusion matrix
prediction <- predict(tree_fit,data.test,type = "class") 
cm <- confusionMatrix(prediction, data.test$target)
cm
data.frame(Accuracy = cm$overall["Accuracy"],Sensitivity = cm$byClass["Sensitivity"],
           Specificity = cm$byClass["Specificity"])

#plot for accuracy vs complexity parameter with lines for each maxdepth
ggplot(results, aes(x = cp, y = accuracy, color = factor(maxdepth))) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy vs Complexity Parameter, colored by max depth",
       x = "Complexity Parameter (cp)",
       y = "Accuracy",
       color = "max depth") +
  theme_minimal()
#plot for accuracy vs minsplit with lines for each minbucket
ggplot(results, aes(x = minsplit, y = accuracy, color = factor(minbucket))) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy vs min split, colored by min bucket",
       x = "min split",
       y = "Accuracy",
       color = "min bucket") +
  theme_minimal()
