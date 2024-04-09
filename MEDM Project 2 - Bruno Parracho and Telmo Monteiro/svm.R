library(factoextra)
library(foreign)
library(MASS)
library(readr)
library(e1071)
library(caret)
library(kernlab)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
# sample 10000 rows from the data set
N=50000
sample_indices <- sample(1:nrow(data_frame),N, replace = FALSE)
data <- data_frame[sample_indices, ]

#sum=summary(data)
# Summary of the dataset

data <- data[, -which(names(data)=="jet_1_b.tag")]
data <- data[, -which(names(data)=="jet_2_b.tag")]
data <- data[, -which(names(data)=="jet_3_b.tag")]
data <- data[, -which(names(data)=="jet_4_b.tag")]

data.train = head(data,0.7*N)
data.test = tail(data,0.3*N)


##################################### SVM not scaled ###############################

#Ago com kernel=radial
model1 <- svm(data.train[,1]~., data = data.train[,-1],kernel="radial")
summary(model1)
predr1 = predict(model1, data.test[,-1])
cm_r=confusionMatrix(predr1, data.test[,1])
print(cm_r)
erro.svmr1=sum(predr1!=data.test[,1])/nrow(data.test)
erro.svmr1


# visualize (classes by color, SV by crosses):

plot(cmdscale(dist(data.train[,-1])), col = as.integer(data.train[,1]),pch = c("o","+")[1:nrow(data.train) %in% model1$index + 1],xlab="x",ylab="y")

#Agora com kernel=linear

model2 = svm(data.train[,1]~., data = data.train[,-1],kernel="linear")
summary(model2)
predr2 = predict(model2, data.test[,-1])
cm_l=confusionMatrix(predr2, data.test[,1])
print(cm_l)
erro.svmr2=sum(predr2!=data.test[,1])/nrow(data.test)
erro.svmr2

plot(cmdscale(dist(data.train[,-1])), col = as.integer(data.train[,1]),pch = c("o","+")[1:nrow(data.train) %in% model2$index + 1],xlab="x",ylab="y")


#Agora com kernel=sigmoid

model3 = svm(data.train[,1]~., data = data.train[,-1],kernel="sigmoid")
summary(model3)
predr3 = predict(model3, data.test[,-1])
cm_s=confusionMatrix(predr3, data.test[,1])
print(cm_s)
erro.svmr3=sum(predr3!=data.test[,1])/nrow(data.test)
erro.svmr3

plot(cmdscale(dist(data.train[,-1])), col = as.integer(data.train[,1]),pch = c("o","+")[1:nrow(data.train) %in% model3$index + 1],xlab="x",ylab="y")


#Ago com kernel=laplace

model5 <- ksvm(data.train[,1]~., data = data.train[,-1], kernel = "laplacedot")
summary(model4)
predr5 = predict(model5, data.test[,-1])
cm_r=confusionMatrix(predr5, data.test[,1])
print(cm_r)
erro.svmr5=sum(predr5!=data.test[,1])/nrow(data.test)
erro.svmr5

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(data.train[,-1])), col = as.integer(data.train[,1]),pch = c("o","+")[1:nrow(data.train) %in% model5$index + 1],xlab="x",ylab="y")



##################################### SVM scaled ####################
data.train_scaled <- scale(data.train[,-1])
data.test_scaled <- scale(data.test[,-1])


#Ago com kernel=radial
model1 <- svm(data.train[,1]~., data = data.train_scaled,kernel="radial")
summary(model1)

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(data.train_scaled)), col = as.integer(data.train[,1]),pch = c("o","+")[1:nrow(data.train) %in% model1$index + 1],xlab="x",ylab="y")
predr1 = predict(model1, data.test_scaled)
cm_r=confusionMatrix(predr1, data.test[,1])
print(cm_r)
erro.svmr1=sum(predr1!=data.test[,1])/nrow(data.test)
erro.svmr1
#Agora com kernel=linear

model2 = svm(data.train[,1]~., data = data.train[,-1],kernel="linear")
summary(model2)
plot(cmdscale(dist(data.train[,-1])), col = as.integer(data.train[,1]),pch = c("o","+")[1:nrow(data.train) %in% model1$index + 1],xlab="x",ylab="y")
predr2 = predict(model2, data.test[,-1])
cm_l=confusionMatrix(predr2, data.test[,1])
print(cm_l)
erro.svmr2=sum(predr2!=data.test[,1])/nrow(data.test)
erro.svmr2
#Agora com kernel=sigmoid

model3 = svm(data.train[,1]~., data = data.train[,-1],kernel="sigmoid")
summary(model3)
plot(cmdscale(dist(data.train[,-1])), col = as.integer(data.train[,1]),pch = c("o","+")[1:nrow(data.train) %in% model1$index + 1],xlab="x",ylab="y")
predr3 = predict(model3, data.test[,-1])
cm_s=confusionMatrix(predr3, data.test[,1])
print(cm_s)
erro.svmr3=sum(predr3!=data.test[,1])/nrow(data.test)
erro.svmr3


