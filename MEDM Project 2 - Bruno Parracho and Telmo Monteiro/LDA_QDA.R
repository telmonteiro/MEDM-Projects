library(foreign)
library(MASS)
library(readr)
library(caret)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
# sample 10000 rows from the data set
sample_indices <- sample(1:nrow(data_frame), 10000, replace = FALSE)
data <- data_frame[sample_indices, ]
#sum=summary(data)
# Summary of the dataset


data <- data[, -which(names(data)=="jet_1_b.tag")]
data <- data[, -which(names(data)=="jet_2_b.tag")]
data <- data[, -which(names(data)=="jet_3_b.tag")]
data <- data[, -which(names(data)=="jet_4_b.tag")]

data.train = head(data,7000)
data.test = tail(data,1000)

target_col <- 1
target.train <- data.train[, target_col]
target.test <- data.test[, target_col]
features.train<- data.train[, -target_col]
features.test<- data.test[, -target_col]

######################## Performance ##############################

#LDA
lda_model <- lda(features.train, target.train)
print(lda_model)

predictions_lda <- predict(lda_model, features.test)

#confusion matrix
conf_matrix_lda <- confusionMatrix(predictions_lda$class, target.test)
print(conf_matrix_lda)

#QDA
qda_model <- qda(features.train, target.train)
print(qda_model)

predictions_qda <- predict(qda_model, features.test)

#confusion matrix
conf_matrix_qda <- confusionMatrix(predictions_qda$class, target.test)
print(conf_matrix_qda)

################### Individual accuracies #################

accuracy_lda<-matrix()
accuracy_qda<-matrix()
for (x in 1:24){
  for (y in 1:24){
    df<-features.train[,c(x,y)]
    df_lda<-lda(df,target.train)
    pred_lda <- predict(df_lda,features.test)
    conf_matrix_lda <- confusionMatrix(pred_lda$class, target.test)
    accuracy_lda[x,y]<-conf_matrix_lda[["overall"]][["Accuracy"]]
    df_qda<-qda(df,target.train)
    pred_qda <- predict(df_qda,df.test)
    conf_matrix_qda <- confusionMatrix(pred_qda$class, target.test)
    accuracy_qda[x,y]<-conf_matrix_qda[["overall"]][["Accuracy"]]
  }
}


#################### Visualize #######################
for (x in 1:28){
for (y in 1:28){
dx=0.01
dy=dx
df=features[,c(x,y)]
min_x=min(df[,1])
max_x=max(df[,1])
min_y=min(df[,2])
max_y=max(df[,2])
x_domain=seq(min_x,max_x,dx)
y_domain=seq(min_y,max_y,dy)
z=as.matrix(expand.grid(x_domain,y_domain),0)
m=length(x_domain)
n=length(y_domain)
df_pred=predict(df_lda,z)$class
plot(df[,1:2],col=target,pch=20,cex=1.5,cex.lab=1.4)
contour(x_domain,y_domain,matrix(df_pred,m,n),levels=0.5, add=TRUE, d=FALSE, lty=2)
}
}

