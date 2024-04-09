setwd("C:/Users/telmo/OneDrive/Ambiente de Trabalho/MEDM Project 1/")      # Change working directory
library(factoextra); library(foreign); library(fields)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
sample_indices <- sample(1:nrow(data_frame), 10000, replace = FALSE)
data <- data_frame[sample_indices, ]
# Separate the target variable (assuming 'target_column' is the name) from predictor variables
target <- data$target
predictors <- data[, -which(names(data) == "target")]

pca_result <- prcomp(predictors,scale=TRUE)
scores <- pca_result$x
eigenvalues <- pca_result$sdev^2
retain_components <- which(eigenvalues > 1)
PCA_13_first <- pca_result$x[, retain_components]

predictors_scale <- scale(predictors, center = TRUE, scale = TRUE) #scaling the original variables

#------------------------------------------------------------------------------------------------------------
library(mclust)
# BIC
BIC <- mclustBIC(PCA_13_first,G=1:13)
plot(BIC)
summary(BIC)

gmm_bic <- Mclust(PCA_13_first, x = BIC, G = 1:13)
summary(gmm_bic, parameters = TRUE)
plot(mgmm_bic, what = "classification")
table(target, gmm_bic$classification)

#---------------------------------------------------------------------------------------------------
# fit the GMM model using the EM algorithm, but for just 2 principal components
BIC <- mclustBIC(PCA_13_first,G=1:2)
plot(BIC)
summary(BIC)

gmm_model <- Mclust(PCA_13_first, x = BIC, G=1:2)
summary(gmm_model, parameters = TRUE)
# Plot the data points colored by cluster assignments
cluster_assignments <- unclass(gmm_model$classification)
plot(PCA_13_first, col = cluster_assignments, pch = 19, main="Mclust classification for first 2 PC")
plot(gmm_model, what = "classification")