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
library(dendextend)
#method = Ward2, distance = euclidian
hierarchical <- hclust(dist(PCA_13_first), method = "ward.D2")
# Convert the dendrogram to a dendrogram object
dend <- as.dendrogram(hierarchical)
# Remove the labels
labels(dend) <- NULL
plot(dend, center = FALSE, main = "Hierarchical Clustering Dendrogram with Ward2 method", 
     xlab = "Data Points (distance = Euclidian)")

#method = Ward2, distance = manhattan
hierarchical <- hclust(dist(PCA_13_first,method="manhattan"), method = "ward.D2")
dend <- as.dendrogram(hierarchical)
labels(dend) <- NULL
plot(dend, center = FALSE, main = "Hierarchical Clustering Dendrogram with Ward2 method", 
     xlab = "Data Points (distance = Manhattan)")

#method = Ward2, distance = maximum
hierarchical <- hclust(dist(PCA_13_first,method="maximum"), method = "ward.D2")
dend <- as.dendrogram(hierarchical)
labels(dend) <- NULL
plot(dend, center = FALSE, main = "Hierarchical Clustering Dendrogram with Ward2 method", 
     xlab = "Data Points (distance = Chebyshev)")
#-----------------------------------------
#method = complete, distance = euclidian
hierarchical <- hclust(dist(PCA_13_first), method = "complete")
dend <- as.dendrogram(hierarchical)
labels(dend) <- NULL
plot(dend, center = FALSE, main = "Hierarchical Clustering Dendrogram with complete method", 
     xlab = "Data Points (distance = Euclidian)")

#method = complete, distance = manhattan
hierarchical <- hclust(dist(PCA_13_first,method="manhattan"), method = "complete")
dend <- as.dendrogram(hierarchical)
labels(dend) <- NULL
plot(dend, center = FALSE, main = "Hierarchical Clustering Dendrogram with complete method", 
     xlab = "Data Points (distance = Manhattan)")

#method = complete, distance = maximum
hierarchical <- hclust(dist(PCA_13_first,method="maximum"), method = "complete")
dend <- as.dendrogram(hierarchical)
labels(dend) <- NULL
plot(dend, center = FALSE, main = "Hierarchical Clustering Dendrogram with complete method", 
     xlab = "Data Points (distance = Chebyshev)")
#-----------------------------------------
#method = average, distance = euclidian
hierarchical <- hclust(dist(PCA_13_first), method = "average")
dend <- as.dendrogram(hierarchical)
labels(dend) <- NULL
plot(dend, center = FALSE, main = "Hierarchical Clustering Dendrogram with average method", 
     xlab = "Data Points (distance = Euclidian)")

#method = average, distance = manhattan
hierarchical <- hclust(dist(PCA_13_first,method="manhattan"), method = "average")
dend <- as.dendrogram(hierarchical)
labels(dend) <- NULL
plot(dend, center = FALSE, main = "Hierarchical Clustering Dendrogram with average method", 
     xlab = "Data Points (distance = Manhattan)")

#method = average, distance = maximum
hierarchical <- hclust(dist(PCA_13_first,method="maximum"), method = "average")
dend <- as.dendrogram(hierarchical)
labels(dend) <- NULL
plot(dend, center = FALSE, main = "Hierarchical Clustering Dendrogram with average method", 
     xlab = "Data Points (distance = Chebyshev)")

#------------------------------------------------------------------------------------------------------------
library(pheatmap)
#heatmaps for 13 PCA with different methods and distance euclidian
pheatmap(PCA_13_first, main = "Heatmap of first 13 PC, method=complete, distance=euclidian",
         show_rownames=FALSE)
pheatmap(PCA_13_first, main = "Heatmap of first 13 PC, method=average, distance=euclidian",
         show_rownames=FALSE,clustering_method = "average")
pheatmap(PCA_13_first, main = "Heatmap of first 13 PC, method=ward.D2, distance=euclidian",
         show_rownames=FALSE,clustering_method = "ward.D")

#same but with distance manhattan
pheatmap(PCA_13_first, main = "Heatmap of first 13 PC, method=complete, distance=manhattan",
         show_rownames=FALSE,clustering_distance_rows="manhattan", clustering_distance_cols="manhattan")
pheatmap(PCA_13_first, main = "Heatmap of first 13 PC, method=average, distance=manhattan", show_rownames=FALSE,
         clustering_method = "average",clustering_distance_rows="manhattan", clustering_distance_cols="manhattan")
pheatmap(PCA_13_first, main = "Heatmap of first 13 PC, method=ward.D2, distance=manhattan",show_rownames=FALSE,
         clustering_method = "ward.D",clustering_distance_rows="correlation", clustering_distance_cols="manhattan")

#same but with distance maximum
pheatmap(PCA_13_first, main = "Heatmap of first 13 PC, method=complete, distance=maximum",
         show_rownames=FALSE,clustering_distance_rows="maximum", clustering_distance_cols="maximum")
pheatmap(PCA_13_first, main = "Heatmap of first 13 PC, method=average, distance=maximum", show_rownames=FALSE,
         clustering_method = "average",clustering_distance_rows="maximum", clustering_distance_cols="maximum")
pheatmap(PCA_13_first, main = "Heatmap of first 13 PC, method=ward.D2, distance=maximum",show_rownames=FALSE,
         clustering_method = "ward.D",clustering_distance_rows="maximum", clustering_distance_cols="maximum")


#heatmaps for original variables with different methods and distance euclidian
#for comparison with the heatmaps done with the principal components
pheatmap(predictors_scale, main = "Heatmap of original variables, method=ward.D2, distance=euclidian",
         show_rownames=FALSE,clustering_method = "ward.D2")