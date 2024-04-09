setwd("C:/Users/telmo/OneDrive/Ambiente de Trabalho/MEDM Project 1/") 
library(factoextra); library(foreign)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
sample_indices <- sample(1:nrow(data_frame), 10000, replace = FALSE)
data <- data_frame[sample_indices, ]
# Separate the target variable (assuming 'target_column' is the name) from predictor variables
target <- data$target
predictors <- data[, -which(names(data) == "target")]

#----------------------------------------------------------------------------------------------------------
pca_result <- prcomp(predictors,scale=TRUE)
scores <- pca_result$x
eigenvalues <- pca_result$sdev^2
retain_components <- which(eigenvalues > 1)
PCA_13_first <- pca_result$x[, retain_components]

#----------------------------------------------------------------------------------------------------------
#K-means clustering
#determining optimal number of clusters
fviz_nbclust(PCA_13_first, kmeans, method = "wss",k.max = 15)+
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
fviz_nbclust(PCA_13_first, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
fviz_nbclust(PCA_13_first, kmeans, nstart = 25,  method = "gap_stat", nboot = 20)+
  labs(subtitle = "Gap statistic method")+
  geom_vline(xintercept = 2, linetype = 3)

cl= kmeans(PCA_13_first, 2, nstart=1, iter.max=20)
print(cl)
# Within cluster sum of squares by cluster:
#[1]  51284.7 124717.5
#(between_SS / total_SS =  11.9 %)
print(cl$size) # 1645 8355

#library(cluster)
#clusplot(PCA_13_first, cl$cluster, color = TRUE, shade = TRUE, lines = 0, 
#         main="ClusPlot of first 2 PC",plotchar = FALSE ,col.p = c("dark blue","red"))

library(lattice)
splom(PCA_13_first,col=cl$cluster+2, main="K-means clustering with K=2 for 13 PC")

#plot very similar to clusplot
fviz_cluster(cl, data = predictors,palette = c("#00AFBB", "#E7B800"), 
             geom = "point",ellipse.type = "convex",ggtheme = theme_bw())


# Create a data frame with Cluster and target columns
data_for_contingency <- data.frame(Cluster = cl$cluster, Target = target)
contingency_table <- table(data_for_contingency)
contingency_table

#----------------------------------------------------------------------------------------------------------
# K-medoids clustering - ignore for now
#determining optimal number of clusters
library(cluster)
library(factoextra)

fviz_nbclust(PCA_13_first, cluster::pam, method = "wss",k.max = 10)+
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
fviz_nbclust(PCA_13_first, cluster::pam, method = "silhouette")+
  labs(subtitle = "Silhouette method")
fviz_nbclust(PCA_13_first, cluster::pam, nstart = 1,  method = "gap_stat", nboot = 10)+
  labs(subtitle = "Gap statistic method")


df <- scale(predictors)
cl_m = pam(df, k=2, metric = "euclidean", stand = FALSE)

fviz_cluster(cl_m, data = df, geom=c("point"))


fviz_nbclust(df, pam, method = "wss")

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df,
                    FUN = pam,
                    K.max = 5, #max clusters to consider
                    B = 10) #total bootstrapped iterations

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)