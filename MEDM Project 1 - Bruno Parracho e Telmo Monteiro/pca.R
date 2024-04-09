library(factoextra); library(foreign)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
sample_indices <- sample(1:nrow(data_frame), 10000, replace = FALSE)
data <- data_frame[sample_indices, ]
# Separate the target variable (assuming 'target_column' is the name) from predictor variables
target <- data$target
predictors <- data[, -which(names(data) == "target")]
#--------------------------------------------------------------------------------------------
#PCA
pca_result <- prcomp(predictors,scale=TRUE)  # Use scale = TRUE to standardize the variables
scores <- pca_result$x
# Summary of the PCA results
summary(pca_result)

#--------------------------------------------------------------------------------------------
# eigenvalues of each PC, for Kaiser criterion:
eigenvalues <- pca_result$sdev^2
barplot(eigenvalues, names.arg = 1:length(eigenvalues), main = "Eigenvalues of PCA",
        xlab = "Principal Components", ylab = "Eigenvalues", col = "lightblue", border = "black")
abline(h = 1, col = "red", lty = 2,lwd = 2)

#--------------------------------------------------------------------------------------------
# % explained variance in Y-axis, components in X-axis
scree_plot <- fviz_eig(pca_result,ncp = 28,addlabels=TRUE)

# to draw the vertical line of Pearson criterion:
cumulative_var <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
# Find the number of components required to explain 80% of the variance
num_components_80percent <- which(cumulative_var >= 0.8)[1]
# Create the scree plot and add a vertical line at 80% explained variance
fviz_eig(pca_result, ncp = 28, addlabels = TRUE) +
  geom_vline(xintercept = num_components_80percent, linetype = "dashed", color = "red", size = 1.5)

#--------------------------------------------------------------------------------------------
#parallel coordinates plot for the 13 first PC
library(GGally)
data_for_parallel_plot <- cbind(data.frame(target = target), scores[,1:13])
parallel_plot <- ggparcoord(data_for_parallel_plot, columns = 2:ncol(data_for_parallel_plot), groupColumn = "target")
parallel_plot <- parallel_plot + labs(title = "Parallel Coordinates Plot")
print(parallel_plot)

#--------------------------------------------------------------------------------------------
biplot <- fviz_pca_biplot(pca_result, repel = TRUE, label = "ind")
custom_labels <- data.frame(var = colnames(pca_result$rotation), x = 0, y = 0)
library(ggrepel)
biplot <- biplot + geom_text_repel(data = custom_labels, aes(x = x, y = y, label = var), size = 4, max.overlaps = Inf)
biplot <- biplot + theme(axis.title = element_text(size = 14))
biplot

#--------------------------------------------------------------------------------------------
#contribution of each variable for the first principal component according to the cos2
fviz_cos2(pca_result,choice="var",axes=1)

#--------------------------------------------------------------------------------------------
#fviz_pca_var(pca_result,col.var="cos2",repel=TRUE)
