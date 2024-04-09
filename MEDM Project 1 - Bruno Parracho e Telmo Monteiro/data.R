library(foreign)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
# sample 10000 rows from the data set
sample_indices <- sample(1:nrow(data_frame), 10000, replace = FALSE)
data <- data_frame[sample_indices, ]
# Summary of the dataset
sum <- summary(data)
sum
#----------------------------------------------------------------------------
# heatmap of correlation between variables
library(tidyr)
# Compute the correlation matrix
cor_variables <- cor(predictors)
# Melt the correlation matrix into long format
cor_variables_long <- as.data.frame(as.table(cor_variables))
# Create a correlation heatmap
ggplot(cor_variables_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed(ratio = 1)