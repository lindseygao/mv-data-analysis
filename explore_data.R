# Explore data
library(tidyverse)
#library(stats)
#library(psych) # for factor.pa()
all_df <- read.csv("all_df.csv")
all_ord_df <- read.csv("ordinal_df.csv")
ratio_df <- read.csv("ratio_df.csv")
sm_ord_df <- read.csv("sm_ordinal_df.csv")

num_df <- all_df[, -2]

# Exploratory PCA 
pca <- prcomp(num_df, scale. = TRUE, tol = .1)
pc <- pca$x
sd <- pca$sdev
loadings <- pca$rotation
covar <- diag(sd^2)
pc <- as.data.frame(pc)
pc_choices <- names(pc)


# pc$demo_free <- df$POLITY
# ggplot(data = pc, mapping = aes(x = PC1, y = PC2, color = demo_free)) + geom_point()

# #Classical MDS cmdscale()
# dist_df <- dist(num_df, method = "euclidean")
# c_mds <- cmdscale(dist_df, k = 10, eig = T)
# 
# 
# points <- c_mds$points # rows give the coordinates of the points chosen to represent dissimilarities
# mds <- as.data.frame(points)
# # ggplot(data = mds, mapping = aes(x = V1,  y= V3)) + geom_point()
# # Plotting V1 against V2 gives an "elbow" 
# 
# # principal axis factoring : fa() fac() fa.sapa()

