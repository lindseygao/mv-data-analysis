# Explore data
library(tidyverse)
library(stats)
library(psych) # for factor.pa()
library(MASS) # for isoMDS() - kruskal's nMDS
library(caret)
library(plotly)
library(mclust) # for finite mixture density clustering
all_df <- read.csv("all_df.csv")
all_ord_df <- read.csv("ordinal_df.csv")
ratio_df <- read.csv("ratio_df.csv")
sm_ord_df <- read.csv("sm_ordinal_df.csv")

country_names <- all_df[, 2]
num_df <- all_df[, -2]
num_df <- num_df[, -1]
all_matrix <- as.matrix(all_df)
data("voting", package = "HSAUR2")
##### Initial Exploratory PCA (see ShinyApp for more in depth exploration)
pca <- prcomp(num_df, scale. = TRUE, tol = .05)
pc <- pca$x
sd <- pca$sdev
loadings <- pca$rotation # matrix of (column) eigenvectors
covar <- diag(sd^2)
pc <- as.data.frame(pc)
pc_choices <- names(pc)

PC_importance <- summary(pca)$importance
PC_importance_df <- as.data.frame(PC_importance)
energy <- as.list(as.data.frame(PC_importance)[3,]) # Cumulative energy

# Finds the summary df: Standard deviation, Proportion of Variance, Cumulative Proportion
# for all PC's that fulfill `prop` requirement
# `prop`: numerical value between 0-1 that filter the data
# `way`: binary (0 || 1) number representing which method to filter
# 0 represents by single PC's variance prop
# 1 represents by cumulative proportion prop
summary_by <- function(prop, way) {
  logical_filter <- as.data.frame(PC_importance)[way + 2,] %>% 
    # 2nd row = `Proportion of Variance`
    # 3rd row = `Cumulative Proportion`
    slice(rep(1:n(), each=3)) <= prop
  rownames(logical_filter) <- NULL
  result <- PC_importance_df[logical_filter] # result includes row names
}


# summary_by_pc_prop <- function(single_prop) {
#   logical_filter <- as.data.frame(PC_importance)[2,] %>% # 2nd row = `Proportion of Variance`
#     slice(rep(1:n(), each=3)) <= single_prop
#   row
# }

# Returns PC options given a summary data frame calculated from `summary_by` function
options_by_summary <- function(summary_df) {
  PC_options <- rownames(summary_df)
}

# Calculates the xth component score for each observation (country)
# `comp_x`: number ranging from 1-length(pca$x)
# `pca`: resulting prcomp object after applying prcomp() to df
# `df`: data frame that we are analyzing
# *Calculated by droping the pca scaling/centering to calculate the
# x component score for each country*
find_xth_comp_score <- function(comp_x, pca, df) {
  center <- pca$center
  scale <- pca$scale
  matrix_df <- as.matrix(df)
  # scale() scales the columns of a given matrix
  # each column of matrix has corresponding value from `center` subtracted
  # each column of matrix divided by corresponding value from `scale`
  # `center` & `scale` are col vectors/lists (each row corresponds to column label of original matrix)
  # Multiply scaled result by xth comp score for each country
  # drop superfluous dimensions of scaled resulting matrix
  xth_PC_score_by_country <- drop(scale(matrix_df, center = center, scale = scale) %*% pca$rotation[,comp_x])
  xth_PC_score_by_country
}

# Returns a country's score for a given PC #
# `comp_num`: Number for a representing PC number
# `country`: String representing a country's name
# Assume that the user input the country through a drop down
# Possible country choices: `country_names`
xth_score_for_country <- function(comp_num, country) {
  index_of_country <- which(country_names == country) # index of that country in list
  PC_score_by_country <- find_xth_comp_score(comp_num, pca, num_df)
  PC_score_by_country[index_of_country]
}


# *******TO DO: Find a list of variables that are interesting to look at*******
max_pc1 <- as.data.frame(abs(pca$rotation)) %>%
  dplyr::arrange(-PC1) %>%
  head(20) %>% 
  row.names() %>% 
  as.list()

var_options <- max_pc1[-c(12,19)] %>% unlist()
additional_var <- c("POLITY", "PARTICIP", "CO2GDP", "ISO14", "POPGRTH")
var_options <- c(var_options, additional_var)

pc$demo_free <- all_df$POLITY
ggplot(data = pc, mapping = aes(x = PC1, y = PC2, color = demo_free)) + geom_point()


# plot the first 3 PCs in 3D -- not useful


######### Other general exploring (PCA) ###########

# Correlation between variables rounded to 2 digits
corr_all_var <- round(cor(num_df[,1:length(num_df)]), 2)

# Plotting how much variance the PCs capture
var_pc_plot <- plot(pca, type="lines")

# Check coefficient of PC
# Look at largest coefficients & corresponding variables

############### Initial Classical MDS cmdscale() ################
dist_df <- dist(num_df, method = "maximum")
c_mds <- cmdscale(dist_df, k = 30, eig = T) # k = maximum dimension of space to represent data
max(abs(dist(num_df) - dist(cmdscale(dist_df, k = 50))))
max(abs(as.data.frame(prcomp(num_df)$x)) - abs(cmdscale(dist_df, k = 100)))

# Check how many coordinates to use
X_eigen <- c_mds$eig
cumsum(X_eigen) / sum(X_eigen) # notice the 1st 2 coordinates capture 99.87% of distances

dis_points <- as.data.frame(c_mds$points) # rows give the coordinates of the points chosen to represent dissimilarities
ggplot(data = dis_points, mapping = aes(x = V1,  y= V2)) + geom_point()
# Plotting V1 against V2 gives an "elbow"


####### Non-metric MDS isoMDS() - Kruskal's nMDS #######

### nMDS with `all_df`
all_iso <- isoMDS(dist_df) # 2-D solution using `dist_df` from cMDS
all_iso_points <- as.data.frame(all_iso$points)
all_iso_plot <- ggplot(all_iso_points, aes(x = V1, y = V2, text = country_names)) + geom_point()
ggplotly(all_iso_plot, tooltip = "text")


### nMDS with `all_ord_df`
num_ord <- all_ord_df[,-1:-2]
ord_dist <- dist(num_ord, method = "euclidean")
ord_iso <- isoMDS(ord_dist)
ord_points <- as.data.frame(ord_iso$points)
# Plotting in 2D
ord_iso_plot <- ggplot(ord_points, aes(x = V1, y = V2, text = country_names)) + geom_point()
ggplotly(ord_iso_plot, tooltip = "text")

#### Correspondence analysis??


############# Exploratory FA: Principal axis factoring => fa() ###################

# Hypothesis for # variables: PCA results?

### Use maximum-likelihood FA to estimate # factors needed 
### *FAILED* to produce useful results - number of factors could range from 1-20??
# Remove redundant features b/c matrix singular => cannot do EFA
corr_matrix <- cor(num_all)
redundant_col <- findCorrelation(corr_matrix, cutoff = .6, names = T) # columns removed
reduced_all <- num_df[!names(num_df) %in% redundant_col]
# Use 40% of data for EFA
set.seed(525)
num_obs <- nrow(reduced_all)
EFA_index <- sample(seq_len(num_obs), ceiling((num_obs * 4)/10))
EFA_data <- reduced_all[EFA_index,]
EFA_corr <- cor(EFA_data)
EFA_redun <- findCorrelation(EFA_corr, cutoff = .3, names = T) # columns removed
EFA_reduced <- EFA_data[!names(EFA_data) %in% EFA_redun]
factanal(EFA_reduced, 10, fm = "minres")
factanal(EFA_reduced, 5, fm = "wls")
factanal(reduced_all, 5, fm = "gls")
factanal(reduced_all, 15, fm = "ols")
factanal(reduced_all, 5, fm = "pa")
factanal(reduced_all, 20, fm = "minchi")

### Use minimum residual EFA
choose_num_fac <- sapply(1:20, function(f) fa(reduced_all, nfactors = f)$PVAL)
EFA_result <- fa(reduced_all, nfactors = 10, scores = "Anderson")

################# Cluster Analysis ###################
##### K means
# standardize each variable by its range
rge <- sapply(num_all, function(x) diff(range(x)))
num_all_s <- sweep(num_all, 2, rge, FUN = "/")
std_var <- sapply(num_all_s, var)

# Plot the within-groups sum of squares for one to six-group solutions
# to see if we can get any indication of the number of groups
n <- nrow(num_all_s)
wss <- rep(0, 6)
wss[1] <- (n - 1) * sum(std_var)
for (i in 2:6) {
  wss[i] <- sum(kmeans(num_all_s, centers = i)$withinss)
}
# Plot shows 2 clusters probably sufficient (maybe 3)
plot(1:6, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")
### 2 group solution:
km_sol <- kmeans(num_all_s, centers = 2)
# group means
group_m <- km_sol$centers * rge

# Plotting the 2 clusters under the 1st 2 principal components halves it along PC1
ggplot(data = pc, mapping = aes(x = PC1, y = PC2, color = km_sol$cluster)) + geom_point(alpha = .5)

# Plotting 2 cluster against cMDS
ggplot(data = dis_points, mapping = aes(x = V1,  y= V2, color = km_sol$cluster)) + geom_point(alpha = .5)

# Plotting 2 cluster against nMDS
ggplot(all_iso_points, aes(x = V1, y = V2, color = km_sol$cluster)) + geom_point(alpha = .5)


### 3 group solution:
km_sol_3 <- kmeans(num_all_s, centers = 3)
# group means
group_m3 <- km_sol_3$centers * rge

# Plotting the 3 clusters under the 1st 2 principal components halves it along PC1
ggplot(data = pc, mapping = aes(x = PC1, y = PC2, color = km_sol_3$cluster)) + geom_point()

# Plotting 3 clusters against nMDS
ggplot(all_iso_points, aes(x = V1, y = V2, color = km_sol_3$cluster)) + geom_point()


##### Finite mixture density model clustering
mc <- Mclust(num_ratio, 1:20)
mc_optimal <- Mclust(num_all, 1, modelNames = "XXX")
ggplot(data = pc, mapping = aes(x = PC1, y = PC2, color = as.factor(mc$classification))) + geom_point()


# Maybe try standardizing data??
df <- scale(num_all) # Standardize the data
mc_scale <- Mclust(df)

########### Confirmatory FA: Use interesting variables fr/ PCA for hypothesis #############
CFA_data <- reduced_all[-EFA_index,]

