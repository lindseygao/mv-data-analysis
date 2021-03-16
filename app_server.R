# server file for shiny web page
library(shiny)
library(tidyverse)
library(stats) # for pca
library(plotly)
library(RColorBrewer)
library(MASS) # for isoMDS() - kruskal's nMDS
library(caret)
library(plotly)
library(mclust)
all_ord_df <- read.csv("ordinal_df.csv")
ratio_df <- read.csv("ratio_df.csv")
sm_ord_df <- read.csv("sm_ordinal_df.csv")
all_df <- read.csv("all_df.csv") # initial main data frame

num_all <- all_df[, -1:-2] # main df w/ only num values to use analysis functions
num_all_ord <- all_ord_df[,-1:-2]
# num_sm_ord <- sm_ord_df[,-1:-2]
num_ratio <- ratio_df[-1:-2]



### Data frames for widgets ###
country_names <- all_df$Country_Standard # list of all country names
# all_var <- names(Filter(is.numeric, num_all)) # list of feature names of `num_all`
max_pc1 <- as.data.frame(abs(pca$rotation)) %>%
  dplyr::arrange(-PC1) %>%
  head(20) %>% 
  row.names() %>% 
  as.list()

var_options <- max_pc1[-c(12,19)] %>% unlist()
additional_var <- c("POLITY", "PARTICIP", "CO2GDP", "ISO14", "POPGRTH", "DAIRY", "MEATS")
var_options <- c(var_options, additional_var) %>% sort()

############################ Exploratory PCA Script ################################
pca <- prcomp(num_all, scale. = TRUE, tol = .05)
pc <- as.data.frame(pca$x)

# sd <- pca$sdev
# loadings <- pca$rotation # matrix of (column) eigen-vectors
# covar <- diag(sd^2)
pc_choices <- names(pc)


PC_importance <- summary(pca)$importance
PC_importance_df <- as.data.frame(PC_importance)


##### PCA Widget Helpers #####
# Way: number => 2 or 3
# 2 -> for single PC
# 3 -> for prop of total variance
limit_PC_slider_label <- function(way) {
  if (way == 2) {
    return("Minimum proportion of variance (single principal component)")
  } else if (way == 3) {
    return("Minimum proportion of cumulative variance")
  }
}

# `min_or_max`: number - either 0 or 1
  # 0 -> min
  # 1 -> max
# way: number - 2 or 3
PC_slider_range <- function(min_or_max, way) {
  if (min_or_max == 0) {
    # looking for min boundary
    if (way == 2) {
      return(.01)
    } else {
      return(.32)
    }
  } else if (min_or_max == 1) {
    if (way == 2) {
      return(.07)
    } else {
      return(1)
    }
  }
}

PC_slider_step <- function(way) {
  if (way == 2) {
    step <- .005
  } else {
    step <- .01
  }
}

#### PCA Summary Helpers ####
# Finds the summary df: Standard deviation, Proportion of Variance, Cumulative Proportion
# for all PC's that fulfill `prop` requirement
# `prop`: numerical value between 0-1 that filter the data
# `way`: binary (0 || 1) number representing which method to filter
  # 2 represents by single PC's variance prop (2nd row)
  # 3 represents by cumulative proportion prop (3rd row)
summary_by <- function(prop, way) {
    # 2nd row = `Proportion of Variance`
    # 3rd row = `Cumulative Proportion`
  way <- as.numeric(way)
  prop <- as.numeric(prop)
  if (way == 2) {
    result <- PC_importance_df %>% select_if(PC_importance_df[way,] >= prop)
  } else if (way == 3) {
    result <- PC_importance_df %>% select_if(PC_importance_df[way,] <= prop)
  }
}

# Returns PC options given a summary data frame calculated from `summary_by` function
options_by_summary <- function(summary_df) {
  PC_options <- colnames(summary_df)
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

# Graphs/Returns 2 principal components
# `PCs`: principal component matrix
# `PC_x`, `PC_y': string label of PC on x-axis, y-axis; ex: "PC1"
# `explore_var`: variable to color points
graph_PCs <- function(PCs, PC_x, PC_y, explore_var) {
  # graph_PC <- PCs
  PCs$country <- country_names
  PCs[[explore_var]] <- all_df[[explore_var]]
  pc_plot <- ggplot(data = PCs, mapping = aes_string(x = PC_x, y = PC_y, color = explore_var, text = "country")) + 
    geom_point() +
    scale_color_gradientn(colors = c("red", "orange", "yellow", "green", "blue"))
  pc_plotly <- ggplotly(pc_plot, tooltip = "text")
}

# Returns a vector of principal component options for a given df of PCs
# `PCs`: df of PCs
# Later used for the drop down options
find_pc_options <- function(PCs) {
  pc_choices <- names(PCs)
}

########################### MDS Script ###############################

######## cMDS ########
df_choices <- c("Data frame containing all features" = "num_all",
                "Data frame containing all ordinal features" = "num_all_ord",
                "Data frame containing all ratio variables" = "num_ratio")
map_df_choices <- list("num_all" = num_all, "num_all_ord" = num_all_ord, "num_ratio" = num_ratio)
df <- map_df_choices[["num_all"]]


# Returns the initial mds df object from using cmdscale()
# `df`: string representing the name of data frame to perform MDS
# `dist_method`: (string) declaring method for calculating distances (ex: "euclidean")
# `dim`: (int) max dimension of space to represent the data (1 <= dim <= # columns(df) - 1)
find_cMDS <- function(df, dist_method) {
  df <- map_df_choices[[df]]
  dist_df <- dist(df, method = dist_method)
  c_mds <- cmdscale(dist_df, k = 10, eig = T) # includes eigenvalues
}

dist_options <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

# find_cMDS_xy_choices <- function(c_mds) {
#   choices <- names(c_mds$points)
# }

plot_cMDS <- function(c_mds) {
  dis_points <- as.data.frame(c_mds$points) # rows give the coordinates of the points chosen to represent dissimilarities
  plot <- ggplot(data = dis_points, mapping = aes(x = V1, y = V2, text = country_names)) + 
    geom_point()
  ggplotly(plot, tooltip = "text")
}

###### nMDS ######
find_nMDS <- function(df, dist_method) {
  df <- map_df_choices[[df]]
  dist_df <- dist(df, method = dist_method)
  isoMDS <- isoMDS(dist_df, k = 2, maxit = 1000, trace = F, tol = 1e-3, p =2)
}

plot_isoMDS <- function(isoMDS) {
  # df <- map_df_choices[[df]]
  # dist_df <- dist(df, method = dist_method)
  # print(dist_df)
  iso_points <- as.data.frame(isoMDS$points)
  plot <- ggplot(iso_points, aes(x =V1, y = V2, text = country_names)) + 
    geom_point()
  ggplotly(plot, tooltip = "text")
}

####################### K-means Clustering Script ############################
##### K means with all variables data frame #####

# standardize each variable by its range
rge <- sapply(num_all, function(x) diff(range(x)))
num_all_s <- sweep(num_all, 2, rge, FUN = "/")

find_km_sol <- function(k) {
  km_sol <- kmeans(num_all_s, centers = k)
}

plot_km_pc <- function(km_object) {
  km_pc_plot <- ggplot(data = pc, mapping = aes(x = PC1, y = PC2, color = km_object$cluster, text = country_names)) + 
    geom_point(alpha = .5)
  ggplotly(km_pc_plot, tooltip = "text")
}


# Plotting cluster results against cMDS 
plot_km_cMDS <- function(km_obj, dist_method) {
  cMDS_obj <- find_cMDS("num_all", dist_method = dist_method)
  points <- as.data.frame(cMDS_obj$points)
  km_cMDS_plot <- ggplot(data = points, mapping = aes(x = V1,  y= V2, color = km_obj$cluster, text = country_names)) + 
    geom_point(alpha = .5)
  ggplotly(km_cMDS_plot, tooltip = "text")
}

# Plotting cluster results against nMDS
plot_km_nMDS <- function(km_obj, dist_method) {
  nMDS_obj <- find_nMDS("num_all", dist_method = dist_method)
  points <- as.data.frame(nMDS_obj$points)
  km_nMDS_plot <- ggplot(data = points, mapping = aes(x = V1, y = V2, color = km_obj$cluster, text = country_names)) +
    geom_point(alpha = .5)
  ggplotly(km_nMDS_plot, tooltip = "text")
}

################### Model Based Clustering Script ###############
mc <- Mclust(num_ratio, 14)

plot_mclust_pc <- function() {
  mc_pc_plot <- ggplot(data = pc, mapping = aes(x = PC1, y = PC2, color = as.factor(mc$classification), text = country_names)) + 
    geom_point(alpha = .5)
  ggplotly(mc_pc_plot, tooltip = "text")
}

plot_mclust_cMDS <- function(dist_method) {
  cMDS_obj <- find_cMDS("num_all", dist_method = dist_method)
  points <- as.data.frame(cMDS_obj$points)
  mc_cMDS_plot <- ggplot(data = points, mapping = aes(x = V1, y = V2, color = as.factor(mc$classification), text = country_names)) +
    geom_point(alpha = .5)
  ggplotly(mc_cMDS_plot, tooltip = "text")
}

plot_mclust_nMDS <- function(dist_method) {
  nMDS_obj <- find_nMDS("num_all", dist_method = dist_method)
  points <- as.data.frame(nMDS_obj$points)
  mc_nMDS_plot <- ggplot(data = points, mapping = aes(x = V1, y = V2, color = as.factor(mc$classification), text = country_names)) +
    geom_point(alpha = .5)
  ggplotly(mc_nMDS_plot, tooltip = "text")
}

################### Server #####################3 
server <- function(input, output, session) {
  ############# PCA ###############
  output$pc_plot <- renderPlotly({ # PCA plot
    plot <- graph_PCs(pc, toString(input$pc_x), toString(input$pc_y), input$pc_color)
    plot
  })
  # use input PCA data to find necessary data to graph (dynamic)
  observeEvent(input$limit_pc_way, {
    updateSliderInput(
      session,
      inputId = "PC_limit_range",
      label = limit_PC_slider_label(input$limit_pc_way),
      min = PC_slider_range(0, input$limit_pc_way),
      max = PC_slider_range(1, input$limit_pc_way),
      step = PC_slider_step(input$limit_pc_way)
    )
  })
  observeEvent(input$PC_limit_range, {
    updateSelectInput(
      session,
      inputId = "pc_x",
      choices = options_by_summary(summary_by(input$PC_limit_range, input$limit_pc_way))
    )
    updateSelectInput(
      session,
      inputId = "pc_y",
      choices = options_by_summary(summary_by(input$PC_limit_range, input$limit_pc_way))
    )
  })
  ############### MDS ###############
  output$cMDS_plot <- renderPlotly({
    plot <- plot_cMDS(find_cMDS(input$df_option, input$dist_method))
    plot
  })
  
  observeEvent(input$iso_dist, {
    if (input$iso_dist == "binary") {
      updateSelectInput(
        session,
        inputId = "iso_df",
        choices = df_choices[-2]
      )
    } else {
      updateSelectInput(
        session,
        inputId = "iso_df",
        choices = df_choices
      )
    }
  })
  output$nMDS_plot <- renderPlotly({
    plot <- plot_isoMDS(find_nMDS(input$iso_df, input$iso_dist))
    plot
  })
  ############### K-Means Plots ############
  output$km_pc_plot <- renderPlotly({
    plot <- plot_km_pc(find_km_sol(input$k_num))
    plot
  })
  output$km_cMDS_plot <- renderPlotly({
    plot <- plot_km_cMDS(find_km_sol(input$k_num), input$km_cMDS_dist)
    plot
  })
  output$km_nMDS_plot <- renderPlotly({
    plot <- plot_km_nMDS(find_km_sol(input$k_num), input$km_nMDS_dist)
    plot
  })
  
  ############### Mclust Plots #############
  output$mc_pc_plot <- renderPlotly({
    plot <- plot_mclust_pc()
    plot
  })
  output$mc_cMDS_plot <- renderPlotly({
    plot <- plot_mclust_cMDS(input$mc_cMDS_dist)
    plot
  })
  output$mc_nMDS_plot <- renderPlotly({
    plot <- plot_mclust_nMDS(input$mc_nMDS_dist)
    plot
  })
}


# observe({req(input$dist_method, input$df_option)
#   updateSelectInput(
#     session,
#     inputId = "cMDS_x_choices",
#     choices = find_cMDS_xy_choices(find_cMDS(input$df_option, input$dist_method))
#   )
#   updateSelectInput(
#     session,
#     inputId = "cMDS_y_choices",
#     choices = find_cMDS_xy_choices(find_cMDS(input$df_option, input$dist_method))
#   )
# })
