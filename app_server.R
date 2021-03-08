# server file for shiny web page
library(shiny)
library(tidyverse)
library(stats) # for pca
library(plotly)
library(RColorBrewer)
all_ord_df <- read.csv("ordinal_df.csv")
ratio_df <- read.csv("ratio_df.csv")
sm_ord_df <- read.csv("sm_ordinal_df.csv")

all_df <- read.csv("all_df.csv") # initial main data frame
num_all <- all_df[, -1:-2] # main df w/ only num values to use analysis functions

### Data frames for widgets ###
country_names <- all_df$Country_Standard # list of all country names
all_var <- names(Filter(is.numeric, all_df)[, -1]) # list of feature names of `all_df`


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

# init_graph <- graph_PCs(init_PCs, "PC1", "PC2", "POLITY")
# Returns a vector of principal component options for a given df of PCs
# `PCs`: df of PCs
# Later used for the drop down options
find_pc_options <- function(PCs) {
  pc_choices <- names(PCs)
}

########################### MDS Script ###############################

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
  
  
}

