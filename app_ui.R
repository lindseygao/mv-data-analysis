# Interactive webpage for exploratory data analysis
library(shiny)
source("app_server.R")

############# PCA Panel UI ############
limit_PC_button <- radioButtons(
  inputId = "limit_pc_way",
  label = "Limit number of principal components by:",
  choices = list("Cumulative proportion of total variance captured by all principal components" = 3,
                 "Minimum proportion of variance captured by 1 principal component" = 2
                 ),
  selected = 3
)

limit_PC_slider <- sliderInput(
  inputId = "PC_limit_range",
  label = "Limit Range",
  value = .35,
  min = 0,
  max = 0
)

pc_choices_x <- selectInput(
  inputId = "pc_x",
  label = "Choose Principal Component on x-axis",
  choices = options_by_summary(summary_by(.35, 3)),
  selected = "PC1"
)
pc_choices_y <- selectInput(
  inputId = "pc_y",
  label = "Choose Principal Component on y-axis",
  choices = options_by_summary(summary_by(.35, 3)),
  selected = "PC2"
)

pc_explore_var <- selectInput(
  inputId = "pc_color",
  label = "Choose variable to color",
  choices = var_options,
  selected = "POLITY"
)

pca_main_explore_plot <- mainPanel(
  "PCA Plot",
  plotlyOutput(outputId = "pc_plot")
)

pca_tab_panel <- tabPanel(
  "PCA",
  sidebarLayout(
    sidebarPanel(
      # put controls here
      limit_PC_button,
      limit_PC_slider,
      pc_choices_x,
      pc_choices_y,
      pc_explore_var
    ),
    pca_main_explore_plot
  )
)

############### MDS Panel UI #############
dist_dropOptions <- selectInput(
  inputId = "dist_method",
  label = "Distance Method for cMDS",
  choices = dist_options,
  selected = "euclidean"
)

df_options <- selectInput(
  inputId = "df_option",
  label = "Data Frame for cMDS",
  choices = df_choices,
  selected = "num_all"
)

nMDS_dist <- selectInput(
  inputId = "iso_dist",
  label = "Distance Method for nMDS",
  choices = dist_options,
  selected = "euclidean"
)

nMDS_df <- selectInput(
  inputId = "iso_df",
  label = "Data Frame for nMDS",
  choices = df_choices,
  selected = "num_all"
)

MDS_plots <- mainPanel(
  h3("Classical MDS Plot"),
  plotlyOutput(outputId = "cMDS_plot"),
  h3("Non-metric MDS Plot (Kruskal)"),
  plotlyOutput(outputId = "nMDS_plot")
)

mds_tabPanel <- tabPanel(
  "MDS",
  sidebarLayout(
    sidebarPanel(
      # cMDS controls
      dist_dropOptions,
      df_options,
      # nMDS controls
      nMDS_dist,
      nMDS_df
    ),
    MDS_plots
  )
)

################### K-Means Cluster Analysis UI #######################
km_cMDS_dist <- selectInput(
  inputId = "km_cMDS_dist",
  label = "Distance Method for cMDS",
  choices = dist_options,
  selected = "euclidean"
)

km_nMDS_dist <- selectInput(
  inputId = "km_nMDS_dist",
  label = "Distance Method for nMDS",
  choices = dist_options,
  selected = "euclidean"
)

km_plots <- mainPanel(
  h3("Clusters on Principal Component Axes"),
  plotlyOutput(outputId = "km_pc_plot"),
  h3("Clusters on Classical MDS Axes"),
  plotlyOutput(outputId = "km_cMDS_plot"),
  h3("Clusters on Non-Metric MDS Axes"),
  plotlyOutput(outputId = "km_nMDS_plot")
)

km_k_selection <- radioButtons(
  inputId = "k_num",
  label = "Number of Clusters for K-Means",
  choices = list(2,3),
  selected = 2
)

km_tabPanel <- tabPanel(
  "K-Means Clustering",
  sidebarLayout(
    sidebarPanel(
      km_k_selection,
      km_cMDS_dist,
      km_nMDS_dist
    ),
    km_plots
  )
)


################### Model-based Cluster Analysis UI ###################
mc_cMDS_dist <- selectInput(
  inputId = "mc_cMDS_dist",
  label = "Distance Method for cMDS",
  choices = dist_options,
  selected = "euclidean"
)

mc_nMDS_dist <- selectInput(
  inputId = "mc_nMDS_dist",
  label = "Distance Method for nMDS",
  choices = dist_options,
  selected = "euclidean"
)

mclust_plots <- mainPanel(
  h3("Clusters on Principal Component Axes"),
  plotlyOutput(outputId = "mc_pc_plot"),
  h3("Clusters on Classical MDS Axes"),
  plotlyOutput(outputId = "mc_cMDS_plot"),
  h3("Clusters on Non-Metric MDS Axes"),
  plotlyOutput(outputId = "mc_nMDS_plot")
)



mclust_tabPanel <- tabPanel(
  "Model-Based Clustering",
  sidebarLayout(
    sidebarPanel(
      mc_cMDS_dist,
      mc_nMDS_dist
    ),
    mclust_plots
  )
)


ui <- navbarPage(
  "Multivariate Data Analysis Methods",
  pca_tab_panel,
  mds_tabPanel,
  km_tabPanel,
  mclust_tabPanel
)



# c_MDS_x <- selectInput(
#   inputId = "cMDS_x_choices",
#   label = "Choices for x axis",
#   choices = find_cMDS_xy_choices(find_cMDS("num_all", "euclidean"))
# )
# 
# c_MDS_y <- selectInput(
#   inputId = "cMDS_y_choices",
#   label = "Choices for y axis",
#   choices = find_cMDS_xy_choices(find_cMDS("num_all", "euclidean"))
# )
