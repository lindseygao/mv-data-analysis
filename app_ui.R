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
  choices = all_var,
  selected = "POLITY"
)

pca_main_explore_plot <- mainPanel(
  "PCA Plot",
  plotlyOutput(outputId = "pc_plot")
)

############### MDS Panel UI #############
dist_dropOptions <- selectInput(
  inputId = "dist_method",
  label = "Method for calculating distances",
  choices = dist_options,
  selected = "euclidean"
)

df_options <- selectInput(
  inputId = "df_option",
  label = "Data frame for calculating distances",
  choices = df_choices,
  selected = "num_all"
)

c_MDS_x <- selectInput(
  inputId = "cMDS_x_choices",
  label = "Choices for x axis",
  choices = find_cMDS_xy_choices(find_cMDS("num_all", "euclidean"))
)

c_MDS_y <- selectInput(
  inputId = "cMDS_y_choices",
  label = "Choices for y axis",
  choices = find_cMDS_xy_choices(find_cMDS("num_all", "euclidean"))
)

cMDS_plot <- mainPanel(
  "Classical MDS Plot",
  # plotlyOutput()
)

mds_tabPanel <- tabPanel(
  "MDS",
  sidebarLayout(
    sidebarPanel(
      # put controls here
      dist_dropOptions,
      df_options,
      c_MDS_x,
      c_MDS_y
    ),
    cMDS_plot
  )
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


ui <- navbarPage(
  "Multivariate Data Analysis Methods",
  pca_tab_panel,
  mds_tabPanel
)
