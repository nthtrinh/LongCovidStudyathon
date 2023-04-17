#### PACKAGES -----
options(encoding = "UTF-8")

# Probably need less
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
# library(ggthemes)
library(plotly)
library(here)
library(scales)
library(dplyr)
library(tibble)
library(stringr)
library(tidyr)
library(epitools)
#library(ggalt)

#### UI -----
ui <- fluidPage(
  theme = shinytheme("spacelab"),

  # title ------
  # shown across tabs
  titlePanel("Long Covid and PASC Studyathon"),

  # set up: pages along the side -----
  navlistPanel(


    ## Introduction  -----
    tabPanel(
      "Background",
      tags$h3("Background"),
      tags$hr(),
      tags$h4(tags$strong("Please note, the results presented here should be considered as 
                       preliminary and subject to change.")),
      tags$hr(),
      # tags$h5("This app is a companion to the study ...."),
      HTML("<br>"),
      tags$h4("Long Covid and PASC Studyathon"),
      tags$hr()
    ),
    
    ## Incidence ------ 
    tabPanel("LCA clustering",	  
             tags$h3("LCA Clustering"),
             tags$h5("Plots are shown below...."),
             tags$hr(),
             tags$h5("Database"),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 pickerInput(inputId = "clustering_database_name_selector",
                             label = "Database",
                             choices = unique(cluster_data$database_name),
                             selected = unique(cluster_data$database_name),
                             options = list(
                               `actions-box` = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"),
                             multiple = TRUE)
             ),
             tags$hr(),
             tags$h5("Cluster settings"),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 pickerInput(inputId = "clustering_num_clust_selector",
                             label = "Number of clusters",
                             choices = unique(cluster_data$num_clust),
                             selected = 4,
                             options = list(
                               `actions-box` = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"),
                             multiple = TRUE)
             ),
             tabsetPanel(type = "tabs",
                         tabPanel("Table of information", 
                             # output info clusters     DTOutput('tbl_incidence_estimates') %>% withSpinner()
                             ), 
                         tabPanel("Plot of clusters",
                                  tags$hr(),
                                  plotlyOutput('plot_cluster', height = "800px") %>% withSpinner() )
             )
    )
  )
)


