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
    tabPanel(
      "Databases",
      tags$h3("Databases"),
      tags$hr()
    ),
    
    ## Incidence ------ 
    tabPanel(
      "LCA clustering",	  
      tags$h3("LCA Clustering"),
      tags$h5("Plots are shown below...."),
      tags$hr(),
      div(
        style="display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "clust_cdm_name",
          label = "Select database",
          choices = names(dataModel),
          selected = names(dataModel)[1],
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"),
          multiple = FALSE)
      ),
      tags$hr(),
      tags$h5("Cluster settings"),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "clust_model",
            label = "Select model",
            choices = models,
            selected = models[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"),
            multiple = FALSE)
      ),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Characterization", 
          DTOutput('tbl_characteristics') %>% withSpinner()
        ), 
        tabPanel(
          "Symptoms",
          plotlyOutput('plot_cluster', height = "800px") %>% withSpinner() )
      )
    ),
    tabPanel(
      "LCA clustering all countries",	  
      tags$h3("LCA Clustering"),
      tags$h5("Plots are shown below...."),
      tags$hr(),
      tags$h5("Cluster settings"),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "clust_model_all_countries",
            label = "Select model",
            choices = models,
            selected = models[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"),
            multiple = FALSE)
      ),
      plotOutput('plot_cluster_all_countries', height = "800px") %>% withSpinner()
    ),
    tabPanel(
      "LCA clustering all models",	  
      tags$h3("LCA Clustering"),
      tags$h5("Plots are shown below...."),
      tags$hr(),
      tags$h5("Cluster settings"),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "clust_db_all",
            label = "Select country",
            choices = unlist(db),
            selected = unlist(db)[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"),
            multiple = FALSE)
      ),
      plotOutput('plot_cluster_all_models', height = "800px") %>% withSpinner()
    )
  )
)


