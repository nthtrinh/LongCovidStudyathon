#### PACKAGES -----
options(encoding = "UTF-8")

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
    tabPanel("Population incidence",	  
             tags$h3("Incidence estimates"),
             tags$h5("Incidence estimates are shown below...."),
             tags$hr(),
             tags$h5("Database and study outcome"),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 pickerInput(inputId = "incidence_database_name_selector",
                             label = "Database",
                             choices = unique(incidence_estimates$database_name),
                             selected = unique(incidence_estimates$database_name),
                             options = list(
                               `actions-box` = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"),
                             multiple = TRUE)
             ),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 pickerInput(inputId = "incidence_outcome_name_selector",
                             label = "Outcome",
                             choices = sort(unique(incidence_estimates$outcome_name)),
                             selected = c("LC_1"),
                             options = list(
                               `actions-box` = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"),
                             multiple = TRUE)
             ),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 pickerInput(inputId = "incidence_denominator_name_selector",
                             label = "Denominator",
                             choices = sort(unique(incidence_estimates$denominator_name)),
                             selected = c("Inf"),
                             options = list(
                               `actions-box` = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"),
                             multiple = TRUE)
             ),
             tags$hr(),
             tags$h5("Population settings"),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 pickerInput(inputId = "incidence_denominator_age_group_selector",
                             label = "Age group",
                             choices = levels(as.factor(incidence_estimates$denominator_age_group)),
                             selected = "0;150",
                             options = list(
                               `actions-box` = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"),
                             multiple = TRUE)
             ),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 pickerInput(inputId = "incidence_denominator_sex_selector",
                             label = "Sex",
                             choices = unique(incidence_estimates$denominator_sex),
                             selected = "Female",
                             options = list(
                               `actions-box` = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"),
                             multiple = TRUE)
             ),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 pickerInput(inputId = "incidence_denominator_vacc_selector",
                             label = "Vaccination status",
                             choices = unique(incidence_estimates$denominator_vacc),
                             selected = "Both",
                             options = list(
                               `actions-box` = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"),
                             multiple = TRUE)
             ),
             tags$hr(),
             tags$h5("Analysis settings"),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 pickerInput(inputId = "incidence_start_date_selector",
                             label = "incidence start date",
                             choices = as.character(sort(unique(incidence_estimates$incidence_start_date))),
                             selected = as.character(sort(unique(incidence_estimates$incidence_start_date))),
                             options = list(
                               `actions-box` = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"),
                             multiple = TRUE)
             ),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 pickerInput(inputId = "interval_time_selector",
                             label = "interval time",
                             choices = as.character(unique(incidence_estimates$analysis_interval)),
                             selected = as.character(unique(incidence_estimates$analysis_interval))[1],
                             options = list(
                               `actions-box` = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"),
                             multiple = FALSE)
             ),
             tabsetPanel(type = "tabs",
                         tabPanel("Table of estimates", 
                                  DTOutput('tbl_incidence_estimates') %>% withSpinner()), 
                         tabPanel("Plot of estimates",
                                  tags$hr(),
                                  tags$h5("Plotting options"),
                                  div(style="display: inline-block;vertical-align:top; width: 150px;",
                                      pickerInput(inputId = "incidence_x_axis",
                                                  label = "X axis",
                                                  choices = c("denominator_age_group", 
                                                              "denominator_sex",
                                                              "denominator_vacc",
                                                              "outcome_name",
                                                              "database_name",
                                                              "incidence_start_date"),
                                                  selected = "incidence_start_date",
                                                  options = list(
                                                    `actions-box` = TRUE,
                                                    size = 10,
                                                    `selected-text-format` = "count > 3"),
                                                  multiple = FALSE,)
                                  ),
                                  div(style="display: inline-block;vertical-align:top; width: 150px;",
                                      pickerInput(inputId = "incidence_plot_facet",
                                                  label = "Facet by",
                                                  choices = c("denominator_age_group", 
                                                              "denominator_sex",
                                                              "denominator_vacc",
                                                              "outcome_name",
                                                              "database_name",
                                                              "denominator_name",
                                                              "incidence_start_date"),
                                                  selected = c("outcome_name",
                                                               "database_name"),
                                                  options = list(
                                                    `actions-box` = TRUE,
                                                    size = 10,
                                                    `selected-text-format` = "count > 3"),
                                                  multiple = TRUE,)
                                  ),
                                  div(style="display: inline-block;vertical-align:top; width: 150px;",
                                      pickerInput(inputId = "incidence_plot_group",
                                                  label = "Colour by",
                                                  choices = c("denominator_age_group", 
                                                              "denominator_sex",
                                                              "denominator_vacc",
                                                              "outcome_name",
                                                              "database_name",
                                                              "denominator_name",
                                                              "incidence_start_date"),
                                                  options = list(
                                                    `actions-box` = TRUE,
                                                    size = 10,
                                                    `selected-text-format` = "count > 3"),
                                                  multiple = TRUE,)
                                  ),
                                  plotlyOutput('plot_incidence_estimates', height = "800px") %>% withSpinner() ), 
                         tabPanel("Attrition table", 
                                  DTOutput('tbl_incidence_attrition') %>% withSpinner())
             )
    )
  )
)

