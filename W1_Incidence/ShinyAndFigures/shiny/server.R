#### SERVER ------
library(openxlsx)
library(stringr)
server <- function(input, output, session) {

  # incidence
  get_incidence_estimates<-reactive({
    
    table<-incidence_estimates %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector)  %>% 
      filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector)  %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector)     %>% 
      filter(denominator_vacc %in% input$incidence_denominator_vacc_selector)   %>% 
      filter(outcome_name %in% input$incidence_outcome_name_selector)  %>%
      filter(denominator_name %in% input$incidence_denominator_name_selector) %>%
      filter(analysis_interval %in% input$interval_time_selector)
    
    table
  }) 
  
  output$tbl_incidence_estimates<-  renderDataTable({
    
    table<-get_incidence_estimates() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(incidence_100000_pys=nice.num.count(incidence_100000_pys)) %>% 
      mutate(incidence_100000_pys_95CI_lower=nice.num.count(incidence_100000_pys_95CI_lower)) %>% 
      mutate(incidence_100000_pys_95CI_upper=nice.num.count(incidence_100000_pys_95CI_upper)) %>% 
      mutate(incidence_100000_pys= ifelse(!is.na(incidence_100000_pys),
                                          paste0(incidence_100000_pys, " (",
                                                 incidence_100000_pys_95CI_lower," to ", 
                                                 incidence_100000_pys_95CI_upper, " )"))) %>% 
      select(!c("incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper",
                "cohort_obscured", "result_obscured","person_days",
                "analysis_min_cell_count","analysis_repeated_events",
                "analysis_outcome_washout",
                "denominator_cohort_id", "outcome_cohort_id")) %>% 
      mutate(n_persons=nice.num.count(n_persons)) %>% 
      mutate(n_events=nice.num.count(n_events)) %>% 
      mutate(person_years=nice.num.count(person_years)) %>%
      relocate(incidence_start_date) %>% 
      relocate(incidence_end_date, .after = incidence_start_date) %>% 
      relocate(person_years, .after = n_persons)
    datatable(table,
              rownames= FALSE,
              # colnames = c('Variable' = 1),
              extensions = 'Buttons',
              # options = list(lengthChange = FALSE,
              #                pageLength = 40,
              #                buttons = c('copy', 'csv', 'excel')))
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB',
                             buttons = list(list(extend = "excel", 
                                                 text = "Download table as excel",
                                                 filename = "PatientProfiles.csv"))
              ))
  } )
  
  output$plot_incidence_estimates<- renderPlotly({ 
    
    table<-get_incidence_estimates() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$incidence_plot_group)){
      if(!is.null(input$incidence_plot_facet)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$incidence_plot_group) ){ 
      
      if(is.null(input$incidence_plot_facet) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$incidence_plot_facet) ){
        if(!is.null(input$incidence_plot_group) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group="Group",
                              colour="facet_var")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  
}
  