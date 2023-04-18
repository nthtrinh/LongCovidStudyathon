#### SERVER ------
server <-	function(input, output, session) {
  
  # lsc -----
  get_lsc <-reactive({
    table <- lsc_table  %>% 
      filter(database_name %in% input$lsc_db) %>% 
      filter(window_name %in% input$lsc_time_window) %>% 
      filter(cohort_name %in%  input$lsc_cohort_name)
    # %>% 
    #   filter(name %in%  input$lsc_icd_chapter)
    # 
    table
  })
  
  
  # lsc table
  output$tbl_lsc <-  renderDataTable({
    
    table <- get_lsc() %>% 
      select(any_of(c("cohort_name",
                      "name" ,
                     "concept",   
                     "proportion",
                     # "concept_count",
                     # "denominator_count",
                    "database_name"))) %>% 
      distinct() %>% 
      pivot_wider(names_from = database_name, 
                  values_from = c("proportion"
                                  # ,
                                  # "concept_count",
                                  # "denominator_count" 
                                  ))
    
    
    perc_cols<- stringr::str_subset(colnames(table), 
                                    paste("cohort_name", "concept", sep = "|"),
                                    negate = TRUE)
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB'
              )) %>% 
      formatPercentage(perc_cols, 2)
  })
  
  # lsc plot
  output$gg_lsc <- renderPlotly({
    
    get_lsc() %>% 
      # mutate(name=if_else(is.na(name), "Other", name)) %>% 
      # arrange(name) %>% 
      mutate(id = 1:nrow(.)) %>% 
      ggplot(aes(group = concept)) +
      geom_point(aes(id, 
                     proportion),
                 size=1.5,
                 shape=19,
                 alpha=0.75) +
      theme_minimal() +
      facet_wrap(.~database_name, scales = "free_x",
                 ncol=4) +
      theme_minimal() +
      scale_y_continuous(labels = percent,
                         limits=c(0,1),expand = c(0,0.03))+
      theme(panel.spacing.x=unit(0, "lines"),
            panel.spacing.y=unit(0, "lines"),
            legend.title = element_blank(),
            legend.position = "bottom",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text=element_text(size=18),
            axis.title=element_text(size=18,face="bold"),
            strip.text = element_text(size=18, face="bold"),
            strip.background = element_rect( fill="#f7f7f7"),
            legend.text=element_text(size=13))+
      guides(shape=guide_legend(nrow=3))+
      ylab("Prevalence")
    
  })
  
  # lsc_comp ----
  get_lsc_comp <-reactive({
    table <- lsc_table  %>% 
      filter(database_name %in% input$lsc_comp_db) %>% 
      filter(window_name %in% input$lsc_comp_time_window) %>% 
      select(any_of(c("cohort_name",
                      "name" ,
                      "concept",   
                      "proportion",
                      "database_name"))) %>% 
      distinct() 
    
    table_target <- table %>% 
      filter(cohort_name %in%  
               input$lsc_comp_cohort_name_1) %>% 
      select(!"cohort_name") %>% 
      mutate(database_name= paste0(database_name, "_a_target")) %>% 
      pivot_wider(names_from = c(database_name),
                  values_from = c("proportion"
                  ))
    
    table_comparator <- table %>% 
      filter(cohort_name %in%  
               input$lsc_comp_cohort_name_2) %>% 
      select(!"cohort_name") %>% 
      mutate(database_name= paste0(database_name, "_b_comparator")) %>% 
      pivot_wider(names_from = c(database_name),
                  values_from = c("proportion"))
    
    
    table <- table_target %>% 
      full_join(table_comparator)
    
    table <- table %>% 
      relocate(sort(names(.))) %>% 
      relocate("concept")
    
    names(table) <- stringr::str_replace_all(names(table), "_a_", "_")
    names(table) <- stringr::str_replace_all(names(table), "_b_", "_")
    
    table
  })
  
  # lsd_comp table
  output$tbl_lsc_comp <-  renderDataTable({
    
    table <- get_lsc_comp() 
    
    perc_cols<- stringr::str_subset(colnames(table), 
                                    "concept", 
                                    negate = TRUE)
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB'
              )) %>% 
      formatPercentage(perc_cols, 2)
  })
  
  # lsd_comp plot
  
  output$gg_lsc_comp <- renderPlotly({
    
   plot_data <- get_lsc_comp() %>% 
     pivot_longer(!concept, 
                  names_to = "database_cohort", 
                  values_to = "proportion") %>% 
     separate(col = "database_cohort", 
                          c("database_name", "cohort")) %>% 
     pivot_wider(names_from = cohort, 
                 values_from = c("proportion"
                 ))
      
      
   plot_data %>% 
     ggplot(aes(group = concept)) +
     geom_abline(linetype="dashed")+
     geom_point(aes(target, comparator),
                size=1.5,
                shape=19,
                alpha=0.75)+
     facet_wrap(.~database_name, scales = "free_x",
                ncol=4) +
     theme_minimal() +
     scale_y_continuous(labels = percent,
                        limits=c(0,1),expand = c(0,0.03)) +
     scale_x_continuous(labels = percent,
                        limits=c(0,1),expand = c(0,0.03))+
     theme(panel.spacing.x=unit(0, "lines"),
           panel.spacing.y=unit(0, "lines"),
           legend.title = element_blank(),
           legend.position = "bottom",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           axis.text=element_text(size=18),
           axis.title=element_text(size=18,face="bold"),
           strip.text = element_text(size=18, face="bold"),
           strip.background = element_rect( fill="#f7f7f7"),
           legend.text=element_text(size=13))+
     guides(shape=guide_legend(nrow=3))
   
    
  })
  
  
  # lsd -----
  get_lsd <-reactive({
    table <- du_table  %>% 
      filter(database_name %in% input$lsd_db) %>% 
      filter(window_name %in% input$lsd_time_window) %>% 
      filter(cohort_name %in%  input$lsd_cohort_name)

    table
  })
  
  
  # lsd table
  output$tbl_lsd <-  renderDataTable({
    
    table <- get_lsd() %>% 
      select(any_of(c("cohort_name",
                      "name" ,
                      "concept",   
                      "proportion",
                      # "concept_count",
                      # "denominator_count",
                      "database_name"))) %>% 
      distinct() %>% 
      pivot_wider(names_from = database_name, 
                  values_from = c("proportion"
                                  # ,
                                  # "concept_count",
                                  # "denominator_count" 
                  ))
    
    perc_cols<- stringr::str_subset(colnames(table), 
                                    paste("cohort_name", "concept", sep = "|"),
                        negate = TRUE)
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB'
              )) %>% 
      formatPercentage(perc_cols, 2)
  })
  
  # lsd plot
  output$gg_lsd <- renderPlotly({
    
    get_lsd() %>% 
      # mutate(name=if_else(is.na(name), "Other", name)) %>% 
      # arrange(name) %>% 
      mutate(id = 1:nrow(.)) %>% 
      ggplot(aes(group = concept)) +
      geom_point(aes(id, 
                     proportion),
                 size=1.5,
                 shape=19,
                 alpha=0.75) +
      theme_minimal() +
      facet_wrap(.~database_name, scales = "free_x",
                 ncol=4) +
      theme_minimal() +
      scale_y_continuous(labels = percent,
                         limits=c(0,1),expand = c(0,0.03))+
      theme(panel.spacing.x=unit(0, "lines"),
            panel.spacing.y=unit(0, "lines"),
            legend.title = element_blank(),
            legend.position = "bottom",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text=element_text(size=18),
            axis.title=element_text(size=18,face="bold"),
            strip.text = element_text(size=18, face="bold"),
            strip.background = element_rect( fill="#f7f7f7"),
            legend.text=element_text(size=13))+
      guides(shape=guide_legend(nrow=3))+
      ylab("Prevalence")
    
  })
  
  # ------
  output$tbl_lsc_hu <-  renderDataTable({
    datatable(lsc_hu,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB'
              )) 
  })
  
  output$tbl_lsc_vacc <-  renderDataTable({
    datatable(lsc_vacc,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB'
              )) 
  })
  

  # close -----
}