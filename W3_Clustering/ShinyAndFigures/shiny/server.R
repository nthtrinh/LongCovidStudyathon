#### SERVER ------
library(openxlsx)
library(stringr)
server <- function(input, output, session) {
  
  getData <- reactive({
    x %>%
      filter(cdm_name == input$clust_cdm_name) %>%
      filter(model == input$clust_model)
  })
  
  # cluster plots
  output$plot_cluster<- renderPlotly({ 
    xx <- getData()
    symptoms_all <- c("percentage_subjects", symptoms)
    symptoms_base <- paste0("base_", symptoms)
    xxx <- xx %>%
      select("cdm_name", "model", "cluster_label", all_of(symptoms_all)) %>%
      pivot_longer(symptoms_all, names_to = "symptom_name", values_to = "percentage") %>%
      left_join(
        xx %>%
          select("cdm_name", "model", "cluster_label", all_of(symptoms_base)) %>%
          pivot_longer(symptoms_base, names_to = "symptom_name", values_to = "percentage_base") %>%
          mutate(symptom_name = gsub("base_", "", symptom_name)),
        by = c("cdm_name", "model", "cluster_label", "symptom_name")
      )
    tit <- paste0(unique(xxx$cdm_name), " (", unique(xxx$model), ")")
    zp1 <- 
      ggplot(xxx,aes(x = symptom_name, y = percentage)) +
      geom_bar(stat = "identity", position = "stack", fill = "cornflowerblue") +
      geom_point(aes(symptom_name, percentage_base), shape = 3) +
      facet_grid(cluster_label ~ .) +
      theme_bw() +
      labs(x = "Symptoms", fill ="Response categories", title = tit) +
      theme( 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),                    
        panel.grid.major.y=element_blank(),
        axis.text.x=element_text(size=10, angle = 90, vjust = 0.5, hjust=1)
      )
    zp1
    
  })
  
  output$tbl_characteristics <- renderDataTable({
    xx <- getData()
    xx <- xx %>% 
      mutate(age = paste0(base_median_age, " [", base_q25_age, "-", base_q75_age, "]")) %>%
      mutate(number_individuals = as.character(sum(number_subjects))) %>%
      mutate(percentage_male = paste0(round(100*base_sexMale), "%")) %>%
      select(cdm_name, model, number_individuals, age, percentage_male) %>%
      distinct() %>%
      pivot_longer(!c(cdm_name, model), names_to = "variable") %>%
      mutate(cluster_label = "all") %>%
      pivot_wider(names_from = cluster_label, values_from = value) %>%
      left_join(
        xx %>%
          mutate(age = paste0(median_age, " [", q25_age, "-", q75_age, "]")) %>%
          mutate(number_individuals = paste0(number_subjects, " (", round(100*number_subjects/sum(number_subjects)), "%)")) %>%
          mutate(percentage_male = paste0(round(100*sexMale), "%")) %>%
          select(cdm_name, model, cluster_label, number_individuals, age, percentage_male) %>%
          pivot_longer(!c(cdm_name, model, cluster_label), names_to = "variable") %>%
          mutate(cluster_label = paste0("cluster_", cluster_label)) %>%
          pivot_wider(names_from = cluster_label, values_from = value)
      )
    datatable(xx)
  })
}
