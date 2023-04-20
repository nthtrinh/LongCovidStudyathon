#### SERVER ------
library(openxlsx)
library(stringr)
library(tidyverse)
library(pheatmap)
server <- function(input, output, session) {
  
  getData <- reactive({
    x %>%
      filter(database_name == input$clust_cdm_name) %>%
      filter(model == input$clust_model)
  })
  
  # cluster plots
  output$plot_cluster<- renderPlotly({ 
    xx <- getData()
    tit <- paste0(unique(xx$database_name), " (", unique(xx$model), ")")
    zp1 <- ggplot(xx,aes(x = cohort_name, y = proportion, fill = significant)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_point(aes(cohort_name, incidence), shape = 3, colour = "black", fill = "black") +
      facet_grid(cluster_id ~ .) +
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
      mutate(age = paste0(all_median_age, " [", all_q25_age, "-", all_q75_age, "]")) %>%
      mutate(number_individuals = as.character(all_N)) %>%
      mutate(percentage_male = paste0(round(100*all_proportion_male), "%")) %>%
      mutate(mean_symptoms = as.character(round(all_mean_symptoms, 2))) %>%
      select(database_name, model, number_individuals, age, percentage_male, mean_symptoms) %>%
      distinct() %>%
      pivot_longer(!c(database_name, model), names_to = "variable") %>%
      mutate(cluster_label = "all") %>%
      pivot_wider(names_from = cluster_label, values_from = value) %>%
      left_join(
        xx %>%
          mutate(age = paste0(median_age, " [", q25_age, "-", q75_age, "]")) %>%
          mutate(number_individuals = paste0(N, " (", round(100*Nprop), "%)")) %>%
          mutate(percentage_male = paste0(round(100*proportion_male), "%")) %>%
          mutate(mean_symptoms = as.character(round(mean_symptoms,2))) %>%
          select(database_name, model, cluster_id, number_individuals, age, percentage_male, mean_symptoms) %>%
          distinct() %>%
          pivot_longer(!c(database_name, model, cluster_id), names_to = "variable") %>%
          mutate(cluster_id = paste0("cluster_", cluster_id)) %>%
          pivot_wider(names_from = cluster_id, values_from = value)
      )
    datatable(xx)
  })
  
  output$plot_cluster_all_countries <- renderPlot({
    props_wide = plot_data %>% 
      filter(model == input$clust_model_all_countries) %>%
      pivot_wider(id_cols = c("database_name", "num_clust", "cluster_id", "N", "Nprop", "median_age", "proportion_male", "BIC"), names_from = cohort_name, values_from = proportion) %>% 
      mutate(rowname = str_glue("{database_name}_{num_clust}_{cluster_id}"))
    
    mat = props_wide %>%
      select(-database_name, -num_clust, -cluster_id, -N, -Nprop, -median_age, -proportion_male, -BIC) %>%
      as.data.frame() %>%
      column_to_rownames("rowname") %>%
      as.matrix()
    
    row_annotations = props_wide %>% 
      # filter(database_name == partner) %>% 
      as.data.frame() %>%
      column_to_rownames("rowname") %>% 
      select(Nprop, median_age, proportion_male) %>% 
      filter(!is.na(proportion_male))
    
    mat = mat[sort(rownames(row_annotations)), order(colnames(mat))]

    p <- pheatmap(
      mat,
      cluster_rows = F, 
      cluster_cols = F, 
      annotation_row = row_annotations,
      breaks = seq(0, 0.4, length.out = 100), 
      color = c("white", colorRampPalette(c("#f0f9e8", "#ccebc5", "#4eb3d3", "#08589e"))(98)),
      gaps_row = which(str_replace(rownames(mat), "_[0-9]+_[0-9]+$", "")[1:(nrow(mat) - 1)] != str_replace(rownames(mat), "_[0-9]+_[0-9]+$", "")[2:nrow(mat)]),
      annotation_colors = list(
        # proportion_male = c("white", "navy"),
        median_age = c("#DEF5E5B3", "#38AAACB3", "#40498EB3", "#0B0405B3"),
        proportion_male = c("#F0F921B3", "#ED7953B3", "#9C179EB3", "#0D0887B3"),
        Nprop = scales::viridis_pal(alpha = 0.7)(4) %>% rev()
      ),
      cellwidth = 20,
      cellheight = 12
    )
    
    p
    
  })
  
  output$plot_cluster_all_models <- renderPlot({
    props_wide = plot_data %>% 
      filter(database_name == input$clust_db_all) %>%
      pivot_wider(id_cols = c("database_name", "num_clust", "cluster_id", "N", "Nprop", "median_age", "proportion_male", "BIC"), names_from = cohort_name, values_from = proportion) %>% 
      mutate(rowname = str_glue("{database_name}_{num_clust}_{cluster_id}"))

    mat = props_wide %>%
      select(-database_name, -num_clust, -cluster_id, -N, -Nprop, -median_age, -proportion_male, -BIC) %>%
      as.data.frame() %>%
      column_to_rownames("rowname") %>%
      as.matrix()
    
    row_annotations = props_wide %>% 
      as.data.frame() %>%
      column_to_rownames("rowname") %>% 
      select(Nprop, median_age, proportion_male) %>% 
      filter(!is.na(proportion_male))
    
    mat = mat[sort(rownames(row_annotations)), order(colnames(mat))]
    
    p <- pheatmap(
      mat,
      cluster_rows = F, 
      cluster_cols = F, 
      annotation_row = row_annotations,
      breaks = seq(0, 0.4, length.out = 100), 
      color = c("white", colorRampPalette(c("#f0f9e8", "#ccebc5", "#4eb3d3", "#08589e"))(98)),
      gaps_row = which(substr(rownames(mat), nchar(rownames(mat)), nchar(rownames(mat))) == "1")[2:5]-1,
      annotation_colors = list(
        # proportion_male = c("white", "navy"),
        median_age = c("#DEF5E5B3", "#38AAACB3", "#40498EB3", "#0B0405B3"),
        proportion_male = c("#F0F921B3", "#ED7953B3", "#9C179EB3", "#0D0887B3"),
        Nprop = scales::viridis_pal(alpha = 0.7)(4) %>% rev()
      ),
      cellwidth = 20,
      cellheight = 12
    )
    
    p
    
  })
  
}
