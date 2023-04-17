#### SERVER ------
library(openxlsx)
library(stringr)
server <- function(input, output, session) {

  # cluster plots
  output$plot_cluster<- renderPlotly({ 
    
    validate(need(length(input$clustering_num_clust_selector) == 1, 
                  "You need to select only one choice of number of clusters"))
    
    table<-cluster_data %>% 
      dplyr::filter(database_name %in% input$clustering_database_name_selector)  %>% 
      dplyr::filter(num_clust %in% input$clustering_num_clust_selector)
    
    names_symptoms <- read.csv(here::here("data","names_symptoms.csv"))
    # Print that from code now!!!
    
    table <- table %>%
      dplyr::select(-c(database_name,num_clust))
    
    clust <- as.numeric(input$clustering_num_clust_selector)
    
    lcmodel <- reshape2::melt(table, level=2)
    lcmodel <- lcmodel %>%
      dplyr::mutate(Var1 = sub("\\..*", "", lcmodel$variable)) %>%
      dplyr::mutate(Var2 = paste0("P",sub(".*.P", "", lcmodel$variable))) %>%
      dplyr::select(-variable) %>%
      dplyr::mutate(class = c(rep(seq(1,clust),nrow(lcmodel) %/% clust)))
    lcmodel$Var1 <- stringr::str_to_title(lcmodel$Var1)
    for(i in 1:nrow(names_symptoms)) {
      lcmodel$Var1[lcmodel$Var1 == paste0("Lc_",i)] <- names_symptoms$cohort_name[i]
    }
    
    name_database <- as.character(input$clustering_database_name_selector)
    
    zp1 <- ggplot(lcmodel,aes(x = Var1, y = value, fill = Var2))
    zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
    zp1 <- zp1 + facet_grid(class ~ .) 
    zp1 <- zp1 + scale_fill_brewer(type="seq", palette="Blues",labels = c("No symptom", "Symptom")) +theme_bw()
    zp1 <- zp1 + labs(x = "Symptoms",y=paste0("Prevalence symptoms in ",name_database), fill ="Response categories")	#x = "Questionnaire items"
    zp1 <- zp1 + theme( axis.text.y=element_blank(),
                        axis.ticks.y=element_blank(),                    
                        panel.grid.major.y=element_blank(),
                        axis.text.x=element_text(size=10, angle = 90, vjust = 0.5, hjust=1))
    zp1 <- zp1 + guides(fill = guide_legend(reverse=TRUE))
    
    mydata <- cluster_init %>%
      dplyr::select(-database_name)
    mydata <- mydata - 1 
    factors <- (colSums(mydata))/(length(mydata[[1]]))	
    factors <- as.matrix(factors)
    rownames(factors) <- names_symptoms$cohort_name[1:24]
    
    # Why NAs?
    
    factors <- factors[order(rownames(factors)),]
    
    # Include prevalence average lines
    for (i in 1:length(factors)) {
      zp1 <- zp1 + geom_segment(x = (i - 0.5), y = factors[[i]], xend = (i + 0.5), yend = factors[[i]])
    }
    
    zp1
    
  })
  
}
  