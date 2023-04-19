library(tidyverse)
library(pheatmap)

setwd("PATH TO THIS SCRIPT")

## Data processing ----
partners = c("BIGAN", "CORIVA-Estonia", "IMASIS", "CHUM", "CPRDGold", "PHARMETRICS")
path_template = "data/{partner}_Results_v2/Clustering/"

load("names_symptoms.RData", verbose = T)

read_cluster_data = function(partner, model){
  path = str_glue(path_template)
  res = list()
  
  res$partner = partner
  res$classes = read_csv(str_glue("{path}/Clustering_LCA_model{model}_predclass.csv"), show_col_types = FALSE)
  res$posterior = read_csv(str_glue("{path}/Clustering_LCA_model{model}_posterior.csv"), show_col_types = FALSE)
  res$bic = read_csv(str_glue("{path}/Clustering_LCA_model{model}_bic.csv"), show_col_types = FALSE)
  res$probs = read_csv(str_glue("{path}/Clustering_LCA_model{model}_probs.csv"), show_col_types = FALSE)%>%
    mutate(num_clust = model)
  res$mat = read_csv(str_glue("{path}/Clustering_LCA_model{model}_y.csv"), show_col_types = FALSE) %>% 
    as.matrix() %>% 
    magrittr::subtract(1)
  res$demographics = read_csv(str_glue("{path}/Clustering_LCA_model{model}_x.csv"), show_col_types = FALSE)
  
  if(file.exists(str_glue("{partner}_Results_v2/names_symptoms_{partner}.csv"))){
    res$names_symptoms = read_csv(str_glue("{partner}_Results_v2/names_symptoms_{partner}.csv"), show_col_types = FALSE, col_names = colnames(names_symptoms)) %>% 
      slice(-1)
  }
  else{
    res$names_symptoms = names_symptoms
  }
  
  return(res)
}

cluster_data = read_cluster_data(partners[3], 4)

process_cluster_data = function(cluster_data){
  # Process the cluster probability matrix
  res = list()
  
  probs_long = cluster_data$probs %>%
    pivot_longer(cols = starts_with("LC"), values_to = "proportion", names_to = "source") %>%
    extract(source, into = c("cohort_definition_id", "prop_type"), regex = "LC_([0-9]+).(Pr.[1-2])") %>%
    mutate(cohort_definition_id = as.numeric(cohort_definition_id)) %>%
    filter(prop_type == "Pr.2") %>%
    select(-prop_type) %>%
    group_by(num_clust, cohort_definition_id) %>%
    mutate(cluster_id = 1:n()) %>%
    left_join(cluster_data$names_symptoms, by = "cohort_definition_id") %>%
    ungroup() %>% 
    mutate(database_name = cluster_data$partner)  
  
  # Add incidence
  incidence = tibble(
    cohort_definition_id = colnames(cluster_data$mat) %>% str_replace("LC_", "") %>% as.numeric(),
    incidence = colMeans(cluster_data$mat) 
  )
  
  probs_long = probs_long %>% 
    left_join(incidence, by = "cohort_definition_id") %>% 
    mutate(fold_change = proportion / incidence)
  
  # Add cluster_size
  cluster_size = cluster_data$classes %>% 
    table() %>% 
    as.data.frame() %>% 
    rename("cluster_id" = x, "N" = Freq) %>% 
    mutate(Nprop = N / sum(N)) %>% 
    mutate(cluster_id = as.numeric(as.character(cluster_id)))
  
  probs_long = probs_long %>% 
    left_join(cluster_size, by = "cluster_id") 
  
  # Add demographics
  demographics = cluster_data$demographics %>% 
    bind_cols(
      cluster_data$classes %>% 
        rename("cluster_id" = x) 
    ) %>% 
    group_by(cluster_id) %>% 
    summarize(
      average_age = mean(age),
      proportion_male = mean(sexMale)
    )
    
  probs_long = probs_long %>% 
    left_join(demographics, by = "cluster_id") 
  
  # Add BIC 
  probs_long = probs_long %>% 
    mutate(BIC = cluster_data$bic$x)
  
  return(probs_long)
}

plot_data = pmap_df(expand.grid(partners, 4), ~ read_cluster_data(.x, .y) %>% process_cluster_data())
save(plot_data, file = "plot_data.RData")

## Drawing the plot ----
load("plot_data.RData")

props_wide = plot_data %>% 
  pivot_wider(id_cols = c("database_name", "num_clust", "cluster_id", "N", "Nprop", "average_age", "proportion_male", "BIC"), names_from = cohort_name, values_from = proportion) %>% 
  mutate(rowname = str_glue("{database_name}_{num_clust}_{cluster_id}")) 

mat = props_wide %>%
  # filter(database_name == partner) %>% 
  select(-database_name, -num_clust, -cluster_id, -N, -Nprop, -average_age, -proportion_male, -BIC) %>%
  as.data.frame() %>%
  column_to_rownames("rowname") %>%
  as.matrix()

row_annotations = props_wide %>% 
  # filter(database_name == partner) %>% 
  as.data.frame() %>%
  column_to_rownames("rowname") %>% 
  select(Nprop, average_age, proportion_male) %>% 
  filter(!is.na(proportion_male))

mat = mat[sort(rownames(row_annotations)), order(colnames(mat))]

pheatmap(
  mat,
  cluster_rows = F, 
  cluster_cols = F, 
  annotation_row = row_annotations,
  breaks = seq(0, 0.4, length.out = 100), 
  color = c("white", colorRampPalette(c("#f0f9e8", "#ccebc5", "#4eb3d3", "#08589e"))(98)),
  gaps_row = which(str_replace(rownames(mat), "_[0-9]+_[0-9]+$", "")[1:(nrow(mat) - 1)] != str_replace(rownames(mat), "_[0-9]+_[0-9]+$", "")[2:nrow(mat)]),
  annotation_colors = list(
    # proportion_male = c("white", "navy"),
    average_age = c("#DEF5E5B3", "#38AAACB3", "#40498EB3", "#0B0405B3"),
    proportion_male = c("#F0F921B3", "#ED7953B3", "#9C179EB3", "#0D0887B3"),
    Nprop = scales::viridis_pal(alpha = 0.7)(4) %>% rev()
  ),
  cellwidth = 20,
  cellheight = 12,
  filename = str_glue("All_partners.pdf")
)



# for(partner in partners){
#   props_wide = data %>% 
#     pivot_wider(id_cols = c("database_name", "num_clust", "cluster_id", "N", "average_age", "proportion_male", "BIC"), names_from = cohort_name, values_from = proportion) %>% 
#     mutate(rowname = str_glue("{database_name}_{num_clust}_{cluster_id}")) 
#   
#   mat = props_wide %>%
#     filter(database_name == partner) %>% 
#     select(-database_name, -num_clust, -cluster_id, -N, -average_age, -proportion_male, -BIC) %>%
#     as.data.frame() %>%
#     column_to_rownames("rowname") %>%
#     as.matrix()
#   
#   row_annotations = props_wide %>% 
#     filter(database_name == partner) %>% 
#     as.data.frame() %>%
#     column_to_rownames("rowname") %>% 
#     select(N, average_age, proportion_male, BIC) 
#   
#   pheatmap(
#     mat[, order(colnames(mat))], 
#     cluster_rows = F, 
#     cluster_cols = F, 
#     annotation_row = row_annotations,
#     breaks = seq(0, 0.4, length.out = 99), 
#     gaps_row = which(str_replace(rownames(mat), "_[0-9]+$", "")[1:(nrow(mat) - 1)] != str_replace(rownames(mat), "_[0-9]+$", "")[2:nrow(mat)]),
#     annotation_colors = list(
#       proportion_male = c("white", "navy"),
#       Nprop = c("white", "black")
#     ),
#     cellwidth = 20,
#     cellheight = 12,
#     filename = str_glue("~/Desktop/ClusterResults/{partner}_clusters.png")
#   )
# }
# 
# 
# 
# 
# props_wide = data %>% 
#   pivot_wider(id_cols = c("database_name", "num_clust", "cluster_id", "N", "average_age", "proportion_male", "BIC"), names_from = cohort_name, values_from = proportion) %>% 
#   mutate(rowname = str_glue("{database_name}_{num_clust}_{cluster_id}")) 
# 
# mat = props_wide %>%
#   filter(database_name == "CPRDGold") %>% 
#   select(-database_name, -num_clust, -cluster_id, -N, -average_age, -proportion_male, -BIC) %>%
#   as.data.frame() %>%
#   column_to_rownames("rowname") %>%
#   as.matrix()
# 
# row_annotations = props_wide %>% 
#   filter(database_name == "CPRDGold") %>% 
#   as.data.frame() %>%
#   column_to_rownames("rowname") %>% 
#   select(N, average_age, proportion_male, BIC) 
# 
# pheatmap(
#   mat[, order(colnames(mat))], 
#   cluster_rows = F, 
#   cluster_cols = F, 
#   annotation_row = row_annotations,
#   breaks = seq(0, 0.4, length.out = 99), 
#   gaps_row = which(str_replace(rownames(mat), "_[0-9]+$", "")[1:(nrow(mat) - 1)] != str_replace(rownames(mat), "_[0-9]+$", "")[2:nrow(mat)]),
#   annotation_colors = list(
#     proportion_male = c("firebrick", "white", "navy"),
#     BIC = c("white", "black")
#   ),
#   cellwidth = 20,
#   cellheight = 12
# )
# 
