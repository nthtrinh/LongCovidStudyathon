library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)
library(tibble)

db <- c("CPRDGold", "BIGAN")
zipFile <- lapply(db, function(x){return(paste0("data/", x, "_Results"))}) %>% unlist

cluLCA <- list()
cluInit <- list()
# Should also include characterisation of clusters: HU, vacc and age/sex information

# Read zip files from all databases output folders
for (k in 1:length(zipFiles)) {
  working_csv <- read.csv(here::here(zipFile[k], paste0("Clustering/","Clustering_LCA_model2_probs.csv")))
  cluLCA[[1+6*(k-1)]] <- working_csv %>%
    dplyr::mutate(num_clust = 2) %>%
    dplyr::mutate(database_name = db[k])
  working_csv <- read.csv(here::here(zipFile[k], paste0("Clustering/","Clustering_LCA_model3_probs.csv")))
  cluLCA[[2+6*(k-1)]] <- working_csv %>%
    dplyr::mutate(num_clust = 3) %>%
    dplyr::mutate(database_name = db[k])
  working_csv <- read.csv(here::here(zipFile[k], paste0("Clustering/","Clustering_LCA_model4_probs.csv")))
  cluLCA[[3+6*(k-1)]] <- working_csv %>%
    dplyr::mutate(num_clust = 4) %>%
    dplyr::mutate(database_name = db[k])
  working_csv <- read.csv(here::here(zipFile[k], paste0("Clustering/","Clustering_LCA_model5_probs.csv")))
  cluLCA[[4+6*(k-1)]] <- working_csv %>%
    dplyr::mutate(num_clust = 5) %>%
    dplyr::mutate(database_name = db[k])
  working_csv <- read.csv(here::here(zipFile[k], paste0("Clustering/","Clustering_LCA_model6_probs.csv")))
  cluLCA[[5+6*(k-1)]] <- working_csv %>%
    dplyr::mutate(num_clust = 6) %>%
    dplyr::mutate(database_name = db[k])
  working_csv <- read.csv(here::here(zipFile[k], paste0("Clustering/","Clustering_LCA_model7_probs.csv")))
  cluLCA[[6+6*(k-1)]] <- working_csv %>%
    dplyr::mutate(num_clust = 7) %>%
    dplyr::mutate(database_name = db[k])
  
  working_csv <- read.csv(here::here(zipFile[k], paste0("Clustering/","Clustering_LCA_model7_y.csv")))
  cluInit[[k]] <- working_csv %>%
    dplyr::mutate(database_name = db[k])
}

cluster_data <- bind_rows(cluLCA)
cluster_init <- bind_rows(cluInit)

save(
  cluster_data,
  cluster_init,
  file = here("data", "dataShiny.RData")
)

