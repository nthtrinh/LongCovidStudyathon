library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)
library(tibble)

db <- c("CPRDGold", "BIGAN")
zipFiles <- lapply(db, function(x){return(here("data", paste0(x, "_Results")))}) %>% unlist

incEst <- list()
# incAtt <- list()

# Still should do attrition
# Should check if files to be read exist first

# Temporal folder to delete later
output_temp <- file.path(here::here("Temp"))
if (!file.exists(output_temp)){
  dir.create(output_temp, recursive = TRUE)}

counter <- 1
extract_incidence <- function(incEst, type, filenames) {
  for(x in filenames) {
    #name_file_inc <- list.files(path = here::here(zipFiles[k],"IP", x), pattern = "incidence_estimates")
    unzip(here::here(zipFiles[k],"IP",x), exdir = output_temp)
    file_inc <- list.files(path = here::here(output_temp, substr(x, 1, nchar(x) - 4)), pattern = "incidence_es*", full.names = TRUE)
    df <- read.csv(file_inc)
    unlink(here::here(output_temp, substr(x, 1, nchar(x) - 4)), recursive=TRUE)
    df <- as_tibble(df)
    df <- df %>% 
      dplyr::mutate(outcome_cohort_id = as.factor(outcome_cohort_id))
    
    df <- 
      dplyr::case_when(
        type == 1 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Allpop_","",gsub("NonVacc.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Allpop") %>%
          dplyr::mutate(denominator_vacc = "NonVacc"),
        type == 2 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Allpop_","",gsub("_Vacc.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Allpop") %>%
          dplyr::mutate(denominator_vacc = "Vacc"),
        type == 3 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Allpop_","",gsub("_AllandSex.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Allpop") %>%
          dplyr::mutate(denominator_vacc = "Both"),
        type == 4 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Allpop_","",gsub("_Age.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Allpop") %>%
          dplyr::mutate(denominator_vacc = "Both"),
        type == 5 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Inf_","",gsub("NonVacc.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Inf") %>%
          dplyr::mutate(denominator_vacc = "NonVacc"),
        type == 6 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Inf_","",gsub("_Vacc.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Inf") %>%
          dplyr::mutate(denominator_vacc = "Vacc"),
        type == 7 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Inf_","",gsub("_AllandSex.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Inf") %>%
          dplyr::mutate(denominator_vacc = "Both"),
        type == 8 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Inf_","",gsub("_Age.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Inf") %>%
          dplyr::mutate(denominator_vacc = "Both"),
        type == 9 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Reinf_","",gsub("NonVacc.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Reinf") %>%
          dplyr::mutate(denominator_vacc = "NonVacc"),
        type == 10 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Reinf_","",gsub("_Vacc.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Reinf") %>%
          dplyr::mutate(denominator_vacc = "Vacc"),
        type == 11 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Reinf_","",gsub("_AllandSex.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Reinf") %>%
          dplyr::mutate(denominator_vacc = "Both"),
        type == 12 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Reinf_","",gsub("_Age.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Reinf") %>%
          dplyr::mutate(denominator_vacc = "Both"),
        type == 13 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Neg_","",gsub("NonVacc.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Neg") %>%
          dplyr::mutate(denominator_vacc = "NonVacc"),
        type == 14 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Neg_","",gsub("_Vacc.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Neg") %>%
          dplyr::mutate(denominator_vacc = "Vacc"),
        type == 15 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Neg_","",gsub("_AllandSex.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Neg") %>%
          dplyr::mutate(denominator_vacc = "Both"),
        type == 16 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Neg_","",gsub("_Age.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Neg") %>%
          dplyr::mutate(denominator_vacc = "Both"),
        type == 17 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Flu_","",gsub("NonVacc.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Flu") %>%
          dplyr::mutate(denominator_vacc = "NonVacc"),
        type == 18 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Flu_","",gsub("_Vacc.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Flu") %>%
          dplyr::mutate(denominator_vacc = "Vacc"),
        type == 19 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Flu_","",gsub("_AllandSex.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Flu") %>%
          dplyr::mutate(denominator_vacc = "Both"),
        type == 20 ~ df %>% 
          dplyr::mutate(outcome_name = gsub("Flu_","",gsub("_Age.zip","",x))) %>% 
          dplyr::mutate(denominator_name = "Flu") %>%
          dplyr::mutate(denominator_vacc = "Both")
      )          
    incEst[[counter]] <- df
    counter <- counter + 1
  }
  return(incEst)
}

# Read zip files from all databases output folders
for (k in 1:length(zipFiles)) {
  # First all files in the IP output folder
  IP_file_names <- list.files(path = here::here(zipFiles[k],"IP"))
  IP_file_names_full <- list.files(path = here::here(zipFiles[k],"IP"), full.names = TRUE)

    incEst <- extract_incidence(incEst,1,IP_file_names[grepl("Allpop.*NonVacc", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,2,IP_file_names[grepl("Allpop.*_Vacc", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,3,IP_file_names[grepl("Allpop.*AllandSex", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,4,IP_file_names[grepl("Allpop.*Age", IP_file_names)])
    counter <- 1 + length(incEst)
    
    incEst <- extract_incidence(incEst,5,IP_file_names[grepl("Inf.*NonVacc", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,6,IP_file_names[grepl("Inf.*_Vacc", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,7,IP_file_names[grepl("Inf.*AllandSex", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,8,IP_file_names[grepl("Inf.*Age", IP_file_names)])
    counter <- 1 + length(incEst)
    
    incEst <- extract_incidence(incEst,9,IP_file_names[grepl("Reinf.*NonVacc", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,10,IP_file_names[grepl("Reinf.*_Vacc", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,11,IP_file_names[grepl("Reinf.*AllandSex", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,12,IP_file_names[grepl("Reinf.*Age", IP_file_names)])
    counter <- 1 + length(incEst)
    
    
    incEst <- extract_incidence(incEst,13,IP_file_names[grepl("Neg.*NonVacc", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,14,IP_file_names[grepl("Neg.*_Vacc", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,15,IP_file_names[grepl("Neg.*AllandSex", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,16,IP_file_names[grepl("Neg.*Age", IP_file_names)])
    counter <- 1 + length(incEst)
    
    incEst <- extract_incidence(incEst,17,IP_file_names[grepl("Flu.*NonVacc", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,18,IP_file_names[grepl("Flu.*_Vacc", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,19,IP_file_names[grepl("Flu.*AllandSex", IP_file_names)])
    counter <- 1 + length(incEst)
    incEst <- extract_incidence(incEst,20,IP_file_names[grepl("Flu.*Age", IP_file_names)])
    counter <- 1 + length(incEst)

}


# Probably change age groups to more readable format

# incidence estimates
incidence_estimates <- bind_rows(incEst) 

#incidence_estimates <- incidence_estimates_bef

names_cohorts <- read.csv(here::here("data","CPRDGold_cohorts.csv"))

get_tablename <- list(
  "overlap" = "slcp_overlapipcohorts",
  "overlap_any" = "slcp_overlapccohorts",
  "base" = "slcp_basecohorts",
  "LC" = "slcp_lccohorts",
  "PASC" = "slcp_pasccohorts",
  "MC" = "slcp_mccohorts"
)

# Change outcome names
for(i in num:length(incidence_estimates$outcome_name)) {
  if(length(grep("overlap_any",incidence_estimates$outcome_name[i]) != 0 )) {
    incidence_estimates$outcome_name[i] <- names_cohorts %>% dplyr::filter(table_name == get_tablename["overlap_any"]) %>% 
      dplyr::filter(cohort_definition_id == stringr::str_extract_all(incidence_estimates$outcome_name[i], "(?>-)*[[:digit:]]+\\.*[[:digit:]]*")) %>%
      dplyr::select(cohort_name) %>% dplyr::pull()
  } else if(length(grep("overlap",incidence_estimates$outcome_name[i]) != 0 )) {
    incidence_estimates$outcome_name[i] <- names_cohorts %>% dplyr::filter(table_name == get_tablename["overlap"]) %>% 
      dplyr::filter(cohort_definition_id == stringr::str_extract_all(incidence_estimates$outcome_name[i], "(?>-)*[[:digit:]]+\\.*[[:digit:]]*")) %>%
      dplyr::select(cohort_name) %>% dplyr::pull()
  } else if(length(grep("base",incidence_estimates$outcome_name[i]) != 0 )) {
    incidence_estimates$outcome_name[i] <- names_cohorts %>% dplyr::filter(table_name == get_tablename["base"]) %>% 
      dplyr::filter(cohort_definition_id == stringr::str_extract_all(incidence_estimates$outcome_name[i], "(?>-)*[[:digit:]]+\\.*[[:digit:]]*")) %>%
      dplyr::select(cohort_name) %>% dplyr::pull()
  } else if(length(grep("LC",incidence_estimates$outcome_name[i]) != 0 )) {
    incidence_estimates$outcome_name[i] <- names_cohorts %>% dplyr::filter(table_name == get_tablename["LC"]) %>% 
      dplyr::filter(cohort_definition_id == stringr::str_extract_all(incidence_estimates$outcome_name[i], "(?>-)*[[:digit:]]+\\.*[[:digit:]]*")) %>%
      dplyr::select(cohort_name) %>% dplyr::pull()
  } else if(length(grep("MC",incidence_estimates$outcome_name[i]) != 0 )) {
    incidence_estimates$outcome_name[i] <- names_cohorts %>% dplyr::filter(table_name == get_tablename["MC"]) %>% 
      dplyr::filter(cohort_definition_id == stringr::str_extract_all(incidence_estimates$outcome_name[i], "(?>-)*[[:digit:]]+\\.*[[:digit:]]*")) %>%
      dplyr::select(cohort_name) %>% dplyr::pull()
  } else if(length(grep("PASC",incidence_estimates$outcome_name[i]) != 0 )) {
    incidence_estimates$outcome_name[i] <- names_cohorts %>% dplyr::filter(table_name == get_tablename["PASC"]) %>% 
      dplyr::filter(cohort_definition_id == stringr::str_extract_all(incidence_estimates$outcome_name[i], "(?>-)*[[:digit:]]+\\.*[[:digit:]]*")) %>%
      dplyr::select(cohort_name) %>% dplyr::pull()
  } else {
    
  }
  }

save(
  incidence_estimates,
  # incidence_attrition,
  file = here("data", "dataShiny.RData")
)

# Delete temporal directory
if (file.exists(output_temp)){
  unlink(output_temp, recursive = TRUE)}
