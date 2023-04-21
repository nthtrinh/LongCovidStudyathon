## packages
library(dplyr)
devtools::load_all(path = here::here("TreatmentPatterns_code"))
databaseNames <- c("CPRDGold", "BIGAN", "IPCI")

for (i in 1:length(databaseNames)) {

## Prepare data

tp_counts <- read.csv(here::here("data", paste(databaseNames[i], "_LC_any_inf_percentage_groups_treated_noyear.csv", sep = "")))
tp_table  <- read.csv(here::here("data", paste(databaseNames[i], "_LC_any_inf_inputsunburst_all.csv", sep = "")))

numOfDrugs <- 10

drugNames <- tp_counts[1:(nrow(tp_counts)-3),] %>%
  dplyr::arrange(desc(.data$event_cohort_name1)) %>%
  dplyr::slice_head(n = numOfDrugs) %>%
  dplyr::select(outcomes) %>%
  dplyr::pull()

firstDurg <- tp_table$path %>%
  stringr::str_split(pattern = "-") %>%
  sapply("[[",1)
  


tpToPlot <- rbind(tp_table[firstDurg %in% drugNames,] %>%
                    mutate(path = gsub("-End", "", .data$path)),
      tp_table[!(firstDurg %in% drugNames),] %>%
        mutate(path = "Others",
               freq = sum(freq)) %>%
        distinct()) %>%
  mutate(path = gsub("HMG-CoA_inhibitors", "HMG_CoA_inhibitors", .data$path))

createSunburstPlot(tpToPlot,
                   folder = here::here(),
                   fileName = databaseNames[i],
                   title = databaseNames[i ])

}



