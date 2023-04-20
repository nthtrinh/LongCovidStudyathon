## packages
library(dplyr)

## Prepare data
tp_counts <- read.csv(here::here("data", "CPRDGold_LC any + inf_percentage_groups_treated_noyear.csv"))
tp_table  <- read.csv(here::here("data", "CPRDGold_LC any + inf_inputsunburst_all.csv"))

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

devtools::load_all(path = here::here("TreatmentPatterns_code"))
createSunburstPlot(tpToPlot,
                   folder = here::here(),
                   fileName = "sunburstPlot")





