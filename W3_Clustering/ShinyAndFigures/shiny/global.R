# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(floor(x),
                big.mark=",", nsmall = 0, scientific=FALSE))}
library(here)
library(dplyr)
library(readr)
library(tidyr)
#load(here("data", "dataShiny.RData"))

db <- list.files(here("data"), full.names = TRUE)
db <- db[lapply(db, dir.exists) %>% unlist()]
db <- lapply(db, basename)

symptoms <- c(
  "Abdominal_pain", "Allergy", "Altered_smell_taste", "Anxiety", "Blurred_vision",
  "Chest pain or angina", "Cognitive_dysfunction_brain_fog", "Cough", "Depression wo recurrence", 
  "Dizziness", "Dyspnea", "Fatigue_malaise", "Gastrointestinal_issues", "Headache", "Intermittent_fever",
  "Joint_pain", "Memory_issues", "Menstrual_problems", "Muscle_spasms_and_pain", "Neuralgia",
  "Pins_and_needles_sensation", "Postexertional_fatigue", "Sleep_disorder", "Tachycardia", "Tinnitus_hearing_problems"
)

dataModel <- list()
for (databaseName in db) {
  names <- read_csv(here("data", databaseName, paste0("names_symptoms_", databaseName, ".csv")), show_col_types = FALSE) %>%
    rename("col_name" = "...1") %>%
    mutate(col_name = paste0("LC_", col_name))
  x <- read_csv(here("data", databaseName, "Clustering_LCA_model4_y.csv"), show_col_types = FALSE) %>%
    bind_cols(
      read_csv(here("data", databaseName, "Clustering_LCA_model4_x.csv"), show_col_types = FALSE)
    ) %>%
    select(-"X.Intercept.") %>%
    mutate(cdm_name = databaseName) %>%
    bind_cols(
      read_csv(here("data", databaseName, "Clustering_LCA_model4_predclass.csv"), show_col_types = FALSE)
    ) %>%
    rename("cluster4" = "x")
  for (k in 1:nrow(names)) {
    x <- x %>%
      rename(!!names$x[k] := !!names$col_name[k]) %>%
      mutate(!!names$x[k] := .data[[names$x[k]]] - 1)
  }
  for (s in symptoms) {
    if (!(s %in% colnames(x))) {
      x <- x %>% mutate(!!s := 0)
    }
  }
  dataModel[[databaseName]] <- x
}

models <- "cluster4"
cdm_names <- names(dataModel)
x <- NULL
tib <- expand_grid(model = models, cdm_name = cdm_names)
for (k in 1:nrow(tib)) {
  yy <- dataModel[[tib$cdm_name[k]]] %>%
    summarise(across(all_of(symptoms), mean)) %>%
    bind_cols(
      dataModel[[tib$cdm_name[k]]] %>%
        summarise(
          median_age = median(age),
          q25_age = quantile(age, 0.25),
          q75_age = quantile(age, 0.75),
          sexMale = mean(sexMale)
        )
    ) %>%
    rename_with(~ paste0("base_", .x))
  xx <- dataModel[[tib$cdm_name[k]]] %>%
    group_by(.data[[tib$model[k]]]) %>%
    summarise(across(all_of(symptoms), mean)) %>%
    inner_join(
      dataModel[[tib$cdm_name[k]]] %>%
        group_by(.data[[tib$model[k]]]) %>%
        summarise(
          number_subjects = n(),
          median_age = median(age),
          q25_age = quantile(age, 0.25),
          q75_age = quantile(age, 0.75),
          sexMale = mean(sexMale)
        ),
      by = tib$model[k]
    ) %>%
    mutate(percentage_subjects = number_subjects/sum(number_subjects)) %>%
    bind_cols(yy) %>%
    mutate(cdm_name = tib$cdm_name[k]) %>%
    pivot_longer(tib$model[k], names_to = "model", values_to = "cluster_label") %>%
    relocate(c("cdm_name", "model", "cluster_label", "number_subjects", "percentage_subjects"))
  x <- bind_rows(x, xx)
}



