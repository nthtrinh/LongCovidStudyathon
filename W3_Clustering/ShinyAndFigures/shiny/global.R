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

models <- c("model2", "model3", "model4", "model5", "model6", "model7")
dataModel <- list()
for (databaseName in db) {
  dataModel[[databaseName]] <- list()
  names <- read_csv(here("data", databaseName, paste0("names_symptoms_", databaseName, ".csv")), show_col_types = FALSE) %>%
    rename("col_name" = "...1") %>%
    mutate(col_name = paste0("LC_", col_name))
  for (model in models) {
    x <- read_csv(here("data", databaseName, paste0("Clustering_LCA_", model, "_y.csv")), show_col_types = FALSE) %>%
      bind_cols(
        read_csv(here("data", databaseName, paste0("Clustering_LCA_", model, "_x.csv")), show_col_types = FALSE)
      ) %>%
      select(-"X.Intercept.") %>%
      mutate(cdm_name = databaseName) %>%
      bind_cols(
        read_csv(here("data", databaseName, paste0("Clustering_LCA_", model, "_predclass.csv")), show_col_types = FALSE)
      ) %>%
      rename(!!model := "x")
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
    x <- x %>%
      mutate(
      num_symptoms = 
      Abdominal_pain + Allergy + Altered_smell_taste + Anxiety + Blurred_vision + 
      `Chest pain or angina` + Cognitive_dysfunction_brain_fog + Cough + 
      `Depression wo recurrence` + Dizziness + Dyspnea + Fatigue_malaise + 
      Gastrointestinal_issues + Headache + Intermittent_fever + Joint_pain + 
      Memory_issues + Menstrual_problems + Muscle_spasms_and_pain + Neuralgia + 
        Pins_and_needles_sensation + Postexertional_fatigue + Sleep_disorder + 
        Tachycardia + Tinnitus_hearing_problems
    )
    dataModel[[databaseName]][[model]] <- x
  }
}

x <- NULL
tib <- expand_grid(model = models, cdm_name = unlist(db))
for (k in 1:nrow(tib)) {
  yy <- dataModel[[tib$cdm_name[k]]][[tib$model[k]]] %>%
    summarise(across(all_of(symptoms), mean)) %>%
    bind_cols(
      dataModel[[tib$cdm_name[k]]][[tib$model[k]]] %>%
        summarise(
          all_N = n(),
          median_age = median(age),
          q25_age = quantile(age, 0.25),
          q75_age = quantile(age, 0.75),
          sexMale = mean(sexMale),
          all_mean_symptoms = mean(num_symptoms)
        )
    ) %>%
    pivot_longer(all_of(symptoms), names_to = "cohort_name", values_to = "incidence") %>%
    rename(
      "all_median_age" = "median_age",
      "all_q25_age" = "q25_age",
      "all_q75_age" = "q75_age",
      "all_sexMale" = "sexMale"
    )
  xx <- dataModel[[tib$cdm_name[k]]][[tib$model[k]]] %>%
    group_by(.data[[tib$model[k]]]) %>%
    summarise(across(all_of(symptoms), mean)) %>%
    inner_join(
      dataModel[[tib$cdm_name[k]]][[tib$model[k]]] %>%
        group_by(.data[[tib$model[k]]]) %>%
        summarise(
          number_subjects = n(),
          median_age = median(age),
          q25_age = quantile(age, 0.25),
          q75_age = quantile(age, 0.75),
          sexMale = mean(sexMale),
          mean_symptoms = mean(num_symptoms)
        ),
      by = tib$model[k]
    ) %>%
    mutate(percentage_subjects = number_subjects/sum(number_subjects)) %>%
    mutate(cdm_name = tib$cdm_name[k]) %>%
    pivot_longer(tib$model[k], names_to = "model", values_to = "cluster_label") %>%
    relocate(c("cdm_name", "model", "cluster_label", "number_subjects", "percentage_subjects")) %>%
    pivot_longer(all_of(symptoms), names_to = "cohort_name", values_to = "proportion") %>%
    left_join(
      yy, by = "cohort_name"
    )
  x <- bind_rows(x, xx)
}

 x <- x %>%
   rename(
     cluster_id = cluster_label,
     database_name = cdm_name,
     N = number_subjects,
     Nprop = percentage_subjects,
     proportion_male = sexMale,
     all_proportion_male = all_sexMale
   ) %>%
  mutate(
    num_clust = as.numeric(substr(model, 6, 6)),
    fold_change = proportion/incidence
  )
 
x <- x %>%
  #head(1) %>%
  rowwise() %>%
  mutate(
    significant = prop.test(x = c(proportion*N, incidence*all_N), n = c(N, all_N))$p.value
  ) %>%
  mutate(
    significant = if_else(
      significant < 0.05,
      if_else(
        proportion > incidence,
        "greater",
        "lower"
      ),
      "no significant"
    )
  ) %>%
  mutate(significant = if_else(is.na(significant), "no significant", significant))

plot_data <- x %>%
  mutate(BIC = 1)


