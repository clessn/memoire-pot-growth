# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/with_adj_saliency.rds")

saliency_vars <- names(Data %>% select(starts_with("adj_saliency")))

# Compute models ----------------------------------------------------------

ses <- c("granular", "age_cat", "educ", "income", "male", "lang")

for (i in saliency_vars){
  model <- potgrowth::iv_model(
    data = Data,
    iv_to_predict = i,
    ses = ses)
  saveRDS(model,
          paste0("_SharedFolder_memoire-pot-growth/data/marts/models/attitude_models/saliency_models/", i, ".rds"))
  message(i)
}

