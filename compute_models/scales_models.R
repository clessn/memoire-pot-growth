# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/with_adj_saliency.rds")

scales <- names(Data %>% select(starts_with("scale_"), -scale_leftRight))

# Compute models ----------------------------------------------------------

ses <- c("granular", "age_cat", "educ", "income", "male", "lang")

for (i in scales){
  model <- potgrowth::iv_model(
    data = Data,
    iv_to_predict = i,
    ses = ses)
  saveRDS(model,
          paste0("_SharedFolder_memoire-pot-growth/data/marts/models/attitude_models/scales_models/", i, ".rds"))
  message(i)
}

