# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/with_adj_saliency.rds")

media_vars <- names(Data %>% select(starts_with("reception_polinfo")))
socialmedia_vars <- names(Data %>% select(starts_with("reception_socialmedia")))

# Compute models ----------------------------------------------------------

ses <- c("granular", "age_cat", "educ", "income", "male", "lang")

for (i in media_vars){
  model <- potgrowth::iv_model(
    data = Data,
    iv_to_predict = i,
    ses = ses)
  saveRDS(model,
          paste0("_SharedFolder_memoire-pot-growth/data/marts/models/reception_models/media_models/", i, ".rds"))
  message(i)
}

for (i in socialmedia_vars){
  model <- potgrowth::iv_model(
    data = Data,
    iv_to_predict = i,
    ses = ses)
  saveRDS(model,
          paste0("_SharedFolder_memoire-pot-growth/data/marts/models/reception_models/socialmedia_models/", i, ".rds"))
  message(i)
}

howreach_model <- potgrowth::iv_model(
  data = Data,
  iv_to_predict = "reception_howreach",
  ses = ses,
  model_type = "multinom")
saveRDS(howreach_model,
        "_SharedFolder_memoire-pot-growth/data/marts/models/reception_models/reception_howreach.rds")
