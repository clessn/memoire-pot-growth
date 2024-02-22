## political_trust
## political_knowledge

# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/with_adj_saliency.rds")

# Compute models ----------------------------------------------------------

ses <- c("granular", "age_cat", "educ", "income", "male", "lang")

political_trust_model <- potgrowth::iv_model(
    data = Data,
    iv_to_predict = "political_trust",
    ses = ses)
saveRDS(political_trust_model,
        "_SharedFolder_memoire-pot-growth/data/marts/models/static_attitudes_models/political_trust_model.rds")

political_knowledge_model <- potgrowth::iv_model(
  data = Data,
  iv_to_predict = "political_knowledge",
  ses = ses)
saveRDS(political_knowledge_model,
        "_SharedFolder_memoire-pot-growth/data/marts/models/static_attitudes_models/political_knowledge_model.rds")
