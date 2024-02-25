# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/with_adj_saliency.rds")

# Compute vote_solidity and potential_for_growth models -------------------

for (i in potgrowth::qc_parties){
  solidity_model <- potgrowth::votemodel_lm(
    data = potgrowth::get_model_data(party = i, model_type = "vote_solidity",
                                     data = Data,
                                     ses_and_vis_to_include = c("granular", "age_cat", "educ", "income", "male", "lang"))
  )
  saveRDS(solidity_model, paste0("_SharedFolder_memoire-pot-growth/data/marts/models/vote_models/vote_solidity_", i, ".rds"))
  potgrowth_model <- potgrowth::votemodel_lm(
    data = potgrowth::get_model_data(party = i, model_type = "potential_for_growth",
                                     data = Data,
                                     ses_and_vis_to_include = c("granular", "age_cat", "educ", "income", "male", "lang"))
  )
  saveRDS(potgrowth_model, paste0("_SharedFolder_memoire-pot-growth/data/marts/models/vote_models/potential_for_growth_", i, ".rds"))
  message(i)
}

# Vote intent -------------------------------------------------------------

data_voteint <- potgrowth::get_voteint_model_data(
  data = Data,
  ses_and_vis_to_include = c("granular", "age_cat", "educ", "income", "male", "lang")
)

model <- potgrowth::votemodel_multinom(
  data = data_voteint,
  parties = potgrowth::qc_parties
)
saveRDS(model, "_SharedFolder_memoire-pot-growth/data/marts/models/vote_models/vote_int.rds")

