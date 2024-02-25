# Packages ----------------------------------------------------------------
library(dplyr)
library(nnet)

# Data --------------------------------------------------------------------
data <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/sim_data/1_with_ivs.rds")

# Compute attitude gaps ----------------------------------------------------
aggregated_issue_positions <- potgrowth::aggregate_party_positions("_SharedFolder_memoire-pot-growth/data/warehouse/party_positions/expert_surveys/",
                                                                   colnames = potgrowth::qc_parties)
factanal_object <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/scales/factanal.rds")
factanal_object$loadings
scale_names <- c("souv_langfr", "libertarian", "woke", "laicite", "immigration", "lien3")
party_scales <- potgrowth::compute_party_scales(aggregated_issue_positions = aggregated_issue_positions,
                                                factanal_object = factanal_object,
                                                scale_names = scale_names)
data_gaps <- potgrowth::compute_attitude_gaps(party_scales_matrix = party_scales,
                                             survey_data = data)

# Predict models ----------------------------------------------------------

models_list <- list.files("_SharedFolder_memoire-pot-growth/data/marts/models/vote_models/",
                          full.names = TRUE)
parties <- potgrowth::qc_parties

voteint_model <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/models/vote_models/vote_int.rds")
voteints_matrix <- predict(voteint_model, newdata = data_gaps, type = "probs")

for (i in 1:length(parties)){
  partyi <- parties[i]
  datai <- data_gaps %>%
    ungroup() %>% 
    mutate(party = partyi,
           voteint = unlist(voteints_matrix[, partyi]))
  potential_for_growth_model <- readRDS(paste0("_SharedFolder_memoire-pot-growth/data/marts/models/vote_models/potential_for_growth_", partyi, ".rds"))
  vote_solidity_model <- readRDS(paste0("_SharedFolder_memoire-pot-growth/data/marts/models/vote_models/vote_solidity_", partyi, ".rds"))
  gaps <- as.data.frame(sapply(scale_names,
                               FUN = function(x){
                                 return(data_gaps[[paste0("attitudegap_", partyi, "_scale_", x)]])
                               }))
  names(gaps) <- paste0("attitudegap_scale_", names(gaps))
  newdatai <- cbind(datai, gaps)
  potential_for_growth_values <- predict(object = potential_for_growth_model, newdata = newdatai)
  potential_for_growth_values <- ifelse(potential_for_growth_values < -1, -1, potential_for_growth_values)
  potential_for_growth_values <- ifelse(potential_for_growth_values > 0, 0, potential_for_growth_values)
  datai$potential_for_growth <- potential_for_growth_values
  vote_solidity_values <- predict(object = vote_solidity_model, newdata = newdatai)
  #vote_solidity_values <- vote_solidity_values + abs(min(vote_solidity_values)) ### artificially put minimum value at 0
  #vote_solidity_values <- ifelse(vote_solidity_values < 0, 0, vote_solidity_values)
  #vote_solidity_values <- ifelse(vote_solidity_values > 1, 1, vote_solidity_values)
  datai$vote_solidity <- vote_solidity_values
  if (i == 1){
    output_df <- datai
  } else {
    output_df <- rbind(output_df, datai)
  }
  message(partyi)
}

output_df <- output_df %>% 
  mutate(vote_solidity = vote_solidity + abs(min(vote_solidity))) %>%  ### artificially put minimum value at 0
  arrange(profil_id)

saveRDS(output_df, "_SharedFolder_memoire-pot-growth/data/marts/sim_data/2_with_vote_variables.rds")
