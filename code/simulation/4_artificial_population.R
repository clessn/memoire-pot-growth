# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

data <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/sim_data/3_with_probs_switch.rds")

# Create variable that contains the number of times this profile will be repeated --------
data$repeats <- ifelse(data$prct * data$total_pop < 1, 1, round(data$prct * data$total_pop)) ## minimum is 1

# Create artificial population --------------------------------------------

set.seed(123)
for (i in sort(unique(data$profil_id))){
  if (i == 1){
    message(paste0(max(data$profil_id), " profiles starting. Progress:"))
  }
  profilei_df <- data[data$profil_id == i,]
  repeatsi <- profilei_df$repeats[1]
  rowi_df <- profilei_df[1,] %>% 
    select(-party, -voteint, -potential_for_growth, -vote_solidity,
           -prob_switch_vote, -prob_switch_no_vote, -repeats)
  datai <- rowi_df[rep(1, repeatsi),]
  datai$base_voteint <- sample(profilei_df$party, size = repeatsi,
                          replace = TRUE, prob = profilei_df$voteint)
  probs_switch_no_vote <- profilei_df$prob_switch_no_vote
  names(probs_switch_no_vote) <- profilei_df$party
  datai$prob_switch_no_vote <- probs_switch_no_vote[datai$base_voteint]
  saveRDS(datai, paste0("_SharedFolder_memoire-pot-growth/data/marts/sim_data/4_artificial_population/", i, ".rds"))
  if(i %% 100 == 0){
    message(paste0(i, "/", max(data$profil_id), " - ", round(i/max(data$profil_id) * 100), "%"))
  }
}
