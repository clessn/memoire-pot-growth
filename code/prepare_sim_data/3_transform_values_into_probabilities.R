# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

data <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/sim_data/2_with_vote_variables.rds")

# Potential for growth  ---------------------------------------------------

output <- data %>% 
  group_by(profil_id) %>% 
  mutate(prob_switch_vote = potgrowth::softmax(potential_for_growth * 10)) ### softmax is really scale sensible. adjust this arbitrarily

# Vote solidity ----------------------------------------------------

### for now, simply calculate it like this.
output$prob_switch_no_vote <- 1 - output$vote_solidity

saveRDS(output, "_SharedFolder_memoire-pot-growth/data/marts/sim_data/3_with_probs_switch.rds")
