# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

data <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/sim_data/3_with_probs_switch.rds")

# Create artificial population --------------------------------------------
## repeat each profile 50 times and simulate vote intention.

repeats <- 50
set.seed(123)
for (i in sort(unique(data$profil_id))){
  if (i == 1){
    message(paste0(max(data$profil_id), " profiles starting. Progress:"))
  }
  profilei_df <- data[data$profil_id == i,]
  #rowi_df <- profilei_df[1,] %>% 
  #  select(profil_id)
  #datai <- rowi_df[rep(1, repeats),]
  datai <- data.frame(profil_id = rep(i, repeats))
  datai$base_voteint <- sample(profilei_df$party, size = repeats,
                          replace = TRUE, prob = profilei_df$voteint)
  probs_switch_no_vote <- profilei_df$prob_switch_no_vote
  names(probs_switch_no_vote) <- profilei_df$party
  datai$prob_switch_no_vote <- probs_switch_no_vote[datai$base_voteint]
  saveRDS(datai, paste0("_SharedFolder_memoire-pot-growth/data/marts/sim_data/4_artificial_population/", i, ".rds"))
  if(nrow(datai) == 0){
    message(paste0(i, " has 0 rows"))
  }
  if(i %% 100 == 0){
    message(paste0(i, "/", max(data$profil_id), " - ", round(i/max(data$profil_id) * 100), "%"))
  }
}

#files <- list.files("_SharedFolder_memoire-pot-growth/data/marts/sim_data/4_artificial_population",
#                    full.names = TRUE)
#
#for (i in 1:length(files)){
#  if (i == 1){
#    output <- readRDS(files[i])
#  } else {
#    output <- rbind(output, readRDS(files[i]))
#  }
#  if (i %% 100 == 0){
#    message(paste0(i, "/", length(files), " - ", round(i/length(files) * 100), "%"))
#  }
#}
