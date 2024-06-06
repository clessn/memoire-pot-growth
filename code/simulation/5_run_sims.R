# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
files <- list.files("_SharedFolder_memoire-pot-growth/data/marts/sim_data/4_artificial_population",
                    full.names = TRUE)

by_profile <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/sim_data/3_with_probs_switch.rds")

nsims <- 500
for (i in 1:length(files)){
  data <- readRDS(files[i])
  profil_idi <- unique(data$profil_id)
  probs_all <- by_profile$prob_switch_vote[by_profile$profil_id == profil_idi]
  names(probs_all) <- by_profile$party[by_profile$profil_id == profil_idi]
  simresult <- as.data.frame(t(sapply(function(p) sample(c(0, 1), size = nsims,
                                                                    replace = TRUE, prob = c(1-p, p)),
                                                 X = data$prob_switch_no_vote))) %>% 
    setNames(paste0("sim_id", 1:ncol(.))) %>%
    mutate(id = 1:nrow(.),
           base_voteint = data$base_voteint) %>% 
    tidyr::pivot_longer(., cols = starts_with("sim_id"),
                        names_to = "sim_id",
                        names_prefix = "sim_id",
                        values_to = "switch") %>% 
    mutate(sim_id = as.numeric(sim_id))
  ### create list of potential for growth when switching
  for (j in potgrowth::qc_parties){
    if (j == potgrowth::qc_parties[1]){
      probs_list <- list(
        j = probs_all[names(probs_all) != j] / sum(probs_all[names(probs_all) != j])
      )
      names(probs_list) <- j
    } else {
      probs_list[[j]] <- probs_all[names(probs_all) != j] / sum(probs_all[names(probs_all) != j])
    }
  }
  simresult$outcome_party <- sapply(
    X = simresult$base_voteint,
    FUN = function(base_voteint){
      sample(
        x = names(probs_list[[base_voteint]]),
        size = 1,
        prob = probs_list[[base_voteint]]
      )
    }
  )
  simresult$outcome_party[simresult$switch == 0] <- simresult$base_voteint[simresult$switch == 0]
  simresult_agg <- simresult %>% 
    group_by(sim_id, outcome_party) %>% 
    summarise(n_voters = n(), .groups = 'drop') %>%
    tidyr::complete(sim_id, outcome_party, fill = list(n_voters = 0)) %>% 
    group_by(sim_id) %>%
    mutate(total_voters_in_segment = sum(n_voters),
           prop = n_voters / total_voters_in_segment,
           profil_id = profil_idi) %>% 
    relocate(profil_id)
  saveRDS(simresult_agg,
          paste0("_SharedFolder_memoire-pot-growth/data/marts/sim_data/5_sim_results/", profil_idi, ".rds"))
  if (i %% 100 == 0){
    message(paste0(i, "/", length(files), " - ", round(i/length(files) * 100), "%"))
  }
}

library(ggplot2)
simresult_agg %>% 
  ggplot(aes(x = prop, y = outcome_party)) +
  ggridges::geom_density_ridges()
