# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Cores -------------------------------------------------------------------

# Data --------------------------------------------------------------------
no_cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(no_cores)
doParallel::registerDoParallel(cl)
files <- list.files(path = "_SharedFolder_memoire-pot-growth/data/marts/sim_data/5_sim_results/",
                    full.names = TRUE)
data <- pbapply::pblapply(X = files, FUN = readRDS,
                               cl = cl)
data <- data.table::rbindlist(data)
parallel::stopCluster(cl)

profil_data <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/sim_data/3_with_probs_switch.rds") %>% 
  select(profil_id, riding_id, riding_name, total_pop, granular, male, age_cat,
         lang, income, educ, prct, party)

data <- left_join(data, profil_data, by = c("profil_id", "outcome_party" = "party")) %>% 
  rename(party = outcome_party)


# Number of seats by party ------------------------------------------------

agg <- data %>% 
  mutate(party_riding_prop = prct * prop) %>% 
  group_by(sim_id, party, riding_id, riding_name) %>% 
  summarise(prop = sum(party_riding_prop)) %>% 
  arrange(party, sim_id)

seats <- agg %>% 
  group_by(sim_id, riding_id, riding_name) %>% 
  mutate(maxprop = max(prop)) %>% 
  filter(prop == maxprop)

party_results <- seats %>% 
  group_by(sim_id, party, .drop = TRUE) %>% 
  summarise(n = n()) %>% 
  tidyr::complete(fill = list(n = 0))

ggplot(party_results, aes(x = n, y = party)) +
  ggridges::geom_density_ridges(scale = 0.95,
                                bandwidth = 0.35,
                                color = NA, aes(fill = party),
                                alpha = 0.75, show.legend = FALSE) +
  clessnverse::theme_clean_light() +
  scale_fill_manual(values = potgrowth::qc_party_colors) +
  scale_x_continuous(breaks = seq(from = 0, to = 125, by = 5)) +
  ylab("") + xlab("Nombre de sièges")
