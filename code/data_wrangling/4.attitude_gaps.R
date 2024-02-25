# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/with_scales.rds")

aggregated_issue_positions <- potgrowth::aggregate_party_positions("_SharedFolder_memoire-pot-growth/data/warehouse/party_positions/expert_surveys/",
                                          colnames = potgrowth::qc_parties)
factanal_object <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/scales/factanal.rds")
factanal_object$loadings
scale_names <- c("souv_langfr", "libertarian", "woke", "laicite", "immigration", "lien3")


# Compute party scales ----------------------------------------------------
party_scales <- potgrowth::compute_party_scales(aggregated_issue_positions = aggregated_issue_positions,
                                                factanal_object = factanal_object,
                                                scale_names = scale_names)

party_scales["CAQ", "scale_souv_langfr"]
party_scales["CAQ", "scale_libertarian"]
party_scales["PCQ", "scale_libertarian"]
party_scales["QS", "scale_libertarian"]

# Compute attitude gaps ---------------------------------------------------

DataGaps <- potgrowth::compute_attitude_gaps(party_scales_matrix = party_scales,
                                             survey_data = Data)

saveRDS(DataGaps, "_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/withgaps.rds")

# Explo -------------------------------------------------------------------
as.data.frame(party_scales) %>% 
  mutate(party = rownames(.)) %>% 
  tidyr::pivot_longer(., cols = starts_with("scale")) %>% 
  ggplot(aes(x = value, y = name)) +
  geom_point(aes(group = party, color = party),
             size = 4, alpha = 0.7) +
  clessnverse::theme_clean_light() +
  scale_color_manual(values = potgrowth::qc_party_colors)


Data %>% 
  tidyr::pivot_longer(., cols = starts_with("scale")) %>% 
  rename(party = voteInt) %>% 
  ggplot(aes(x = value, y = name)) +
  facet_wrap(~party) +
  ggridges::geom_density_ridges(aes(fill = party),
                                color = NA, alpha = 0.6) +
  geom_point(data = as.data.frame(party_scales) %>% 
               mutate(party = rownames(.)) %>% 
               tidyr::pivot_longer(., cols = starts_with("scale")),
             aes(group = party), color = "black",
             size = 4, alpha = 0.7) +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  scale_fill_manual(values = potgrowth::qc_party_colors)

