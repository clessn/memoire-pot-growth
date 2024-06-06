# Est-ce Que les Souverainistes Convaincus Voteraient PLQ si le PLQ Devient Souverainiste?

# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/survey_data_holes.rds") %>% 
  tidyr::drop_na(iss_nationalisme_souv)

clessnverse::count_na(data$irc_PLQ)
clessnverse::count_na(data$mi_issue_nationalisme)
clessnverse::count_na(data$political_knowledge)

## survey data
data <- data %>% 
  select(id, source_id, age_cat, educ, income, male, lang, religion, religiosity,
         irc_CAQ, irc_PLQ, irc_PQ, irc_PCQ, irc_QS,
         partyId, iss_nationalisme_souv, mi_issue_nationalisme) %>% 
  tidyr::drop_na() %>% 
  tidyr::pivot_longer(.,
                      cols = starts_with("irc"),
                      names_to = "party",
                      names_prefix = "irc_",
                      values_to = "irc") %>% 
  mutate(voter = ifelse(irc >= 0, 1, 0))

## party positions on souverainete
df_party_positions <- potgrowth::aggregate_party_positions("_SharedFolder_memoire-pot-growth/data/warehouse/party_positions/expert_surveys/",
                                                        colnames = potgrowth::qc_parties) %>% 
  filter(VARIABLE == "iss_nationalisme_souv")
party_positions_vec <- df_party_positions$position
names(party_positions_vec) <- df_party_positions$party

# Descriptive -------------------------------------------------------------

graph1 <- data %>% 
  group_by(iss_nationalisme_souv, party, voter) %>% 
  summarise(n = n(),
            mean_irc = mean(irc)) %>% 
  group_by(iss_nationalisme_souv, party) %>% 
  mutate(prop = n / sum(n),
         iss_nationalisme_souv = case_when(
           iss_nationalisme_souv == 1 ~ "Fortement d'accord",
           iss_nationalisme_souv == 0.75 ~ "Plutôt d'accord",
           iss_nationalisme_souv == 0.5 ~ "Neutre",
           iss_nationalisme_souv == 0.25 ~ "Plutôt en désaccord",
           iss_nationalisme_souv == 0 ~ "Fortement en\ndésaccord"
         ),
         iss_nationalisme_souv = factor(iss_nationalisme_souv,
                                        levels = c("Fortement en\ndésaccord", "Plutôt en désaccord",
                                                   "Neutre",
                                                   "Plutôt d'accord", "Fortement d'accord")))

linewidth <- 4.5

## only vote
graph1 %>% 
  tidyr::pivot_wider(id_cols = c(iss_nationalisme_souv, party),
                     names_from = "voter",
                     names_prefix = "voter",
                     values_from = c("prop", "mean_irc")) %>% 
  ggplot(aes(x = party, y = prop_voter0)) +
  facet_wrap(~iss_nationalisme_souv, nrow = 1,
             strip.position = "bottom") +
  geom_linerange(aes(ymin = -prop_voter0, ymax = 0),
                 color = "grey", linewidth = linewidth) +
  geom_linerange(aes(ymin = 0, ymax = prop_voter1,
                     color = party),
                 show.legend = FALSE, linewidth = linewidth - 0.3) +
  clessnverse::theme_clean_light() +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  scale_y_continuous(limits = c(-1, 1),
                     breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
                     labels = abs(c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1))*100) +
  ylab(expression("Non-électeurs" ~ phantom() %<-% phantom() ~ phantom() ~ phantom() ~ "Proportion (%)" ~ phantom() %->% phantom() ~ phantom() ~ phantom() ~ phantom() ~ "Électeurs")) +
  xlab("Le Québec devrait devenir un état indépendant\n") +
  geom_text(aes(label = party, y = prop_voter1 + 0.02),
            angle = 90, hjust = 0, size = 2.75) +
  theme(axis.text.x = element_blank(),
        axis.title.y = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(hjust = 0.5, size = 10),
        panel.background = element_rect(color = "grey90"),
        strip.text.x = element_text(size = 7.5, vjust = 1))
ggsave("_SharedFolder_memoire-pot-growth/cecd2024/graphs/souv_plq/descriptive_without_irc.png",
       width = 7, height = 6)

## with RCI layer 
graph1 %>% 
  tidyr::pivot_wider(id_cols = c(iss_nationalisme_souv, party),
                     names_from = "voter",
                     names_prefix = "voter",
                     values_from = c("prop", "mean_irc")) %>% 
  ggplot(aes(x = party, y = prop_voter0)) +
  facet_wrap(~iss_nationalisme_souv, nrow = 1,
             strip.position = "bottom") +
  geom_linerange(aes(ymin = -prop_voter0, ymax = 0,
                     color = mean_irc_voter0),
                 linewidth = linewidth, alpha = 1) +
  scale_color_gradientn(name = "IRC moyen: non-électeurs",
                        colors = c("black",
                                   "#DAA520",
                                   "#50C878",
                                   "#003366"),
                        breaks = rev(c(-0.8, -0.75, -0.7, -0.65, -0.6, -0.55))) +
  guides(color = guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_linerange(aes(ymin = 0, ymax = prop_voter1,
                     color = party, alpha = mean_irc_voter1),
                 linewidth = linewidth - 0.3) +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  guides(color = "none",
         alpha = guide_legend(title.position = "top")) +
  scale_alpha_continuous(name = "IRC moyen: électeurs", range = c(0.1, 1)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  clessnverse::theme_clean_light() +
  scale_y_continuous(limits = c(-1, 1),
                     breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
                     labels = abs(c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1))*100) +
  ylab(expression("Non-électeurs" ~ phantom() %<-% phantom() ~ phantom() ~ phantom() ~ "Proportion (%)" ~ phantom() %->% phantom() ~ phantom() ~ phantom() ~ phantom() ~ "Électeurs")) +
  xlab("Le Québec devrait devenir un état indépendant\n") +
  geom_text(aes(label = party, y = prop_voter1 + 0.02),
            angle = 90, hjust = 0, size = 2.75) +
  theme(legend.title = element_text(),
        legend.position = "right",
        axis.title.y = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(hjust = 0.5, size = 10),
        panel.background = element_rect(color = "grey90"),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = 7.5, vjust = 1))

ggsave("_SharedFolder_memoire-pot-growth/cecd2024/graphs/souv_plq/descriptive_with_irc.png",
       width = 8, height = 6)


# Modeling -------------------------------------------------------------------

model_data <- data %>% 
  mutate(attgap = 1 - abs(iss_nationalisme_souv - party_positions_vec[party])) %>% 
  select(-id, -source_id, -voter, -attgap)

for (i in potgrowth::qc_parties){
  mdatai <- model_data %>% 
    filter(party == i) %>%
    select(-party)
  potgrowth <- mdatai %>% 
    filter(irc < 0)
  votesol <- mdatai %>% 
    filter(irc >= 0)
  model_potgrowth <- lm(irc ~ . + religion * religiosity + age_cat * lang,
                        data = potgrowth)
  model_votesol <- lm(irc ~ . + religion * religiosity + age_cat * lang,
                        data = votesol)
  if (i == potgrowth::qc_parties[1]){
    potgrowth_models_list <- list(
      i = model_potgrowth
    )
    names(potgrowth_models_list) <- i
    votesol_models_list <- list(
      i = model_votesol
    )
    names(votesol_models_list) <- i
  } else {
    potgrowth_models_list[[i]] <- model_potgrowth
    votesol_models_list[[i]] <- model_votesol
  }
  message(i)
}


# Predict -----------------------------------------------------------------

t <- marginaleffects::predictions(model = potgrowth_models_list$CAQ,
                                  by = c("iss_nationalism_souv"))

