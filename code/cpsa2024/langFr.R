# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

survey_data <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/cpsa2024/survey_data.rds")

# Party positions ---------------------------------------------------------

party_positions <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/cpsa2024/party_positions.rds")

# Graph -------------------------------------------------------------------

graph_data <- potgrowth::dynamic_potgrowth_data(
  data = survey_data,
  parties = c("PQ", "PLQ", "CAQ", "QS", "PCQ"),
  issues = "iss_lang_worriedFrProv"
) %>% 
  mutate(estimate_irc = ifelse(estimate_irc > 0, 0, estimate_irc),
         estimate_irc = ifelse(estimate_irc < -1, -1, estimate_irc),
         conf_low_irc = ifelse(conf_low_irc > 0, 0, conf_low_irc),
         conf_low_irc = ifelse(conf_low_irc < -1, -1, conf_low_irc)) %>% 
  left_join(party_positions, by = c("party", "issue")) %>% 
  mutate(is_party_position = ifelse(position == party_position, 1, 0))


graph_data %>%
  filter(party %in% c("PQ", "QS", "CAQ")) %>% 
  ggplot(aes(x = position, y = estimate_irc, 
                       color = party, group = party)) +
  geom_point(aes(alpha = is_party_position, fill = party),
             position = position_dodge(width = 0.6),
             size = 17, shape = 23, show.legend = FALSE) +
  geom_line(aes(color = party, group = party),
            position = position_dodge(width = 0.6),
            linetype = "dashed", alpha = 0.2) +
  potgrowth::geom_irc(conf_low_irc, conf_high_irc, estimate_irc,
                        conf_low_vote, conf_high_vote, estimate_vote,
                      dodge = 0.6) +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  scale_fill_manual(values = potgrowth::qc_party_colors) +
  scale_alpha_continuous(range = c(0, 0.15)) +
  scale_x_discrete(breaks = c("0", "1"),
                   expand = c(0.2, 0.2),
                   labels = c("Not at all worried\nabout french in Quebec",
                              "Worried about\nfrench in Quebec")) +
  scale_y_continuous(breaks = c(-1, -0.75, -0.5, -0.25, 0),
                     labels = c(-1, -0.75, -0.5, -0.25, 0) * 10,
                     limits = c(-1, 0)) +
  guides(alpha = "none") +
  clessnize::theme_clean_light() +
  xlab("\nWorry about french in Quebec") +
  ylab("Potential for growth\n(predicted RCI of non-voters)") +
  theme(panel.background = element_rect(color = "grey80"))

ggsave("_SharedFolder_memoire-pot-growth/graphs/cpsa2024/dynamic_potgrowth/langFr_qs_caq_pq.png",
       width = 7, height = 6, dpi = 300)

graph_data %>%
  filter(issue == "iss_nationalisme_souv" &
           party %in% c("PCQ", "CAQ", "QS")) %>% 
  ggplot(aes(x = position, y = estimate_irc, 
             color = party, group = party)) +
  geom_point(aes(alpha = is_party_position, fill = party),
             position = position_dodge(width = 0.6),
             size = 17, shape = 23, show.legend = FALSE) +
  geom_line(aes(color = party, group = party),
            position = position_dodge(width = 0.6),
            linetype = "dashed", alpha = 0.2) +
  potgrowth::geom_irc(conf_low_irc, conf_high_irc, estimate_irc,
                      conf_low_vote, conf_high_vote, estimate_vote,
                      dodge = 0.6) +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  scale_fill_manual(values = potgrowth::qc_party_colors) +
  scale_alpha_continuous(range = c(0, 0.15)) +
  scale_x_discrete(breaks = c("0", "1"),
                   expand = c(0.2, 0.2),
                   labels = c("Strongly in favor\nthat Quebec stays in Canada",
                              "Strongly in favor\nthat Quebec becomes independent")) +
  scale_y_continuous(breaks = c(-1, -0.75, -0.5, -0.25, 0),
                     labels = c(-1, -0.75, -0.5, -0.25, 0) * 10,
                     limits = c(-1, 0)) +
  guides(alpha = "none") +
  clessnize::theme_clean_light() +
  xlab("\nPosition on Quebec's independence") +
  ylab("Potential for growth\n(predicted RCI of non-voters)") +
  theme(panel.background = element_rect(color = "grey80"))

ggsave("_SharedFolder_memoire-pot-growth/graphs/cpsa2024/dynamic_potgrowth/souverainete_caq_qs_pcq.png",
       width = 8, height = 6, dpi = 300)



  