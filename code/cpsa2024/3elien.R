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
  issues = "iss_lien3_accord",
  ses_interactions = c("region")
) %>% 
  mutate(estimate_irc = ifelse(estimate_irc > 0, 0, estimate_irc),
         estimate_irc = ifelse(estimate_irc < -1, -1, estimate_irc),
         conf_low_irc = ifelse(conf_low_irc > 0, 0, conf_low_irc),
         conf_low_irc = ifelse(conf_low_irc < -1, -1, conf_low_irc),
         conf_high_irc = ifelse(conf_high_irc > 0, 0, conf_high_irc),
         conf_high_irc = ifelse(conf_high_irc < -1, -1, conf_high_irc)
  ) %>% 
  left_join(party_positions, by = c("party", "issue")) %>% 
  mutate(is_party_position = ifelse(position == party_position, 1, 0))

graph_data %>%
  filter(party %in% c("QS", "CAQ") &
           region == "capitale_nationale") %>% 
  ggplot(aes(x = position, y = estimate_irc, 
             color = party, group = party)) +
  geom_point(aes(alpha = is_party_position, fill = party),
             position = position_dodge(width = 0.5),
             size = 13, shape = 23, show.legend = FALSE) +
  geom_line(aes(color = party, group = party),
            position = position_dodge(width = 0.5),
            linetype = "dashed", alpha = 0.2) +
  potgrowth::geom_irc(conf_low_irc, conf_high_irc, estimate_irc,
                      conf_low_vote, conf_high_vote, estimate_vote,
                      dodge = 0.5) +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  scale_fill_manual(values = potgrowth::qc_party_colors) +
  scale_alpha_continuous(range = c(0, 0.15)) +
  scale_x_discrete(breaks = c("0", "1"),
                   expand = c(0.2, 0.2),
                   labels = c("Strongly disagree",
                              "Strongly agree")) +
  scale_y_continuous(breaks = c(-1, -0.75, -0.5, -0.25, 0),
                     labels = c(-1, -0.75, -0.5, -0.25, 0) * 10,
                     limits = c(-1, 0)) +
  guides(alpha = "none") +
  clessnize::theme_clean_light() +
  xlab("\nPosition on Quebec City's 3rd link") +
  ylab("Potential for growth\n(predicted RCI of non-voters)") +
  theme(panel.background = element_rect(color = "grey80"))

ggsave("_SharedFolder_memoire-pot-growth/graphs/cpsa2024/dynamic_potgrowth/lien3_qs_caq_capitale_nationale.png",
       width = 7, height = 6, dpi = 300)



graph_data %>%
  filter(party %in% c("PQ", "PLQ", "PCQ") &
           region == "capitale_nationale") %>% 
  ggplot(aes(x = position, y = estimate_irc, 
             color = party, group = party)) +
  geom_point(aes(alpha = is_party_position, fill = party),
             position = position_dodge(width = 0.5),
             size = 13, shape = 23, show.legend = FALSE) +
  geom_line(aes(color = party, group = party),
            position = position_dodge(width = 0.5),
            linetype = "dashed", alpha = 0.2) +
  potgrowth::geom_irc(conf_low_irc, conf_high_irc, estimate_irc,
                      conf_low_vote, conf_high_vote, estimate_vote,
                      dodge = 0.5) +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  scale_fill_manual(values = potgrowth::qc_party_colors) +
  scale_alpha_continuous(range = c(0, 0.15)) +
  scale_x_discrete(breaks = c("0", "1"),
                   expand = c(0.2, 0.2),
                   labels = c("Strongly disagree",
                              "Strongly agree")) +
  scale_y_continuous(breaks = c(-1, -0.75, -0.5, -0.25, 0),
                     labels = c(-1, -0.75, -0.5, -0.25, 0) * 10,
                     limits = c(-1, 0)) +
  guides(alpha = "none") +
  clessnize::theme_clean_light() +
  xlab("\nPosition on Quebec City's 3rd link") +
  ylab("Potential for growth\n(predicted RCI of non-voters)") +
  theme(panel.background = element_rect(color = "grey80"))

ggsave("_SharedFolder_memoire-pot-growth/graphs/cpsa2024/dynamic_potgrowth/lien3_pq_plq_pcq_capitale_nationale.png",
       width = 7, height = 6, dpi = 300)



# Graph by age group -------------------------------------------------------------------

graph_data <- potgrowth::dynamic_potgrowth_data(
  data = survey_data,
  parties = c("PQ", "PLQ", "CAQ", "QS", "PCQ"),
  issues = "iss_lien3_accord",
  ses_interactions = c("age_cat", "region")
) %>% 
  mutate(estimate_irc = ifelse(estimate_irc > 0, 0, estimate_irc),
         estimate_irc = ifelse(estimate_irc < -1, -1, estimate_irc),
         conf_low_irc = ifelse(conf_low_irc > 0, 0, conf_low_irc),
         conf_low_irc = ifelse(conf_low_irc < -1, -1, conf_low_irc),
         conf_high_irc = ifelse(conf_high_irc > 0, 0, conf_high_irc),
         conf_high_irc = ifelse(conf_high_irc < -1, -1, conf_high_irc)
         ) %>% 
  left_join(party_positions, by = c("party", "issue")) %>% 
  mutate(is_party_position = ifelse(position == party_position, 1, 0))

graph_data %>%
  filter(party %in% c("QS", "CAQ") &
           region == "capitale_nationale") %>% 
  ggplot(aes(x = position, y = estimate_irc, 
                       color = party, group = party)) +
  facet_wrap(~age_cat) +
  geom_point(aes(alpha = is_party_position, fill = party),
             position = position_dodge(width = 0.5),
             size = 13, shape = 23, show.legend = FALSE) +
  geom_line(aes(color = party, group = party),
            position = position_dodge(width = 0.5),
            linetype = "dashed", alpha = 0.2) +
  potgrowth::geom_irc(conf_low_irc, conf_high_irc, estimate_irc,
                        conf_low_vote, conf_high_vote, estimate_vote,
                      dodge = 0.5) +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  scale_fill_manual(values = potgrowth::qc_party_colors) +
  scale_alpha_continuous(range = c(0, 0.15)) +
  scale_x_discrete(breaks = c("0", "1"),
                   expand = c(0.2, 0.2),
                   labels = c("Strongly disagree",
                              "Strongly agree")) +
  scale_y_continuous(breaks = c(-1, -0.75, -0.5, -0.25, 0),
                     labels = c(-1, -0.75, -0.5, -0.25, 0) * 10,
                     limits = c(-1, 0)) +
  guides(alpha = "none") +
  clessnize::theme_clean_light() +
  xlab("\nPosition on Quebec City's 3rd link") +
  ylab("Potential for growth\n(predicted RCI of non-voters)") +
  theme(panel.background = element_rect(color = "grey80"))

ggsave("_SharedFolder_memoire-pot-growth/graphs/cpsa2024/dynamic_potgrowth/lien3_qs_caq_age_capitale_nationale.png",
       width = 12, height = 10, dpi = 300)

graph_data %>%
  filter(party %in% c("PQ", "PLQ") &
           region == "capitale_nationale") %>% 
  ggplot(aes(x = position, y = estimate_irc, 
             color = party, group = party)) +
  facet_wrap(~age_cat) +
  geom_point(aes(alpha = is_party_position, fill = party),
             position = position_dodge(width = 0.5),
             size = 13, shape = 23, show.legend = FALSE) +
  geom_line(aes(color = party, group = party),
            position = position_dodge(width = 0.5),
            linetype = "dashed", alpha = 0.2) +
  potgrowth::geom_irc(conf_low_irc, conf_high_irc, estimate_irc,
                      conf_low_vote, conf_high_vote, estimate_vote,
                      dodge = 0.5) +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  scale_fill_manual(values = potgrowth::qc_party_colors) +
  scale_alpha_continuous(range = c(0, 0.15)) +
  scale_x_discrete(breaks = c("0", "1"),
                   expand = c(0.2, 0.2),
                   labels = c("Strongly disagree",
                              "Strongly agree")) +
  scale_y_continuous(breaks = c(-1, -0.75, -0.5, -0.25, 0),
                     labels = c(-1, -0.75, -0.5, -0.25, 0) * 10,
                     limits = c(-1, 0)) +
  guides(alpha = "none") +
  clessnize::theme_clean_light() +
  xlab("\nPosition on Quebec City's 3rd link") +
  ylab("Potential for growth\n(predicted RCI of non-voters)") +
  theme(panel.background = element_rect(color = "grey80"))

ggsave("_SharedFolder_memoire-pot-growth/graphs/cpsa2024/dynamic_potgrowth/lien3_pq_plq_age_capitale_nationale.png",
       width = 12, height = 10, dpi = 300)
