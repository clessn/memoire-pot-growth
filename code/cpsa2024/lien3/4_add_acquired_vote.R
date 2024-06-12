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
  parties = c("PCQ", "CAQ"),
  issues = "iss_lien3_accord",
) %>% 
  mutate(estimate_irc = ifelse(estimate_irc > 0, 0, estimate_irc),
         estimate_irc = ifelse(estimate_irc < -1, -1, estimate_irc),
         conf_low_irc = ifelse(conf_low_irc > 0, 0, conf_low_irc),
         conf_low_irc = ifelse(conf_low_irc < -1, -1, conf_low_irc)) %>% 
  left_join(party_positions, by = c("party", "issue")) %>% 
  mutate(is_party_position = ifelse(position == party_position, 1, 0))

choices <- c(
  "0" = "Strongly disagree",
  "0.33" = "Somewhat disagree",
  "0.67" = "Somewhat agree",
  "1" = "Strongly agree"
)

graph_data %>%
  ggplot(aes(x = position, y = estimate_irc, 
             color = party, group = party)) +
  geom_point(aes(alpha = is_party_position, fill = party),
             position = position_dodge(width = 0.5),
             size = 17, shape = 23, show.legend = FALSE) +
  geom_line(aes(color = party, group = party),
            position = position_dodge(width = 0.5),
            linetype = "dashed", alpha = 0.2) +
  potgrowth::geom_irc(conf_low_irc, conf_high_irc, estimate_irc,
                      conf_low_vote, conf_high_vote, estimate_vote,
                      dodge = 0.5) +
  scale_color_manual(values = potgrowth::qc_party_colors,
                     labels = c("PCQ" = "CPQ", "CAQ")) +
  scale_fill_manual(values = potgrowth::qc_party_colors) +
  scale_alpha_continuous(range = c(0, 0.15)) +
  scale_x_discrete(labels = choices,
                   breaks = c("0", "0.33", "0.67", "1")) +
  scale_y_continuous(breaks = c(-1, -0.75, -0.5, -0.25, 0),
                     labels = c(-1, -0.75, -0.5, -0.25, 0) * 10,
                     limits = c(-1, 0)) +
  guides(alpha = "none") +
  clessnize::theme_clean_light() +
  labs(caption = "Diamonds indicate the parties' positions on the issue.\nBars indicate the proportion of acquired voters in this segment.") +
  xlab("\nPosition on Quebec City's 3rd link") +
  ylab("Potential for Growth\n(predicted RCI of non-voters)")

ggsave("_SharedFolder_memoire-pot-growth/graphs/cpsa2024/dynamic_potgrowth/lien3/4_add_acquired_vote.png",
       width = 8, height = 6, dpi = 300)
