# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
party_positions <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/cpsa2024/party_positions.rds") %>% 
  filter(issue == "iss_nationalisme_souv") %>% 
  mutate(y = case_when(
    party == "CAQ" ~ 0,
    party == "PQ" ~ 0,
    party == "QS" ~ 0.1,
    party == "PLQ" ~ 0.1,
    party == "PCQ" ~ 0
  ),
  party_position = ifelse(party_position == 0.25, 0.33, party_position),
  party_position = ifelse(party_position == 0.75, 0.67, party_position),
  party_label = case_when(
    party == "CAQ" ~ "CAQ",
    party == "PQ" ~ "PQ",
    party == "QS" ~ "QS",
    party == "PLQ" ~ "QLP",
    party == "PCQ" ~ "CPQ"
  ))

# Graph -------------------------------------------------------------------

choices <- c(
  "0" = "Strongly in favor\nof Quebec remaining\nin Canada",
  "0.33" = "Moderately in favor\nof Quebec remaining\nin Canada",
  "0.67" = "Moderately in favor\nof Quebec becoming\na sovereign country",
  "1" = "Strongly in favor\nof Quebec becoming\na sovereign country"
)

ggplot(party_positions, aes(x = party_position, y = y)) +
  geom_text(aes(label = party_label, color = party),
            show.legend = FALSE, hjust = 0.5) +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  ylim(-0.05, 0.15) +
  clessnize::theme_clean_light() +
  xlab("\nPosition on Quebec's independence") +
  scale_x_continuous(labels = choices,
                     breaks = c(0, 0.33, 0.67, 1),
                     expand = c(0.1, 0.1)) +
  labs(caption = "Aggregation of 13 expert surveys.") +
  theme(axis.text.x = element_text(size = 6.5, hjust = 0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(size = 6))

ggsave("_SharedFolder_memoire-pot-growth/graphs/cpsa2024/dynamic_potgrowth/souverainete/2_parties_position.png",
       width = 7, height = 1.4)
