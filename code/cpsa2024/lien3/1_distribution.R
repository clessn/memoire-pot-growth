# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
data <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/cpsa2024/survey_data.rds") %>% 
  filter(issue == "iss_lien3_accord" &
           party == "CAQ")

table(data$position, data$source_id, useNA = "always")

# Plot --------------------------------------------------------------------

choices <- c(
  "0" = "Strongly disagree",
  "0.33" = "Somewhat disagree",
  "0.67" = "Somewhat agree",
  "1" = "Strongly agree"
)

data %>% 
  group_by(position) %>%
  summarise(n = n()) %>% 
  ggplot(aes(x = position, y = n)) +
  geom_col() +
  xlab("\nPosition on Quebec City's 3rd link") +
  ylab("Number of respondents") +
  clessnize::theme_clean_light() +
  geom_text(aes(y = n + 40, label = n)) +
  scale_x_continuous(labels = choices,
                     breaks = c(0, 0.33, 0.67, 1))

ggsave("_SharedFolder_memoire-pot-growth/graphs/cpsa2024/dynamic_potgrowth/lien3/1_distribution.png",
       width = 8, height = 6, dpi = 300)
