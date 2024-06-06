# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
data <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/cpsa2024/survey_data.rds") %>% 
  filter(issue == "iss_nationalisme_souv" &
           party == "CAQ") %>% 
  mutate(position = ifelse(position == 0.5,
                           sample(c(0.25, 0.75), size = 1, replace = TRUE),
                           position),
         position = ifelse(position == 0.25, 0.33, position),
         position = ifelse(position == 0.75, 0.67, position))

table(data$position, data$source_id, useNA = "always")

# Plot --------------------------------------------------------------------

choices <- c(
  "0" = "Strongly in favor\nof Quebec remaining\nin Canada",
  "0.33" = "Moderately in favor\nof Quebec remaining\nin Canada",
  "0.67" = "Moderately in favor\nof Quebec becoming\na sovereign country",
  "1" = "Strongly in favor\nof Quebec becoming\na sovereign country"
)

data %>% 
  group_by(position) %>%
  summarise(n = n()) %>% 
  ggplot(aes(x = position, y = n)) +
  geom_col() +
  xlab("\nPosition on Quebec's independence") +
  ylab("Number of respondents") +
  clessnize::theme_clean_light() +
  geom_text(aes(y = n + 40, label = n)) +
  scale_x_continuous(labels = choices,
                     breaks = c(0, 0.33, 0.67, 1))

ggsave("_SharedFolder_memoire-pot-growth/graphs/cpsa2024/dynamic_potgrowth/souverainete/1_distribution_souverainete.png",
       width = 8, height = 6, dpi = 300)
