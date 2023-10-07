# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
d <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/ces.rds")


# Only select relevant variables ------------------------------------------

## ses

### DONE issqcsovbin (separate qc-roc)
### DONE issqcdone (separate qc-roc)
### DONE issWomDoneALL
### DONE issGapRichPoorALL (2000 present)

table(d$year, d$issQcSovBinALL)
table(d$year, d$issQcDoneALL)
table(d$year, d$issWomDoneALL)
table(d$year, d$issGapRichPoorALL)

# 1. % of neutral ---------------------------------------------------------
agg <- d %>% 
  select(year, quebec, issQcDoneALL,
         issWomDoneALL, issGapRichPoorALL) %>% 
  pivot_longer(cols = starts_with("iss")) %>% 
  drop_na() %>% 
  group_by(year, quebec, name, value) %>% 
  summarise(n = n()) %>% 
  group_by(year, quebec, name) %>% 
  mutate(total = sum(n),
         prop = n/total*100)

agg %>% 
  filter(value == 0.5) %>%
  mutate(name = case_when(
    name == "issGapRichPoorALL" ~ "Faire +\nRéduire gap riches-pauvres",
    name == "issWomDoneALL" ~ "Faire +\npour les femmes",
    name == "issQcDoneALL" ~ "Faire +\npour le Québec"
  ),
  quebec = ifelse(quebec == 1, "Québec", "ROC")) %>% 
  ggplot(aes(x = year, y = prop)) +
  geom_point(aes(color = quebec)) +
  geom_line(aes(group = interaction(name, quebec),
                color = quebec)) +
  facet_wrap(~name) +
  ylab("Proportion de\nréponses neutres") +
  clessnverse::theme_clean_light() +
  scale_color_manual(values = c("Québec" = "lightblue",
                                "ROC" = "darkred")) +
  scale_x_continuous(breaks = c(seq(1990, 2025, by = 5)))

ggsave("_SharedFolder_memoire-pot-growth/graphs/explo/prop_neutres.png",
       width = 8, height = 4)


# 2. % of dont know refused NA --------------------------------------------------------

agg2 <- d %>% 
  select(year, quebec, issQcDoneALL,
         issWomDoneALL, issGapRichPoorALL, issQcSovBinALL) %>% 
  pivot_longer(cols = starts_with("iss")) %>% 
  mutate(value = ifelse(is.na(value), "NA", value)) %>% 
  group_by(year, quebec, name, value) %>% 
  summarise(n = n()) %>% 
  group_by(year, quebec, name) %>% 
  mutate(total = sum(n),
         prop = n/total*100) %>% 
  drop_na(quebec) %>% 
  filter(prop != 100)

agg2 %>% 
  filter(value == "NA") %>%
  mutate(name = case_when(
    name == "issGapRichPoorALL" ~ "Faire +\nRéduire gap riches-pauvres",
    name == "issWomDoneALL" ~ "Faire +\npour les femmes",
    name == "issQcDoneALL" ~ "Faire +\npour le Québec",
    name == "issQcSovBinALL" ~ "Souveraineté du Québec"
  ),
  quebec = ifelse(quebec == 1, "Québec", "ROC")) %>% 
  ggplot(aes(x = year, y = prop)) +
  geom_point(aes(color = quebec)) +
  geom_line(aes(group = interaction(name, quebec),
                color = quebec)) +
  facet_wrap(~name) +
  ylab("Proportion de\nnon-réponses") +
  clessnverse::theme_clean_light() +
  scale_color_manual(values = c("Québec" = "lightblue",
                                "ROC" = "darkred")) +
  scale_x_continuous(breaks = c(seq(1965, 2025, by = 5)))

ggsave("_SharedFolder_memoire-pot-growth/graphs/explo/prop_na.png",
       width = 8, height = 4)

# 3. % at extremes --------------------------------------------------------
agg3 <- d %>% 
  select(year, quebec, issQcDoneALL,
         issWomDoneALL, issGapRichPoorALL, issQcSovBinALL) %>% 
  pivot_longer(cols = starts_with("iss")) %>% 
  drop_na() %>% 
  mutate(extreme = ifelse(value %in% c(1, 0), 1, 0)) %>% 
  group_by(year, quebec, name, extreme) %>% 
  summarise(n = n()) %>% 
  group_by(year, quebec, name) %>% 
  mutate(total = sum(n),
         prop = n/total*100)

agg3 %>% 
  filter(extreme == 1) %>% 
  mutate(name = case_when(
    name == "issGapRichPoorALL" ~ "Faire +\nRéduire gap riches-pauvres",
    name == "issWomDoneALL" ~ "Faire +\npour les femmes",
    name == "issQcDoneALL" ~ "Faire +\npour le Québec",
    name == "issQcSovBinALL" ~ "Souveraineté du Québec"
  ),
  quebec = ifelse(quebec == 1, "Québec", "ROC")) %>% 
  ggplot(aes(x = year, y = prop)) +
  geom_point(aes(color = quebec)) +
  geom_line(aes(group = interaction(name, quebec),
                color = quebec)) +
  facet_wrap(~name) +
  ylab("Proportion de réponses extrêmes\n(fortement en faveur/défaveur)") +
  clessnverse::theme_clean_light() +
  scale_color_manual(values = c("Québec" = "lightblue",
                                "ROC" = "darkred")) +
  scale_x_continuous(breaks = c(seq(1965, 2025, by = 5)))

ggsave("_SharedFolder_memoire-pot-growth/graphs/explo/prop_extreme.png",
       width = 8, height = 4)


# 4. with position --------------------------------------------------------

#agg <- d %>% 
#  select(year, quebec, issQcDoneALL,
#         issWomDoneALL, issGapRichPoorALL) %>% 
#  pivot_longer(cols = starts_with("iss")) %>% 
#  drop_na() %>% 
#  group_by(year, quebec, name, value) %>% 
#  summarise(n = n()) %>% 
#  mutate(position = case_when(
#    value %in% c(0, 0.25) ~ 0,
#    value == 0.5 ~ 0.5,
#    value %in% c(0.75, 1) ~ 1
#  )) %>% 
#  group_by(year, quebec, name, position) %>% 
#  mutate(prop_position = n/sum(n),
#         value2 = prop_position*value,
#         value3 = sum(value2)) %>% 
#  group_by(year, quebec, name) %>% 
#  mutate(total = sum(n),
#         prop = n/total*100)

agg %>% 
  filter(value %in% c(0.75, 1)) %>% 
  mutate(name = case_when(
    name == "issGapRichPoorALL" ~ "Faire +\nRéduire gap riches-pauvres",
    name == "issWomDoneALL" ~ "Faire +\npour les femmes",
    name == "issQcDoneALL" ~ "Faire +\npour le Québec"
  ),
  value = ifelse(value == 1, "Fortement en faveur",
                 "Modérément en faveur"),
  value = factor(value, levels = c("Modérément en faveur",
                                   "Fortement en faveur")),
  quebec = ifelse(quebec == 1, "Québec", "ROC")) %>% 
  ggplot(aes(x = year, y = prop)) +
  geom_point(aes(color = quebec,
                 alpha = value)) +
  geom_line(aes(group = interaction(name, quebec, value),
                color = quebec,
                alpha = value)) +
  facet_wrap(~name) +
  ylab("Proportion de\nréponses") +
  clessnverse::theme_clean_light() +
  scale_color_manual(values = c("Québec" = "lightblue",
                                "ROC" = "darkred")) +
  scale_alpha_discrete(range = c(0.35, 1)) +
  scale_x_continuous(breaks = c(seq(1990, 2025, by = 5)))

ggsave("_SharedFolder_memoire-pot-growth/graphs/explo/prop_faveur.png",
       width = 8, height = 4)



# Factoral analysis of 3 clivage indicators -------------------------------

### agg = neutral answers
aggb <- agg %>% filter(value == 0.5) %>% 
  mutate(value = "neutral")
### agg2 = no answers
agg2b <- agg2 %>% filter(value == "NA") %>% 
  mutate(value = "noanswer")
### agg3 = % extreme
agg3b <- agg3 %>% filter(extreme == 1) %>% 
  rename(value = extreme) %>% 
  mutate(value = "extreme")

all <- rbind(aggb, agg2b, agg3b) %>% 
  select(year, quebec, name, value, prop) %>% 
  pivot_wider(id_cols = c("year", "quebec", "name"),
              names_from = "value",
              values_from = prop) %>% 
  ungroup() %>% 
  drop_na() %>% 
  mutate(neutral = 100-neutral,
         noanswer = 100-noanswer)

f <- factanal(all %>% select(neutral, noanswer, extreme),
              factors = 1)
f$loadings

fq <- factanal(all %>% filter(quebec == 1) %>%
                 select(neutral, noanswer, extreme),
              factors = 1)
fq$loadings

fc <- factanal(all %>% filter(quebec == 0) %>%
                 select(neutral, noanswer, extreme),
              factors = 1)
fc$loadings


# Macro indicator ---------------------------------------------------------

graph <- all %>% 
  mutate(neutral = clessnverse::normalize_min_max(neutral),
         extreme = clessnverse::normalize_min_max(extreme),
         macro = (neutral + extreme) / 2,
         name = case_when(
           name == "issGapRichPoorALL" ~ "Faire +\nRéduire gap riches-pauvres",
           name == "issWomDoneALL" ~ "Faire +\npour les femmes",
           name == "issQcDoneALL" ~ "Faire +\npour le Québec"
         ),
         quebec = ifelse(quebec == 1, "Québec", "ROC")) %>% 
  select(-noanswer) %>% 
  pivot_longer(cols = c("neutral", "extreme", "macro"),
               names_to = "indicator",
               values_to = "value")

ggplot(graph, aes(x = year, y = value)) +
  geom_point(aes(color = quebec,
               alpha = indicator)) +
  geom_line(aes(group = interaction(name, quebec, indicator),
                color = quebec,
                alpha = indicator,
                linetype = indicator)) +
  facet_grid(cols = vars(name),
             rows = vars(quebec)) +
  ylab("Indicateur de clivage") +
  clessnverse::theme_clean_light() +
  scale_color_manual(values = c("Québec" = "lightblue",
                                "ROC" = "darkred")) +
  scale_alpha_manual(values = c("macro" = 1,
                                "neutral" = 0.4,
                                "extreme" = 0.4)) +
  scale_linetype_manual(values = c("macro" = "solid",
                                   "neutral" = "dashed",
                                   "extreme" = "dotted")) +
  scale_x_continuous(breaks = c(seq(1990, 2025, by = 5)))

ggsave("_SharedFolder_memoire-pot-growth/graphs/explo/macroindicator.png",
       width = 8, height = 4)
