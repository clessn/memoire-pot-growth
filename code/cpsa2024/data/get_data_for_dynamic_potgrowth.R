# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

labels_df <- readODS::read_ods("_SharedFolder_memoire-pot-growth/data/warehouse/issue_labels")

issues <- labels_df$variable
to_reverse <- labels_df$reversed
names(to_reverse) <- issues
question_labels <- labels_df$question_label
names(question_labels) <- issues
labels0 <- labels_df$label0
names(labels0) <- issues
labels1 <- labels_df$label1
names(labels1) <- issues

ridings_regions <- read.csv("_SharedFolder_memoire-pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
  filter(level == "prov2022") %>% 
  mutate(riding_id = factor(as.character(riding_id), ordered = FALSE)) %>% 
  select(riding_id, granular)

## survey data
survey_data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/survey_data_holes.rds") %>% 
  select(id, source_id, riding_id, age_cat, income, male, educ, lang, religion,
         all_of(issues), starts_with("irc_")) %>% 
  tidyr::pivot_longer(.,
                      cols = starts_with("irc"),
                      names_to = "party",
                      names_prefix = "irc_",
                      values_to = "irc") %>% 
  mutate(voter = ifelse(irc >= 0, 1, 0)) %>% 
  tidyr::pivot_longer(.,
                      cols = starts_with("iss_"),
                      names_to = "issue",
                      values_to = "position") %>% 
  tidyr::drop_na() %>% 
  mutate(position = ifelse(position == 0.66, 0.67, position),
         position = ifelse(to_reverse[issue] == 1, 1 - position, position)) %>% 
  left_join(ridings_regions, by = "riding_id") %>% 
  relocate(granular, .after = riding_id) %>% 
  rename(region = granular) %>% 
  mutate(region = factor(region, ordered = FALSE)) %>% 
  #### larger regions
  mutate(region = case_when(
    as.character(region) %in% c("mtl_east", "mtl_west", "laval") ~ "montreal_laval",
    as.character(region) %in% c("chaudiere_appalaches", "capitale_nationale") ~ "capitale_nationale",
    as.character(region) %in% c("monteregie", "lanaudiere", "laurentides", "outaouais") ~ "rmr_ouest",
    as.character(region) %in% c("estrie", "centre_du_quebec") ~ "sud",
    as.character(region) %in% c("bas_saint_laurent", "gaspesie_iles_de_la_madeleine") ~ "est",
    as.character(region) %in% c("nord-du-quebec", "saguenay_lac_saint_jean", "cote_nord", "abitibi_temiscamingue", "mauricie") ~ "nord"
  ))

table(survey_data$region)

## Party positions ---------------------------------------------------------

df_party_positions <- potgrowth::aggregate_party_positions("_SharedFolder_memoire-pot-growth/data/warehouse/party_positions/expert_surveys/",
                                                           colnames = potgrowth::qc_parties) %>% 
  mutate(position = ifelse(to_reverse[VARIABLE] == 1, 1 - position, position)) %>% 
  tidyr::drop_na() %>% 
  rename(party_position = position)

## round party positions to closest likert scale
for (i in 1:length(unique(df_party_positions$VARIABLE))){
  issuei <- unique(df_party_positions$VARIABLE)[i]
  breaks <- sort(unique(survey_data$position[survey_data$issue == issuei]))
  actual_party_positions <- df_party_positions$party_position[df_party_positions$VARIABLE == issuei]
  new_party_positions <-  breaks[sapply(actual_party_positions, FUN = function(x){
    which(abs(breaks - x) == min(abs(breaks - x)))[1]
  })]
  df_party_positions$party_position[df_party_positions$VARIABLE == issuei] <- new_party_positions
}

## Join

data <- left_join(survey_data, df_party_positions, by = c("party", "issue" = "VARIABLE"))

saveRDS(data, "_SharedFolder_memoire-pot-growth/data/marts/cpsa2024/survey_data.rds")
