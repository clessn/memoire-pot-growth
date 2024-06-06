# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

labels_df <- readODS::read_ods("_SharedFolder_memoire-pot-growth/data/warehouse/issue_labels")

issues <- labels_df$variable
to_reverse <- labels_df$reversed
names(to_reverse) <- issues

## Party positions ---------------------------------------------------------

df_party_positions <- potgrowth::aggregate_party_positions("_SharedFolder_memoire-pot-growth/data/warehouse/party_positions/expert_surveys/",
                                                           colnames = potgrowth::qc_parties) %>% 
  mutate(position = ifelse(to_reverse[VARIABLE] == 1, 1 - position, position))

survey_data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/survey_data_holes.rds") %>% 
  select(all_of(issues)) %>% 
  tidyr::pivot_longer(.,
                      cols = starts_with("iss_"),
                      names_to = "issue",
                      values_to = "position") %>% 
  mutate(position = ifelse(position == 0.66, 0.67, position),
         position = ifelse(to_reverse[issue] == 1, 1 - position, position))

for (i in 1:length(issues)){
  issuei <- issues[i]
  df_party_positionsi <- df_party_positions %>%
    filter(VARIABLE == issuei)
  party_positions_vec <- df_party_positionsi$position
  names(party_positions_vec) <- df_party_positionsi$party
  positions <- survey_data %>%
    filter(issue == issuei) %>%
    tidyr::drop_na() %>% 
    pull(position) %>%
    unique()
  party_position <-  positions[sapply(party_positions_vec, FUN = function(x){
    which(abs(positions - x) == min(abs(positions - x)))[1]
  })]
  names(party_position) <- names(party_positions_vec)
  if (i == 1){
    party_positions_df <- data.frame(party_position) %>% 
      tibble::rownames_to_column(var = "party") %>% 
      mutate(issue = issuei)
  } else {
    party_positions_df <- rbind(party_positions_df,
                                data.frame(party_position) %>% 
                                  tibble::rownames_to_column(var = "party") %>% 
                                  mutate(issue = issuei))
  }
  message(issuei)
}

saveRDS(party_positions_df, "_SharedFolder_memoire-pot-growth/data/marts/cpsa2024/party_positions.rds")
