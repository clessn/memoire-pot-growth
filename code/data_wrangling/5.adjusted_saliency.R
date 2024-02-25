# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/withgaps.rds")

# Wrangling ---------------------------------------------------------------

output <- potgrowth::compute_respondents_saliency(data = Data)

Final <- cbind(Data, output)

saveRDS(Final, "_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/with_adj_saliency.rds")
