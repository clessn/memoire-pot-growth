# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/survey_data_holes.rds") %>% 
  tidyr::drop_na(riding_id)
Id_data <- Data[, c("id", "source_id", "riding_id")]
Data <- Data %>% select(-c("id", "source_id", "riding_id"))

# Use missForest to imput missing data ------------------------------------------

## number of iterations to do. So if iterations = 10, the dataset will be multiplied by 10.
iterations <- 5
for (i in 1:iterations){
  message(paste0("-------- ITERATION #", i , " STARTING ------------"))
  imp_di <- missForest::missForest(Data)
  di <- imp_di$ximp
  if (i == 1){
    Output <- di
  } else {
    Output <- rbind(Output, di)
  }
  message(paste0("-------- ITERATION #", i , " IS DONE ------------"))
}

#### join riding name and region

source("wrangling/0.config.R")