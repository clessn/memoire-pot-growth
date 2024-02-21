# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/after_imputation1.rds") %>% 
  ## fix region variable as factor
  mutate(granular = factor(granular))

FaData <- Data %>% 
  select(starts_with("iss"), -iss_enviro_envMeat, -iss_enviro_envTransp)

# Attitude scales ---------------------------------------------------------

## Exploratory afe ---------------------------------------------------------

fa <- factanal(FaData, 6, rotation = "promax")
fa$loadings

saveRDS(fa, "_SharedFolder_memoire-pot-growth/data/warehouse/scales/factanal.rds")

## 6 scales
### 1 Quebec sovereignty/Protection of french
### 2 Libertarian/COVID
### 3 New left
### 4 Laicite
### 5 Immigration
### 6 3e lien

## Add scales to Data ------------------------------------------------------

Data$scale_souv_langfr <- potgrowth::compute_scale_scores(factanal_object = fa,
                                                          survey_data = Data,
                                                          scale_order = 1)
hist(Data$scale_souv_langfr)

Data$scale_libertarian <- potgrowth::compute_scale_scores(factanal_object = fa,
                                                   survey_data = Data,
                                                   scale_order = 2)
hist(Data$scale_libertarian)

Data$scale_woke <- potgrowth::compute_scale_scores(factanal_object = fa,
                                                   survey_data = Data,
                                                   scale_order = 3)
hist(Data$scale_woke)

Data$scale_laicite <- potgrowth::compute_scale_scores(factanal_object = fa,
                                                      survey_data = Data,
                                                      scale_order = 4)
hist(Data$scale_laicite)

Data$scale_immigration <- potgrowth::compute_scale_scores(factanal_object = fa,
                                                      survey_data = Data,
                                                      scale_order = 5)
hist(Data$scale_immigration)

Data$scale_lien3 <- potgrowth::compute_scale_scores(factanal_object = fa,
                                                          survey_data = Data,
                                                          scale_order = 6)
hist(Data$scale_lien3)

# Political trust ---------------------------------------------------------

Data$political_trust <- (Data$poltrust_govDoJust + Data$poltrust_politicensPreocup +
                           Data$poltrust_politiciensBienveillants + Data$poltrust_trustPartisPol) / 4
hist(Data$political_trust)

# Save it -----------------------------------------------------------------

saveRDS(Data, "_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/with_scales.rds")
