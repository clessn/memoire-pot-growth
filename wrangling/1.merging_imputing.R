# Packages ----------------------------------------------------------------
library(tidyverse)
library(mice)
source("functions/refining.R", encoding = "UTF-8")

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/surveys.rds") %>% 
  ## NO vote intent question in june, dropping it
  filter(source_id != "june")

find_region <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/census_data.rds") %>% 
  pull(., region)
names(find_region) <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/census_data.rds") %>% 
  pull(., riding_id) 

parties <- c("CAQ", "PLQ", "PQ", "QS", "PCQ")

CleanData <- data.frame(
  id = Data$id,
  source_id = Data$source_id,
  riding_name = Data$riding,
  riding_id = Data$riding_id,
  region = find_region[as.character(Data$riding_id)]
)
table(CleanData$region)


# # ! Respondents to keep pilote 1 ----------------------------------------

## in this survey, some respondents were removed in the cleaning.
clean_survey <- sondr::read_any_csv("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot1_2022/Pilote1_clean.csv")
respondents_to_keep_pilote1 <- clean_survey$id ## vector containing the rows to keep

# remove unwanted df from environment
rm(clean_survey)

## ! Respondents to remove from pilote 2 -------------------------------------
all_respondents_df <- sondr::read_any_csv("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv")
## this df contains all 1970 respondents of this survey, but with the RCI cleaned.

#### for the RCI
####### if the respondent has answered NA to 4 or less parties,
######### this means that the NAs should become 0.5, the default position of the slider in Qualtrics
######### (programming error in Qualtrics)
raw1 <- readRDS("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/Pilote2.rds")
raw1$nas <- rowSums(is.na(raw1 %>% select(starts_with("potGrowth"))))
table(raw1$nas)
# 0 means the respondent answered for the 5 parties (nothing to do)
# 1,2,3 or 4 means the respondent answered the question but skipped some parties
##### (which means the respondent didnt change the party from the default 5 position in Qualtrics)
# 5 means the respondent didnt answer the question

### Remove respondents who didnt answer potGrowth question
respondents_to_remove_pilote2 <- which(raw1$nas==5)

## remove unwanted dataframes
rm(all_respondents_df, raw1)

# Checking columns are present in which source_id ----------------------------

n_respondents <- Data %>% 
  group_by(source_id) %>% 
  summarise(n = n())
ns <- n_respondents$n
names(ns) <- n_respondents$source_id

Check <- Data %>% 
  group_by(source_id) %>% 
  mutate(n_respondents = n()) %>% 
  summarise_all(list(~sum(is.na(.)))) %>% 
  pivot_longer(., cols = -source_id) %>%
  mutate(prop = value/ns[source_id],
         there = ifelse(prop>=0.9, 0, 1)) %>%
  select(-value, -prop) %>% 
  pivot_wider(names_from = source_id,
              values_from = there)

# SES (age, educ, income, gender, lang, religion/religiosity) -----------------------------------

# Age ---------------------------------------------------------------------
CleanData$age <- Data$ses_age_final
CleanData$age[CleanData$age==15] <- 18
CleanData$age_cat <- Data$ageC

# Educ --------------------------------------------------------------------
CleanData$educ <- Data$educ

# Income ------------------------------------------------------------------
CleanData$income <- Data$income

# Male --------------------------------------------------------------------
CleanData$male <- Data$male

# Lang --------------------------------------------------------------------
CleanData$lang <- Data$lang

# Religion ----------------------------------------------------------------

CleanData$religion <- NA

## omnibus january ---------------------------------------------------------
rel_raw <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/omnibus/january/january.Sav",
                            variable = "C1")
table(rel_raw)
rel_clean <- NA
rel_clean <- case_when(
  rel_raw %in% c(1, 2) ~ "no_religion",
  rel_raw == 8 ~ "catholic",
  !(rel_raw %in% c(1, 2, 8)) ~ "other",
)
table(rel_clean)

CleanData$religion[CleanData$source_id == "january"] <- rel_clean

## omnibus february ---------------------------------------------------------
rel_raw <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february.Sav",
                                variable = "C1")
table(rel_raw)
rel_clean <- NA
rel_clean <- case_when(
  rel_raw %in% c(1, 2) ~ "no_religion",
  rel_raw == 8 ~ "catholic",
  !(rel_raw %in% c(1, 2, 8)) ~ "other",
)
table(rel_clean)

CleanData$religion[CleanData$source_id == "february"] <- rel_clean

## omnibus march ---------------------------------------------------------
rel_raw <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/omnibus/march/march.Sav",
                                variable = "C1")
table(rel_raw)
rel_clean <- NA
rel_clean <- case_when(
  rel_raw %in% c(1, 2) ~ "no_religion",
  rel_raw == 8 ~ "catholic",
  !(rel_raw %in% c(1, 2, 8)) ~ "other",
)
table(rel_clean)

CleanData$religion[CleanData$source_id == "march"] <- rel_clean

## omnibus april ---------------------------------------------------------
rel_raw <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/omnibus/april/april.Sav",
                                variable = "C1")
table(rel_raw)
rel_clean <- NA
rel_clean <- case_when(
  rel_raw %in% c(1, 2) ~ "no_religion",
  rel_raw == 8 ~ "catholic",
  !(rel_raw %in% c(1, 2, 8)) ~ "other",
)
table(rel_clean)

CleanData$religion[CleanData$source_id == "april"] <- rel_clean

## omnibus may ---------------------------------------------------------
rel_raw <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/omnibus/may/may.Sav",
                                variable = "C1")
table(rel_raw)
rel_clean <- NA
rel_clean <- case_when(
  rel_raw %in% c(1, 2) ~ "no_religion",
  rel_raw == 8 ~ "catholic",
  !(rel_raw %in% c(1, 2, 8)) ~ "other",
)
table(rel_clean)

CleanData$religion[CleanData$source_id == "may"] <- rel_clean

## pilote 1 ---------------------------------------------------------
rel_raw <- as.vector(haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot1_2022/ULA12-BASE-1500.sav")$Q5)
rel_raw <- rel_raw[respondents_to_keep_pilote1]
table(rel_raw)
rel_clean <- NA
rel_clean <- case_when(
  rel_raw %in% c(1, 2) ~ "no_religion",
  rel_raw == 8 ~ "catholic",
  !(rel_raw %in% c(1, 2, 8)) ~ "other",
)
table(rel_clean)

CleanData$religion[CleanData$source_id == "pilote1"] <- rel_clean


## pilote 2 ---------------------------------------------------------
rel_raw_fr <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                variable = "religion")[-c(1, 2)]
rel_raw_fr[rel_raw_fr == ""] <- NA

rel_raw_en <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                   variable = "religion.1")[-c(1, 2)]
rel_raw_en[rel_raw_en == ""] <- NA

rel_raw <- coalesce(rel_raw_fr, rel_raw_en)

rel_raw <- as.numeric(rel_raw[-respondents_to_remove_pilote2])

table(rel_raw)
rel_clean <- NA
rel_clean <- case_when(
  rel_raw %in% c(1, 2) ~ "no_religion",
  rel_raw == 8 ~ "catholic",
  !(rel_raw %in% c(1, 2, 8)) ~ "other",
)
table(rel_clean)

CleanData$religion[CleanData$source_id == "pilote2"] <- rel_clean

# Religiosity -------------------------------------------------------------

CleanData$religiosity <- NA

## omnibus january ---------------------------------------------------------
rel_raw <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/omnibus/january/january.Sav",
                                variable = "C4_A1")
table(rel_raw)
rel_clean <- rel_raw / 10
table(rel_clean)
CleanData$religiosity[CleanData$source_id == "january"] <- rel_clean

## pilote 1 ---------------------------------------------------------
rel_raw <- as.vector(haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot1_2022/ULA12-BASE-1500.sav")$Q90_A1)
rel_raw <- rel_raw[respondents_to_keep_pilote1]
table(rel_raw)
rel_clean <- NA
rel_clean <- rel_raw / 10
table(rel_clean)
CleanData$religiosity[CleanData$source_id == "pilote1"] <- rel_clean

## pilote 2 ---------------------------------------------------------
rel_raw_fr <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                   variable = "FR_importantOfRelig_1")[-c(1, 2)]
rel_raw_fr[rel_raw_fr == ""] <- NA

rel_raw_en <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                   variable = "EN_importantRelig_1")[-c(1, 2)]
rel_raw_en[rel_raw_en == ""] <- NA

rel_raw <- coalesce(rel_raw_fr, rel_raw_en)

rel_raw <- as.numeric(rel_raw[-respondents_to_remove_pilote2])
table(rel_raw)
rel_raw <- round(rel_raw/10, 0)
table(rel_raw)
rel_clean <- rel_raw / 10
table(rel_clean)

CleanData$religiosity[CleanData$source_id == "pilote2"] <- rel_clean


hist(CleanData$age)
table(CleanData$age_cat)
table(CleanData$educ)
table(CleanData$income)
table(CleanData$male)
table(CleanData$lang)
table(CleanData$religion, useNA = "always")
table(CleanData$religiosity, useNA = "always")
table(CleanData$religiosity, CleanData$religion, useNA = "always")

# Vote intent -------------------------------------------------------------
CleanData$voteInt_CAQ_drop <- coalesce(Data$voteIntCAQ, Data$op_intent_CAQ)
CleanData$voteInt_PLQ_drop <- coalesce(Data$voteIntPLQ, Data$op_intent_PLQ)
CleanData$voteInt_PQ_drop  <- coalesce(Data$voteIntPQ, Data$op_intent_PQ)
CleanData$voteInt_QS_drop  <- coalesce(Data$voteIntQS, Data$op_intent_QS)
CleanData$voteInt_PCQ_drop <- coalesce(Data$voteIntPCQ, Data$op_intent_PCQ)

table(CleanData$voteInt_CAQ_drop)
table(CleanData$voteInt_PLQ_drop)
table(CleanData$voteInt_PQ_drop)
table(CleanData$voteInt_QS_drop)
table(CleanData$voteInt_PCQ_drop)

CleanData <- CleanData %>%
  mutate(voteInt = as.factor(ifelse(voteInt_CAQ_drop == 1, "CAQ",
                                    ifelse(voteInt_PLQ_drop == 1, "PLQ",
                                           ifelse(voteInt_PQ_drop == 1, "PQ",
                                                  ifelse(voteInt_QS_drop == 1, "QS",
                                                         ifelse(voteInt_PCQ_drop == 1, "PCQ", NA))))))) %>% 
  select(-ends_with("_drop"))

# RCI ---------------------------------------------------------------------
CleanData$irc_CAQ <- Data$ircCAQ
CleanData$irc_PLQ <- Data$ircPLQ
CleanData$irc_PQ <- Data$ircPQ
CleanData$irc_QS <- Data$ircQS
CleanData$irc_PCQ <- Data$ircPCQ

CleanData$ircCAQ_model <- transform_rci(CleanData$irc_CAQ)
CleanData$ircPLQ_model <- transform_rci(CleanData$irc_PLQ)
CleanData$ircPQ_model <- transform_rci(CleanData$irc_PQ)
CleanData$ircQS_model <- transform_rci(CleanData$irc_QS)
CleanData$ircPCQ_model <- transform_rci(CleanData$irc_PCQ)

CleanData$probVote_CAQ <- Data$potGrowthCAQ
CleanData$probVote_PLQ <- Data$potGrowthPLQ
CleanData$probVote_QS <- Data$potGrowthQS
CleanData$probVote_PQ <- Data$potGrowthPQ
CleanData$probVote_PCQ <- Data$potGrowthPCQ

# Party identification ----------------------------------------------------

CleanData$partyId_CAQ_drop <- Data$party_id_caquiste
table(CleanData$partyId_CAQ_drop)

CleanData$partyId_PLQ_drop <- Data$party_id_lib
table(CleanData$partyId_PLQ_drop)

CleanData$partyId_PQ_drop <- Data$party_id_pequiste
table(CleanData$partyId_PQ_drop)

CleanData$partyId_QS_drop <- Data$party_id_solidaire
table(CleanData$partyId_QS_drop)

CleanData$partyId_PCQ_drop <- Data$party_id_cons
table(CleanData$partyId_PCQ_drop)

CleanData <- CleanData %>%
  mutate(partyId = as.factor(ifelse(partyId_CAQ_drop == 1, "CAQ",
                                    ifelse(partyId_PLQ_drop == 1, "PLQ",
                                           ifelse(partyId_PQ_drop == 1, "PQ",
                                                  ifelse(partyId_QS_drop == 1, "QS",
                                                         ifelse(partyId_PCQ_drop == 1, "PCQ", NA)))))),
         partyId = factor(partyId, levels = c(levels(partyId), "none"))) %>% 
  select(-ends_with("_drop"))

table(CleanData$source_id, CleanData$partyId, useNA = "always")

CleanData$partyId[CleanData$source_id %in% c("pilote1", "pilote2") &
                    is.na(CleanData$partyId)] <- "none"

table(CleanData$source_id, CleanData$partyId, useNA = "always")

# Attitude questions ---------------------------------------------------------------

## Left-right scale --------------------------------------------------------------
#### rightist
#### rightLeft

CleanData$scale_leftRight <- coalesce(Data$rightist, Data$rightLeft)
hist(CleanData$scale_leftRight)

## Nationalism/souverainete -------------------------------------------------------------
#### isQcBeforeCan feel_more_queb_than_can
#### isSouverainiste quebec_should_be_independent
#### iss_quebRightDirect

table(Data$isQcBeforeCan, Data$source_id)
## march, april and may need to be removed, no positive cases.
Data$isQcBeforeCan[Data$source_id %in% c("march", "april", "may")] <- NA
CleanData$iss_nationalisme_qcBefCan <- coalesce(Data$isQcBeforeCan, Data$feel_more_queb_than_can)
table(CleanData$iss_nationalisme_qcBefCan, CleanData$source_id)
CleanData$iss_nationalisme_qcBefCan[CleanData$iss_nationalisme_qcBefCan>0.5] <- 1
CleanData$iss_nationalisme_qcBefCan[CleanData$iss_nationalisme_qcBefCan<=0.5] <- 0
table(CleanData$iss_nationalisme_qcBefCan)

Data$isSouverainiste[Data$isSouverainiste == 0.33] <- 0.25
Data$isSouverainiste[Data$isSouverainiste == 0.66] <- 0.75
CleanData$iss_nationalisme_souv <- coalesce(Data$isSouverainiste, Data$quebec_should_be_independent)
hist(CleanData$iss_nationalisme_souv)
table(CleanData$iss_nationalisme_souv, CleanData$source_id)

CleanData$iss_nationalisme_qcRightDirection <- Data$iss_quebRightDirect
table(CleanData$iss_nationalisme_qcRightDirection)

fa <- CleanData %>%
  select(iss_nationalisme_qcBefCan, iss_nationalisme_souv, iss_nationalisme_qcRightDirection) %>% 
  drop_na()

topdown_fa(fa)

## Langue francaise --------------------------------------------------------
#### iss_frenchInDanger
#### issWorriedFrenchMtl
#### issWorriedFrenchProv
#### english_cegep_must_pass_fr_lang_test
#### business_in_french_only
#### afraid_of_french_disappearance

CleanData$iss_lang_frenchDanger <- Data$iss_frenchInDanger
table(CleanData$iss_lang_frenchDanger)

CleanData$iss_lang_worriedFrMtl <- Data$iss_worriedFrInMtl
table(CleanData$iss_lang_worriedFrMtl)
hist(CleanData$iss_lang_worriedFrMtl)

CleanData$iss_lang_worriedFrProv <- Data$iss_worriedFrInProvQc
table(CleanData$iss_lang_worriedFrProv)

CleanData$iss_lang_englishCegep <- Data$english_cegep_must_pass_fr_lang_test
table(CleanData$iss_lang_englishCegep)

CleanData$iss_lang_businessFrench <- Data$business_in_french_only
table(CleanData$iss_lang_businessFrench)

CleanData$iss_lang_afraidDisappear <- Data$afraid_of_french_disappearance
table(CleanData$iss_lang_afraidDisappear)


## Laicite -----------------------------------------------------------------
#### secularism_must_be_encouraged
#### religious_symbol_at_work_not_allowed
#### teachers_not_allowed_wear_rel_symbol
#### ses_isCatho
#### ses_religiosity
#### religion_very_important

hist(Data$secularism_must_be_encouraged)
CleanData$iss_laic_secularismEncouraged <- Data$secularism_must_be_encouraged
hist(CleanData$iss_laic_secularismEncouraged)

hist(Data$religious_symbol_at_work_not_allowed)
CleanData$iss_laic_relSignsWorkNo <- Data$religious_symbol_at_work_not_allowed
hist(CleanData$iss_laic_relSignsWorkNo)

hist(Data$teachers_not_allowed_wear_rel_symbol)
CleanData$iss_laic_relSignsTeachersNo <- Data$teachers_not_allowed_wear_rel_symbol
hist(CleanData$iss_laic_relSignsTeachersNo)

hist(Data$religion_very_important)
CleanData$iss_laic_religionImportant <- Data$religion_very_important
hist(CleanData$iss_laic_religionImportant)


## Immigration -------------------------------------------------------------
#### iss_immigBenefitQc
#### iss_immigShouldAdapt
#### immigrants_must_learn_french_at_arrival
#### less_immigrant_in_qc
#### immigrants_threat_to_province_culture

hist(Data$iss_immigBenefitQc)
CleanData$iss_immig_immBenefit <- Data$iss_immigBenefitQc
hist(CleanData$iss_immig_immBenefit)
CleanData$iss_immig_immBenefit <- finverser(CleanData$iss_immig_immBenefit)
hist(CleanData$iss_immig_immBenefit)

hist(Data$iss_immigShouldAdapt)
CleanData$iss_immig_immAdapt <- Data$iss_immigShouldAdapt
hist(CleanData$iss_immig_immAdapt)

hist(Data$immigrants_must_learn_french_at_arrival)
CleanData$iss_immig_immLearnFr <- Data$immigrants_must_learn_french_at_arrival 
hist(CleanData$iss_immig_immLearnFr)

hist(Data$less_immigrant_in_qc)
CleanData$iss_immig_immLess <- Data$less_immigrant_in_qc
hist(CleanData$iss_immig_immLess)

hist(Data$immigrants_threat_to_province_culture)
CleanData$iss_immig_immThreat <- Data$immigrants_threat_to_province_culture
hist(CleanData$iss_immig_immThreat)


## Wokisme -----------------------------------------------------------------
#### starts_with "woke"
#### iss_sysRacismQc
#### iss_qcWhiteMenFav
#### artists_accused_of_sex_offence_can_work

hist(Data$woke_whiteAreRacists)
CleanData$iss_newleft_wokeWhiteRac <- Data$woke_whiteAreRacists
hist(CleanData$iss_newleft_wokeWhiteRac)

hist(Data$woke_objRealityExists)
CleanData$iss_newleft_wokeObjReal <- Data$woke_objRealityExists
hist(CleanData$iss_newleft_wokeObjReal)
CleanData$iss_newleft_wokeObjReal <- finverser(CleanData$iss_newleft_wokeObjReal)
hist(CleanData$iss_newleft_wokeObjReal)

hist(Data$woke_noWhitesRaceQuestions)
CleanData$iss_newleft_wokenoWhites <- Data$woke_noWhitesRaceQuestions
hist(CleanData$iss_newleft_wokenoWhites)

hist(Data$woke_censorRacistWorks)
CleanData$iss_newleft_wokeCensor <- Data$woke_censorRacistWorks
hist(CleanData$iss_newleft_wokeCensor)

hist(Data$woke_socCtrlByWhiteMen)
CleanData$iss_newleft_wokeSocCtrl <- Data$woke_socCtrlByWhiteMen
hist(CleanData$iss_newleft_wokeSocCtrl)

hist(Data$woke_richToSucceed)
CleanData$iss_newleft_wokeRich <- Data$woke_richToSucceed
hist(CleanData$iss_newleft_wokeRich)

hist(Data$iss_sysRacismQc)
CleanData$iss_newleft_wokeSysRaci <- Data$iss_sysRacismQc
hist(CleanData$iss_newleft_wokeSysRaci)

hist(Data$iss_qcWhiteMenFav)
CleanData$iss_newleft_wokeWhiteMenFav <- Data$iss_qcWhiteMenFav
hist(CleanData$iss_newleft_wokeWhiteMenFav)


hist(Data$artists_accused_of_sex_offence_can_work)
CleanData$iss_newleft_wokeArtists <- Data$artists_accused_of_sex_offence_can_work
hist(CleanData$iss_newleft_wokeArtists)
CleanData$iss_newleft_wokeArtists <- finverser(CleanData$iss_newleft_wokeArtists)
hist(CleanData$iss_newleft_wokeArtists)


## 3e lien -----------------------------------------------------------------

### omnibus february --------------------------------------------------------

## go get raw data
table(CleanData$source_id) ### 1200 februarys, so we need to have 1200 per vector
l1 <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february.Sav")$L1
attributes(l1)
table(l1)
cleanl1 <- NA
cleanl1[l1 == 1] <- 1
cleanl1[l1 == 2] <- 0.67
cleanl1[l1 == 3] <- 0.33
cleanl1[l1 == 4] <- 0
CleanData$iss_3elien_accord <- ifelse(CleanData$source_id == "february", cleanl1, NA)

table(CleanData$source_id) ### 1200 februarys, so we need to have 1200 per vector
l2 <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february.Sav")$L2
attributes(l2)
table(l2)
cleanl2 <- NA
cleanl2[l2 == 1] <- 1
cleanl2[l2 == 2] <- 0.67
cleanl2[l2 == 3] <- 0.33
cleanl2[l2 == 4] <- 0
CleanData$iss_3elien_accordDim <- ifelse(CleanData$source_id == "february", cleanl2, NA)


### omnibus april -----------------------------------------------------------

table(CleanData$source_id) ### 1300 april, so we need to have 1300 per vector
l1 <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/april/april.Sav")$L1
attributes(l1)
table(l1)
cleanl1 <- NA
cleanl1[l1 == 1] <- 1
cleanl1[l1 == 2] <- 0.67
cleanl1[l1 == 3] <- 0.33
cleanl1[l1 == 4] <- 0
CleanData$iss_3elien_accord <- ifelse(CleanData$source_id == "april", cleanl1, CleanData$iss_3elien_accord)

table(CleanData$source_id) ### 1300 april, so we need to have 1300 per vector
l2 <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/april/april.Sav")$L2
attributes(l2)
table(l2)
cleanl2 <- NA
cleanl2[l2 == 1] <- 1 ## fait trop de place au transport coll
cleanl2[l2 == 2] <- 0.5 ## fait juste assez de place au transport coll
cleanl2[l2 == 3] <- 0 ## fait pas assez de place au transport coll
CleanData$iss_3elien_tooMuchPublicTransport <- ifelse(CleanData$source_id == "april", cleanl2, NA)


table(CleanData$iss_3elien_accord)
table(CleanData$iss_3elien_accordDim)
table(CleanData$iss_3elien_tooMuchPublicTransport)

table(CleanData$iss_3elien_accord, CleanData$source_id)
table(CleanData$iss_3elien_accordDim, CleanData$source_id)
table(CleanData$iss_3elien_tooMuchPublicTransport, CleanData$source_id)


## Environnement --------------------------------------------------------
#### iss_gvtMoreEnv
#### my_lifestyle_is_not_harmful_to_environment
#### cons_meat
#### public transport

hist(Data$iss_gvtMoreEnv)
CleanData$iss_enviro_envGvtMore <- Data$iss_gvtMoreEnv
hist(CleanData$iss_enviro_envGvtMore)

hist(Data$my_lifestyle_is_not_harmful_to_environment)
CleanData$iss_enviro_envLifestyle <- Data$my_lifestyle_is_not_harmful_to_environment
hist(CleanData$iss_enviro_envLifestyle)
CleanData$iss_enviro_envLifestyle <- finverser(CleanData$iss_enviro_envLifestyle)

Data$cons_meat <- NA
Data$cons_meat[Data$cons_meat_never == 1] <- 7
Data$cons_meat[Data$cons_meat_almost_never == 1] <- 6
Data$cons_meat[Data$cons_meat_once_month == 1] <- 5
Data$cons_meat[Data$cons_meat_once_week == 1] <- 4
Data$cons_meat[Data$cons_meat_few_week == 1] <- 3
Data$cons_meat[Data$cons_meat_daily == 1] <- 2
Data$cons_meat[Data$cons_meat_few_daily == 1] <- 1
Data$cons_meat <- clessnverse::normalize_min_max(Data$cons_meat)
hist(Data$cons_meat)
CleanData$iss_enviro_envMeat <- Data$cons_meat
hist(CleanData$iss_enviro_envMeat)

Data$public_transport <- Data$act_transport_Bicycle+Data$act_transport_PublicTransportation+Data$act_transport_Walk
hist(Data$public_transport)
CleanData$iss_enviro_envTransp <- Data$public_transport
hist(CleanData$iss_enviro_envTransp)


# Political knowledge ------------------------------------------------

CleanData$political_knowledge <- NA

## omnibus may
##### C25)  En utilisant l'échelle ci-dessous, comment évaluez-vous votre niveau de connaissances politiques ?

raw <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/omnibus/may/may.Sav",
                            variable = "C25")
table(raw)
clean <- NA
clean[raw == 1] <- 1
clean[raw == 2] <- 0.75
clean[raw == 3] <- 0.5
clean[raw == 4] <- 0.25
clean[raw == 5] <- 0
table(clean)

CleanData$political_knowledge[CleanData$source_id == "may"] <- clean

## datagotchi pilote 2 2022
##### FR_scalePol En utilisant l'échelle ci-dessous, comment évaluez-vous votre niveau de connaissances politiques?
##### EN_knowPol Using the scale below, how would you rate your level of political knowledge?

raw_fr <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                   variable = "FR_scalePol")[-c(1, 2)]
raw_fr[raw_fr == ""] <- NA

raw_en <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv",
                                   variable = "EN_knowPol")[-c(1, 2)]
raw_en[raw_en == ""] <- NA

raw <- as.numeric(coalesce(raw_fr, raw_en))

raw <- raw[-respondents_to_remove_pilote2]

table(raw)

clean <- NA
clean[raw == 1] <- 1
clean[raw == 4] <- 0.75
clean[raw == 5] <- 0.5
clean[raw == 6] <- 0.25
clean[raw == 7] <- 0
table(clean)

CleanData$political_knowledge[CleanData$source_id == "pilote2"] <- clean

# Media and social media --------------------------------------------------

## MEDIA ATTENTION ####

## Which media ####


## MEDIA SOCIAUX ####

### checker february et march + les autres omnibus et pilote.
### conceptualisation nécessaire
###

# Political trust, cynism -------------------------------------------------



# Imput missing data with mice --------------------------------------------



# Make scales (REDO) -------------------------------------------------------------

CleanData$scale_nationaSouv <-   (CleanData$iss_nationalisme_qcRightDirection+
                                    CleanData$iss_nationalisme_souv+
                                    CleanData$iss_nationalisme_qcBefCan)/3
hist(CleanData$scale_nationaSouv)


CleanData$scale_langFr <- (CleanData$drop_frenchDanger +
                             CleanData$drop_worriedFrMtl +
                             CleanData$drop_worriedFrProv +
                             CleanData$drop_englishCegep +
                             CleanData$drop_businessFrench +
                             CleanData$drop_afraidDisappear)/6
hist(CleanData$scale_langFr)


CleanData$scale_laicite <-   (CleanData$iss_laic_relSignsTeachersNo +
                              CleanData$iss_laic_relSignsWorkNo +
                              CleanData$iss_laic_religionImportant +
                              CleanData$iss_laic_secularismEncouraged)/4
hist(CleanData$scale_laicite)

CleanData$scale_immigration <-   finverser((CleanData$iss_immig_immAdapt+
                                            CleanData$iss_immig_immBenefit+
                                            CleanData$iss_immig_immLearnFr+
                                            CleanData$iss_immig_immLess+
                                            CleanData$iss_immig_immThreat)/5)
hist(CleanData$scale_immigration)

CleanData$scale_woke <-   (CleanData$iss_newleft_wokeWhiteRac +
                           CleanData$iss_newleft_wokenoWhites+
                           CleanData$iss_newleft_wokeCensor+
                           CleanData$iss_newleft_wokeSocCtrl+
                           CleanData$iss_newleft_wokeRich+
                           CleanData$iss_newleft_wokeSysRaci+
                           CleanData$iss_newleft_wokeWhiteMenFav+
                           CleanData$iss_newleft_wokeArtists)/8
hist(CleanData$scale_woke)

CleanData$scale_3elien <- (CleanData$iss_3elien_Accord+CleanData$iss_3elien_Dim)/2
hist(CleanData$scale_3elien)

CleanData$scale_enviro <- (CleanData$drop_envGvtMore+CleanData$drop_envMeat+
                             CleanData$drop_envLifestyle+CleanData$drop_envTransp)/4