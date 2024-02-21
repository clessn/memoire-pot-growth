# Packages ----------------------------------------------------------------
library(tidyverse)
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


## ! Respondents to keep pilote 1 ----------------------------------------

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

# SES (age, educ, income, gender, lang, religion, religiosity) -----------------------------------

## Age ---------------------------------------------------------------------
CleanData$age <- Data$ses_age_final
CleanData$age[CleanData$age==15] <- 18
CleanData$age_cat <- Data$ageC

## Educ --------------------------------------------------------------------
CleanData$educ <- Data$educ

## Income ------------------------------------------------------------------
CleanData$income <- Data$income

## Male --------------------------------------------------------------------
CleanData$male <- Data$male

## Lang --------------------------------------------------------------------
CleanData$lang <- Data$lang

## Religion ----------------------------------------------------------------

CleanData$religion <- NA

### omnibus january ---------------------------------------------------------
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

### omnibus february ---------------------------------------------------------
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

### omnibus march ---------------------------------------------------------
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

### omnibus april ---------------------------------------------------------
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

### omnibus may ---------------------------------------------------------
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

### pilote 1 ---------------------------------------------------------
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

### pilote 2 ---------------------------------------------------------
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

## Religiosity -------------------------------------------------------------

CleanData$religiosity <- NA

### omnibus january ---------------------------------------------------------
rel_raw <- sondr::load_variable(file = "_SharedFolder_memoire-pot-growth/data/lake/omnibus/january/january.Sav",
                                variable = "C4_A1")
table(rel_raw)
rel_clean <- rel_raw / 10
table(rel_clean)
CleanData$religiosity[CleanData$source_id == "january"] <- rel_clean

### pilote 1 ---------------------------------------------------------
rel_raw <- as.vector(haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot1_2022/ULA12-BASE-1500.sav")$Q90_A1)
rel_raw <- rel_raw[respondents_to_keep_pilote1]
table(rel_raw)
rel_clean <- NA
rel_clean <- rel_raw / 10
table(rel_clean)
CleanData$religiosity[CleanData$source_id == "pilote1"] <- rel_clean

### pilote 2 ---------------------------------------------------------
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

#hist(Data$religion_very_important)
#CleanData$iss_laic_religionImportant <- Data$religion_very_important
#hist(CleanData$iss_laic_religionImportant)


## Immigration -------------------------------------------------------------
#### iss_immigBenefitQc
#### iss_immigShouldAdapt
#### immigrants_must_learn_french_at_arrival
#### less_immigrant_in_qc
#### immigrants_threat_to_province_culture

hist(Data$iss_immigBenefitQc)
CleanData$iss_immig_immNoBenefit <- Data$iss_immigBenefitQc
hist(CleanData$iss_immig_immNoBenefit)
CleanData$iss_immig_immNoBenefit <- finverser(CleanData$iss_immig_immNoBenefit)
hist(CleanData$iss_immig_immNoBenefit)

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

## Libertarisme ------------------------------------------------------------

Jan <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/january/january.Sav")
attributes(Jan$QWOKE_A7)
attributes(Jan$QWOKE_A8)
attributes(Jan$QWOKE_A9)

clean_qwoke <- function(raw){
  clean <- NA
  clean[raw == 1] <- 1
  clean[raw == 2] <- 0.67
  clean[raw == 3] <- 0.33
  clean[raw == 4] <- 0
  return(clean)
}

clean_qwokes <- function(source_id, CleanData){
  Raw <- haven::read_sav(paste0("_SharedFolder_memoire-pot-growth/data/lake/omnibus/", source_id, "/", source_id, ".Sav"))
  CleanData$iss_liberty_qcMissLiberty[CleanData$source_id == source_id] <- clean_qwoke(Raw$QWOKE_A7)
  CleanData$iss_liberty_covidRevealAutoritharian[CleanData$source_id == source_id] <- clean_qwoke(Raw$QWOKE_A8)
  CleanData$iss_liberty_mesuresSanitairesDictature[CleanData$source_id == source_id] <- clean_qwoke(Raw$QWOKE_A9)
  return(CleanData)
}

# january QWOKE_A7, QWOKE_A8, QWOKE_A9
CleanData <- clean_qwokes("january", CleanData)

# march QWOKE_A7, QWOKE_A8, QWOKE_A9
CleanData <- clean_qwokes("march", CleanData)

# april QWOKE_A7, QWOKE_A8, QWOKE_A9
CleanData <- clean_qwokes("april", CleanData)

# may QWOKE_A7, QWOKE_A8, QWOKE_A9
CleanData <- clean_qwokes("may", CleanData)

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
CleanData$iss_lien3_accord <- ifelse(CleanData$source_id == "february", cleanl1, NA)

table(CleanData$source_id) ### 1200 februarys, so we need to have 1200 per vector
l2 <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february.Sav")$L2
attributes(l2)
table(l2)
cleanl2 <- NA
cleanl2[l2 == 1] <- 1
cleanl2[l2 == 2] <- 0.67
cleanl2[l2 == 3] <- 0.33
cleanl2[l2 == 4] <- 0
CleanData$iss_lien3_accordDim <- ifelse(CleanData$source_id == "february", cleanl2, NA)


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
CleanData$iss_lien3_accord <- ifelse(CleanData$source_id == "april", cleanl1, CleanData$iss_lien3_accord)

table(CleanData$source_id) ### 1300 april, so we need to have 1300 per vector
l2 <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/april/april.Sav")$L2
attributes(l2)
table(l2)
cleanl2 <- NA
cleanl2[l2 == 1] <- 1 ## fait trop de place au transport coll
cleanl2[l2 == 2] <- 0.5 ## fait juste assez de place au transport coll
cleanl2[l2 == 3] <- 0 ## fait pas assez de place au transport coll
CleanData$iss_lien3_tooMuchPublicTransport <- ifelse(CleanData$source_id == "april", cleanl2, NA)


table(CleanData$iss_lien3_accord)
table(CleanData$iss_lien3_accordDim)
table(CleanData$iss_lien3_tooMuchPublicTransport)

table(CleanData$iss_lien3_accord, CleanData$source_id)
table(CleanData$iss_lien3_accordDim, CleanData$source_id)
table(CleanData$iss_lien3_tooMuchPublicTransport, CleanData$source_id)


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

# Political trust, cynism -------------------------------------------------

# january C11
Jan <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/january/january.Sav")
attributes(Jan$C11)
raw <- Jan$C11
table(raw)
clean <- NA
clean[raw == 1] <- 1
clean[raw == 2] <- 0.75
clean[raw == 3] <- 0.5
clean[raw == 4] <- 0.25
clean[raw == 5] <- 0
CleanData$poltrust_govDoJust[CleanData$source_id == "january"] <- clean 
table(clean)
rm(Jan)

# february
Feb <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february.Sav")

## N4_A2
attributes(Feb$N4_A2)
raw <- Feb$N4_A2
table(raw)
clean <- NA
clean[raw == 1] <- 1
clean[raw == 2] <- 0.67
clean[raw == 3] <- 0.33
clean[raw == 4] <- 0
CleanData$poltrust_politiciensBienveillants[CleanData$source_id == "february"] <- clean 
table(clean)

## N4_A3
attributes(Feb$N4_A3)
raw <- Feb$N4_A3
table(raw)
clean <- NA
clean[raw == 1] <- 0
clean[raw == 2] <- 0.33
clean[raw == 3] <- 0.67
clean[raw == 4] <- 1
CleanData$poltrust_politicensPreocup[CleanData$source_id == "february"] <- clean 
table(clean)

## N15
attributes(Feb$N15)
raw <- Feb$N15
table(raw)
clean <- NA
clean[raw == 1] <- 0
clean[raw == 2] <- 0.25
clean[raw == 3] <- 0.5
clean[raw == 4] <- 0.75
clean[raw == 5] <- 1
CleanData$poltrust_trustPartisPol[CleanData$source_id == "february"] <- clean 
table(clean)

rm(Feb)

# Open question: MI issue -------------------------------------------------

# february C7
# march C6
# april C6
# may C6
# datagotchi pilot 1 Q83O (in sav)
# datagotchi pilot 2 FR_openFutur, EN_openPersonally

#### LOAD data from gpt here
MI_issues <- read.csv("_SharedFolder_memoire-pot-growth/data/warehouse/most_important_issues.csv",
                      sep = ",")
names(MI_issues)[-1] <- paste0("mi_issue_", names(MI_issues)[-1]) 

#### Join with CleanData
CleanData <- left_join(CleanData, MI_issues, by = "id")

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

table(CleanData$political_knowledge)

# Reception of message --------------------------------------------------

## Which traditional media? -------------------------------------------------

media_list <- c("RDI" = "rdi", "TVA Nouvelles" = "tva",
                "Le Devoir" = "ledevoir", "La Presse" = "lapresse",
                "Radio-Canada (SRC)" = "radiocan", "LCN" = "lcn",
                "CTV" = "ctv", "CBC" = "cbc", "The Gazette" = "thegazette", 
                "Journal de Québec" = "journaldequebec", 
                "Journal de Montréal" = "journalmontreal", "Le Soleil" = "lesoleil", 
                "98.5 FM" = "fm985", "National Post" = "nationalpost", 
                "Globe and Mail" = "globemail", "Global News" = "globalnews", 
                "CNN" = "cnn", "Fox News" = "foxnews", "Guardian" = "guardian")

# Initialiser les colonnes dans CleanData
for (col_name in media_list) {
  CleanData[[paste0("reception_polinfo_", col_name)]] <- NA
}

### February ----------------------------------------------------------------

open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february_open_clean.xlsx") %>% 
  select(QUEST = `{ID de fiche}`, O_C14_clean) %>% 
  mutate(O_C14_clean = trimws(O_C14_clean))

mediafeb <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february.Sav") %>% 
  select(QUEST) %>% 
  mutate(QUEST = as.character(QUEST)) %>% 
  left_join(., open, by = "QUEST")
  
for (media_name in names(media_list)) {
  media_abb <- media_list[media_name]
  column_name <- paste0("reception_polinfo_", media_abb)
  CleanData[CleanData$source_id == "february", column_name] <- as.integer(grepl(media_name, mediafeb$O_C14_clean, fixed = TRUE))
}


### March ----------------------------------------------------------------

open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/march/march_open_clean.xlsx") %>% 
  select(QUEST = `{ID de fiche}`, O_C11_clean) %>% 
  mutate(O_C11_clean = trimws(O_C11_clean))

mediamarch <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/march/march.Sav") %>% 
  select(QUEST) %>% 
  mutate(QUEST = as.character(QUEST)) %>% 
  left_join(., open, by = "QUEST")

for (media_name in names(media_list)) {
  media_abb <- media_list[media_name]
  column_name <- paste0("reception_polinfo_", media_abb)
  CleanData[CleanData$source_id == "march", column_name] <- as.integer(grepl(media_name, mediamarch$O_C11_clean, fixed = TRUE))
}

### April ----------------------------------------------------------------

#open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/april/april_open.xlsx") %>% 
#  select(QUEST = `{ID de fiche}`, O_C11_clean) %>% 
#  mutate(O_C11_clean = trimws(O_C11_clean))

#mediamarch <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/march/march.Sav") %>% 
#  select(QUEST) %>% 
#  mutate(QUEST = as.character(QUEST)) %>% 
#  left_join(., open, by = "QUEST")
#
#for (media_name in names(media_list)) {
#  media_abb <- media_list[media_name]
#  column_name <- paste0("polinfo_", media_abb)
#  CleanData[CleanData$source_id == "march", column_name] <- as.integer(grepl(media_name, mediamarch$O_C11_clean, fixed = TRUE))
#}

### May ---------------------------------------------------------------------

open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/may/may_open_clean.xlsx") %>% 
  select(QUEST, O_C11_clean) %>% 
  mutate(O_C11_clean = trimws(O_C11_clean))

mediamay <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/may/may.Sav") %>% 
  select(QUEST) %>% 
  mutate(QUEST = as.character(QUEST)) %>% 
  left_join(., open, by = "QUEST")

for (media_name in names(media_list)) {
  media_abb <- media_list[media_name]
  column_name <- paste0("reception_polinfo_", media_abb)
  CleanData[CleanData$source_id == "may", column_name] <- as.integer(grepl(media_name, mediamay$O_C11_clean, fixed = TRUE))
}

#mediadata <- CleanData %>% 
#  select(starts_with("reception_polinfo_")) %>% 
#  drop_na()
#
#saveRDS(mediadata, "_SharedFolder_memoire-pot-growth/data/warehouse/media_analysis/mediadata.rds")

## Which social media ------------------------------------------------------------

code_to_social <- c(`1` = "facebook", `2` = "twitter", `3` = "instagram", 
                    `4` = "linkedin", `5` = "tiktok", `6` = "youtube")

# Initialiser les colonnes dans CleanData
for (col_name in code_to_social) {
  CleanData[[paste0("reception_socialmedia_", col_name)]] <- NA
}

### February ----------------------------------------------------------------

feb <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february.Sav") %>% 
  ## need to be in first three choices
  select(all_of(paste0("N1_M", 1:3)))

feb_transformed <- feb
for (col in names(feb_transformed)) {
  feb_transformed[[col]] <- as.character(code_to_social[feb[[col]]])
}

feb_transformed$all_media <- apply(feb_transformed, 1, function(x) paste(na.omit(x), collapse = ", "))

# Créer des colonnes binaires pour chaque réseau social
for (media in code_to_social) {
  column_name <- paste0("reception_socialmedia_", media)
  CleanData[CleanData$source_id == "february", column_name] <- as.integer(grepl(media, feb_transformed$all_media, fixed = TRUE))
}

table(CleanData$reception_socialmedia_tiktok, CleanData$age)

### Pilote 1 ----------------------------------------------------------------

code_to_social <- c(`1` = "facebook", `2` = "twitter", `3` = "instagram", 
                    `7` = "linkedin", `5` = "tiktok", `8` = "youtube")

pilote1raw <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot1_2022/ULA12-BASE-1500.sav") %>% 
  slice(respondents_to_keep_pilote1) %>% 
  select(Q34) %>% 
  mutate(Q34 = code_to_social[Q34]) %>% 
  fastDummies::dummy_columns(., select_columns = "Q34") %>% 
  select(-Q34, -Q34_NA) %>% 
  replace_na(list("Q34_facebook" = 0,
                  "Q34_twitter" = 0,
                  "Q34_instagram" = 0,
                  "Q34_linkedin" = 0,
                  "Q34_tiktok" = 0,
                  "Q34_youtube" = 0))

names(pilote1raw) <- gsub("Q34_", "reception_socialmedia_", names(pilote1raw))

for (col_name in names(pilote1raw)) {
  CleanData[CleanData$source_id == "pilote1", col_name] <- pilote1raw[[col_name]]
}

table(CleanData$source_id, CleanData$reception_socialmedia_facebook)

## Best way to reach you ---------------------------------------------------

reachfeb <- sondr::load_variable("_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february.Sav",
                                 variable_name = "N12")

CleanData$reception_howreach <- NA
CleanData$reception_howreach[CleanData$source_id == "february" & reachfeb %in% c(1, 2, 3)] <- "direct" # poste, telephone et porte-à-porte
CleanData$reception_howreach[CleanData$source_id == "february" & reachfeb == 4] <- "tv"
CleanData$reception_howreach[CleanData$source_id == "february" & reachfeb == 5] <- "email"
CleanData$reception_howreach[CleanData$source_id == "february" & reachfeb == 6] <- "socialmedia"
CleanData$reception_howreach[CleanData$source_id == "february" & reachfeb %in% c(7, 8)] <- "journal_radio"
table(CleanData$reception_howreach)

CleanData$reception_howreach <- factor(CleanData$reception_howreach)

table(CleanData$reception_howreach)

# Transform chr --> factor ------------------------------------------------

CleanData <- CleanData %>% select(-riding_name)
CleanData <- CleanData %>% select(-region)
CleanData$riding_id <- factor(CleanData$riding_id)

CleanData$age_cat <- factor(CleanData$age_cat,
                            levels = c("15m29", "age30m44",
                                       "age45m59", "age60m74",
                                       "age75p"))
CleanData$educ <- factor(CleanData$educ,
                         levels = c("educHSB", "educColl", "educUniv"))
CleanData$income <- factor(CleanData$income,
                         levels = c("incomeLow", "incomeMid", "incomeHigh"))
CleanData$lang <- factor(CleanData$lang)
CleanData$religion <- factor(CleanData$religion)

# Save it -----------------------------------------------------------------

saveRDS(CleanData, "_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/survey_data_holes.rds")
