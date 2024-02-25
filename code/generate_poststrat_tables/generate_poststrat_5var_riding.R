# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

SurveyData <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/with_adj_saliency.rds") %>% 
  ## only select indepedent variables
  select(male, age_cat, lang, income, educ)

CensusWide <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/dimensions/census/provqc2022_5var/census_data.rds") %>% 
  mutate(varname = paste0(var, "_", category)) %>% 
  tidyr::pivot_wider(., id_cols = c("riding_id", "total_pop", "total_pop14p"),
                     names_from = "varname",
                     values_from = "n")

source("wrangling/0.config.R")
riding_infos <- riding_infos %>% 
  select(-prov_terr, -large)

# Generate strat table ----------------------------------------------------

for (i in 1:nrow(CensusWide)) {
  options(dplyr.summarise.inform = FALSE)
  options(dplyr.left_join.inform = FALSE)
  riding_idi <- CensusWide$riding_id[i]
  prop_age14p <- CensusWide$total_pop14p[i]
  censusGender <- c("male" = CensusWide$male_1[i],
                    "female" = CensusWide$male_0[i])
  censusPropsGender <- censusGender/sum(censusGender)
  censusLangue <- c("french" = CensusWide$lang_french[i],
                    "english_other" = CensusWide$lang_english_other[i])
  censusPropsLangue <- censusLangue/sum(censusLangue)
  censusAge <- c("age15m29" = CensusWide$age_cat_age15m29[i],
                 "age30m44" = CensusWide$age_cat_age30m44[i],
                 "age45m59" = CensusWide$age_cat_age45m59[i],
                 "age60m74" = CensusWide$age_cat_age60m74[i],
                 "age75p" = CensusWide$age_cat_age75p[i])
  censusPropsAge <- censusAge/sum(censusAge)
  censusIncome <- c("incomeLow" =  CensusWide$income_incomeLow[i],
                    "incomeMid" =  CensusWide$income_incomeMid[i],
                    "incomeHigh" = CensusWide$income_incomeHigh[i])
  censusPropsIncome <- censusIncome/sum(censusIncome)
  censusEduc <- c("educHSB" =   CensusWide$educ_educHSB[i],
                  "educColl" =  CensusWide$educ_educColl[i],
                  "educUniv" =  CensusWide$educ_educUniv[i])
  censusPropsEduc <- censusEduc/sum(censusEduc)
  FirstStrat <- SurveyData %>%
    select(male, age_cat) %>%
    na.omit() %>%
    group_by(male, age_cat) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(prct = n / sum(n)) %>% 
    group_by(age_cat) %>% 
    mutate(prct = sum(prct))
  FirstStrat$adjustCoef <- censusPropsAge[as.character(FirstStrat$age_cat)]/FirstStrat$prct
  FirstStrat$newFreq <- FirstStrat$n*FirstStrat$adjustCoef
  FirstStrat <- FirstStrat %>% 
    ungroup() %>% 
    select(male, age_cat, newFreq) %>%
    rename(n=newFreq) %>%
    mutate(prct=n / sum(n))

  LastStage <- FirstStrat
  
  for (j in 3:length(names(SurveyData))) {
    vars <- (names(SurveyData)[1:j])
    
    if (vars[j]=="lang") {
      censusProps <- censusPropsLangue
    } else if (vars[j]=="income") {
      censusProps <- censusPropsIncome
    } else if (vars[j]=="educ") {
      censusProps <- censusPropsEduc
    }
    
    Strat <- SurveyData %>%
      select(any_of(vars)) %>%
      na.omit() %>%
      group_by_at(vars) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      mutate(prct = n / sum(n))
    args <- paste0("unique(Strat$", vars, ")", collapse = ", ")
    
    AllCombs <- eval(parse(text = paste0("expand.grid(", args, ")")))
    names(AllCombs) <- vars
    
    Strat <- left_join(AllCombs, Strat) %>% 
      replace(is.na(.), 0) %>%
      group_by_at(vars[-1]) %>%
      mutate(prct2 = sum(prct))
    
    varj <- vars[j]
    Strat$adjustCoef <- censusProps[as.character(Strat[[varj]])]/Strat$prct2
    Strat$adjustCoef <- ifelse(Strat$adjustCoef %in% c(-Inf, Inf), 0, Strat$adjustCoef)
    Strat$newFreq <- Strat$n*Strat$adjustCoef
    
    Strat <- Strat %>% 
      select(any_of(vars), newFreq) %>%
      rename(n=newFreq) %>%
      group_by_at(vars[-j]) %>%
      mutate(prct=(n/sum(n)))
    
    LastStagej <- LastStage %>% 
      select(-n) %>% 
      rename(prct_ls = prct)
    
    Strat2 <- left_join(Strat, LastStagej) %>% 
      mutate(prct = prct*prct_ls)
    
    sum(Strat2$prct)
    LastStage <- Strat2 %>% 
      select(-prct_ls)
  }
  if (i == 1) {
    StratTable <- LastStage %>%
      mutate(riding_id = riding_idi)
  }
  else {
    TempStrat <- LastStage %>%
      mutate(riding_id = riding_idi)
    StratTable <- rbind(StratTable,TempStrat)
  }
  print(paste0(round(i/nrow(CensusWide)*100),"% - ", riding_idi))
}

### Check that every riding has a sum of 1
check <- StratTable %>% 
  group_by(riding_id) %>% 
  summarise(sum = sum(prct))
table(check$sum)

# Clean and reorder columns -----------------------------------------------

Output <- StratTable %>% 
  left_join(., riding_infos, by = "riding_id") %>% 
  relocate(riding_id, riding_name, granular, total_pop = pop_n)
saveRDS(Output, "_SharedFolder_memoire-pot-growth/data/warehouse/dimensions/census/provqc2022_5var/synthetic_poststrat_table.rds")
