# Packages ----------------------------------------------------------------
library(tidyverse)
library(openai)

# Test --------------------------------------------------------------------
answers <- c(
  "Vaccination covid",
  "Racism and gender equality",
  "Enviromental issues and animal rights",
  "L'ÉCONOMIE. Mettre en valeur et développer nos ressources naturelles, incluant le pétrole et les mines, de façon responsable et respectueuse de l'environnement.",
  "L’argent"
)

for (i in 1:length(answers)){
  answeri <- answers[i]
  chat_prompt <- create_chat_completion(
    model = "gpt-4",
    messages = list(
      list(
        "role" = "system",
        "content" = "You are a helpful assistant trained to assist in categorizing open answers about Quebecers' most important political issues into R comprehensible vectors.
        I have 10 issues to categorize: (1) Quebec's sovereignty/nationalism, (2) the protection of the French language in Quebec, (3) state secularism (laicite), (4) quebec immigration laws,
        (5) new left attitudes (systemic racism, transgender, social rights, etc.), (6) libertrian attitudes (included whatever related to anything in link to how COVID-19 restrained liberties),
        (7) 3rd transport link between Quebec and Lévis (8) the environment, (9) economic and (10) health system or personal health issues (not COVID related).
        I want you to return me a vector containing 8 values between 0, 0.5 and 1, one value for each issue, where 0 = not at all important, 0.5 = somewhat important and 1 = very important.
        I want a vector like this as the output AND NOTHING ELSE: 'c(nationalisme = 1, lang = 0.5, laic = 0.5, immig = 0, newleft = 0.5, liberty = 1, 3elien = 0, enviro = 0, economy = 0, health = 0)'.
        If you don't understand the open answer or whatever, still return this vector with 0 values everywhere.
        Respondents were asked: 'What is the most important issue for you personnally?'
        So for example, if the open answer of a respondent is 'Abbolition passeport vaccinal', I want this as the output:
        c(nationalisme = 0, lang = 0, laic = 0, immig = 0, newleft = 0, liberty = 1, 3elien = 0, enviro = 0, economy = 0, health = 0).
        Other example, if the open answer of a respondent is 'La crise climatique', I want this as the output:
        c(nationalisme = 0, lang = 0, laic = 0, immig = 0, newleft = 0, liberty = 0, 3elien = 0, enviro = 1, economy = 0, health = 0)
        "
      ),
      list(
        "role" = "user",
        "content" = paste0("Output the vector of categorized issues for this answer: ", answeri, ".")
      )
    )
  )
  print(chat_prompt[["choices"]][["message.content"]])
}

# Gather all open answers -------------------------------------------------

## Get a vector of the ids with open answers about MI issue ----------------
ids <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/surveys.rds") %>% 
  filter(source_id %in% c("february", "march", "april",
                          "may", "pilote1", "pilote2")) %>% ## source_id with open answers about MI issue
  pull(., id)

Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/surveys.rds")

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

## Merge open answers ------------------------------------------------------
table(Data$source_id)

rm_open_answers <- function(source_id, idvar_in_survey = "QUEST", idvar_in_open = "{ID de fiche}"){
  survey_ids <- haven::read_sav(paste0("_SharedFolder_memoire-pot-growth/data/lake/omnibus/", source_id, "/", source_id, ".Sav"))[[idvar_in_survey]]
  open_answers <- readxl::read_excel(paste0("_SharedFolder_memoire-pot-growth/data/lake/omnibus/", source_id, "/", source_id, "_open.xlsx"))[[idvar_in_open]]
  output <- open_answers[!(open_answers %in% survey_ids)]
  return(output)
}

rm_open_answers(source_id = "february",
                 idvar_in_survey = "QUEST",
                 idvar_in_open = "{ID de fiche}")

rm_open_answers(source_id = "march")

## February ----------------------------------------------------------------

open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february_open.xlsx")
feb_answers <- open$O_C7[!(open$`{ID de fiche}` %in% rm_open_answers("february"))]

## March -------------------------------------------------------------------

open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/march/march_open.xlsx")
march_answers <- open$O_C6[!(open$`{ID de fiche}` %in% rm_open_answers("march"))]

## April -------------------------------------------------------------------

open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/april/april_open.xlsx")
april_answers <- open$O_C6[!(open$QUEST %in% rm_open_answers("april", idvar_in_open = "QUEST"))]

## May -------------------------------------------------------------------

open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/may/may_open.xlsx")
may_answers <- open$O_C6[!(open$QUEST %in% rm_open_answers("may", idvar_in_open = "QUEST"))]

## Pilote1 -----------------------------------------------------------------

pilote1_answers <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot1_2022/ULA12-BASE-1500.sav")$Q83O[respondents_to_keep_pilote1]

## Pilote2 -----------------------------------------------------------------

pilote2_answers_fr <- sondr::read_any_csv("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv")$FR_openFutur[-respondents_to_remove_pilote2][-c(1:2)]
pilote2_answers_fr[is.na(pilote2_answers_fr)] <- NA
pilote2_answers_en <- sondr::read_any_csv("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv")$EN_openPersonally[-respondents_to_remove_pilote2][-c(1:2)]
pilote2_answers_en[is.na(pilote2_answers_en)] <- NA
pilote2_answers <- coalesce(pilote2_answers_fr, pilote2_answers_en)

rm(pilote2_answers_fr, pilote2_answers_en)

# Merge it all ------------------------------------------------------------

open_answers <- c(feb_answers, march_answers, april_answers, may_answers, pilote1_answers, pilote2_answers)
names(open_answers) <- c(Data$id[Data$source_id == "february"],
                         Data$id[Data$source_id == "march"],
                         Data$id[Data$source_id == "april"],
                         Data$id[Data$source_id == "may"],
                         Data$id[Data$source_id == "pilote1"],
                         Data$id[Data$source_id == "pilote2"])

# Prompt GPT --------------------------------------------------------------

k <- nrow(read.csv("_SharedFolder_memoire-pot-growth/data/warehouse/most_important_issues.csv")) + 1
for (i in k:length(open_answers)){
  answeri <- open_answers[i]
  
  request_successful <- FALSE
  attempt_time <- 0
  
  print(paste("Traitement de la réponse :", answeri))
  
  while (!request_successful && attempt_time <= 5) {
    start_time <- Sys.time()
    
    chat_prompt <- tryCatch({
      R.utils::withTimeout({
        create_chat_completion(
          model = "gpt-4",
          messages = list(
            list(
              "role" = "system",
              "content" = "You are a helpful assistant trained to assist in categorizing open answers about Quebecers' most important political issues into R comprehensible vectors.
        I have 10 issues to categorize: (1) Quebec's sovereignty/nationalism, (2) the protection of the French language in Quebec, (3) state secularism (laicite), (4) quebec immigration laws,
        (5) new left attitudes (systemic racism, transgender, social rights, etc.), (6) libertrian attitudes (included whatever related to anything in link to how COVID-19 restrained liberties),
        (7) 3rd transport link between Quebec and Lévis (8) the environment, (9) economic and (10) health system or personal health issues (not COVID related).
        I want you to return me a vector containing 8 values between 0, 0.5 and 1, one value for each issue, where 0 = not at all important, 0.5 = somewhat important and 1 = very important.
        I want a vector like this as the output AND NOTHING ELSE: 'c(nationalisme = 1, lang = 0.5, laic = 0.5, immig = 0, newleft = 0.5, liberty = 1, lien3 = 0, enviro = 0, economy = 0, health = 0)'.
        If you don't understand the open answer or whatever, still return this vector with 0 values everywhere.
        If I give you '...' or 'NA' or anything that's not comprehensible, return this vector with 0 values everywhere.
        Respondents were asked: 'What is the most important issue for you personnally?'
        So for example, if the open answer of a respondent is 'Abbolition passeport vaccinal', I want this as the output:
        c(nationalisme = 0, lang = 0, laic = 0, immig = 0, newleft = 0, liberty = 1, lien3 = 0, enviro = 0, economy = 0, health = 0).
        Other example, if the open answer of a respondent is 'La crise climatique', I want this as the output:
        c(nationalisme = 0, lang = 0, laic = 0, immig = 0, newleft = 0, liberty = 0, lien3 = 0, enviro = 1, economy = 0, health = 0).
        Other example, if the open answer of a respondent is '...' or 'NA', I want this as the output:
        c(nationalisme = 0, lang = 0, laic = 0, immig = 0, newleft = 0, liberty = 0, lien3 = 0, enviro = 0, economy = 0, health = 0)
        "),
            list(
              "role" = "user",
              "content" = paste0("Output the vector of categorized issues for this answer: ", answeri, ".")
            )
          )
        )
      }, timeout = 10)  # Timeout en secondes
    }, error = function(e) {
      cat("Erreur ou timeout pour la réponse :", i, "-", answeri, "\n")
      NULL  # En cas d'erreur ou de timeout, retourne NULL
    })
    
    end_time <- Sys.time()
    attempt_time <- as.numeric(end_time - start_time, units = "secs")
    
    if (!is.null(chat_prompt)) {
      request_successful <- TRUE
      cat("Réponse obtenue avec succès pour :", i, "-", answeri, "\n")
      vector <- eval(parse(text = chat_prompt$choices$message.content))
      message(paste0(answeri, " ==> "))
      print(vector)
      if (i == 1){
        write.table(data.frame(id = names(answeri), t(vector)),
                    "_SharedFolder_memoire-pot-growth/data/warehouse/most_important_issues.csv",
                    row.names = FALSE, sep = ",")
        } else {
          write.table(data.frame(id = names(answeri), t(vector)),
                      "_SharedFolder_memoire-pot-growth/data/warehouse/most_important_issues.csv",
                      append = TRUE, col.names = FALSE, row.names = FALSE,
                      sep = ",")
        }
    } else {
      cat("Tentative échouée, nouvelle tentative pour :", i, "-", answeri, "\n")
      Sys.sleep(1)  # Petite pause avant de réessayer
    }
  }
}
