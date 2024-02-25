# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Raw <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/census/statistiques-recensement-2021-CEP.xls",
                          sheet = "125 CEP 2022")

row1 <- unlist(Raw[1,])
row1[is.na(row1)] <- ""
### paste row 1 in column name
names(Raw) <- paste0(names(Raw), row1)

# Associate riding names to their codes -----------------------------------
raw_riding_names <- names(Raw)[-c(1:3)]

riding_infos <- readRDS("_SharedFolder_memoire-pot-growth/data/lake/census/riding_infos.rds")

clean_riding_names <- riding_infos$riding_name[stringdist::amatch(x = raw_riding_names, table = riding_infos$riding_name, maxDist = 2)]
df <- data.frame(raw_riding_names, clean_riding_names) ## check if worked
## worked, associate names in Raw to their riding_ids

riding_ids <- riding_infos$riding_id[stringdist::amatch(x = raw_riding_names, table = riding_infos$riding_name, maxDist = 2)]
names(riding_ids) <- raw_riding_names

# Wrangling ---------------------------------------------------------------

get_value <- function(riding_id = 648, characteristic, df = Raw){
  riding_column <- names(riding_ids)[riding_ids == riding_id]
  value <- df[[riding_column]][which(df$characteristic == characteristic)]
  return(as.numeric(value))
}


get_riding_df <- function(riding_id = 648){
  riding_column <- names(riding_ids)[riding_ids == riding_id]
  total_pop <- get_value(riding_id = riding_id,
                         characteristic = "Population totale 2021 (Données intégrales)")
  pop_014 <- get_value(riding_id = riding_id,
                       characteristic = "0 à 14 ans (nombre)")
  total_pop14p <- total_pop - pop_014
  age15m29 <- get_value(riding_id = riding_id,
                        characteristic = "15 à 29 ans  (nombre)")
  age30m44 <- get_value(riding_id = riding_id,
                        characteristic = "30 à 44 ans  (nombre)")
  age45m59 <- get_value(riding_id = riding_id,
                        characteristic = "45 à 59 ans  (nombre)")
  age60m74 <- get_value(riding_id = riding_id,
                        characteristic = "60 à 74 ans  (nombre)")
  age75p <- get_value(riding_id = riding_id,
                      characteristic = "75 et plus  (nombre)")
  langue_df <- Raw[276:286,]
  french <- langue_df[[riding_column]][which(langue_df$characteristic == "Français (nombre)")]
  english_other <- as.numeric(langue_df[[riding_column]][which(langue_df$characteristic == "Anglais (nombre)")]) +
    as.numeric(langue_df[[riding_column]][which(langue_df$characteristic == "Langues non officielles (nombre)")])
  ## income
  incomedf <- Raw[1394:1415,]
  inclow <- get_value(riding_id = riding_id,
                      characteristic = "Sans revenu",
                      df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "Moins de 10 000 $",
              df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "10 000 $ à 19 999 $",
              df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "20 000 $ à 29 999 $",
              df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "30 000 $ à 39 999 $",
              df = incomedf)
  incmid <- get_value(riding_id = riding_id,
                      characteristic = "40 000 $ à 49 999 $",
                      df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "50 000 $ à 59 999 $",
              df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "60 000 $ à 69 999 $",
              df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "70 000 $ à 79 999 $",
              df = incomedf)+
    get_value(riding_id = riding_id,
              characteristic = "80 000 $ à 89 999 $",
              df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "90 000 $ à 99 999 $",
              df = incomedf)
  inchigh <- get_value(riding_id = riding_id,
                       characteristic = "100 000 $ et plus (nombre)",
                       df = incomedf)
  ## education
  educdf <- Raw[2365:2380,]
  educlow <- get_value(riding_id = riding_id,
                       characteristic = "Aucun certificat, diplôme ou grade",
                       df = educdf) +
    get_value(riding_id = riding_id,
              characteristic = "Diplôme d’études secondaires ou attestation d’équivalence",
              df = educdf)
  educmid <- get_value(riding_id = riding_id,
                       characteristic = "Certificat ou diplôme d'études postsecondaires inférieur au baccalauréat",
                       df = educdf)
  educhigh <- get_value(riding_id = riding_id,
                        characteristic = "Certificat, diplôme ou grade universitaire du niveau du baccalauréat ou supérieur",
                        df = educdf)
  df <- data.frame(
    riding_id = riding_id,
    var = c(rep("male", 2), rep("age_cat", 5), rep("lang", 2), rep("income", 3), rep("educ", 3)),
    category = c("1", "0", "age15m29", "age30m44", "age45m59", "age60m74", "age75p", "french", "english_other",
                 "incomeLow", "incomeMid", "incomeHigh", "educHSB", "educColl", "educUniv"),
    n = c(
      get_value(riding_id = riding_id, characteristic = "Hommes + (nombre)"), # men
      get_value(riding_id = riding_id, characteristic = "Femmes + (nombre)"), # women
      age15m29,
      age30m44,
      age45m59,
      age60m74,
      age75p,
      french,
      english_other,
      inclow,
      incmid,
      inchigh,
      educlow,
      educmid,
      educhigh
    ),
    total_pop = total_pop,
    total_pop14p = total_pop14p
  )
}

t <- get_riding_df()


for (i in 1:length(riding_ids)){
  riding_idi <- riding_ids[i]
  if (i == 1){
    Clean <- get_riding_df(riding_idi)
  } else {
    Clean <- rbind(Clean, get_riding_df(riding_idi))
  }
  print(i)
  print(riding_idi)
}

Clean$n <- as.numeric(Clean$n)

saveRDS(Clean, "_SharedFolder_memoire-pot-growth/data/warehouse/dimensions/census/provqc2022_5var/census_data.rds")
