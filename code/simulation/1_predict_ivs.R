# Packages ----------------------------------------------------------------
library(dplyr)
library(nnet)

# Data --------------------------------------------------------------------
data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/dimensions/census/provqc2022_5var/synthetic_poststrat_table.rds") %>% 
  ungroup() %>% 
  mutate(profil_id = 1:nrow(.)) %>% 
  relocate(profil_id)
  
# Predict models ----------------------------------------------------------

models_list <- c(list.files("_SharedFolder_memoire-pot-growth/data/marts/models/attitude_models",
                          recursive = TRUE, full.names = TRUE),
                 list.files("_SharedFolder_memoire-pot-growth/data/marts/models/reception_models",
                            recursive = TRUE, full.names = TRUE),
                 list.files("_SharedFolder_memoire-pot-growth/data/marts/models/static_attitudes_models/",
                            recursive = TRUE, full.names = TRUE))

for (i in models_list){
  model <- readRDS(i)
  varname <- tools::file_path_sans_ext(basename(i))
  if (identical(class(model), "lm")){
    values <- predict(object = model, newdata = data)
    values <- ifelse(values < 0, 0, values)
    values <- ifelse(values > 1, 1, values)
    data[[varname]] <- values
  } else if (identical(class(model), c("multinom", "nnet"))){
    data[[varname]] <- predict(model, newdata = data,
                               type = "class")
  }
  message(paste0(which(models_list == i), " ", varname))
}

saveRDS(data, "_SharedFolder_memoire-pot-growth/data/marts/sim_data/1_with_ivs.rds")
