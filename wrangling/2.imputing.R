# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------

Data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/survey_data_holes.rds") %>% 
  tidyr::drop_na(riding_id)
source("wrangling/0.config.R")
riding_infos <- riding_infos %>% select(-pop_n, -prov_terr) %>% 
  mutate(riding_id = factor(riding_id))
Id_data <- Data[, c("id", "source_id", "riding_id")] %>% 
#### join riding name and region on Id_data
  left_join(., riding_infos, by = "riding_id")
Data <- Data %>% select(-c("id", "source_id", "riding_id"))

# TEST to choose parallelize argument -------------------------------------

#### we want to know if missForest takes a long time to run because of:
####### 1.  number of variables
####### 2. number of observations

no_cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(no_cores)
doParallel::registerDoParallel(cl)

start <- Sys.time()
test <- missForest::missForest(Data[sample(1:nrow(Data),
                                           size = 1000,
                                           replace = TRUE),],
                               parallelize = c("variables"))
end <- Sys.time()
end - start

#start <- Sys.time()
#test <- missForest::missForest(Data[sample(1:nrow(Data),
#                                           size = 1000,
#                                           replace = TRUE),],
#                               parallelize = c("forests"))
#end <- Sys.time()
#end - start

#### `parallelize = c("variables")` is faster

start <- Sys.time()
test <- missForest::missForest(Data,
                               parallelize = c("variables"),
                               verbose = TRUE)
end <- Sys.time()
end - start

doParallel::stopImplicitCluster()

# Set up parallel cores ---------------------------------------------------

#no_cores <- parallel::detectCores() - 1
#cl <- parallel::makeCluster(no_cores)
#doParallel::registerDoParallel(cl)

# Use missForest to imput missing data ------------------------------------------

## number of iterations to do. So if iterations = 10, the dataset will be multiplied by 10.
#iterations <- 5
#for (i in 1:iterations){
#  message(paste0("-------- ITERATION #", i , " STARTING ------------"))
#  imp_di <- missForest::missForest(Data, parallelize = c("variables"),
#                                   verbose = TRUE, maxiter = 5)
#  di <- imp_di$ximp
#  if (i == 1){
#    Output <- di
#  } else {
#    Output <- rbind(Output, di)
#  }
#  message(paste0("-------- ITERATION #", i , " IS DONE ------------"))
#}

#parallel::stopCluster(cl)

#saveRDS(Output, "_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/after_imputation.rds")
Output <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/after_imputation.rds")

#### bind Id_data on Output

Data <- cbind(
  do.call(rbind, replicate(5, Id_data, simplify = FALSE)),
  Output
)

saveRDS(Data, "_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/final.rds")
