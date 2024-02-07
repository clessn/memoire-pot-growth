# Pacakges ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(factoextra)
library(FactoMineR)


# Data --------------------------------------------------------------------
data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/media_analysis/mediadata.rds")
#data <- as.data.frame(lapply(data, factor))
names(data) <- gsub("reception_polinfo_", "", names(data))

data <- data %>% 
  select(ledevoir, lesoleil, lapresse, radiocan, journaldequebec, journalmontreal,
         guardian, globemail, foxnews, nationalpost, globalnews, thegazette, cbc, ctv,
         cnn, tva)

# Pca ---------------------------------------------------------------------

pca <- FactoMineR::PCA(data, graph = FALSE)

fviz_pca_var(pca, ggtheme = theme_minimal(),
             repel = TRUE)


# fa ----------------------------------------------------------------------

fa <- factanal(data, factors = 3)
fa


pca <- FactoMineR::FAMD(base = data, graph = FALSE)

mca <- FactoMineR::MCA(data, graph = FALSE)

fviz_mca_var(mca,
                repel = TRUE,
                ggtheme = theme_minimal())
