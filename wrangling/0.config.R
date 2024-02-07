clean_issues <- c("nationalisme" = "Nationalisme/\nSouveraineté",
                  "lang" = "Protection\nlangue française",
                  "laic" = "Laïcité\nde l'État",
                  "immig" = "Immigration",
                  "newleft" = "Nouvelle gauche",
                  "liberty" = "Libertarisme",
                  "lien3" = "Troisième lien",
                  "enviro" = "Environnement")

issues <- names(clean_issues)

partys <- c("CAQ", "PLQ", "PQ", "QS", "PCQ")

party_colors <- c("CAQ" = "#00cccc", "PLQ" = "#ED1A2D", "PQ" = "#004C9D", "QS" = "#FF5605", "PCQ" = "purple")

riding_infos <- left_join(readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/dimensions/prov_ridings/data.rds"),
                          read.csv("_SharedFolder_memoire-pot-growth/data/warehouse/dimensions/ridings_regions/region_ridings.csv") %>% 
                            filter(level == "prov2022") %>% 
                            select(-level, -riding_name, -density),
                          by = "riding_id")