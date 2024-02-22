# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
data <- potgrowth::aggregate_party_positions("_SharedFolder_memoire-pot-growth/data/warehouse/party_positions/expert_surveys/",
                                             colnames = potgrowth::qc_parties)
template <- readODS::read_ods("_SharedFolder_memoire-pot-growth/data/warehouse/party_positions/expert_surveys/template.ods")

### invert some scales for interpretability
vars_to_reverse <- c("iss_laic_secularismEncouraged",
                     "iss_laic_relSignsWorkNo",
                     "iss_laic_relSignsTeachersNo",
                     "iss_immig_immNoBenefit",
                     "iss_immig_immLess",
                     "iss_newleft_wokeObjReal",
                     "iss_newleft_wokeArtists")

for (i in vars_to_reverse) {
  data$position[data$VARIABLE == i] <- 1 - data$position[data$VARIABLE == i]
  label0 <- template$`LABEL 0`[template$VARIABLE == i]
  label1 <- template$`LABEL 1`[template$VARIABLE == i]
  template$`LABEL 0`[template$VARIABLE == i] <- label1
  template$`LABEL 1`[template$VARIABLE == i] <- label0
}


data_graph <- left_join(data, template, by = "VARIABLE") 

data_graph$graph_label <- stringr::str_replace_all(data_graph$graph_label, "\\\\n", "\n")
data_graph$`LABEL 0` <- stringr::str_replace_all(data_graph$`LABEL 0`, "\\\\n", "\n")
data_graph$`LABEL 1` <- stringr::str_replace_all(data_graph$`LABEL 1`, "\\\\n", "\n")
data_graph$order <- rep(1:length(unique(data_graph$graph_label)), each = 5)

# graph -------------------------------------------------------------------

ggplot(data_graph, aes(x = position, y = reorder(graph_label, order))) +
  scale_x_continuous(limits = c(-0.5, 1.5),
                     breaks = c()) +
  geom_point(position = position_dodge2(width = 0.5),
             aes(group = party, color = party),
             size = 4, alpha = 0.6, stroke = NA,
             show.legend = FALSE) +
  clessnverse::theme_clean_light() +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  xlab("") + ylab("") +
  geom_text(x = -0.1, hjust = 1, aes(label = `LABEL 0`),
            size = 2, color = "grey40") +
  geom_text(x = 1.1, hjust = 0, aes(label = `LABEL 1`),
            size = 2, color = "grey40")
ggsave("_SharedFolder_memoire-pot-growth/graphs/explo/clessn_predict_party_positions.png",
       width = 12, height = 20)

# Dispersion --------------------------------------------------------------

## sd
## ecart entre chaque parti

path <- "_SharedFolder_memoire-pot-growth/data/warehouse/party_positions/expert_surveys/"
files <- list.files(path, full.names = TRUE)
files <- files[grep("template", files, invert = TRUE)]
wide <- dplyr::bind_rows(lapply(files, potgrowth::read_one_file))
df <- tidyr::pivot_longer(wide, cols = potgrowth::qc_parties,
                          names_to = "party", 
                          values_to = "position") %>%
  dplyr::group_by(VARIABLE, party) %>%
  dplyr::summarise(mean_position = mean(position, na.rm = TRUE),
                   sd = stats::sd(position, na.rm = TRUE))

vars_to_reverse <- c("iss_laic_secularismEncouraged",
                     "iss_laic_relSignsWorkNo",
                     "iss_laic_relSignsTeachersNo",
                     "iss_immig_immNoBenefit",
                     "iss_immig_immLess",
                     "iss_newleft_wokeObjReal",
                     "iss_newleft_wokeArtists")

for (i in vars_to_reverse) {
  df$mean_position[df$VARIABLE == i] <- 1 - df$mean_position[df$VARIABLE == i]
}

data_graph <- left_join(df, template, by = "VARIABLE")  %>% 
  mutate(ci = mean_position - 1.96 * sd,
         cu = mean_position + 1.96 * sd,
         ci = ifelse(ci < 0, 0, ci),
         cu = ifelse(cu > 1, 1, cu))

data_graph$graph_label <- stringr::str_replace_all(data_graph$graph_label, "\\\\n", "\n")
data_graph$`LABEL 0` <- stringr::str_replace_all(data_graph$`LABEL 0`, "\\\\n", "\n")
data_graph$`LABEL 1` <- stringr::str_replace_all(data_graph$`LABEL 1`, "\\\\n", "\n")
data_graph$order <- rep(1:length(unique(data_graph$graph_label)), each = 5)

# graph -------------------------------------------------------------------

ggplot(data_graph, aes(x = mean_position, y = reorder(graph_label, order))) +
  scale_x_continuous(limits = c(-0.5, 1.5),
                     breaks = c()) +
  geom_point(position = position_dodge2(width = 0.5),
             aes(group = party, color = party),
             size = 4, alpha = 0.4, stroke = NA,
             show.legend = FALSE) +
  geom_linerange(position = position_dodge2(width = 0.5),
                 aes(group = party, color = party,
                     xmin = ci, xmax = cu),
                 alpha = 0.4, show.legend = FALSE) +
  clessnverse::theme_clean_light() +
  scale_color_manual(values = potgrowth::qc_party_colors) +
  xlab("") + ylab("") +
  labs(caption = "Les lignes représentent l'intervalle de confiance 95%.") +
  geom_text(x = -0.1, hjust = 1, aes(label = `LABEL 0`),
            size = 2, color = "grey40") +
  geom_text(x = 1.1, hjust = 0, aes(label = `LABEL 1`),
            size = 2, color = "grey40") +
  theme(plot.caption = element_text(hjust = 1))
ggsave("_SharedFolder_memoire-pot-growth/graphs/explo/clessn_predict_party_positions_dispersion.png",
       width = 15, height = 20)
