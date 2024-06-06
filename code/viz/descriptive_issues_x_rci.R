# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

labels_df <- readODS::read_ods("_SharedFolder_memoire-pot-growth/data/warehouse/issue_labels")

issues <- labels_df$variable
to_reverse <- labels_df$reversed
names(to_reverse) <- issues
question_labels <- labels_df$question_label
names(question_labels) <- issues
labels0 <- labels_df$label0
names(labels0) <- issues
labels1 <- labels_df$label1
names(labels1) <- issues

data <- readRDS("_SharedFolder_memoire-pot-growth/data/warehouse/survey_data/survey_data_holes.rds") %>% 
  select(id, source_id, age_cat, income, male, educ, lang, religion,
         all_of(issues), starts_with("irc_")) %>% 
  tidyr::pivot_longer(.,
                      cols = starts_with("irc"),
                      names_to = "party",
                      names_prefix = "irc_",
                      values_to = "irc") %>% 
  mutate(voter = ifelse(irc >= 0, 1, 0)) %>% 
  tidyr::pivot_longer(.,
                      cols = starts_with("iss_"),
                      names_to = "issue",
                      values_to = "position") %>% 
  tidyr::drop_na() %>% 
  mutate(position = ifelse(position == 0.66, 0.67, position),
         position = ifelse(to_reverse[issue] == 1, 1 - position, position))


## Party positions ---------------------------------------------------------

df_party_positions <- potgrowth::aggregate_party_positions("_SharedFolder_memoire-pot-growth/data/warehouse/party_positions/expert_surveys/",
                                                           colnames = potgrowth::qc_parties) %>% 
  mutate(position = ifelse(to_reverse[VARIABLE] == 1, 1 - position, position))

# Function ----------------------------------------------------------------

graph1 <- data %>%
  group_by(issue, position, party, voter) %>% 
  summarise(n = n(),
            mean_irc = mean(irc)) %>% 
  group_by(issue, position, party) %>% 
  mutate(prop = n / sum(n))

wide <- graph1 %>%
  tidyr::pivot_wider(id_cols = c(issue, position, party),
                     names_from = "voter",
                     names_prefix = "voter",
                     values_from = c("n", "prop", "mean_irc"))

linewidth <- 4.5

# Facet position ----------------------------------------------------------
for (i in 1:length(issues)){
  issuei <- issues[i]
  df_party_positionsi <- df_party_positions %>%
    filter(VARIABLE == issuei)
  party_positions_vec <- df_party_positionsi$position
  names(party_positions_vec) <- df_party_positionsi$party
  graphdata <- wide %>% 
    filter(issue == issuei)
  positions <- sort(unique(graphdata$position))
  party_positions <-  positions[sapply(party_positions_vec, FUN = function(x){
    which(abs(positions - x) == min(abs(positions - x)))[1]
  })]
  names(party_positions) <- names(party_positions_vec)
  graphdata$party_position <- party_positions[graphdata$party]
  graphdata$bold <- ifelse(graphdata$party_position == graphdata$position, 1, 0)
  labels <- rep("", length(positions))
  labels[1] <- labels0[issuei]
  labels[length(labels)] <- labels1[issuei]
  names(labels) <- positions
  labels_wrapped <- stringr::str_wrap(labels, width = 20)
  names(labels_wrapped) <- names(labels)
  question_label <- question_labels[issuei]
  question_label <- stringr::str_wrap(question_label, width = 45)
  ggplot(graphdata, aes(x = party, y = prop_voter0)) +
    facet_wrap(~position, nrow = 1,
               strip.position = "bottom",
               labeller = labeller(position = labels_wrapped)) +
    geom_text(aes(label = party, y = prop_voter1 + 0.02,
                  alpha = bold),
              angle = 90, hjust = 0, size = 2.75, show.legend = FALSE) +
    geom_linerange(aes(ymin = -prop_voter0, ymax = 0),
                   color = "grey", linewidth = linewidth) +
    geom_linerange(aes(ymin = 0, ymax = prop_voter1,
                       color = party),
                   show.legend = FALSE, linewidth = linewidth - 0.3) +
    clessnize::theme_clean_light() +
    scale_color_manual(values = potgrowth::qc_party_colors) +
    scale_y_continuous(limits = c(-1, 1),
                       breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
                       labels = abs(c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1))*100) +
    ylab(expression("Non-électeurs" ~ phantom() %<-% phantom() ~ phantom() ~ phantom() ~ "Proportion (%)" ~ phantom() %->% phantom() ~ phantom() ~ phantom() ~ phantom() ~ "Électeurs")) +
    xlab(question_label) +
    labs(caption = "\nLa teinte plus foncée du nom d'un parti indique sa position sur l'enjeu.") +
    theme(axis.text.x = element_blank(),
          axis.title.y = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_text(hjust = 0.5, size = 10),
          panel.background = element_rect(color = "grey90"),
          strip.text.x = element_text(size = 7.5, vjust = 1))
  ggsave(paste0("_SharedFolder_memoire-pot-growth/cecd2024/graphs/issues_x_rci/descriptive/facet_position/", issuei, "_no_irc.png"),
         width = 7, height = 6)
  ggplot(graphdata, aes(x = party, y = prop_voter0)) +
    facet_wrap(~position, nrow = 1,
               strip.position = "bottom",
               labeller = labeller(position = labels_wrapped)) +
    geom_text(aes(label = party, y = prop_voter1 + 0.02,
                  alpha = bold), show.legend = FALSE,
              angle = 90, hjust = 0, size = 2.75) +
    geom_linerange(aes(ymin = -prop_voter0, ymax = 0,
                       color = mean_irc_voter0),
                   linewidth = linewidth, alpha = 1) +
    scale_color_gradientn(name = "IRC moyen: non-électeurs",
                          colors = c("black",
                                     "#DAA520",
                                     "#50C878",
                                     "#003366"),
                          values = scales::rescale(c(quantile(wide$mean_irc_voter0, c(0.1, 0.25, 0.5, 0.75, 0.9)))),
                          breaks = c(-0.6, -0.7, -0.8)) +
    ggnewscale::new_scale_color() +
    geom_linerange(aes(ymin = 0, ymax = prop_voter1,
                       color = party, alpha = mean_irc_voter1),
                   linewidth = linewidth - 0.3) +
    scale_color_manual(values = potgrowth::qc_party_colors) +
    guides(alpha = guide_legend(title.position = "top", order = 1),
           color = "none") +
    scale_alpha_continuous(name = "IRC moyen: électeurs", range = c(0.1, 1)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    clessnize::theme_clean_light() +
    scale_y_continuous(limits = c(-1, 1),
                       breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
                       labels = abs(c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1))*100) +
    ylab(expression("Non-électeurs" ~ phantom() %<-% phantom() ~ phantom() ~ phantom() ~ "Proportion (%)" ~ phantom() %->% phantom() ~ phantom() ~ phantom() ~ phantom() ~ "Électeurs")) +
    xlab(question_label) +
    labs(caption = "\nLa teinte plus foncée du nom d'un parti indique sa position sur l'enjeu.") +
    theme(legend.title = element_text(),
          legend.position = "right",
          axis.title.y = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_text(hjust = 0.5, size = 10),
          panel.background = element_rect(color = "grey90"),
          axis.text.x = element_blank(),
          strip.text.x = element_text(size = 7.5, vjust = 1))
  ggsave(paste0("_SharedFolder_memoire-pot-growth/cecd2024/graphs/issues_x_rci/descriptive/facet_position/", issuei, "_irc.png"),
         width = 8, height = 6)
  message(paste0(i, " - ", issuei))
}


# Facet party -------------------------------------------------------------
for (i in 1:length(issues)){
  issuei <- issues[i]
  df_party_positionsi <- df_party_positions %>%
    filter(VARIABLE == issuei)
  party_positions_vec <- df_party_positionsi$position
  names(party_positions_vec) <- df_party_positionsi$party
  graphdata <- wide %>% 
    filter(issue == issuei)
  n <- pull(graphdata %>%
    filter(party == "CAQ") %>% 
    group_by(party) %>% 
    mutate(n = n_voter0 + n_voter1) %>% 
    summarise(n = sum(n)), n)
  positions <- sort(unique(graphdata$position))
  party_positions <-  positions[sapply(party_positions_vec, FUN = function(x){
    which(abs(positions - x) == min(abs(positions - x)))[1]
  })]
  names(party_positions) <- names(party_positions_vec)
  graphdata$party_position <- party_positions[graphdata$party]
  graphdata$bold <- ifelse(graphdata$party_position == graphdata$position, 1, 0)
  labels <- rep("", length(positions))
  labels[1] <- labels0[issuei]
  labels[length(labels)] <- labels1[issuei]
  names(labels) <- positions
  labels_wrapped <- stringr::str_wrap(labels, width = 20)
  names(labels_wrapped) <- names(labels)
  question_label <- question_labels[issuei]
  question_label <- stringr::str_wrap(question_label, width = 60)
  question_label <- paste0("\n", question_label)
  ggplot(graphdata, aes(x = position, y = prop_voter0)) +
    facet_wrap(~party, nrow = 1) +
    geom_linerange(data = graphdata[graphdata$bold == 1,],
                   aes(ymin = -prop_voter0 - 0.009, ymax = 0),
                   color = "black", linewidth = linewidth + 1) +
    geom_linerange(data = graphdata[graphdata$bold == 1,],
                   aes(ymax = prop_voter1 + 0.009, ymin = 0),
                   color = "black", linewidth = linewidth + 1) +
    geom_linerange(aes(ymin = -prop_voter0, ymax = 0),
                   color = "grey",  linewidth = linewidth) +
    geom_linerange(aes(ymin = 0, ymax = prop_voter1,
                       color = party),
                   show.legend = FALSE, linewidth = linewidth) +
    clessnize::theme_clean_light() +
    scale_color_manual(values = potgrowth::qc_party_colors) +
    scale_x_continuous(labels = c(labels_wrapped[1], labels_wrapped[length(labels_wrapped)]),
                       breaks = c(0.1, 0.9),
                       expand = c(0.1, 0.1)) +
    scale_y_continuous(limits = c(-1, 1),
                       breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
                       labels = abs(c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1))*100) +
    ylab(expression("Non-électeurs" ~ phantom() %<-% phantom() ~ phantom() ~ phantom() ~ "Proportion (%)" ~ phantom() %->% phantom() ~ phantom() ~ phantom() ~ phantom() ~ "Électeurs")) +
    xlab(question_label) +
    labs(caption = paste0("\nLa barre encadrée de noir représente la position du parti.\nn = ", n, ", données de sondage 2022")) +
    theme(axis.text.x = element_text(angle = 90, size = 6, hjust = 1, vjust = 0.5),
          axis.title.y = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_text(hjust = 0.5, size = 10),
          panel.background = element_rect(color = "grey90"),
          strip.text.x = element_text(size = 7.5, vjust = 1))
  ggsave(paste0("_SharedFolder_memoire-pot-growth/cecd2024/graphs/issues_x_rci/descriptive/facet_party/", issuei, "_no_irc.png"),
         width = 7, height = 6)
  ggplot(graphdata, aes(x = position, y = prop_voter0)) +
    facet_wrap(~party, nrow = 1) +
    geom_linerange(data = graphdata[graphdata$bold == 1,],
                   aes(ymin = -prop_voter0 - 0.009, ymax = 0),
                   color = "black", linewidth = linewidth + 1) +
    geom_linerange(data = graphdata[graphdata$bold == 1,],
                   aes(ymax = prop_voter1 + 0.009, ymin = 0),
                   color = "black", linewidth = linewidth + 1) +
    geom_linerange(data = graphdata[graphdata$bold == 1,],
                   aes(ymax = prop_voter1, ymin = 0),
                   color = "white", linewidth = linewidth) +
    geom_linerange(aes(ymin = -prop_voter0, ymax = 0,
                       color = mean_irc_voter0),
                   linewidth = linewidth, alpha = 1) +
    scale_color_gradientn(name = "IRC moyen: non-électeurs",
                          colors = c("black",
                                     "#DAA520",
                                     "#50C878",
                                     "#003366"),
                          values = scales::rescale(c(quantile(wide$mean_irc_voter0, c(0.1, 0.25, 0.5, 0.75, 0.9)))),
                          breaks = c(-0.5, -0.55, -0.6, -0.65, -0.7, -0.75, -0.8, -0.85)) +
    ggnewscale::new_scale_color() +
    geom_linerange(aes(ymin = 0, ymax = prop_voter1,
                       color = party, alpha = mean_irc_voter1),
                   linewidth = linewidth) +
    scale_color_manual(values = potgrowth::qc_party_colors) +
    guides(alpha = guide_legend(title.position = "top", order = 1),
           color = "none") +
    scale_alpha_continuous(name = "IRC moyen: électeurs", range = c(0.1, 1)) +
    #geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    clessnize::theme_clean_light() +
    scale_x_continuous(labels = c(labels_wrapped[1], labels_wrapped[length(labels_wrapped)]),
                       breaks = c(0.1, 0.9),
                       expand = c(0.1, 0.1)) +
    scale_y_continuous(limits = c(-1, 1),
                       breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
                       labels = abs(c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1))*100) +
    ylab(expression("Non-électeurs" ~ phantom() %<-% phantom() ~ phantom() ~ phantom() ~ "Proportion (%)" ~ phantom() %->% phantom() ~ phantom() ~ phantom() ~ phantom() ~ "Électeurs")) +
    xlab(question_label) +
    labs(caption = paste0("\nLa barre encadrée de noir représente la position du parti.\nn = ", n, ", données de sondage 2022")) +
    theme(legend.title = element_text(),
          legend.position = "right",
          axis.text.x = element_text(angle = 90, size = 6, hjust = 1, vjust = 0.5),
          axis.title.y = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_text(hjust = 0.5, size = 10),
          panel.background = element_rect(color = "grey90"),
          strip.text.x = element_text(size = 7.5, vjust = 1))
  ggsave(paste0("_SharedFolder_memoire-pot-growth/cecd2024/graphs/issues_x_rci/descriptive/facet_party/", issuei, "_irc.png"),
         width = 8, height = 6)
  if (issuei %in% c("iss_nationalisme_souv", "iss_lang_worriedFrMtl", "iss_laic_relSignsTeachersNo",
                    "iss_lien3_accordDim", "iss_newleft_wokeSysRaci", "iss_immig_immLess")){
    ggsave(paste0("_SharedFolder_memoire-pot-growth/cecd2024/graphs/final/", issuei, ".png"),
           width = 8, height = 6)
  }
  message(paste0(i, " - ", issuei))
}
