# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(plotly)

# Data --------------------------------------------------------------------

survey_data <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/cpsa2024/survey_data.rds")

# Party positions ---------------------------------------------------------

party_positions <- readRDS("_SharedFolder_memoire-pot-growth/data/marts/cpsa2024/party_positions.rds") %>% 
  mutate(party_position = ifelse(party_position == 0.25, 0.33, party_position),
         party_position = ifelse(party_position == 0.75, 0.67, party_position))

# Graph -------------------------------------------------------------------

graph_data <- potgrowth::dynamic_potgrowth_data(
  data = survey_data,
  parties = c("CAQ", "QS", "PCQ", "PQ", "PLQ"),
  issues = "iss_nationalisme_souv",
) %>% 
  mutate(estimate_irc = ifelse(estimate_irc > 0, 0, estimate_irc),
         estimate_irc = ifelse(estimate_irc < -1, -1, estimate_irc),
         conf_low_irc = ifelse(conf_low_irc > 0, 0, conf_low_irc),
         conf_low_irc = ifelse(conf_low_irc < -1, -1, conf_low_irc),
         position = ifelse(position == 0.25, 0.33, position),
         position = ifelse(position == 0.75, 0.67, position)) %>% 
  filter(position != "0.5") %>% 
  left_join(party_positions, by = c("party", "issue")) %>% 
  mutate(is_party_position = ifelse(position == party_position, 1, 0))

choices <- c(
  "0" = "Strongly in favor\nof Quebec remaining\nin Canada",
  "0.33" = "Moderately in favor\nof Quebec remaining\nin Canada",
  "0.67" = "Moderately in favor\nof Quebec becoming\na sovereign country",
  "1" = "Strongly in favor\nof Quebec becoming\na sovereign country"
)

plot <- graph_data %>%
  ggplot(aes(x = position, y = estimate_irc, 
             color = party, group = party,
             text = paste0("% acquired voters: ", round(estimate_vote * 100), "%"))) +
  geom_point(aes(alpha = is_party_position, fill = party),
             position = position_dodge(width = 0.5),
             size = 17, shape = 23, show.legend = FALSE) +
  geom_linerange(aes(ymin = conf_low_irc, ymax = conf_high_irc),
                 position = position_dodge(width = 0.5)) +
  geom_line(aes(color = party, group = party),
            position = position_dodge(width = 0.5),
            linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = potgrowth::qc_party_colors,
                     labels = c("QS", "CAQ", "PCQ" = "CPQ")) +
  scale_fill_manual(values = potgrowth::qc_party_colors) +
  scale_alpha_continuous(range = c(0, 0.15)) +
  scale_x_discrete(labels = choices,
                   breaks = c("0", "0.33", "0.67", "1")) +
  scale_y_continuous(breaks = c(-1, -0.75, -0.5, -0.25, 0),
                     labels = c(-1, -0.75, -0.5, -0.25, 0) * 10,
                     limits = c(-1, 0)) +
  labs(caption = "Diamonds indicate the parties' positions on the issue.\nVertical lines indicate the 95% confidence interval.") +
  guides(alpha = "none") +
  clessnize::theme_clean_light() +
  xlab("\nPosition on Quebec's independence") +
  ylab("Potential for Growth\n(predicted RCI of non-voters)")

xtickvals <- c(0, 0.33, 0.67, 1)
xticklabels <- c("Strongly in favor\nof Quebec remaining\nin Canada",
                "Moderately in favor\nof Quebec remaining\nin Canada",
                "Moderately in favor\nof Quebec becoming\na sovereign country",
                "Strongly in favor\nof Quebec becoming\na sovereign country")
names(xticklabels) <- xtickvals


plot_ly(data = graph_data2,
          width = 700, height = 700,
          x = ~position,
          y = ~estimate_irc,
          split = ~party,
          color = ~party,
          legendgroup = ~party,
          colors = potgrowth::qc_party_colors) %>% 
  add_markers(data = party_positions,
              text = ~paste0(party, "'s position:\n", xticklabel),
              hoverinfo = "text",
              marker = list(size = 40,
                            symbol = "diamond",
                            opacity = ~line_opacity),
              showlegend = FALSE) %>%
  add_lines(data = graph_data2,
            line = list(width = 1),
            showlegend = FALSE,
            hoverinfo = "none") %>% 
  add_markers(marker = list(size = 15),
              error_y = list(array = ~ sd),
              text = ~paste0("Acquired votes in segment<br>", progress_bar),
              hoverinfo = 'text') %>% 
  layout(
    yaxis = list(range = c(-1, 0), title = "Potential for Growth\n(predicted RCI of non-voters)\n\n",
                 tickvals = seq(from = -1, to = 0, by = 0.1),
                 ticktext = seq(from = -10, to = 0, by = 1),
                 zeroline = FALSE),
    xaxis = list(title = "\nPosition on Quebec's independence\n",
                 tickvals = xtickvals,
                 ticktext = xticklabels,
                 zeroline = FALSE)
  )

htmlwidgets::saveWidget(p, "_SharedFolder_memoire-pot-growth/graphs/plotly/iss_souv.html",
                        selfcontained = TRUE)

rsconnect::rpubsUpload(title = "Quebec independence",
                       contentFile = "_SharedFolder_memoire-pot-growth/graphs/plotly/iss_souv.html",
                       originalDoc = NULL)




