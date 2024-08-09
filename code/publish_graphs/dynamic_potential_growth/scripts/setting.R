# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(plotly)

# Data --------------------------------------------------------------------
survey_data <- readRDS("../../../../_SharedFolder_memoire-pot-growth/data/marts/cpsa2024/survey_data.rds")

# Party positions ---------------------------------------------------------
party_positions <- readRDS("../../../../_SharedFolder_memoire-pot-growth/data/marts/cpsa2024/party_positions.rds") %>% 
  mutate(party_position = ifelse(party_position == 0.25 & issue == "iss_nationalisme_souv", 0.33, party_position),
         party_position = ifelse(party_position == 0.75 & issue == "iss_nationalisme_souv", 0.67, party_position))


# Issues ------------------------------------------------------------------

issues_df <- readODS::read_ods("../../../../_SharedFolder_memoire-pot-growth/data/marts/issue_labels.ods")

issue_colors <- c(
  "#FF5733", # Rouge vif
  "#33FF57", # Vert vif
  "#3357FF", # Bleu vif
  "#FF33A1", # Rose vif
  "#FFD700", # Or
  "#FF8C00", # Orange foncé
  "#8A2BE2", # Bleu violet
  "#00CED1", # Cyan foncé
  "#DC143C", # Cramoisi
  "#00FF7F", # Vert printemps
  "#FF1493", # Rose profond
  "#7FFF00", # Chartreuse
  "#FF4500", # Orange rouge
  "#2E8B57", # Vert mer
  "#DAA520", # Jaune doré
  "#9400D3", # Violet foncé
  "#FF6347", # Tomate
  "#4682B4"  # Bleu acier
)

# Function -------------------------------------------------------------------

#generate_quarto_page <- function(title, issue_slug, choices) {
#  page_content <- paste0(
#    "---\n",
#    "title: \"", title, "\"\n",
#    "format: html\n",
#    "editor: visual\n",
#    "---\n\n",
#    "<!-- {{< include ../scripts/setting.qmd >}} -->\n\n",
#    "```{r, echo=FALSE, message=FALSE, warning=FALSE}\n",
#    "#| file: ../scripts/setting.R\n",
#    "```\n\n",
#    "```{r, echo=FALSE, message=FALSE, warning=FALSE}\n",
#    "#| fig-format: svg\n\n",
#    "#print(get_graph)\n\n",
#    "issue_slug <- \"", issue_slug, "\"\n",
#    "choices <- c(\n",
#    paste0("  \"", names(choices), "\" = \"", choices, "\",\n", collapse = ""),
#    ")\n",
#    "xlabel <- \"Position on Quebec independence\"\n",
#    "potgrowth::get_quarto_graph(survey_data, issue_slug, choices, xlabel)\n",
#    "```\n\n",
#    "<span style=\"font-size: 9pt;\">Diamonds indicate the parties' positions on the issue. Data from 2022.</span>\n"
#  )
#  return(page_content)
#}
#

