## -------------------------------------------- ##
# Nick's Exploration of the Survey Data
## -------------------------------------------- ##
# Purpose
## Check out parts of the survey data that feel interesting but might not be interesting beyond myself.

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Get set up
source(file.path("-setup.r"))

# Clear environment/collect garbage
rm(list = ls()); gc()

# Load any custom functions
purrr::walk(.x = dir(path = file.path("tools"), 
    pattern = "*.r", full.names = TRUE),
  .f = ~ source(file = .x))

# Read in data
svy_v01 <- read.csv(file.path("data", "01_tidied-responses.csv")) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
    .fns = ~ ifelse(nchar(.) == 0, yes = NA, no = .)))

# Check structure
dplyr::glimpse(svy_v01)

# Read in survey question lookup table too
lkup <- read.csv(file.path("data", "01_question-lookup-table.csv"))
# dplyr::glimpse(lkup)

## -------------------------------------------- ##
# AI Attittude * Career Stage ----
## -------------------------------------------- ##

# Make custom color palette
attitude_cols <- c("Opposed to GenAI" = "#8f2d56", "Cautious" = "#d81159",
  "A mix of caution and enthusiasm" = "#ffbc42",
  "Enthusiastic" = "#0496ff", "Very enthusiastic" = "#006ba6",
  "Indifferent" = "#adb5bd",
  "Other" = "#343a40")

# Process data & graph
graph_select_one(df = svy_v01, q = "Gen_Attitude", grp = "Career_Stage") +
  facet_grid(. ~ Career_Stage) +
  scale_fill_manual(values = attitude_cols) +
  labs(x = "", y = "Percent Responses (%)",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Gen_Attitude"], width = 80)) +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
  theme(legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 20),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    axis.text.x = element_blank(),
    axis.title.x = element_blank())

# Export locally
ggsave(file.path("graphs", "lyon_ai-attitude_by-career.png"),
  height = 9, width = 15, units = "in")

## -------------------------------------------- ##
# AI Use Freq * Career Stage ----
## -------------------------------------------- ##

# Make color palette
freq_cols <- c("Daily" = "#e9ecef", "Weekly" = "#adb5bd", "Monthly" = "#6c757d", "Yearly" = "#343a40", "Never" = "#000")

# Process data & graph
graph_select_one(df = svy_v01, q = "AIUse_Freq", grp = "Career_Stage") +
  facet_grid(. ~ Career_Stage) +
  scale_fill_manual(values = freq_cols) +
  labs(x = "", y = "Percent Responses (%)",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "AIUse_Freq"], width = 80)) +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
  theme(legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 20),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    axis.text.x = element_blank(),
    axis.title.x = element_blank())

# Export locally
ggsave(file.path("graphs", "lyon_freq-ai_by-career.png"),
  height = 9, width = 15, units = "in")

## -------------------------------------------- ##
# AI Use Freq * Career Stage ----
## -------------------------------------------- ##

# Make color palette
freq_cols <- c("Daily" = "#e9ecef", "Weekly" = "#adb5bd", "Monthly" = "#6c757d", "Yearly" = "#343a40", "Never" = "#000", "I do not use data science in my research/role" = "#ff0000")

# Process data & graph
graph_select_one(df = svy_v01, q = "DS_Freq", grp = "Career_Stage") +
  facet_grid(. ~ Career_Stage) +
  scale_fill_manual(values = freq_cols) +
  labs(x = "", y = "Percent Responses (%)",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "DS_Freq"], width = 80)) +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
  theme(legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 20),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 13),
    axis.text.x = element_blank(),
    axis.title.x = element_blank())

# Export locally
ggsave(file.path("graphs", "lyon_freq-ds_by-career.png"),
  height = 9, width = 15, units = "in")  

# End ----
