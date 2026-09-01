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

# Remove no desired
svy_v02 <- svy_v01 %>% 
  dplyr::filter(!is.na(Training_Desired))

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

# Process data
attitude_df <- prep_select_one(df = svy_v01, q = "Gen_Attitude", grp = "Career_Stage") %>% 
  dplyr::mutate(Gen_Attitude = factor(x = as.character(Gen_Attitude), levels = c(
    "Very enthusiastic", "Enthusiastic", "A mix of caution and enthusiasm", "Cautious",
    "Opposed to GenAI", "Indifferent", "Other")))

# Check structure
dplyr::glimpse(attitude_df)

# Process data & graph
ggplot(attitude_df, aes(x = 'x', y = percent, fill = Gen_Attitude, color = 'y')) +
# aes(x = as.character(q), y = percent, fill = .data[[q]], 
#     color = "x")) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::scale_color_manual(values = "#000") +
  ggplot2::guides(color = "none") +
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

## -------------------------------------------- ##
# AI Use Freq * Career Stage ----
## -------------------------------------------- ##
  
# Do general prep for this question
svy_prep <- svy_v01 %>% 
  dplyr::filter(!Gen_Attitude %in% c("Indifferent", "Other"))

# Identify focal question
focal_q <- "TechSkill_Interest"

# Loop across desired grouping variables
for(focal_grp in c("Career_Stage", "Gen_Attitude")){
  # focal_grp <- "Career_Stage"

  # Prepare data
  ready_df <- prep_select_all(df = svy_v01, q = focal_q, grp = focal_grp) %>% 
    dplyr::mutate(value_wrap = stringr::str_wrap(string = value, width = 40),
      .after = value) %>% 
    dplyr::arrange(dplyr::desc(percent))

  # Check structure
  # dplyr::glimpse(ready_df)

  # Make a graph
  ggplot(ready_df[1:5, ], aes(x = percent, y = value_wrap, fill = value, color = 'x')) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = "#000") +
  guides(color = "none") +
  labs(x = "Percent Responses (%)", y = "",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == focal_q], 
      width = 60)) +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
  theme(legend.position = "none",
    legend.title = element_blank(), 
    strip.text = element_text(size = 16),
    axis.title.y = element_blank())


}


## -------------------------------------------- ##
# Career * Attitude Double Facet Graphs ----
## -------------------------------------------- ##

# Loop across key questions
for(focal_q in c("Task_interest", "TechSkill_Interest", "AIUse_reasons")){
  # focal_q <- "Task_interest"

  # Progress message
  message("Checking out ", focal_q, " question")

  # Prepare the data for that question
  ready_df <- prep_select_all(df = svy_v01, q = focal_q, 
    grp = c("Career_Stage", "Gen_Attitude")) %>% 
    dplyr::mutate(
      Career = dplyr::case_when(
        Career_Stage == "Preparation Stage (Currently pursuing a graduate degree)" ~ "A: (Prep)",
        Career_Stage == "Early Career Stage (1–9 years of experience post-degree)" ~ "B: 1-9 Years",
        Career_Stage == "Mid-Career Stage (10–25 years of experience)" ~ "C: 10-25 years",
        Career_Stage == "Mature Career Stage (26+ years of experience)" ~ "D: 26+ Years"), 
      Att = dplyr::case_when(
        Gen_Attitude == "Very enthusiastic" ~ "A: Love",
        Gen_Attitude == "Enthusiastic" ~ "B: Like",
        Gen_Attitude == "A mix of caution and enthusiasm" ~ "C: Mixed",
        Gen_Attitude == "Cautious" ~ "D: Cautious",
        Gen_Attitude == "Opposed to GenAI" ~ "E: Opposed",
        TRUE ~ Gen_Attitude),
      Career_Att = paste0(Career, "__", Att),
      .after = Gen_Attitude) %>% 
    dplyr::filter(!Gen_Attitude %in% c("Indifferent", "Other")) %>% 
    dplyr::mutate(value_wrap = stringr::str_wrap(string = value, width = 40),
      .after = value)

  # Check structure
  # dplyr::glimpse(ready_df)

  # Loop across group combos
  for(focal_grp in sort(unique(ready_df$Career_Att))){
    # focal_grp <- "B: 1-9 Years__C: Mixed"

    # Progress message
    message("Making graph for '", focal_grp, "'")

    # Subset data
    ready_sub <- ready_df %>% 
      dplyr::filter(Career_Att == focal_grp) %>% 
      dplyr::arrange(dplyr::desc(percent))

    # Graph it
    ggplot(ready_sub[1:5, ], aes(x = percent, y = value_wrap, fill = value, color = 'x')) +
      geom_bar(stat = "identity") +
      facet_grid(Career_Att ~ .) +
      scale_color_manual(values = "#000") +
      guides(color = "none") +
      labs(x = "Percent Responses (%)", y = "",
        title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == focal_q], 
          width = 60)) +
      supportR::theme_lyon(title_size = 20, text_size = 16) +
      theme(legend.position = "none",
        legend.title = element_blank(), 
        strip.text = element_text(size = 16),
        axis.title.y = element_blank())

    # Export locally
    ggsave(file.path("graphs", paste0("lyon_career-attitude-double-facet_", 
      tolower(focal_q), "_", tolower(focal_grp), ".png")),
      height = 8, width = 10, units = "in")  
    
  }
}







# End ----
