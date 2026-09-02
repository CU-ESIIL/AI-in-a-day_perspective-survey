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
# Grouped Select All Graphs ----
## -------------------------------------------- ##

# Make a palette that will work across grouping variables
selectall_cols <- c(
  "Prep (graduate student)" = "#99e2b4",
  "Early (1-9 years post-degree)" = "#67b99a",
  "Mid-Career (10-25 years)" = "#358f80",
  "Mature (26+ Years)" = "#036666",
  "Opposed to GenAI" = "#8f2d56",
  "Cautious" = "#d81159",
  "A mix of caution and enthusiasm" = "#ffbc42",
  "Enthusiastic" = "#0496ff", 
  "Very enthusiastic" = "#006ba6",
  "Indifferent" = "#adb5bd",
  "Other" = "#343a40")

# Do general prep for this question
svy_prep <- svy_v01 %>% 
  dplyr::mutate(Career_Stage = dplyr::case_when(
    Career_Stage == "Preparation Stage (Currently pursuing a graduate degree)" ~ "Prep (graduate student)",
    Career_Stage == "Early Career Stage (1–9 years of experience post-degree)" ~ "Early (1-9 years post-degree)",
    Career_Stage == "Mid-Career Stage (10–25 years of experience)" ~ "Mid-Career (10-25 years)",
    Career_Stage == "Mature Career Stage (26+ years of experience)" ~ "Mature (26+ Years)")) %>% 
  dplyr::filter(!Gen_Attitude %in% c("Indifferent", "Other"))

# Check structure
dplyr::glimpse(svy_prep)

# Identify focal question
for(focal_q in c("Task_interest", "TechSkill_Interest", "AIUse_reasons")){
  # focal_q <- "TechSkill_Interest"

  # Progress message
  message("Checking out ", focal_q, " question")

  # Loop across desired grouping variables
  for(focal_grp in c("Career_Stage", "Gen_Attitude")){
    # focal_grp <- "Career_Stage"

    # Progress message
    message("Making graph for '", focal_grp, "'")

    # Prepare data
    ready_df <- prep_select_all(df = svy_prep, q = focal_q, grp = focal_grp) %>% 
      dplyr::mutate(value_wrap = stringr::str_wrap(string = value, width = 40),
        .after = value) %>% 
      dplyr::arrange(dplyr::desc(percent))

    # Check structure
    # dplyr::glimpse(ready_df)

    # Get a more plot-ready version
    ready_sub <- ready_df %>% 
      dplyr::group_by(value) %>% 
      dplyr::mutate(perc_tot = sum(percent, na.rm = TRUE)) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(perc_tot) %>% 
      dplyr::mutate(value = factor(value, levels = unique(value)),
        value_wrap = factor(value_wrap, levels = unique(value_wrap)))

    # Loop across that to get top 5 responses per group variable
    ready_list <- list()
    for(sub_grp in sort(unique(ready_df[[focal_grp]]))){
      # sub_grp <- "Early Career Stage (1–9 years of experience post-degree)"

      ready_list[[sub_grp]] <- ready_df[ready_df[[focal_grp]] == sub_grp, ] %>% 
        dplyr::arrange(dplyr::desc(percent)) %>% 
        dplyr::slice_head(n = 5)
    }

    # Unlist the top 5 stuff
    ready_top <- purrr::list_rbind(x = ready_list) %>% 
      dplyr::group_by(value) %>% 
      dplyr::mutate(perc_tot = sum(percent, na.rm = TRUE)) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(perc_tot) %>% 
      dplyr::mutate(value = factor(value, levels = unique(value)),
        value_wrap = factor(value_wrap, levels = unique(value_wrap)))
    
    # Do grouping factor re-leveling
    if(focal_grp == "Gen_Attitude"){
      ready_sub$Gen_Attitude <- factor(ready_sub$Gen_Attitude, 
        levels = rev(c("Opposed to GenAI", "Cautious", 
          "A mix of caution and enthusiasm", "Enthusiastic", "Very enthusiastic", 
          "Indifferent", "Other"))) 
      ready_top$Gen_Attitude <- factor(ready_top$Gen_Attitude, 
        levels = rev(c("Opposed to GenAI", "Cautious", 
          "A mix of caution and enthusiasm", "Enthusiastic", "Very enthusiastic", 
          "Indifferent", "Other"))) }
    if(focal_grp == "Career_Stage"){
      ready_sub$Career_Stage <- factor(ready_sub$Career_Stage, 
        levels = c("Prep (graduate student)", "Early (1-9 years post-degree)", 
          "Mid-Career (10-25 years)", "Mature (26+ Years)")) 
      ready_top$Career_Stage <- factor(ready_top$Career_Stage, 
        levels = c("Prep (graduate student)", "Early (1-9 years post-degree)", 
          "Mid-Career (10-25 years)", "Mature (26+ Years)"))  }
    
    # Make a graph
    ggplot(ready_sub, aes(x = percent, y = value_wrap, fill = .data[[focal_grp]], color = 'x')) +
      geom_bar(stat = "identity") +
      scale_color_manual(values = "#000") +
      scale_fill_manual(values = selectall_cols) +
      guides(color = "none") +
      labs(x = "Percent Responses (%)", y = "",
        title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == focal_q], 
          width = 80)) +
      supportR::theme_lyon(title_size = 20, text_size = 16) +
      theme(legend.title = element_blank(), 
        legend.position = "bottom",
        axis.title.y = element_blank())

    # Generate graph file name
    facet_name <- paste0("lyon_grouped-select-alls_", gsub("_", "-", tolower(focal_q)), "_", 
      gsub("_", "-", tolower(focal_grp)))

    # Export the graph!
    ggsave(file.path("graphs", paste0(facet_name, ".png")), height = 14, width = 12, units = "in") 
    
    # Make a 'top 5' version of that graph
    ggplot(ready_top, aes(x = percent, y = value_wrap, fill = .data[[focal_grp]], color = 'x')) +
      geom_bar(stat = "identity") +
      scale_color_manual(values = "#000") +
      scale_fill_manual(values = selectall_cols) +
      guides(color = "none") +
      labs(x = "Percent Responses (%)", y = "",
        title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == focal_q], 
          width = 80)) +
      supportR::theme_lyon(title_size = 20, text_size = 16) +
      theme(legend.title = element_blank(), 
        legend.position = "bottom",
        axis.title.y = element_blank())
    
    # And export it too
    ggsave(file.path("graphs", paste0(facet_name, "_top-5.png")), height = 14, width = 12, units = "in") 
  }
}

# End ----
