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

## -------------------------------------------- ##
# Select All ----
## -------------------------------------------- ##

# Define key vars
df <- svy_v01; q <- "AIUse_Freq"; grp <- "Gen_Attitude"; summarize = TRUE

# Error checks for 'df'
if(is.null(df) || "data.frame" %in% class(df) != TRUE)
  stop("'df' must be provided as a dataframe-like object")

# Error checks for 'q'
if(is.null(q) || length(q) != 1 || is.character(q) != TRUE || q %in% names(df) != TRUE)
  stop("'q' must match a single column name in 'df'")

# Error checks for 'grp'
if(is.null(grp) != TRUE){
  if(is.character(grp) != TRUE || all(grp %in% names(df)) != TRUE)
    stop("'All entries in 'grp' must be an exact match for columns in 'df'")
}

# Remove NAs in relevant question
df_v02 <- df[!is.na(df[[q]]),]

# Check structure
dplyr::glimpse(df_v02)

# If desired, also remove NAs from grouping column(s)
if(is.null(grp) != TRUE){
  for(g in seq_along(grp)){
    df_v02 <- df_v02[!is.na(df_v02[[grp[g]]]),]
  }
}

# Pare down columns
need_cols <- intersect(x = names(df_v02), y = c("ResponseId", q, grp))
df_v03 <- dplyr::select(.data = df_v02, dplyr::all_of(need_cols))

# Check structure
dplyr::glimpse(df_v03)

# Handle response order
if(paste0(q, "__value") %in% names(df)){
  df_v04 <- dplyr::arrange(.data = df_v03, dplyr::across(dplyr::starts_with(paste0(q, "__value"))))
} else { df_v04 <- dplyr::arrange(.data = df_v03, dplyr::across(dplyr::starts_with(q))) }

# Check structure
dplyr::glimpse(df_v04)

# Make response into a factor
if(is.factor(df[[q]])){
  df_v05 <- df_v04
} else {
  df_v05 <- df_v04
  df_v05[[q]] <- factor(x = df_v05[[q]], levels = unique(df_v05[[q]]))
}

# Check structure
dplyr::glimpse(df_v05)

# Make grouping variables a factor (if any are provided)
if(is.null(grp) != TRUE){
  for(k in seq_along(grp)){
    df_v05 <- df_v05 %>% 
      dplyr::arrange(dplyr::across(dplyr::starts_with(paste0(grp[g], "__value"))))
    df_v05[[grp[g]]] <- factor(x = df_v05[[grp[g]]], levels = unique(df_v05[[grp[g]]]))
  }
}

# Check structure
dplyr::glimpse(df_v05)

# Count total respondents
df_v06 <- dplyr::mutate(.data = df_v05, total_respondents = length(unique(ResponseId)))

# Check structure
dplyr::glimpse(df_v06)

# Assign correct grouping structure
if(is.null(grp) != TRUE){
  df_v07 <- df_v06 %>% 
    dplyr::group_by(dplyr::across(dplyr::all_of(c(grp, "total_respondents")))) %>% 
    dplyr::mutate(grp_respondents = length(unique(ResponseId))) %>% 
    dplyr::ungroup()
} else {
  df_v07 <- dplyr::mutate(df_v06, grp_respondents = total_respondents)
}

# Check structure
dplyr::glimpse(df_v07)

# Summarize response data
df_v08 <- df_v07 %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(
    c(grp, q, "total_respondents", "grp_respondents")))) %>% 
  dplyr::summarize(unique_respondents = length(unique(ResponseId)),
    .groups = "drop") %>% 
  dplyr::mutate(percent = round((unique_respondents / total_respondents) * 100, digits = 1),
    relative_percent = round((unique_respondents / grp_respondents) * 100, digits = 1)) %>% 
  dplyr::arrange(dplyr::desc(percent))

# Check structure
dplyr::glimpse(df_v08)

# Do some axis wrapping
df_v09 <- df_v08

# Make custom color palette
attitude_cols <- c("Opposed to GenAI" = "#8f2d56", "Cautious" = "#d81159",
  "A mix of caution and enthusiasm" = "#ffbc42",
  "Enthusiastic" = "#0496ff", "Very enthusiastic" = "#006ba6",
  "Indifferent" = "#adb5bd",
  "Other" = "#343a40")

# Exploratory graph
rel_plot <- ggplot(df_v09, aes(x = relative_percent, y = Gen_Attitude, 
    fill = AIUse_Freq, color = "x")) +
  ggplot2::geom_bar(stat = "identity") +
  labs(title = "RELATIVE") +
  # scale_fill_manual(values = attitude_cols) +
  ggplot2::scale_color_manual(values = "#000") +
  ggplot2::guides(color = "none")

# And (for comparison) non-relative exploratory graph
abs_plot <- ggplot(df_v09, aes(x = percent, y = AIUse_Freq, 
    fill = Gen_Attitude, color = "x")) +
  ggplot2::geom_bar(stat = "identity") +
  labs(title = "ABSOLUTE") +
  scale_fill_manual(values = attitude_cols) +
  ggplot2::scale_color_manual(values = "#000") +
  ggplot2::guides(color = "none")

# Export both locally
ggsave(rel_plot, filename = file.path("graphs", "lyon_relative-percent-test.png"), width = 10, height = 8, units = "in")
ggsave(abs_plot, filename = file.path("graphs", "lyon_absolute-percent-test.png"), width = 10, height = 8, units = "in")

# End ----

