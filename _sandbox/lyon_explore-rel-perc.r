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
df <- svy_v01; q <- "AIUse_reasons"; grp <- "Gen_Attitude"; summarize = TRUE

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

# Error checks for 'summarize'
if(!is.logical(summarize)){
  warning("'summarize' must be a logical. Coercing to TRUE")
  summarize <- TRUE }
  
# Remove NAs in relevant question
df_v02 <- df[!is.na(df[[q]]),]

# If desired, also remove NAs from grouping column(s)
if(is.null(grp) != TRUE){
  for(g in seq_along(grp)){
    df_v02 <- df_v02[!is.na(df_v02[[grp[g]]]),]
  }
}

# Pare down columns
need_cols <- intersect(x = names(df_v02), y = c("ResponseId", q, grp))
df_v03 <- dplyr::select(.data = df_v02, dplyr::all_of(need_cols))

# Handle difference between commas in actual response text versus collapsing char
df_v04 <- df_v03 %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = q, replacement = "question", x = .)) %>% 
  dplyr::mutate(question = gsub(", ", "___", question)) %>% 
  dplyr::mutate(question = gsub(",", ";", question)) %>% 
  dplyr::mutate(question = gsub("___", ", ", question))

# Make grouping variables a factor (if any are provided)
if(is.null(grp) != TRUE){
  for(k in seq_along(grp)){
    df_v04 <- df_v04 %>% 
      dplyr::arrange(dplyr::across(dplyr::starts_with(paste0(grp[g], "__value"))))
    df_v04[[grp[g]]] <- factor(x = df_v04[[grp[g]]], levels = unique(df_v04[[grp[g]]]))
  }
}

# Count number of boxes checked per question
delim_ct <- stringr::str_count(string = df_v04$question, pattern = ";")
max_delim <- max(delim_ct, na.rm = TRUE)

# Get one row per 'checked box' in original question
df_v05 <- df_v04 %>% 
  tidyr::separate_wider_delim(cols = question, delim = ";",
    names = c(paste0("box", 1:(max_delim + 1))), too_few = "align_start") %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("box")) %>% 
  dplyr::select(-name) %>% 
  dplyr::filter(!is.na(value))

# Do generally-needed tidying of those responses
df_v06 <- df_v05 %>% 
  dplyr::mutate(value = gsub(pattern = "’", replacement = "'", x = value))

# Count total respondents
df_v07 <- dplyr::mutate(.data = df_v06, total_respondents = length(unique(ResponseId)))

# Assign correct grouping structure
if(is.null(grp) != TRUE){
  df_v08 <- df_v07 %>% 
    dplyr::group_by(dplyr::across(dplyr::all_of(c(grp, "value", "total_respondents"))))
} else {
  df_v08 <- df_v07 %>% 
    dplyr::group_by(value, total_respondents)
}

# Summarize response data
if(summarize == TRUE){
  df_v09 <- df_v08 %>% 
    dplyr::summarize(unique_respondents = length(unique(ResponseId)),
      .groups = "drop") %>% 
    dplyr::mutate(percent = round((unique_respondents / total_respondents) * 100, digits = 1)) %>% 
    dplyr::arrange(dplyr::desc(percent))   
} else { df_v09 <- dplyr::ungroup(df_v08) }

# Check structure
dplyr::glimpse(df_v09)

# End ----

