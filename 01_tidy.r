## -------------------------------------------- ##
# Survey Data Tidying
## -------------------------------------------- ##
# Purpose
## Get the data into tidy format for analysis/visualization
## !!! Raw survey data are stored in a Box folder only IRB-approved group members can access !!!
## !!! You must _manually_ download that file and place it in a local "data" folder in this project's folder !!!

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, psych, supportR)

# Get set up
source(file.path("-setup.r"))

# Clear environment/collect garbage
rm(list = ls()); gc()

## -------------------------------------------- ##
# Load Data ----
## -------------------------------------------- ##

# Define data names (for easy future updating)
labs_file <- "AI in a Day - Perspectives Survey_August 18, 2026_Labels.csv"
vals_file <- "AI in a Day - Perspectives Survey_August 18, 2026_Values.csv"

# Read in the data where response labels are retained
labs_v01 <- read.csv(file.path("data", labs_file))

# Check structure
dplyr::glimpse(labs_v01)

# Read in data where responses are given number codes (for questions on scales)
vals_v01 <- read.csv(file.path("data", vals_file))

# Check structure
dplyr::glimpse(vals_v01)

## -------------------------------------------- ##
# Generate Question Ref Table
## -------------------------------------------- ##

# Split off question lookup table
## (for future reference / so we can remove it from the 'actual' data)
lookup <- labs_v01 %>% 
  dplyr::filter(StartDate == "Start Date") %>% 
  tidyr::pivot_longer(cols = dplyr::everything(),
    names_to = "name_in_data",
    values_to = "question_text")

# Check structure
dplyr::glimpse(lookup)

## -------------------------------------------- ##
# Combine Data ----
## -------------------------------------------- ##

# Identify 'selected choice' questions
(select_qs <- lookup %>% 
  dplyr::filter(stringr::str_detect(string = question_text, pattern = "Selected Choice")))

# Streamline the values data to only numeric columns that only accept one answer
vals_v02 <- vals_v01 %>% 
  dplyr::filter(StartDate != "Start Date" & 
    stringr::str_detect(string = StartDate, pattern = "ImportId") != TRUE) %>% 
  dplyr::select(ResponseId, dplyr::all_of(select_qs$name_in_data), dplyr::ends_with("_Freq"),
    Career_Stage, Prof_Role, Neurodiverse:FirstGen) %>% 
  dplyr::select(-dplyr::where(fn = ~ any(stringr::str_detect(string = ., pattern = ",")))) %>% 
  dplyr::rename_with(.fn = ~ paste0(., "__value"),
    .cols = -ResponseId) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("__value"), .fns = as.numeric))

# What questions are lost via this?
setdiff(x = names(vals_v01), y = gsub(pattern = "__value", replacement = "", names(vals_v02)))

# Check structure
dplyr::glimpse(vals_v02)

# Join the two datasets by responseId and drop duplicate cols
svy_v01 <- labs_v01 %>% 
  dplyr::full_join(x = ., y = vals_v02, by = c("ResponseId")) %>% 
  dplyr::rename_with(.fn = ~ gsub(pattern = "\\.x", replacement = "", x = .),
    .cols = dplyr::ends_with(".x")) %>% 
  dplyr::select(-dplyr::ends_with(".y")) %>% 
  dplyr::filter(StartDate != "Start Date" & 
    stringr::str_detect(string = StartDate, pattern = "ImportId") != TRUE)

# Check structure
dplyr::glimpse(svy_v01)

## -------------------------------------------- ##
# Streamline Full Data ----
## -------------------------------------------- ##

# Check Captcha scores & survey duration to identify likely bots
# psych::multi.hist(as.numeric(svy_v01$Q_RecaptchaScore))
# psych::multi.hist(as.numeric(svy_v01$Duration..in.seconds.))

# Remove response metadata, empty columns, and preview/non-consenting/bot rows
svy_v02 <- svy_v01 %>% 
  dplyr::filter(DistributionChannel != "preview") %>% 
  dplyr::filter(ConsentQ == "Yes") %>% 
  dplyr::filter(Q_RecaptchaScore > 0) %>% # PLACEHOLDER!
  dplyr::filter(Duration..in.seconds. > 0) %>% # PLACEHOLDER!
  dplyr::relocate(ResponseId, .before = dplyr::everything()) %>% 
  dplyr::select(-StartDate:-ConsentQ) %>% 
  dplyr::select(-where(fn = ~ all(is.na(.) | nchar(.) == 0)))

# Check structure
dplyr::glimpse(svy_v02)

## -------------------------------------------- ##
# Reorder Columns ----
## -------------------------------------------- ##

# Get columns into intuitive order
svy_v03 <- svy_v02 %>% 
  dplyr::relocate(dplyr::starts_with("AIUse_Freq"), .after = ResponseId) %>% 
  dplyr::relocate(dplyr::starts_with("Gen_Attitude"), .after = LOs_3) %>% 
  dplyr::relocate(dplyr::starts_with("Policies"), .before = Career_Stage) %>% 
  dplyr::relocate(dplyr::starts_with("Career_Stage"), .before = Prof_Role) %>% 
  dplyr::relocate(dplyr::starts_with("Work_Sector"), .before = Formal_Ed) %>% 
  dplyr::relocate(dplyr::starts_with("Formal_Ed"), .before = Field) %>% 
  dplyr::relocate(dplyr::starts_with("Field"), .before = DS_Freq) %>% 
  dplyr::relocate(dplyr::starts_with("DS_Freq"), .before = GenAI_Resources) %>% 
  dplyr::relocate(dplyr::starts_with("Gender"), .after = GenAI_Resources) %>% 
  dplyr::relocate(dplyr::starts_with("LGBTQIA"), .before = Race_Ethnicity) %>% 
  dplyr::relocate(dplyr::starts_with("Neurodiverse"), .before = Caregiver) %>% 
  dplyr::relocate(dplyr::starts_with("Caregiver"), .before = FirstGen) %>% 
  dplyr::relocate(dplyr::starts_with("FirstGen"), .after = dplyr::everything())

# Check structure
dplyr::glimpse(svy_v03)

## -------------------------------------------- ##
# Streamline Categories ----
## -------------------------------------------- ##

# For visualization purposes, we want some of the category names to be simplified
svy_v04 <- svy_v03 %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::where(fn = is.character),
    .fns = ~ gsub(pattern = "Other \\(please specify\\)", replacement = "Other", x = .))) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::where(fn = is.character),
    .fns = ~ dplyr::case_when(
      . == "Prefer to self-identify:" ~ "Prefer to self-identify",
      TRUE ~ .)))

# Check structure
dplyr::glimpse(svy_v04)

## -------------------------------------------- ##
# Export Outputs ----
## -------------------------------------------- ##

# Make a final data object
svy_v99 <- svy_v04

# Check its structure
dplyr::glimpse(svy_v99)

# Export locally
write.csv(x = svy_v99, row.names = FALSE, na = '',
  file = file.path("data", "01_tidied-responses.csv"))

# Export question lookup
write.csv(x = lookup, row.names = FALSE, na = '',
  file = file.path("data", "01_question-lookup-table.csv"))

# End ----
