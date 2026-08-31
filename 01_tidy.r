## -------------------------------------------- ##
# Survey Data Tidying
## -------------------------------------------- ##
# Purpose
## Get the data into tidy format for analysis/visualization and add country
## !!! Raw survey data are stored in a Box folder only IRB-approved group members can access !!!
## !!! You must _manually_ download that file and place it in a local "data" folder in this project's folder !!!

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, psych, janitor, supportR, sf, rnaturalearth)

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

# Read in data where responses are given number codes (for questions on scales)
vals_v01 <- read.csv(file.path("data", vals_file))

# Check structure
# dplyr::glimpse(vals_v01)

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
  dplyr::select(ResponseId, dplyr::all_of(select_qs$name_in_data), 
    dplyr::ends_with("_Freq"), Career_Stage, Prof_Role, Neurodiverse:FirstGen) %>%
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
    stringr::str_detect(string = StartDate, pattern = "ImportId") != TRUE) %>%
  # convert numeric to numeric, read in as character due to keys in first row
  dplyr::mutate(across(c("Progress", "Duration..in.seconds.",
    "LocationLatitude", "LocationLongitude", "Q_RecaptchaScore"), ~ as.numeric(.)))

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
  # Remove responses from prior to survey going public/live
  dplyr::filter(DistributionChannel != "preview") %>%
  dplyr::filter(StartDate >= "2026-07-14 00:00:00") %>%
  dplyr::filter(ConsentQ == "Yes") %>%
  # current qualtrics docs; indicates v3 google recaptcha at present
  # Best practice would probably be to strip a few more, though the ones with
  # low or missing recaptchas seem very real on inspection, leave for now
  # https://www.qualtrics.com/support/survey-platform/survey-module/survey-checker/fraud-detection/
  # https://developers.google.com/recaptcha/docs/v3#interpreting_the_score
  dplyr::filter(Q_RecaptchaScore > 0 & !is.na(Q_RecaptchaScore)) %>% # PLACEHOLDER!
  dplyr::filter(Duration..in.seconds. > 0) %>% # PLACEHOLDER!
  dplyr::relocate(ResponseId, .before = dplyr::everything()) %>%
  dplyr::select(-StartDate:-IPAddress, -Finished:-ExternalReference, -DistributionChannel, -UserLanguage) %>%
  filter(!if_all(-ResponseId, ~ . == "" | is.na(.)))

# Check lost columns
supportR::diff_check(old = names(svy_v01), new = names(svy_v02))

# Check structure
dplyr::glimpse(svy_v02)

## -------------------------------------------- ##
# Add Countries ----
## -------------------------------------------- ##

# Identify best guess of country based on lat/long
world_map <- ne_countries(scale = "medium", returnclass = "sf")
svy_spatial <- st_as_sf(svy_v02,
  coords = c("LocationLongitude", "LocationLatitude"),
  crs = 4326, remove = FALSE)

# Add it to the data
svy_v03 <- sf::st_join(svy_spatial, world_map %>% dplyr::select(name),
  join = st_intersects) %>%
  sf::st_drop_geometry(-geometry) %>%
  dplyr::rename(Country = name)

# Check structure
dplyr::glimpse(svy_v03)

## -------------------------------------------- ##
# Streamline Categories ----
## -------------------------------------------- ##

# For visualization purposes, we want some of the category names to be simplified
svy_v04 <- svy_v03 %>%
  dplyr::mutate(dplyr::across(.cols = dplyr::where(fn = is.character),
    .fns = ~ gsub(pattern = "Other \\(please specify\\)", replacement = "Other", x = .) )) %>%
  dplyr::mutate(dplyr::across(.cols = dplyr::where(fn = is.character),
    .fns = ~ dplyr::case_when(
      . == "Prefer to self-identify:" ~ "Prefer to self-identify",
      TRUE ~ .) ))

# Check structure
dplyr::glimpse(svy_v04)

## -------------------------------------------- ##
# Remove Unnecessary Info ----
## -------------------------------------------- ##

# Remove empty spaces / unneeded info
svy_v05 <- svy_v04 %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(trimws(.), ""))) %>%
  # remove identifying info
  janitor::remove_empty("cols") %>%
  dplyr::select(-LocationLatitude, -LocationLongitude) %>%
  janitor::remove_constant()

# Check structure
dplyr::glimpse(svy_v05)

## -------------------------------------------- ##
# Collapse Free Text to Categories ----
## -------------------------------------------- ##

# Some free text answers basically fit a category option, fix those here
svy_v06 <- svy_v05



# Check structure
dplyr::glimpse(svy_v06)

## -------------------------------------------- ##
# Redact Identifying Info in Free Text Cols ----
## -------------------------------------------- ##

# Identify all free text columns
free_cols <- c("AI_tools", "AIUse_reasons_15_TEXT", 
  paste0("LOs_", 1:3), "Challenges_15_TEXT", "Field_8_TEXT", "Formal_Ed_4_TEXT", "Gen_Attitude_21_TEXT",
  "Policies_8_TEXT", "Prof_Role_8_TEXT", "PromisingOpps_14_TEXT", "Task_interest_19_TEXT", "GenAI_Resources",
  "TechSkill_Interest_15_TEXT", "Training_Desired_9_TEXT", "Training_Received_9_TEXT", "Work_Sector_5_TEXT",
  "Gender_4_TEXT", "Race_Ethnicity_7_TEXT")

# Check that's all the columns
supportR::diff_check(old = names(svy_v06), new = free_cols)

# Remove identifying information from free text columns
svy_v07 <- svy_v06 %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::all_of(free_cols),
    .fns = ~ dplyr::case_when(
      stringr::str_detect(string = ., 
        pattern = "UCSB AI@Work Training") ~ gsub(pattern = "UCSB AI@Work Training", replacement = "[institutional AI certificate program]", x = .),
      stringr::str_detect(., "STEM Career Center") ~ gsub("STEM Career Center", "[career center]", x = .),
      stringr::str_detect(., "UCSB|UMass|Yale|CSU") ~ gsub("UCSB|UMass|Yale|CSU", "[institution]", x = .),
      stringr::str_detect(., "research from MEDS") ~ gsub("MEDS", "[graduate program]", x = .),
      stringr::str_detect(., "March 2026") ~ gsub("March 2026", "[date]", x = .),
      stringr::str_detect(., "CIT AI Gateway|CreateAI Builder") ~ gsub("CIT AI Gateway|CreateAI Builder", "[institutional AI access]", x = .),
      stringr::str_detect(., "governors state-university-wide program") ~ gsub("governors state-university-wide program", "[state-level program]", x = .),
      stringr::str_detect(., "CREDIT and Earth2Studio|Earth2Studio and CREDIT") ~ gsub("CREDIT", "[institutional AI tool]", x = .),
      # stringr::str_detect(., "") ~ gsub("", "[]", x = .),
      TRUE ~ .))) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::all_of(free_cols),
    .fns = ~ dplyr::case_when(
      stringr::str_detect(., "Environmental Markets Lab") ~ gsub("Environmental Markets Lab", "[research lab]", x = .),
      stringr::str_detect(., "AI-related recommendations for administration, considerations for faculty, our values statement, some results of a faculty survey we conducted last year, and a bibliography of helpful sources about AI.") ~ gsub("AI-related recommendations for administration, considerations for faculty, our values statement, some results of a faculty survey we conducted last year, and a bibliography of helpful sources about AI.", "[quote]", x = .),
      # stringr::str_detect(., "") ~ gsub("", "[]", x = .),
      TRUE ~ .)))

# Check structure
dplyr::glimpse(svy_v07)

## -------------------------------------------- ##
# Reorder Columns ----
## -------------------------------------------- ##

# Get columns into intuitive order
svy_v08 <- svy_v07 %>%
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
  dplyr::relocate(dplyr::starts_with("FirstGen"), .after = dplyr::everything()) %>% 
  dplyr::relocate(ResponseId, Progress:Q_RecaptchaScore, Country, .before = dplyr::everything())

# Check structure
dplyr::glimpse(svy_v08)

## -------------------------------------------- ##
# Export Outputs ----
## -------------------------------------------- ##

# Make a final data object
svy_v99 <- svy_v08

# Check its structure
dplyr::glimpse(svy_v99)

# Export locally
write.csv(x = svy_v99, row.names = FALSE, na = "",
  file = file.path("data", "01_tidied-responses.csv"))

# Export question lookup
write.csv(x = lookup, row.names = FALSE, na = "",
  file = file.path("data", "01_question-lookup-table.csv"))

## -------------------------------------------- ##
# Remove Text Columns ----
## -------------------------------------------- ##

# Ditch free text columns (too identifiable)
svy_notxt <- svy_v99 %>% 
  dplyr::select(-dplyr::ends_with("_TEXT", ignore.case = FALSE)) %>% 
  dplyr::select(-dplyr::starts_with("LOs_", ignore.case = FALSE)) %>% 
  dplyr::select(-ResponseId, -AI_tools, -GenAI_Resources)

# What is lost?
supportR::diff_check(old = names(svy_v08), new = names(svy_notxt))

# Check structure
names(svy_notxt)
dplyr::glimpse(svy_notxt)

# Export question lookup
write.csv(x = svy_notxt, row.names = FALSE, na = "",
  file = file.path("data", "01_tidied-responses_no-free-text.csv"))

# End ----
