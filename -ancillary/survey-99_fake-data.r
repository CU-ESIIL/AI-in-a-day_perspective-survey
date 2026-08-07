## -------------------------------------------- ##
# Survey Data Breaking
## -------------------------------------------- ##
# Purpose
## Break row association with data to make it usable by non-IRB-approved people

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR, googledrive)

# Get set up
source(file.path("scripts", "survey", "-setup.r"))

# Clear environment/collect garbage
rm(list = ls()); gc()

# Load any custom functions
purrr::walk(.x = dir(path = file.path("scripts", "tools"), pattern = "*.r", full.names = TRUE),
  .f = ~ source(file = .x))

# Read in data
fake_v01 <- read.csv(file.path("data", "survey-01_tidied.csv")) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
    .fns = ~ ifelse(nchar(.) == 0, yes = NA, no = .)))

# Check structure
dplyr::glimpse(fake_v01)

## -------------------------------------------- ##
# Remove Text Columns ----
## -------------------------------------------- ##

# Ditch free text columns (too identifiable)
fake_v02 <- fake_v01 %>% 
  dplyr::select(-dplyr::ends_with("_TEXT", ignore.case = FALSE)) %>% 
  dplyr::select(-dplyr::starts_with("LOs_", ignore.case = FALSE)) %>% 
  dplyr::select(-ResponseId, -AI_tools)

# What is lost?
supportR::diff_check(old = names(fake_v01), new = names(fake_v02))

# Check structure
dplyr::glimpse(fake_v02)

## -------------------------------------------- ##
# Break Row Association ----
## -------------------------------------------- ##

# Make a new object
fake_v03 <- data.frame("fake_row" = 1:nrow(fake_v02))

# Loop across columns
for(col in names(fake_v02)){
  # col <- "FirstGen__value"

  # Progress message
  message("Breaking row relationship for '", col, "'")

  # Randomize the order
  col_rand <- sample(x = fake_v02[[col]], size = length(fake_v02[[col]]), replace = FALSE)

  # Attach it to the fake data
  fake_v03[[col]] <- col_rand }

# Check structure
dplyr::glimpse(fake_v03)

## -------------------------------------------- ##
# Export ----
## -------------------------------------------- ##

# Make a final object
fake_v99 <- fake_v03

# Check structure
dplyr::glimpse(fake_v99)

# Define output filename
fake_name <- "fake-survey-data_2026-08.csv"

# Export locally
write.csv(x = fake_v99, row.names = FALSE, na = "",
  file = file.path("data", fake_name))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", fake_name), overwrite = TRUE, 
  path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/10aVF7_SL1Zb1IDny2yi7b5Lj3Mq-D1RX"))

# End ----
