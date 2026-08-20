## -------------------------------------------- ##
# Broken Survey Data Download ----
## -------------------------------------------- ##
# Purpose
## Download the survey data where inter-column relationships have been randomized/broken
## That file is stored in our Drive

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Get set up
source(file.path("-setup.r"))

# Clear environment/collect garbage
rm(list = ls()); gc()

## -------------------------------------------- ##
# Download it ----
## -------------------------------------------- ##

# Identify key
fake_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/10aVF7_SL1Zb1IDny2yi7b5Lj3Mq-D1RX")) %>% 
  dplyr::filter(name %in% c("broken-row-survey-data.csv",
    "01_question-lookup-table.csv"))

# Did that work?
fake_drive

# Download the fake data and question lookup table
purrr::walk2(.x = fake_drive$id, .y = fake_drive$name,
  .f = ~ googledrive::drive_download(file = .x, overwrite = TRUE,
    path = file.path("data", .y)))

# Clear environment + collect garbage
rm(list = ls()); gc()

# End ----
