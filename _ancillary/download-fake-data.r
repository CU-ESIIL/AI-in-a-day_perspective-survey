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
  dplyr::filter(name == "broken-row-survey-data.csv")

# Did that work?
fake_drive

# Download the data key
googledrive::drive_download(file = fake_drive$id, overwrite = TRUE,
  path = file.path("data", fake_drive$name))

# Clear environment + collect garbage
rm(list = ls()); gc()

# End ----
