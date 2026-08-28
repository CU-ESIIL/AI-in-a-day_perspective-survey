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
# TBD ----
## -------------------------------------------- ##

# End ----
