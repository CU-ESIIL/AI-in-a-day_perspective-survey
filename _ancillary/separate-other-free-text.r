## -------------------------------------------- ##
# Separate 'Other' Free Text Responses
## -------------------------------------------- ##
# Purpose
## Split off the free text answers some people added for categorical questions where "Other (please specify)" was an option
## Doing this so that Lyon, Emery, and Quarderer can review these and look for places where we feel confident that the free text response _can_ be placed in one of the non-"Other" categories

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

# Get set up
source(file.path("-setup.r"))

# Clear environment/collect garbage
rm(list = ls()); gc()

# Read in data
oth_v01 <- read.csv(file.path("data", "01_tidied-responses.csv")) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
    .fns = ~ ifelse(nchar(.) == 0, yes = NA, no = .)))

# Check structure
dplyr::glimpse(oth_v01)

## -------------------------------------------- ##
# TBD ----
## -------------------------------------------- ##




# End ----
