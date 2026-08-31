## -------------------------------------------- ##
# Script 01 (Tidy Data) Uploads ----
## -------------------------------------------- ##
# Purpose
## Upload graphs produced by script #1 (`01_tidy.r`) to the group's Shared Drive

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Get set up
source(file.path("-setup.r"))

# Clear environment/collect garbage
rm(list = ls()); gc()

## -------------------------------------------- ##
# Upload Script Outputs ----
## -------------------------------------------- ##

# Identify local files
(local_outs <- dir(path = file.path("data"), pattern = "01_"))

# Identify relevant Drive folder
drive_dest <- googledrive::as_id("https://drive.google.com/drive/u/1/folders/1o5U3EJXnYZ-WB49GjY-WGXNU5_SaIH3B")

# Upload all outputs to that folder (overwriting where one already exists)
purrr::walk(.x = local_outs,
  .f = ~ googledrive::drive_upload(media = file.path("data", .x), overwrite = TRUE,
    path =  drive_dest))

# Clear environment + collect garbage
rm(list = ls()); gc()

# End ----
