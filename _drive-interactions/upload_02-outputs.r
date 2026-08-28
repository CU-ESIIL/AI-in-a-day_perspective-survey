## -------------------------------------------- ##
# Script 02 (Single Q Viz) Uploads ----
## -------------------------------------------- ##
# Purpose
## Upload graphs produced by script #2 (`02_viz-single-qs.r`) to the group's Shared Drive

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
(local_outs <- dir(path = file.path("graphs"), pattern = "02_"))

# Identify relevant Drive folder
drive_dest <- googledrive::as_id("https://drive.google.com/drive/folders/1Vx6VV3ox3xXvuj6S0SAs-HEjOEbEJ7vU")

# Upload all outputs to that folder (overwriting where one already exists)
purrr::walk(.x = local_outs,
  .f = ~ googledrive::drive_upload(media = file.path("graphs", .x), overwrite = TRUE,
    path =  drive_dest))

# Clear environment + collect garbage
rm(list = ls()); gc()

# End ----
