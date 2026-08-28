## -------------------------------------------- ##
# Script 03 (Multi Q Viz) Uploads ----
## -------------------------------------------- ##
# Purpose
## Upload graphs produced by script #3 (`02_viz-multi-qs.r`) to the group's Shared Drive

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
(local_outs <- dir(path = file.path("graphs"), pattern = "03_"))

# Identify relevant Drive folder
drive_dest <- googledrive::as_id("https://drive.google.com/drive/folders/10yM6nSrL3ss3Ip3MDEI2dNlS2FB7uu3B")

# Upload all outputs to that folder (overwriting where one already exists)
purrr::walk(.x = local_outs,
  .f = ~ googledrive::drive_upload(media = file.path("graphs", .x), overwrite = TRUE,
    path =  drive_dest))

# Clear environment + collect garbage
rm(list = ls()); gc()

# End ----
