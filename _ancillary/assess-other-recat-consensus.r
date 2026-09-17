## -------------------------------------------- ##
# Check Consensus for 'Other' Free Text Response Re-Categorization
## -------------------------------------------- ##
# Purpose
## Nate Emery, Nate Quarderer, and Nick Lyon each (separately) tried to re-categorize answers of "other" for select-one questions
## Based on the free text responses
## This script combines the GoogleSheets used to do this and evaluates consensus
## Described here: https://github.com/CU-ESIIL/AI-in-a-day_perspective-survey/issues/5

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, googledrive)

# Get set up
source(file.path("-setup.r"))

# Clear environment/collect garbage
rm(list = ls()); gc()

## -------------------------------------------- ##
# Download Data (from Drive) ----
## -------------------------------------------- ##

# Find file in Drive
## The below assumes you've authenticated the `googledrive` R package
recat_drive <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/135USWMmp0KdtWwqQqqBHzuz_7Yro7ZZW")) %>% 
  dplyr::filter(stringr::str_detect(string = name, pattern = "re-categorization"))

# Download it!
if(nrow(recat_drive) == 1){
  googledrive::drive_download(file = recat_drive$id, overwrite = TRUE,
    path = file.path("data", "other-free-text-recat_select-one-qs.xlsx"))
}

## -------------------------------------------- ##
# Load Data ----
## -------------------------------------------- ##

# Check sheets in data
readxl::excel_sheets(file.path("data", "other-free-text-recat_select-one-qs.xlsx"))

# Load the relevant files
align_list <- list()
for(focal_name in c("Emery", "Quarderer", "Lyon")){
  # focal_name <- "Emery"

  # Load data
  single_df <- readxl::read_excel(file.path("data", "other-free-text-recat_select-one-qs.xlsx"),
    sheet = focal_name)
  
  # Rename one column more simply
  single_v2 <- supportR::safe_rename(data = single_df, 
    bad_names = paste0("Suggested_Recategorization_", focal_name),
    good_names = focal_name)
  
  # Check structure
  # dplyr::glimpse(single_v2)

  # Ditch redundant columns (given we will join the data) for all but one
  if(focal_name != "Emery"){
    single_v2 <- single_v2 %>% 
    dplyr::select(-Free_Text, -Question)
  }

  # Add it to the list
  align_list[[focal_name]] <- single_v2
} # Close loop

## -------------------------------------------- ##
# Assess Consensus ----
## -------------------------------------------- ##

# Combine the files
align_v01 <- dplyr::full_join(x = align_list[["Emery"]], y = align_list[["Quarderer"]],
    by = dplyr::join_by(ResponseId, Question_Code)) %>% 
  dplyr::full_join(x = ., y = align_list[["Lyon"]],
    by = dplyr::join_by(ResponseId, Question_Code))

# Check structure
dplyr::glimpse(align_v01)

# Do some post-processing
align_v02 <- align_v01 %>% 
  dplyr::mutate(consensus = sum(c(Emery == Quarderer, 
    Quarderer == Lyon, Emery == Lyon), na.rm = TRUE))

# Check structure
dplyr::glimpse(align_v02)

## -------------------------------------------- ##
# Export ----
## -------------------------------------------- ##

# Make a final object
align_v99 <- align_v02

# One last structure check
dplyr::glimpse(align_v99)

# Export
write.csv(x = align_v99, row.names = FALSE, na = '',
  file = file.path("data", "other-recats.csv"))

# End ----
