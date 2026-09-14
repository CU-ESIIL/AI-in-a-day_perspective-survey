## -------------------------------------------- ##
# Separate 'Other' Free Text Responses
## -------------------------------------------- ##
# Purpose
## Split off the free text answers some people added for categorical questions where "Other (please specify)" was an option
## Doing this so that Lyon, Emery, and Quarderer can review these and look for places where we feel confident that the free text response _can_ be placed in one of the non-"Other" categories

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, magrittr, supportR)

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

# Read in survey question lookup table too
lkup <- read.csv(file.path("data", "01_question-lookup-table.csv"))
## dplyr::glimpse(lkup)

## -------------------------------------------- ##
# Strip Data of Interest ----
## -------------------------------------------- ##

# Identify all 'select one' categorical columns with a corresponding free text option
text_cols <- sort(c("Gen_Attitude_21_TEXT", "Policies_8_TEXT", "Prof_Role_8_TEXT",
"Work_Sector_5_TEXT", "Formal_Ed_4_TEXT", "Gender_4_TEXT"))
## Dropping "Field" & "Field_8_TEXT" until full set of options is known
(cat_cols <- gsub(pattern = "_[[:digit:]]{1,2}_TEXT", replacement = "", x = text_cols))

# Iterate across those questions (make a list to store outputs)
oth_list <- list()
for(k in seq_along(cat_cols)){
  # k <- 1

  # Progress message
  message("Extracting 'other' answers for ", k)

  # Identify all allowed entries for this question
  allow_cats <- setdiff(x = unique(oth_v01[[cat_cols[[k]]]]), y = c("", NA))

  # Pare down to just that question and its free text response
  ## And only rows where "Other" was the category and a free text response was given at all
  focal_q <- oth_v01 %>% 
    dplyr::select(ResponseId, dplyr::all_of(c(cat_cols[[k]], text_cols[[k]]))) %>% 
    dplyr::filter(stringr::str_detect(string = tolower(oth_v01[[cat_cols[[k]]]]), 
      pattern = "other"))
  focal_q %<>% 
    dplyr::filter(!is.na(focal_q[[text_cols[[k]]]]))

  # Add a column for allowed categories and for the question
  focal_q %<>% 
    dplyr::mutate(Accepted_Categories = paste(allow_cats, collapse = ";; "),
      Question = lkup$question_text[lkup$name_in_data == cat_cols[[k]]],
      Question_Code = cat_cols[[k]], 
      .after = ResponseId)

  # Check structure
  ## dplyr::glimpse(focal_q)

  # Rename the question-specific columns
  focal_done <- supportR::safe_rename(data = focal_q,
    bad_names = c(cat_cols[[k]], text_cols[[k]]),
    good_names = c("Chosen_Category", "Free_Text"))

  # Check structure
  ## dplyr::glimpse(focal_done)
  
  # Add it to the list
  oth_list[[cat_cols[[k]]]] <- focal_done }

## -------------------------------------------- ##
# Check Outputs ----
## -------------------------------------------- ##

# Unlist the list
oth_v02 <- purrr::list_rbind(x = oth_list)

# Check structure
dplyr::glimpse(oth_v02)
# tibble::view(oth_v02)

## -------------------------------------------- ##
# Export ----
## -------------------------------------------- ##

# Make a final data object
oth_v99 <- oth_v02

# Check structure
dplyr::glimpse(oth_v99)

# Export locally
write.csv(x = oth_v99, row.names = FALSE, na = '',
  file = file.path("data", "free-text-other-clarifications.csv"))

# End ----
