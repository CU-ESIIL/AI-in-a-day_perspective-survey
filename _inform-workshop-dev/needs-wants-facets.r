## -------------------------------------------- ##
# Workshop Dev - Needs/Wants Facets
## -------------------------------------------- ##
# Purpose
## Visualize the task/skill interest from the survey faceted by variables likely to define learner profiles

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
# Needs / Wants Facets ----
## -------------------------------------------- ##

# Iterate across needs / wants
for(focal_q in c("Task_interest", "TechSkill_Interest")){
  # focal_q <- "Task_interest"

  # And grouping variables
  for(focal_gp in c("AIUse_FreqBins")){
    # focal_gp <- "AIUse_FreqBins"

    # Progress message
    message("Graphing ", focal_q, " faceted by ", focal_gp)

    # Prepare the data for the chosen question/group
    df <- prep_select_all(df = svy_v01, q = focal_q, grp = focal_gp) %>% 
      dplyr::mutate(value = stringr::str_wrap(string = value, 40))
    
    # Check structure
    # dplyr::glimpse(df)

    # Make both absolute and relative versions of the graph
    for(focal_perc in c("Relative", "Absolute")){
      # focal_perc <- "Relative"

      # Get column name in data from percent type
      col_name <- ifelse(focal_perc == "Relative",
        yes = "relative_percent", no = "percent")
      
      # Make the plot
      plot <- ggplot(df, aes(x = .data[[col_name]], y = value, 
          fill = value, color = 'x')) +
        geom_bar(stat = "identity") +
        labs(x = paste0(focal_perc, " Percent (%)")) +
        facet_grid(. ~ AIUse_FreqBins) +
        geom_vline(xintercept = 50, linetype = 2) +
        scale_color_manual(values = "black") +
        guides(color = "none") +
        supportR::theme_lyon() + 
        theme(legend.position = "none",
          strip.text = element_text(size = 16))
        
      # Export locally
      ggsave(plot, height = 14, width = 16, units = "in",
        filename = file.path("graphs", "workshop-dev", 
          paste0(tolower(focal_gp), "_",
            tolower(stringr::str_sub(string = focal_q, start = 1, end = 4)), "_",
            tolower(focal_perc), "-percent.png")))

    } # Close plot loop
  } # Close group loop
} # Close question loop

# Clear environment
rm(list = ls()); gc()
 
# End ----
