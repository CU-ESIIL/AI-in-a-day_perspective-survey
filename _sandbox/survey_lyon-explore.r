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
# Training Received vs Training Desired ----
## -------------------------------------------- ##

# Prep both 'training received' and 'training desired' dataframes
trainrec_df <- prep_select_all(df = svy_v01, q = "Training_Received") %>% 
  dplyr::rename_with(.fn = ~ paste0("had_", .), .cols = -value)
traindes_df <- prep_select_all(df = svy_v01, q = "Training_Desired") %>% 
  dplyr::rename_with(.fn = ~ paste0("want_", .), .cols = -value)

# Join the two questions' prepared dfs and calculate difference
train_v01 <- dplyr::full_join(x = trainrec_df, y = traindes_df, by = "value") %>% 
  dplyr::mutate(value = stringr::str_wrap(string = value, width = 40))

# Check structure
dplyr::glimpse(train_v01)

# Prepare this for graphing of want/had
train_v02 <- train_v01 %>% 
  dplyr::select(value, dplyr::ends_with("percent")) %>% 
  tidyr::pivot_longer(cols = -value, values_to = "percent_responses") %>% 
  dplyr::mutate(status = ifelse(name == "had_percent",
    yes = "received", no = "desires"))

# Check structure
dplyr::glimpse(train_v02)

# Make a graph
ggplot(train_v02, aes(x = percent_responses, y = value, 
    fill = status, color = 'x')) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_color_manual(values = "#000") +
  labs(x = "Percent Respondents (%)", y = "") +
  guides(color = "none") +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 18),
      legend.position = "inside",
      legend.position.inside = c(0.8, 0.95))
  
# Export locally
ggsave(file.path("graphs", "lyon-explore_training-mismatch.png"),
  height = 15, width = 15, units = "in")
      
## -------------------------------------------- ##
# Training Receive/Want Difference ----
## -------------------------------------------- ##

# Calculate difference between received and desired training
train_diff <- train_v01 %>% 
  dplyr::mutate(diff_percent = had_percent - want_percent) %>% 
  dplyr::arrange(dplyr::desc(diff_percent)) %>% 
  dplyr::mutate(value = factor(value, levels = unique(value)))

# Check structure
dplyr::glimpse(train_diff)

# Make a test graph
ggplot(data = train_diff, aes(x = diff_percent, y = value, 
  fill = value, color = 'x')) +
geom_bar(stat = "identity") +
xlim(c(-50, 50)) +
scale_color_manual(values = "#000") +
labs(x = "Percent Respondents (%)", y = "") +
guides(color = "none") +
supportR::theme_lyon(title_size = 20, text_size = 16) +
  theme(axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "none")

# Export locally
ggsave(file.path("graphs", "lyon-explore_training-difference.png"),
  height = 15, width = 15, units = "in")

# End ----
