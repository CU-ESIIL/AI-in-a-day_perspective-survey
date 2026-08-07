## -------------------------------------------- ##
# Survey Data Preliminary Visualization
## -------------------------------------------- ##
# Purpose
## Do preliminary analysis/visualization of survey data

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
# AI Frequency Graph ----
## -------------------------------------------- ##

# Make custom color palette
ai_freq_cols <- c("Daily" = "#e9ecef", "Weekly" = "#adb5bd", 
  "Monthly" = "#6c757d", "Yearly" = "#343a40", "Never" = "#000")

# Actually make graph
graph_select_one(df = svy_v01, q = "AIUse_Freq") +
  geom_hline(yintercept = 75, linetype = 3, color = "#000") +
  geom_hline(yintercept = 50, linetype = 2, color = "#fff") +
  geom_hline(yintercept = 25, linetype = 3, color = "#fff") +
  scale_fill_manual(values = ai_freq_cols) +
  labs(x = "", y = "Percent Responses (%)",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "AIUse_Freq"], width = 70)) +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
  theme(axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_ai-frequency.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("ai_freq_cols"))

## -------------------------------------------- ##
# AI Use Reasons Graph ----
## -------------------------------------------- ##

# Prep the data for graphing
ai_reason_df <- prep_select_all(df = svy_v01, q = "AIUse_reasons") %>% 
  dplyr::mutate(value = stringr::str_wrap(string = value, width = 40)) %>% 
  dplyr::mutate(value = factor(value, levels = rev(unique(value))))

# Check structure
dplyr::glimpse(ai_reason_df)

# Make graph
ggplot(data = ai_reason_df, aes(x = percent, y = value, 
    fill = value, color = 'x')) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = "#000") +
  labs(x = "Percent Respondents (%)", y = "",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "AIUse_reasons"], width = 70)) +
  guides(color = "none") +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none")

# Export locally
ggsave(file.path("graphs", "survey-02_ai-reasons.png"),
  height = 15, width = 15, units = "in")
  
# Tidy environment
rm(list = c("ai_reason_df"))

## -------------------------------------------- ##
# Task Interest Graph ----
## -------------------------------------------- ##

# Prep the data for graphing
task_df <- prep_select_all(df = svy_v01, q = "Task_interest") %>% 
  dplyr::mutate(value = stringr::str_wrap(string = value, width = 40)) %>% 
  dplyr::mutate(value = factor(value, levels = rev(unique(value))))

# Check structure
dplyr::glimpse(task_df)

# Make graph
ggplot(data = task_df, aes(x = percent, y = value, 
    fill = value, color = 'x')) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = "#000") +
  labs(x = "Percent Respondents (%)", y = "",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Task_interest"], width = 70)) +
  guides(color = "none") +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none")

# Export locally
ggsave(file.path("graphs", "survey-02_task-interest.png"),
  height = 15, width = 15, units = "in")
  
# Tidy environment
rm(list = c("task_df"))

## -------------------------------------------- ##
# Tech Skill Interest Graph ----
## -------------------------------------------- ##

# Prep the data for graphing
skill_df <- prep_select_all(df = svy_v01, q = "TechSkill_Interest") %>% 
  dplyr::mutate(value = stringr::str_wrap(string = value, width = 40)) %>% 
  dplyr::mutate(value = factor(value, levels = rev(unique(value))))

# Check structure
dplyr::glimpse(skill_df)

# Make graph
ggplot(data = skill_df, aes(x = percent, y = value, 
    fill = value, color = 'x')) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = "#000") +
  labs(x = "Percent Respondents (%)", y = "",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "TechSkill_Interest"], width = 70)) +
  guides(color = "none") +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none")

# Export locally
ggsave(file.path("graphs", "survey-02_tech-skill-interest.png"),
  height = 15, width = 15, units = "in")
  
# Tidy environment
rm(list = c("skill_df"))

## -------------------------------------------- ##
# Training Received Graph ----
## -------------------------------------------- ##

# Prep the data for graphing
trainrec_df <- prep_select_all(df = svy_v01, q = "Training_Received") %>% 
  dplyr::mutate(value = stringr::str_wrap(string = value, width = 40)) %>% 
  dplyr::mutate(value = factor(value, levels = rev(unique(value))))

# Check structure
dplyr::glimpse(trainrec_df)

# Make graph
ggplot(data = trainrec_df, aes(x = percent, y = value, 
    fill = value, color = 'x')) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = "#000") +
  labs(x = "Percent Respondents (%)", y = "",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Training_Received"], width = 70)) +
  guides(color = "none") +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none")

# Export locally
ggsave(file.path("graphs", "survey-02_training-received.png"),
  height = 15, width = 15, units = "in")
  
# Tidy environment
rm(list = c("trainrec_df"))

## -------------------------------------------- ##
# Training Desired Graph ----
## -------------------------------------------- ##

# Prep the data for graphing
traindes_df <- prep_select_all(df = svy_v01, q = "Training_Desired") %>% 
  dplyr::mutate(value = stringr::str_wrap(string = value, width = 40)) %>% 
  dplyr::mutate(value = factor(value, levels = rev(unique(value))))

# Check structure
dplyr::glimpse(traindes_df)

# Make graph
ggplot(data = traindes_df, aes(x = percent, y = value, 
    fill = value, color = 'x')) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = "#000") +
  labs(x = "Percent Respondents (%)", y = "",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Training_Desired"], width = 70)) +
  guides(color = "none") +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none")

# Export locally
ggsave(file.path("graphs", "survey-02_training-desired.png"),
  height = 15, width = 15, units = "in")
  
# Tidy environment
rm(list = c("traindes_df"))

## -------------------------------------------- ##
# General Attitude Graph ----
## -------------------------------------------- ##

# Make custom color palette
attitude_cols <- c("Opposed to GenAI" = "#8f2d56", "Cautious" = "#d81159",
  "A mix of caution and enthusiasm" = "#ffbc42",
  "Enthusiastic" = "#0496ff", "Very enthusiastic" = "#006ba6",
  "Indifferent" = "#adb5bd",
  "Other" = "#343a40")

# Actually make graph
svy_v01 %>% 
  dplyr::mutate(Gen_Attitude = dplyr::case_when(
    Gen_Attitude == "Other (please specify)" ~ "Other",
    TRUE ~ Gen_Attitude)) %>%
  graph_select_one(df = ., q = "Gen_Attitude") +
    geom_hline(yintercept = 75, linetype = 3, color = "#000") +
    geom_hline(yintercept = 50, linetype = 2, color = "#000") +
    geom_hline(yintercept = 25, linetype = 3, color = "#000") +
    scale_fill_manual(values = attitude_cols) +
    labs(x = "", y = "Percent Responses (%)",
      title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Gen_Attitude"], width = 70)) +
    supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_general-attitude.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("attitude_cols"))

## -------------------------------------------- ##
# Promising Opportunities Graph ----
## -------------------------------------------- ##

# Prep the data for graphing
prom_df <- prep_select_all(df = svy_v01, q = "PromisingOpps") %>% 
  dplyr::mutate(value = stringr::str_wrap(string = value, width = 40)) %>% 
  dplyr::mutate(value = factor(value, levels = rev(unique(value))))

# Check structure
dplyr::glimpse(prom_df)

# Make graph
ggplot(data = prom_df, aes(x = percent, y = value, 
    fill = value, color = 'x')) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = "#000") +
  labs(x = "Percent Respondents (%)", y = "",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "PromisingOpps"], width = 70)) +
  guides(color = "none") +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none")

# Export locally
ggsave(file.path("graphs", "survey-02_opportunities.png"),
  height = 15, width = 15, units = "in")
  
# Tidy environment
rm(list = c("prom_df"))

## -------------------------------------------- ##
# Challenges Graph ----
## -------------------------------------------- ##

# Prep the data for graphing
chal_df <- prep_select_all(df = svy_v01, q = "Challenges") %>% 
  dplyr::mutate(value = stringr::str_wrap(string = value, width = 40)) %>% 
  dplyr::mutate(value = factor(value, levels = rev(unique(value))))

# Check structure
dplyr::glimpse(chal_df)

# Make graph
ggplot(data = chal_df, aes(x = percent, y = value, 
    fill = value, color = 'x')) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = "#000") +
  labs(x = "Percent Respondents (%)", y = "",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Challenges"], width = 70)) +
  guides(color = "none") +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none")

# Export locally
ggsave(file.path("graphs", "survey-02_challenges.png"),
  height = 15, width = 15, units = "in")
  
# Tidy environment
rm(list = c("chal_df"))

## -------------------------------------------- ##
# Policies Graph ----
## -------------------------------------------- ##

# Check contents
unique(svy_v01$Policies)

# Make custom color palette
policy_cols <- c(
  "Very restrictive/opposed" = "#8f2d56",
  "Somewhat restrictive/opposed" = "#d81159",
  "Neutral" = "#ffbc42",
  "Somewhat permissive/supportive" = "#0496ff", 
  "Very permissive/supportive" = "#006ba6",
  "No institutional policy" = "#000",
  "Other" = "#343a40")

# Actually make graph
svy_v01 %>% 
  dplyr::mutate(Policies = dplyr::case_when(
    Policies == "Other (please specify)" ~ "Other",
    Policies == "There are not any policies or guidelines at my institution" ~ "No institutional policy",
    TRUE ~ Policies)) %>% 
  graph_select_one(df = ., q = "Policies") +
    geom_hline(yintercept = 75, linetype = 3, color = "#000") +
    geom_hline(yintercept = 50, linetype = 2, color = "#000") +
    geom_hline(yintercept = 25, linetype = 3, color = "#000") +
    scale_fill_manual(values = policy_cols) +
    labs(x = "", y = "Percent Responses (%)",
      title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Policies"], width = 70)) +
    supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_policies.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("policy_cols"))
  
## -------------------------------------------- ##
# Career Stage Graph ----
## -------------------------------------------- ##

# Check contents
unique(svy_v01$Career_Stage)

# Make custom color palette
stage_cols <- c(
  "Prep (graduate student)" = "#99e2b4",
  "Early (1-9 years post-degree)" = "#67b99a",
  "Mid-Career (10-25 years)" = "#358f80",
  "Mature (26+ Years)" = "#036666")

# Actually make graph
svy_v01 %>% 
  dplyr::mutate(Career_Stage = dplyr::case_when(
    Career_Stage == "Preparation Stage (Currently pursuing a graduate degree)" ~ "Prep (graduate student)",
    Career_Stage == "Early Career Stage (1–9 years of experience post-degree)" ~ "Early (1-9 years post-degree)",
    Career_Stage == "Mid-Career Stage (10–25 years of experience)" ~ "Mid-Career (10-25 years)",
    Career_Stage == "Mature Career Stage (26+ years of experience)" ~ "Mature (26+ Years)")) %>% 
  graph_select_one(df = ., q = "Career_Stage") +
    geom_hline(yintercept = 75, linetype = 3, color = "#000") +
    geom_hline(yintercept = 50, linetype = 2, color = "#000") +
    geom_hline(yintercept = 25, linetype = 3, color = "#000") +
    scale_fill_manual(values = stage_cols) +
    labs(x = "", y = "Percent Responses (%)",
      title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Career_Stage"], width = 70)) +
    supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_career-stage.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("stage_cols"))
  
## -------------------------------------------- ##
# Professional Role Graph ----
## -------------------------------------------- ##

# Prep the data for graphing
role_df <- prep_select_all(df = svy_v01, q = "Prof_Role") %>% 
  dplyr::mutate(value = stringr::str_wrap(string = value, width = 40)) %>% 
  dplyr::mutate(value = factor(value, levels = rev(unique(value))))

# Check structure
dplyr::glimpse(role_df)

# Make graph
ggplot(data = role_df, aes(x = percent, y = value, 
    fill = value, color = 'x')) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = "#000") +
  labs(x = "Percent Respondents (%)", y = "",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Prof_Role"], width = 70)) +
  guides(color = "none") +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none")

# Export locally
ggsave(file.path("graphs", "survey-02_professional-role.png"),
  height = 15, width = 15, units = "in")
  
# Tidy environment
rm(list = c("role_df"))

## -------------------------------------------- ##
# Work Sector Graph ----
## -------------------------------------------- ##

# Check contents
unique(svy_v01$Work_Sector)

# Make custom color palette
sector_cols <- c(
  "Academia (Higher Education)" = "#adb5bd",
  "Non-Profit / NGO" = "#f79256",
  "Government (Federal, State, Local, Tribal)" = "#f8e16c",
  "Industry / Private Sector" = "#ffc2b4",
  "Other" = "#343a40")

# Actually make graph
svy_v01 %>% 
  dplyr::mutate(Work_Sector = dplyr::case_when(
    Work_Sector == "Other (please specify)" ~ "Other",
    TRUE ~ Work_Sector)) %>% 
  graph_select_one(df = ., q = "Work_Sector") +
    # geom_hline(yintercept = 75, linetype = 3, color = "#000") +
    # geom_hline(yintercept = 50, linetype = 2, color = "#000") +
    geom_hline(yintercept = 25, linetype = 3, color = "#000") +
    scale_fill_manual(values = sector_cols) +
    labs(x = "", y = "Percent Responses (%)",
      title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Work_Sector"], width = 70)) +
    supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_work-sector.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("sector_cols"))
  
## -------------------------------------------- ##
# Formal Education Graph ----
## -------------------------------------------- ##

# Check contents
unique(svy_v01$Formal_Ed)

# Make custom color palette
formal_ed_cols <- c(
  "Doctoral" = "#b5838d",
  "Master's" = "#e5989b",
  "4-Year" = "#ffb4a2",
  "Other" = "#343a40")

# Actually make graph
svy_v01 %>% 
  dplyr::mutate(Formal_Ed = dplyr::case_when(
    Formal_Ed == "Doctoral degree (Ph.D., Sc.D., etc.)" ~ "Doctoral",
    Formal_Ed == "Master’s degree (M.S., M.A., etc.)" ~ "Master's",
    Formal_Ed == "4-year undergraduate degree (B.S., B.A., etc.)" ~ "4-Year",
    Formal_Ed == "Other (please specify)" ~ "Other",
    TRUE ~ Formal_Ed)) %>% 
  graph_select_one(df = ., q = "Formal_Ed") +
    geom_hline(yintercept = 75, linetype = 3, color = "#000") +
    geom_hline(yintercept = 50, linetype = 2, color = "#000") +
    geom_hline(yintercept = 25, linetype = 3, color = "#000") +
    scale_fill_manual(values = formal_ed_cols) +
    labs(x = "", y = "Percent Responses (%)",
      title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Formal_Ed"], width = 70)) +
    supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_formal-education.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("formal_ed_cols"))

## -------------------------------------------- ##
# Field Graph ----
## -------------------------------------------- ##

unique(svy_v01$Field)
# <frequency!>

# !!! Note: holding off on drafting this graph's code until we get more responses !!!

## -------------------------------------------- ##
# Data Science Frequency Graph ----
## -------------------------------------------- ##

# Check contents
unique(svy_v01$DS_Freq)

# Make custom color palette
ds_freq_cols <- c("Daily" = "#e9ecef", "Weekly" = "#adb5bd", 
  "Monthly" = "#6c757d", "Yearly" = "#343a40", "Never" = "#000")

# Actually make graph
svy_v01 %>% 
  dplyr::mutate(DS_Freq = dplyr::case_when(
    DS_Freq == "I do not use data science in my research/role" ~ "Never",
    TRUE ~ DS_Freq)) %>% 
  graph_select_one(df = ., q = "DS_Freq") +
    geom_hline(yintercept = 75, linetype = 3, color = "#000") +
    geom_hline(yintercept = 50, linetype = 2, color = "#fff") +
    geom_hline(yintercept = 25, linetype = 3, color = "#fff") +
    scale_fill_manual(values = ds_freq_cols) +
    labs(x = "", y = "Percent Responses (%)",
      title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "DS_Freq"], width = 70)) +
    supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_data-science-frequency.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("ds_freq_cols"))
  
## -------------------------------------------- ##
# Gender Graph ----
## -------------------------------------------- ##

# Check contents
unique(svy_v01$Gender)

# Make custom color palette
gender_cols <- c(
  "Woman" = "#ff193b",
  "Non-binary" = "#ffc719",
  "Man" = "#9529ff",
  "Prefer to self-identify" = "#adb5bd",
  "Prefer not to answer" = "#000")

# Actually make graph
svy_v01 %>% 
  dplyr::mutate(Gender = dplyr::case_when(
    Gender == "Prefer to self-identify:" ~ "Prefer to self-identify",
    TRUE ~ Gender)) %>% 
  graph_select_one(df = ., q = "Gender") +
    geom_hline(yintercept = 75, linetype = 3, color = "#000") +
    geom_hline(yintercept = 50, linetype = 2, color = "#000") +
    geom_hline(yintercept = 25, linetype = 3, color = "#000") +
    scale_fill_manual(values = gender_cols) +
    labs(x = "", y = "Percent Responses (%)",
      title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Gender"], width = 70)) +
    supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_demographics_gender.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("gender_cols"))
  
## -------------------------------------------- ##
# LGBT Graph ----
## -------------------------------------------- ##

# Check contents
unique(svy_v01$LGBTQIA)

# Make custom color palette
lgbt_cols <- c(
  "No" = "#826aed",
  "Yes" = "#ffb7ff",
  "Prefer to self-identify" = "#adb5bd",
  "Prefer not to answer" = "#000")

# Actually make graph
svy_v01 %>% 
  dplyr::mutate(LGBTQIA = dplyr::case_when(
    LGBTQIA == "Prefer to self-identify:" ~ "Prefer to self-identify",
    TRUE ~ LGBTQIA)) %>% 
  graph_select_one(df = ., q = "LGBTQIA") +
    geom_hline(yintercept = 75, linetype = 3, color = "#000") +
    geom_hline(yintercept = 50, linetype = 2, color = "#000") +
    geom_hline(yintercept = 25, linetype = 3, color = "#000") +
    scale_fill_manual(values = lgbt_cols) +
    labs(x = "", y = "Percent Responses (%)",
      title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "LGBTQIA"], width = 70)) +
    supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_demographics_lgbtqia.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("lgbt_cols"))

## -------------------------------------------- ##
# Race/Ethnicity Graph ----
## -------------------------------------------- ##

# Prep the data for graphing
race_df <- prep_select_all(df = svy_v01, q = "Race_Ethnicity") %>% 
  dplyr::mutate(value = stringr::str_wrap(string = value, width = 40)) %>% 
  dplyr::mutate(value = factor(value, levels = rev(unique(value))))

# Check structure
dplyr::glimpse(race_df)

# Make graph
ggplot(data = race_df, aes(x = percent, y = value, 
    fill = value, color = 'x')) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = "#000") +
  labs(x = "Percent Respondents (%)", y = "",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Race_Ethnicity"], width = 70)) +
  guides(color = "none") +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none")

# Export locally
ggsave(file.path("graphs", "survey-02_demographics_race-ethnicity.png"),
  height = 15, width = 15, units = "in")
  
# Tidy environment
rm(list = c("race_df"))

## -------------------------------------------- ##
# Neurodiverse Graph ----
## -------------------------------------------- ##

# Check contents
unique(svy_v01$Neurodiverse)

# Make custom color palette
neuro_cols <- c(
  "No" = "#7b2cbf",
  "Yes" = "#ff8500",
  "Prefer not to answer" = "#000")

# Actually make graph
graph_select_one(df = svy_v01, q = "Neurodiverse") +
  geom_hline(yintercept = 75, linetype = 3, color = "#000") +
  geom_hline(yintercept = 50, linetype = 2, color = "#000") +
  geom_hline(yintercept = 25, linetype = 3, color = "#000") +
  scale_fill_manual(values = neuro_cols) +
  labs(x = "", y = "Percent Responses (%)",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Neurodiverse"], width = 70)) +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
  theme(axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_demographics_neurodiverse.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("neuro_cols"))
  
## -------------------------------------------- ##
# Caregiver Graph ----
## -------------------------------------------- ##

# Check contents
unique(svy_v01$Caregiver)

# Make custom color palette
care_cols <- c(
  "Not a caregiver" = "#000",
  "In past but not now" = "#adb5bd",
  "Primary caregiver" = "#e9ecef",
  "Contributor/Secondary caregiver" = "#6c757d",
  "Share equally with another" = "#343a40",
  "Prefer not to answer" = "#fff")

# Actually make graph
svy_v01 %>% 
  dplyr::mutate(Caregiver = dplyr::case_when(
    Caregiver == "Caregiver in the past, but not currently" ~ "In past but not now",
    Caregiver == "Share caregiving responsibilities equally with another person" ~ "Share equally with another",
    TRUE ~ Caregiver)) %>% 
  graph_select_one(df = ., q = "Caregiver") +
    geom_hline(yintercept = 75, linetype = 3, color = "#000") +
    geom_hline(yintercept = 50, linetype = 2, color = "#000") +
    geom_hline(yintercept = 25, linetype = 3, color = "#000") +
    scale_fill_manual(values = care_cols) +
    labs(x = "", y = "Percent Responses (%)",
      title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "Caregiver"], width = 70)) +
    supportR::theme_lyon(title_size = 20, text_size = 16) +
    theme(axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_demographics_caregiver.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("care_cols"))
  
## -------------------------------------------- ##
# FirstGen Graph ----
## -------------------------------------------- ##

# Check contents
unique(svy_v01$FirstGen)

# Make custom color palette
first_gen_cols <- c(
  "No" = "#04471c",
  "Yes" = "#16db65",
  "Prefer not to answer" = "#000")

# Actually make graph
graph_select_one(df = svy_v01, q = "FirstGen") +
  geom_hline(yintercept = 75, linetype = 3, color = "#000") +
  geom_hline(yintercept = 50, linetype = 2, color = "#000") +
  geom_hline(yintercept = 25, linetype = 3, color = "#000") +
  scale_fill_manual(values = first_gen_cols) +
  labs(x = "", y = "Percent Responses (%)",
    title = stringr::str_wrap(string = lkup$question_text[lkup$name_in_data == "FirstGen"], width = 70)) +
  supportR::theme_lyon(title_size = 20, text_size = 16) +
  theme(axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank())

# Export locally
ggsave(file.path("graphs", "survey-02_demographics_first-gen.png"),
  height = 7, width = 7, units = "in")

# Tidy environment
rm(list = c("first_gen_cols"))
  
# End ----
