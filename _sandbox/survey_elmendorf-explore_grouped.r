# -------------------------------------------------
# survey_elmendorf_gpt-oss-120b_explore.R
# -------------------------------------------------
# Exploratory Data Analysis (EDA) for the AI‑in‑a‑Day perspective survey.
# Focus: Multi‑select questions (training received, training desired, task interest, tech skill interest)
#  and their relationship with demographic-ish groups in terms of experience, attitude.
#
# This script lives in the sandbox folder – it is intended for ad‑hoc
# analysis and does **not** modify the FAIR pipeline.
# -------------------------------------------------

# -------------------------------------------------
# 1. Project setup
# -------------------------------------------------
source(file.path("-setup.r"))          # clears environment, creates folders
rm(list = ls()); gc()                  # start with a clean workspace

# ------------------------------------------------------------------
# Set CRAN mirror to the USA (Oregon) HTTPS mirror. This ensures that any
# package installations performed by `librarian::shelf` or other install
# calls use a reliable US-based repository without prompting the user.
# The official US (OR) mirror URL is "https://cran.r-project.org".
# ------------------------------------------------------------------
options(repos = c(CRAN = "https://cran.r-project.org"))

# -------------------------------------------------
# 2. Load required libraries
# -------------------------------------------------
## The project prefers librarian::shelf for reproducible loading.
## If a package is missing it will be installed automatically.
if (!requireNamespace("librarian", quietly = TRUE)) {
  install.packages("librarian")
}
librarian::shelf(
  data.table,
  tidyverse,
  supportR,          # contains the project's ggplot theme
  ggpubr,            # for statistical annotations
  patchwork,         # combine multiple plots
  scales,
  cowplot,
  janitor            # clean column names
)

# -------------------------------------------------
# 3. Source reusable functions (all tools/ scripts)
# -------------------------------------------------
purrr::walk(
  dir("tools", pattern = "\\.r$", full.names = TRUE),
  source
)

# -------------------------------------------------
# 4. Choose dataset (de‑identified vs full) and read it with the required logic
# -------------------------------------------------
real_data <- TRUE   # FALSE → use de‑identified `broken-row‑survey‑data.csv`
data_path <- if (real_data) {
  file.path("data", "01_tidied-responses.csv")
} else {
  file.path("data", "broken-row-survey-data.csv")
}

# Read CSV, convert empty strings to NA, and add a RespondentId column.
svy_v01 <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    dplyr::across(.cols = dplyr::everything(), .fns = ~ ifelse(nchar(.) == 0, NA, .))
)

if(!real_data){
  svy_v01$ResponseId <- c(1:nrow(svy_v01))
}


# Use the same variable name as before for downstream code.
survey_raw <- svy_v01

# Define where graphs will be saved based on the data toggle.
graph_path <- if (real_data) "graphs" else "graphs_fake"

# -------------------------------------------------
# 5. Clean column names & basic preparation
# -------------------------------------------------
survey <- survey_raw %>%
  clean_names()   # snake_case, removes spaces, etc.

# Create a binary flag for *student* vs *non‑student*.
# Heuristic:
#   * Undergraduate formal education → student
#   * Early‑career stage (career_stage__value <= 2) → student
survey <- survey %>%
  mutate(
    # Detect students based on the professional role description:
    #   * `prof_role` must be non‑missing (i.e., the respondent provided a role)
    #   * The string must contain the word "Student" (case‑insensitive)
    # This aligns with the requested definition: people who did not leave
    # `prof_role` blank and did include the word "Student" in that answer.
    is_student = case_when(
      # If the role field is missing or empty, keep NA so we know the data is unavailable
      is.na(.data[["prof_role"]]) | .data[["prof_role"]] == "" ~ NA_real_,
      # If the role contains the word "Student" (case‑insensitive), mark as TRUE
      str_detect(.data[["prof_role"]], regex("Student", ignore_case = TRUE)) ~ TRUE,
      # All other non‑missing roles are considered non‑students
      TRUE ~ FALSE
    )
  )

# ------------------------------------------------------------------
# 5. Re‑level the categorical AI attitude column to reflect the ordered
#    Likert scale defined by `gen_attitude_value` (the cleaned column name).
# ------------------------------------------------------------------
# Re‑level the categorical AI attitude column based on its numeric counterpart.
# We first create an ordered vector of unique attitude labels sorted by the
# associated numeric value, then use that vector as the factor levels.
# Create a vector of unique attitude labels sorted by their numeric value.
# We filter out missing or empty strings and then use `unique()` after sorting to
# ensure that duplicate label entries (which can arise from repeated text in the
# raw data) do not cause factor level duplication errors.
# Re‑level ordered factors in a single step using the numeric ordering columns
survey <- survey %>%
  dplyr::mutate(
    gen_attitude = factor(.data[["gen_attitude"]],
                          levels = unique(.data[["gen_attitude"]][order(.data[["gen_attitude_value"]])]),
                          ordered = TRUE),
    ds_freq = factor(.data[["ds_freq"]],
                     levels = unique(.data[["ds_freq"]][order(.data[["ds_freq_value"]])]),
                     ordered = TRUE),
    career_stage = factor(.data[["career_stage"]],
                          levels = unique(.data[["career_stage"]][order(.data[["career_stage_value"]])]),
                          ordered = TRUE)                  
  )
# Ensure ordinal columns are numeric (they should already be, but be safe).
survey <- survey %>%
  dplyr::mutate(across(ends_with("_value"), as.numeric))

# -------------------------------------------------
# 6. Prepare long‑format tables for multi‑select questions
# -------------------------------------------------
# Split the multi‑select training columns using the unique respondent identifier
  # join back to original data

tech_interest_long<- prep_select_all(survey, q = 'tech_skill_interest', summarize = FALSE) %>%
  rename(tech_skill_interest = value) %>%
  left_join(., (survey %>% select(-tech_skill_interest)), by = 'response_id')

task_interest_long<- prep_select_all(survey, q = 'task_interest', summarize = FALSE) %>%
  rename(task_interest = value) %>%
  left_join(., (survey %>% select(-task_interest)), by = 'response_id')

training_desired_long<- prep_select_all(survey, q = 'training_desired', summarize = FALSE) %>%
  rename(training_desired = value) %>%
  left_join(., (survey %>% select(-training_desired)), by = 'response_id')

training_received_long<- prep_select_all(survey, q = 'training_received', summarize = FALSE) %>%
  rename(training_received = value) %>%
  left_join(., (survey %>% select(-training_received)), by = 'response_id')


## Define function to plot question response by demographic group
# ------------------------------------------------------------------
# Helper function to create a proportion bar plot of training received
# split by an arbitrary categorical variable (e.g., `is_student`,
# `career_stage`). The function returns a ggplot object; the caller can
# decide where to save it. Categories with fewer than 10 responses are dropped


#' Plot proportion of training responses by a grouping variable
#' @param df_long Data frame in long format (output of split_multi)
#' @param group Unquoted column name to group by (e.g., is_student)
#' @param question_col Unquoted column name indicating the training column (default "training_received")
#' @param plot_title Optional title; defaults to auto-generated
#' @return A ggplot object showing percentage bars per response category, colored by group.
#' @details The function filters missing values, aggregates counts, collapses groups with fewer than 10 respondents into "Other", computes percentages, and maps binary groups (0/1 or TRUE/FALSE) to "Yes"/"No". It uses the project's `theme_lyon()`.
#' @examples
#' plot_response_by_group(training_received_long, group = is_student)
#' group must be a category that individuals can fall into ONLY one bucket of, no
#' multi response groups allowed here, though multiresponse questions fine
#' 
plot_response_by_group <- function(df_long, group, question_col = "task_interest", plot_title = NULL) {
  # Tidy evaluation: `group` / `question_col` can be passed unquoted (or as a literal string).
  group_sym    <- rlang::ensym(group)
  question_sym <- rlang::ensym(question_col)
  group_name   <- rlang::as_string(group_sym)   # <- use this string in every join
  
  # Respondents who answered both questions
  base <- df_long %>%
    dplyr::filter(!is.na(!!group_sym), !is.na(!!question_sym))
  
  # Counts per group x response. count() returns an ungrouped tibble (tally() would not).
  base_counts <- base %>%
    dplyr::count(!!group_sym, !!question_sym)
  
  # Respondents per group level
  cat_totals <- base %>%
    dplyr::distinct(response_id, !!group_sym) %>%
    dplyr::count(!!group_sym, name = "total_n")
  
  # Collapse levels with < 10 respondents into "Other", then re-aggregate
  counts <- base_counts %>%
    dplyr::left_join(cat_totals, by = group_name) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!group_sym := ifelse(total_n < 10, "Other", as.character(!!group_sym))) %>%
    dplyr::select(-total_n) %>%
    dplyr::group_by(!!group_sym, !!question_sym) %>%
    dplyr::summarise(n = sum(n), .groups = "drop")
  
  # Denominators after the "Other" recode
  cat_final <- base %>%
    dplyr::distinct(response_id, !!group_sym) %>%
    dplyr::left_join(cat_totals, by = group_name) %>%
    dplyr::mutate(!!group_sym := ifelse(total_n < 10, "Other", as.character(!!group_sym))) %>%
    dplyr::distinct(response_id, !!group_sym) %>%
    dplyr::count(!!group_sym, name = "total_n") %>%
    dplyr::filter(total_n >= 10)   # in case the pooled "Other" is still < 10
  
  counts <- counts %>%
    dplyr::right_join(cat_final, by = group_name) %>%
    dplyr::mutate(pct = (n / total_n) * 100)
  
  # Binary 0/1 or TRUE/FALSE groups get readable Yes/No labels.
  # Note: the recode above turned this column into character, so compare as character.
  cat_vals_raw <- sort(unique(counts[[group_name]]))
  cat_vals_num <- suppressWarnings(as.numeric(cat_vals_raw))
  
  if (!any(is.na(cat_vals_num)) && all(cat_vals_num %in% c(0, 1))) {
    counts <- counts %>%
      dplyr::mutate(!!group_sym := factor(!!group_sym, levels = c("1", "0"), labels = c("Yes", "No")))
  } else if (all(cat_vals_raw %in% c("TRUE", "FALSE"))) {
    counts <- counts %>%
      dplyr::mutate(!!group_sym := factor(!!group_sym, levels = c("TRUE", "FALSE"), labels = c("Yes", "No")))
  }
  
  ggplot2::ggplot(counts, ggplot2::aes(x = !!question_sym, y = pct,
                                       fill = !!group_sym, group = !!group_sym)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
      title = if (!is.null(plot_title)) plot_title else paste0(
        "Response (Percent) by ", rlang::as_label(group_sym)
      ),
      x = "Response Category",
      y = "Percent of Respondents",
      fill = rlang::as_label(group_sym)
    ) +
    theme_lyon() +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}



# Generate plots for training_received and training_desired by various demographic groups
# -------------------------------------------------
# Define the grouping variables for which we want separate plots
# -------------------------------------------------
group_vars <- c("is_student", "career_stage", "ds_freq", "work_sector",
                "gen_attitude")

# Loop over each grouping variable and create/save plots for both received and desired training
# Initialize lists to store plots
recv_plots <- list()
des_plots  <- list()
task_plots <-list()
tech_plots <-list()

# Loop over each grouping variable and create plots for both received and desired training
for (g in group_vars) {
  # Plot training received
  recv_plots[[g]] <- plot_response_by_group(
    training_received_long,
    group = !!rlang::sym(g),
    question_col = "training_received",
    plot_title = paste0("Training Received (Percent) by ", tools::toTitleCase(g))
  )
  
  # Plot training desired
  des_plots[[g]] <- plot_response_by_group(
    training_desired_long,
    group = !!rlang::sym(g),
    question_col = "training_desired",
    plot_title = paste0("Training Desired (Percent) by ", tools::toTitleCase(g))
  )
  
  task_plots[[g]] <-
    p_task <- plot_response_by_group(
      task_interest_long,
      group = !!rlang::sym(g),
      question_col = "task_interest",
      plot_title = paste0("Task interest (Percent) by ", tools::toTitleCase(g))
    )
  
  tech_plots[[g]] <-
    p_tech <- plot_response_by_group(
      tech_interest_long,
      group = !!rlang::sym(g),
      question_col = "tech_skill_interest",
      plot_title = paste0("Tech interest (Percent) by ", tools::toTitleCase(g))
    )
}

# Combine received training plots into a multipanel figure
recv_grid <- cowplot::plot_grid(plotlist = recv_plots, ncol = 1)
ggsave(
  file.path(graph_path, "training_received_multipanel.png"),
  recv_grid,
  width = 16, height = 40, units = "in"
)

# Combine desired training plots into a multipanel figure
des_grid <- cowplot::plot_grid(plotlist = des_plots, ncol = 1)
ggsave(
  file.path(graph_path, "training_desired_multipanel.png"),
  des_grid,
  width = 16, height = 40, units = "in"
)

task_grid <- cowplot::plot_grid(plotlist = task_plots, ncol = 1)
ggsave(
  file.path(graph_path, "task_desired_multipanel.png"),
  task_grid,
  width = 25, height = 40, units = "in"
)

tech_grid <- cowplot::plot_grid(plotlist = tech_plots, ncol = 1)
ggsave(
  file.path(graph_path, "tech_desired_multipanel.png"),
  tech_grid,
  width = 25, height = 40, units = "in"
)

# End of script