# -------------------------------------------------
# survey_elmendorf_gpt-oss-120b_explore.R
# -------------------------------------------------
# Exploratory Data Analysis (EDA) for the AI‑in‑a‑Day perspective survey.
# Focus: AI attitudes and the gap between training received vs desired,
# stratified by career stage (students vs non‑students).
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
    dplyr::across(.cols = dplyr::everything(), .fns = ~ ifelse(nchar(.) == 0, NA, .)),
    ResponseId = dplyr::row_number()
  )

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
# Helper to split a delimited column into long format.
# The survey uses comma‑separated values for multi‑select questions.
# This function:
#   1. Keeps the identifier column (`id_col`).
#   2. Removes rows where the target column is NA or blank.
#   3. Splits on commas, trimming surrounding whitespace.
#   4. Returns a tidy data.frame with one row per selection.
# Split a semicolon‑ or comma‑delimited column into long format while preserving
# all other columns in the original data frame (e.g., `is_student`). The
# original implementation selected only the identifier and the target column,
# which dropped useful context. By removing the explicit `select()` we let
# `tidyr::separate_rows()` automatically repeat all remaining columns for each
# split value.
split_multi <- function(df, id_col, col_name) {
  # Use tidy evaluation to refer to columns by name strings.
  # `!!sym()` converts a string to a symbol within the tidyverse pipeline.
  df %>%
    # Keep rows where the target column is not NA/blank
    filter(!is.na(!!sym(col_name)), !!sym(col_name) != "") %>%
    # Split the delimited values into separate rows, preserving all other cols
    separate_rows(!!sym(col_name), sep = ",") %>%
    # Trim whitespace around each split value
    mutate(!!sym(col_name) := str_trim(!!sym(col_name)))
}

# Split the multi‑select training columns using the unique respondent identifier
training_received_long <- split_multi(survey, "response_id", "training_received")
training_desired_long  <- split_multi(survey, "response_id", "training_desired")


## Define function to plot question response by demographic group
# ------------------------------------------------------------------
# Helper function to create a proportion bar plot of training received
# split by an arbitrary categorical variable (e.g., `is_student`,
# `career_stage`). The function returns a ggplot object; the caller can
# decide where to save it. Categories with fewer than 10 responses are dropped


plot_response_by_group <- function(df_long, group, question_col = "training_received", plot_title = NULL) {
  # Use tidy evaluation so `group` and `question_col` can be passed as unquoted column names.
  group_sym <- rlang::ensym(group)
  question_sym <- rlang::ensym(question_col)
  
  # Compute counts and percentages within each level of the grouping variable.
  base_counts <- df_long %>%
    dplyr::filter(!is.na(!!group_sym), !is.na(!!question_sym)) %>%
    dplyr::group_by(!!group_sym, !!question_sym) %>%
    dplyr::tally()
  
  # Collapse groups with fewer than 10 respondents (across all question categories)
  # into an "Other" bucket before computing percentages.
  cat_totals <- df_long %>%
    dplyr::filter(!is.na(!!group_sym)) %>%
    dplyr::select(response_id, !!group_sym) %>%
    dplyr::distinct() %>%
    dplyr::count(!!group_sym, name = "total_n")
  
  counts <- base_counts %>%
    dplyr::left_join(cat_totals, by = rlang::as_string(group_sym)) %>%
    dplyr::mutate(
      !!group_sym := ifelse(total_n < 10, "Other", as.character(!!group_sym))
    ) %>%
    # Re‑aggregate after recoding to "Other"
    dplyr::group_by(!!group_sym, !!question_sym) %>%
    dplyr::summarise(n = sum(n), .groups = "drop") %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::mutate(pct = n / sum(n) * 100)
  
  # Convert grouping variable to factor with readable "Yes"/"No" labels for binary values.
  # Ensure values are treated as numeric or logical for robust mapping.
  cat_vals_raw <- sort(unique(counts[[rlang::as_string(group_sym)]]))
  # Coerce character representations of 0/1 to numeric if needed
  cat_vals <- suppressWarnings(as.numeric(as.character(cat_vals_raw)))
  if (!any(is.na(cat_vals)) && all(sort(unique(cat_vals)) %in% c(0, 1))) {
    # Numeric 0/1 -> Yes/No (ensure factor levels are ordered Yes then No)
    counts <- counts %>%
      dplyr::mutate(!!group_sym := factor(!!group_sym, levels = c(1, 0), labels = c("Yes", "No")))
  } else if (all(cat_vals_raw %in% c(TRUE, FALSE))) {
    # Logical TRUE/FALSE -> Yes/No
    counts <- counts %>%
      dplyr::mutate(!!group_sym := factor(!!group_sym, levels = c(TRUE, FALSE), labels = c("Yes", "No")))
  }
  
  # Build the plot: x = question, fill/group = group.
  ggplot(counts, aes(x = !!question_sym, y = pct, fill = !!group_sym, group = !!group_sym)) +
    geom_col(position = "dodge") +
    labs(
      title = if (!is.null(plot_title)) plot_title else paste0(
        "Response (Percent) by ", rlang::as_label(group_sym)
      ),
      x = "Response Category",
      y = "Percent of Respondents",
      fill = rlang::as_label(group_sym)
    ) +
    theme_lyon() +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# Generate plots for training_received and training_desired by various demographic groups
# -------------------------------------------------
# Define the grouping variables for which we want separate plots
# -------------------------------------------------
group_vars <- c("is_student", "career_stage", "ds_freq", "work_sector")

# Loop over each grouping variable and create/save plots for both received and desired training
# Initialize lists to store plots
recv_plots <- list()
des_plots  <- list()

# Loop over each grouping variable and create plots for both received and desired training
for (g in group_vars) {
  # Plot training received
  p_recv <- plot_response_by_group(
    training_received_long,
    group = !!rlang::sym(g),
    question_col = "training_received",
    plot_title = paste0("Training Received (Percent) by ", tools::toTitleCase(g))
  )
  recv_plots[[g]] <- p_recv
  
  # Plot training desired
  p_des <- plot_response_by_group(
    training_desired_long,
    group = !!rlang::sym(g),
    question_col = "training_desired",
    plot_title = paste0("Training Desired (Percent) by ", tools::toTitleCase(g))
  )
  des_plots[[g]] <- p_des
}

# Combine received training plots into a multipanel figure
recv_grid <- cowplot::plot_grid(plotlist = recv_plots, ncol = 2)
ggsave(
  file.path(graph_path, "training_received_multipanel.png"),
  recv_grid,
  width = 40, height = 8, units = "in"
)

# Combine desired training plots into a multipanel figure
des_grid <- cowplot::plot_grid(plotlist = des_plots, ncol = 2)
ggsave(
  file.path(graph_path, "training_desired_multipanel.png"),
  des_grid,
  width = 40, height = 8, units = "in"
)

# End of script