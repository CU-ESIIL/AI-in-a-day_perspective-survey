#' @title Closed-Response Ordination and Exploratory Learning Profiles
#'
#' @description
#' Prepare the closed-response portions of the AI in a Day perspective survey,
#' calculate question-weighted Gower dissimilarities among respondents, ordinate
#' those dissimilarities with corrected principal coordinates analysis (PCoA)
#' and non-metric multidimensional scaling (NMDS), and assess candidate learning
#' profiles with partitioning around medoids (PAM).
#'
#' @details
#' Survey responses are a mixture of ordered categories, multi-select questions,
#' demographics, and free text. Euclidean distance is not appropriate for this
#' mixture because arbitrary Qualtrics choice codes do not represent equal
#' numeric intervals. Gower dissimilarity instead compares each variable using
#' rules appropriate to its measurement type and combines the resulting scaled
#' dissimilarities.
#'
#' Each multi-select response is expanded to asymmetric binary indicators. The
#' indicators belonging to one question share a total weight of one, while each
#' single-response question also receives weight one. This prevents questions
#' with many checkbox options from dominating the analysis simply because they
#' produce more columns. Joint absence of a checkbox option is not treated as
#' evidence of similarity.
#'
#' Category columns are retained as the analytical values. Their companion
#' `__value` columns are used only to establish order for variables explicitly
#' declared ordinal. In real-data mode, each active ordinal label/value pair
#' must have a one-to-one mapping. Qualtrics codes for nominal variables are not
#' treated as measurements.
#'
#' Every substantive response column represented in the de-identified fixture
#' contributes to respondent dissimilarity. Identifiers and duplicate
#' `__value` companions are excluded. Career and demographic questions therefore
#' define distances alongside AI use, attitudes, opportunities, challenges, and
#' learning interests. This broad analysis should eventually be compared with a
#' learning-focused sensitivity analysis because demographic differences can
#' otherwise influence the resulting profiles.
#'
#' PCoA is the primary display because it is deterministic. `stats::cmdscale()`
#' adds a constant to make the Gower dissimilarities Euclidean before extracting
#' coordinates. NMDS is retained as a sensitivity analysis; its stress and
#' Shepard plot must be inspected before interpreting a two-dimensional display.
#' Because PCoA decomposes a respondent dissimilarity matrix rather than a
#' variable covariance matrix, it does not produce PCA-style variable loadings.
#' This workflow instead fits each active feature onto both ordinations with
#' `vegan::envfit()`. The resulting arrows are post hoc correlations: direction
#' shows the feature gradient, while arrow length and R-squared show association
#' with the displayed configuration. They do not represent causal effects.
#'
#' PAM is fitted to the full Gower dissimilarity, never to the two plotted axes.
#' Candidate profile counts are compared using silhouette width, minimum profile
#' size, and agreement with repeated 80-percent respondent subsamples.
#'
#' @section Development and real-data modes:
#' The de-identified `broken-row-survey-data.csv` has independently shuffled
#' columns. Fake mode is therefore only a structural test of parsing, missing
#' data handling, ordination, and output generation. Coordinates, associations,
#' profiles, and pairing-audit failures from fake mode have no substantive
#' meaning.
#'
#' Real mode uses intact respondent rows from `01_tidied-responses.csv`. Only
#' outputs from real mode may be interpreted. The active schema is defined by
#' all substantive fields available in the fake structural fixture. Additional
#' free-response fields found only in real data remain excluded until a codebook
#' and privacy procedure are established. `GenAI_Resources`, which is present in
#' the fixture, is included as an exact-response nominal category; this preserves
#' the column without claiming to have semantically coded its contents.
#' A real-data file may omit `ResponseId` for de-identification. In that case the
#' workflow creates a sequential in-memory `analysis_row_id` before filtering;
#' it identifies rows only within that run and is not written to disk.
#'
#' @section Configuration:
#' Configuration uses environment variables so the script remains unchanged
#' across machines and protected data locations:
#'
#' * `AI_DAY_REAL_DATA`: `true` for real data; otherwise fake mode (default).
#' * `AI_DAY_SURVEY_DATA`: optional path overriding the mode-specific default.
#' * `AI_DAY_QUESTION_LOOKUP`: optional path overriding
#'   `data/01_question-lookup-table.csv`. When the lookup exists, its schema,
#'   uniqueness, and coverage of configured questions are validated.
#' * `AI_DAY_EXCLUDE_OPPOSED`: `true` removes respondents whose
#'   `Gen_Attitude` response is exactly `Opposed to GenAI`; otherwise all
#'   attitudes are retained (default). Missing attitudes are retained.
#' * `AI_DAY_ANALYSIS_SCOPE`: `all-substantive` includes all 23 substantive
#'   fixture questions (default); `ai-focused` includes only 11 questions
#'   directly concerning GenAI use, attitudes, policies/resources,
#'   opportunities/challenges, training, and learning interests.
#' * `AI_DAY_MIN_BLOCKS`: minimum answered active questions. The default is 15
#'   for `all-substantive` and 7 for `ai-focused`.
#' * `AI_DAY_MIN_OPTION_N`: selections required to retain a checkbox option
#'   separately; rarer options are pooled within question (default: 5).
#' * `AI_DAY_STABILITY_RUNS`: 80-percent subsamples per candidate profile count
#'   (default: 50; use at least 100 for final reporting).
#' * `AI_DAY_PROFILE_K`: optional profile count. If omitted, the eligible count
#'   with the largest silhouette width is used for exploratory summaries.
#' * `AI_DAY_VECTOR_LABELS`: maximum fitted feature arrows labeled on each
#'   biplot (default: 8). Only the strongest feature is considered per question
#'   before
#'   selecting the strongest arrows, which limits checkbox-question dominance.
#' * `AI_DAY_SEED`: random seed for NMDS and profile stability (default: 20260831).
#'
#' @section Outputs:
#' Intermediate and diagnostic tables are retained as named R objects rather
#' than written to disk. These include `pairing_audit`, `feature_dictionary`,
#' `question_weight_audit`, `question_completion`, `fit_statistics`,
#' `pcoa_eigenvalues`,
#' `active_feature_vectors`, `question_associations`, `profile_diagnostics`,
#' `ordination_scores`, and `profile_feature_summary`.
#'
#' Only figures with interpretive labels are written beneath `graphs_fake/` or
#' `graphs/`: filtered biplots, corrected PCoA and NMDS diagnostics, a
#' respondent-comparability plot, question-level associations, profile
#' diagnostics, and a profile response heatmap.
#' Filenames end in `_all-attitudes.png` or `_exclude-opposed.png`, and every
#' figure contains the same scenario label, so both configurations can coexist.
#' Unadorned ordination point clouds are not saved because their axes cannot be
#' interpreted without fitted feature labels or another explicit diagnostic.
#'
#' @section Reading interpretation outputs:
#' Biplot labels are intentionally filtered, but `active_feature_vectors`
#' retains all fitted features in memory. Long arrows with high R-squared
#' identify responses aligned
#' with the displayed axes; arrows pointing in opposite directions describe
#' contrasting response gradients. The question-association figure aggregates
#' feature R-squared values using the same within-question weights as Gower
#' distance. The profile heatmap shows each profile's deviation from the overall
#' response mean after putting binary prevalence and ordinal means on a common
#' zero-to-one scale. Cell text remains on the original scale: percentages for
#' checkbox options and mean rank for ordinal questions.
#'
#' @section Interpretation safeguards:
#' A strong-looking two-dimensional picture does not by itself establish
#' profiles. Review missingness, corrected PCoA eigenvalues, NMDS stress, the
#' Shepard plot, silhouette widths, subsampling stability, and profile sizes.
#' Inspect `pairing_audit` before assigning direction to ordinal axes. Treat
#' automatically selected PAM solutions as candidates for review,
#' not as validated learning-profile classes.
#'
#' @return
#' The script writes labeled PNG outputs and leaves all analytical tables and
#' principal analysis objects in the interactive R session when sourced.
#'
#' @examples
#' \dontrun{
#' # From the repository root, after downloading the fake fixture into data/:
#' Sys.setenv(AI_DAY_REAL_DATA = "false", AI_DAY_STABILITY_RUNS = "5")
#' source(file.path("_sandbox", "survey_earl-explore_ordination.r"))
#'
#' # Run on an intact, access-controlled real-data file:
#' Sys.setenv(
#'   AI_DAY_REAL_DATA = "true",
#'   AI_DAY_SURVEY_DATA = file.path("data", "01_tidied-responses.csv"),
#'   AI_DAY_STABILITY_RUNS = "100"
#' )
#' source(file.path("_sandbox", "survey_earl-explore_ordination.r"))
#'
#' # Equivalent non-interactive shell execution:
#' # AI_DAY_REAL_DATA=true Rscript _sandbox/survey_earl-explore_ordination.r
#' }

## -------------------------------------------- ##
# Closed-Response Ordination
## -------------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, cluster, vegan, supportR, ggrepel)

# Get set up
source(file.path("-setup.r"))

# Clear environment/collect garbage
rm(list = ls()); gc()

# Load any custom functions
purrr::walk(
  .x = dir(path = "tools", pattern = "*.r", full.names = TRUE),
  .f = ~ source(file = .x)
)

## -------------------------------------------- ##
# Configuration ----
## -------------------------------------------- ##

# User decision guide
#
# AI_DAY_REAL_DATA
#   false (default): use the shuffled structural fixture and write graphs_fake/.
#   true: use intact respondent data and write graphs/. Only this mode supports
#     substantive interpretation.
#
# AI_DAY_ANALYSIS_SCOPE
#   all-substantive (default): use all 23 closed-response questions, including
#     career and demographic characteristics.
#   ai-focused: use 11 GenAI questions and exclude career and demographics from
#     distances and profiles. The separate AI-focused script selects this mode.
#
# AI_DAY_EXCLUDE_OPPOSED
#   false (default): retain respondents with every GenAI attitude.
#   true: remove only respondents coded exactly "Opposed to GenAI". Respondents
#     with a missing attitude remain eligible.
#
# AI_DAY_SURVEY_DATA / AI_DAY_QUESTION_LOOKUP
#   unset (default): use mode-specific data and data/01_question-lookup-table.csv.
#   path: override either default with an approved local file.
#
# AI_DAY_MIN_BLOCKS
#   unset (recommended): require 15/23 answered questions for all-substantive or
#     7/11 for AI-focused.
#   positive integer: override the completion threshold. Lower values retain
#     more respondents but produce distances based on less shared information.
#
# AI_DAY_MIN_OPTION_N
#   5 (default): keep checkbox responses selected by at least five respondents;
#     pool rarer choices within their source question. For nominal questions,
#     rare categories remain in Gower but are omitted from plotted labels.
#
# AI_DAY_STABILITY_RUNS
#   50 (default): repeated 80-percent subsamples for PAM stability.
#   2-5: fast development check. 100 or more: recommended for final analysis.
#
# AI_DAY_PROFILE_K
#   unset (default): choose the eligible k = 2,...,8 with best silhouette width.
#   integer 2,...,8: force that profile count for a sensitivity or planned run.
#
# AI_DAY_VECTOR_LABELS
#   8 (default): label at most eight fitted vectors, one per source question.
#   positive integer: show fewer for clarity or more for detailed inspection.
#
# AI_DAY_SEED
#   20260831 (default): reproducible NMDS starts, permutations, and subsamples.
#   non-negative integer: use another reproducible random sequence.

as_flag <- function(x) {
  tolower(trimws(x)) %in% c("1", "true", "t", "yes", "y")
}

as_integer_setting <- function(name, default, minimum = 1) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, unset = default)))
  if (is.na(value) || value < minimum) {
    stop("'", name, "' must be an integer greater than or equal to ", minimum)
  }
  value
}

# Decision: fake fixture versus intact real respondent data.
real_data <- as_flag(Sys.getenv("AI_DAY_REAL_DATA", unset = "false"))

# Decision: retain all attitudes versus exclude exactly "Opposed to GenAI".
exclude_opposed <- as_flag(Sys.getenv(
  "AI_DAY_EXCLUDE_OPPOSED", unset = "false"
))

# Decision: broad all-substantive profile versus GenAI-only profile.
analysis_scope <- Sys.getenv(
  "AI_DAY_ANALYSIS_SCOPE", unset = "all-substantive"
)
valid_analysis_scopes <- c("all-substantive", "ai-focused")
if (!analysis_scope %in% valid_analysis_scopes) {
  stop(
    "'AI_DAY_ANALYSIS_SCOPE' must be one of: ",
    paste(valid_analysis_scopes, collapse = ", ")
  )
}

# Input paths may be overridden without editing this script.
default_data_path <- if (real_data) {
  file.path("data", "01_tidied-responses_no-free-text.csv")
} else {
  file.path("data", "broken-row-survey-data.csv")
}

data_path <- Sys.getenv("AI_DAY_SURVEY_DATA", unset = default_data_path)
lookup_path <- Sys.getenv(
  "AI_DAY_QUESTION_LOOKUP",
  unset = file.path("data", "01_question-lookup-table.csv")
)
# Completion defaults scale with the number of active question blocks.
default_minimum_active_blocks <- ifelse(
  analysis_scope == "ai-focused", 7, 15
)
minimum_active_blocks <- as_integer_setting(
  "AI_DAY_MIN_BLOCKS", default_minimum_active_blocks
)
# Rare checkbox options are pooled; rare nominal levels remain analytical but
# are not promoted to unstable interpretation labels.
minimum_option_n <- as_integer_setting("AI_DAY_MIN_OPTION_N", 5)

# Stability repetitions trade execution time for precision.
stability_runs <- as_integer_setting("AI_DAY_STABILITY_RUNS", 50)

# The seed makes stochastic ordination and stability results reproducible.
analysis_seed <- as_integer_setting("AI_DAY_SEED", 20260831, minimum = 0)

# Limit visual vectors without discarding the complete in-memory fit table.
vector_label_n <- as_integer_setting("AI_DAY_VECTOR_LABELS", 8)

# Blank selects k diagnostically; an integer forces a candidate profile count.
profile_k_setting <- Sys.getenv("AI_DAY_PROFILE_K", unset = "")

if (!file.exists(data_path)) {
  stop(
    "Survey data not found at '", data_path, "'. Download the fake fixture ",
    "with '_drive-interactions/download_fake-data.r' or set ",
    "'AI_DAY_SURVEY_DATA' to an approved local file."
  )
}

# Output routing keeps fake and real figures physically separate.
graph_path <- ifelse(real_data, "graphs", "graphs_fake")
dir.create(graph_path, recursive = TRUE, showWarnings = FALSE)

attitude_scenario <- if (exclude_opposed) {
  "Excluded GenAI attitude: Opposed to GenAI"
} else {
  "Included all GenAI attitudes"
}
scope_label <- ifelse(
  analysis_scope == "ai-focused",
  "AI-focused questions only",
  "All substantive questions"
)
analysis_scenario <- paste(scope_label, attitude_scenario, sep = "; ")
scenario_suffix <- ifelse(
  exclude_opposed, "exclude-opposed", "all-attitudes"
)
ordination_plot_path <- function(stem) {
  file.path(
    graph_path,
    paste0(
      "ordination_", analysis_scope, "_", stem, "_",
      scenario_suffix, ".png"
    )
  )
}

all_ordinal_questions <- c(
  "AIUse_Freq", "Gen_Attitude", "Policies", "Career_Stage", "Formal_Ed",
  "DS_Freq"
)

all_multiselect_questions <- c(
  "AIUse_reasons", "Task_interest", "TechSkill_Interest",
  "Training_Received", "Training_Desired", "PromisingOpps", "Challenges",
  "Prof_Role", "Race_Ethnicity"
)

all_nominal_questions <- c(
  "Work_Sector", "Field", "GenAI_Resources", "Gender", "LGBTQIA",
  "Neurodiverse", "Caregiver", "FirstGen"
)

# Scope decision: classify only questions that actively define Gower distance.
# Data-science frequency is intentionally omitted from AI-focused mode because
# it describes broader professional practice rather than GenAI specifically.
if (analysis_scope == "ai-focused") {
  active_ordinal_questions <- c(
    "AIUse_Freq", "Gen_Attitude", "Policies"
  )
  active_multiselect_questions <- c(
    "AIUse_reasons", "Task_interest", "TechSkill_Interest",
    "Training_Received", "Training_Desired", "PromisingOpps", "Challenges"
  )
  active_nominal_questions <- "GenAI_Resources"
} else {
  active_ordinal_questions <- all_ordinal_questions
  active_multiselect_questions <- all_multiselect_questions
  active_nominal_questions <- all_nominal_questions
}

all_configured_questions <- tibble::tibble(
  question = c(
    all_ordinal_questions, all_multiselect_questions,
    all_nominal_questions
  ),
  question_label = c(
    "GenAI use frequency", "General attitude toward GenAI",
    "Institutional GenAI policies", "Career stage", "Formal education",
    "Data science use frequency",
    "Factors affecting GenAI use", "Research task learning interests",
    "Technical skill learning interests", "Training already received",
    "Training desired", "Promising GenAI opportunities",
    "Challenges using GenAI", "Professional role", "Race or ethnicity",
    "Work sector", "Primary field or discipline",
    "Institutional GenAI resources", "Gender",
    "LGBTQIA+ identity", "Neurodivergence or accessibility",
    "Caregiving responsibilities", "First-generation college status"
  ),
  analytical_role = c(
    rep("active ordinal", length(all_ordinal_questions)),
    rep("active multi-select", length(all_multiselect_questions)),
    rep("active nominal", length(all_nominal_questions))
  )
)

configured_questions <- all_configured_questions %>%
  dplyr::filter(question %in% c(
    active_ordinal_questions, active_multiselect_questions,
    active_nominal_questions
  ))

if (file.exists(lookup_path)) {
  question_lookup <- read.csv(
    lookup_path, check.names = FALSE, stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  required_lookup_columns <- c("name_in_data", "question_text")

  if (!all(required_lookup_columns %in% names(question_lookup))) {
    stop(
      "Question lookup must contain: ",
      paste(required_lookup_columns, collapse = ", ")
    )
  }
  if (anyDuplicated(question_lookup$name_in_data) > 0) {
    stop("Question lookup contains duplicate 'name_in_data' values")
  }

  question_metadata <- question_lookup %>%
    dplyr::transmute(
      question = trimws(name_in_data),
      question_text = trimws(question_text)
    ) %>%
    dplyr::filter(!is.na(question), !is.na(question_text))

  missing_metadata <- setdiff(
    configured_questions$question, question_metadata$question
  )
  if (length(missing_metadata) > 0) {
    stop(
      "Question lookup lacks configured variables: ",
      paste(missing_metadata, collapse = ", ")
    )
  }
} else {
  message(
    "Question lookup not found at '", lookup_path,
    "'; outputs will use variable names as fallback labels."
  )
  question_metadata <- configured_questions %>%
    dplyr::transmute(question, question_text = question)
}

question_metadata <- configured_questions %>%
  dplyr::select(question, question_label) %>%
  dplyr::left_join(question_metadata, by = "question")

question_metadata_audit <- configured_questions %>%
  dplyr::select(question, question_label, analytical_role) %>%
  dplyr::left_join(
    question_metadata %>% dplyr::select(question, question_text),
    by = "question"
  ) %>%
  dplyr::mutate(
    metadata_source = ifelse(
      file.exists(lookup_path), normalizePath(lookup_path), "variable-name fallback"
    )
  )

fallback_ordinal_levels <- list(
  AIUse_Freq = c("Never", "Yearly", "Monthly", "Weekly", "Daily"),
  DS_Freq = c(
    "I do not use data science in my research/role", "Never", "Yearly",
    "Monthly", "Weekly", "Daily"
  ),
  Gen_Attitude = c(
    "Opposed to GenAI", "Very cautious", "Cautious", "Neutral",
    "A mix of caution and enthusiasm", "Enthusiastic", "Very enthusiastic"
  ),
  Policies = c(
    "Very restrictive/prohibitive", "Somewhat restrictive/prohibitive",
    "Neutral", "Somewhat permissive/supportive", "Very permissive/supportive"
  ),
  Career_Stage = c(
    "Student / In training", "Early Career Stage (1–9 years of experience post-degree)",
    "Mid-Career Stage (10–25 years of experience)",
    "Mature Career Stage (26+ years of experience)"
  ),
  Formal_Ed = c(
    "High school diploma or equivalent",
    "2-year college degree (A.A., A.S., etc.)",
    "4-year undergraduate degree (B.S., B.A., etc.)",
    "Master's degree (M.S., M.A., etc.)",
    "Doctoral degree (Ph.D., Sc.D., etc.)"
  )
)

## -------------------------------------------- ##
# Helper Functions ----
## -------------------------------------------- ##

split_multiselect <- function(x) {
  if (is.na(x) || trimws(x) == "") {
    return(character())
  }

  protected <- gsub(", ", "___COMMA___", x, fixed = TRUE)
  choices <- strsplit(protected, split = ",", fixed = TRUE)[[1]]
  trimws(gsub("___COMMA___", ", ", choices, fixed = TRUE))
}

encode_multiselect <- function(x, question, minimum_n = 5) {
  responses <- lapply(x, split_multiselect)
  option_counts <- sort(table(unlist(responses)), decreasing = TRUE)
  retained_options <- names(option_counts[option_counts >= minimum_n])
  rare_options <- setdiff(names(option_counts), retained_options)

  if (length(rare_options) > 0) {
    retained_options <- c(retained_options, "Other pooled responses")
  }

  if (length(retained_options) == 0) {
    stop("No response options could be encoded for '", question, "'")
  }

  encoded <- matrix(
    0L, nrow = length(responses), ncol = length(retained_options),
    dimnames = list(NULL, retained_options)
  )

  missing_response <- lengths(responses) == 0
  encoded[missing_response, ] <- NA_integer_

  for (row_index in which(!missing_response)) {
    selected <- responses[[row_index]]
    retained_selected <- intersect(selected, retained_options)
    encoded[row_index, retained_selected] <- 1L

    if (length(intersect(selected, rare_options)) > 0) {
      encoded[row_index, "Other pooled responses"] <- 1L
    }
  }

  feature_names <- make.unique(paste(question, colnames(encoded), sep = "__"))
  colnames(encoded) <- feature_names

  dictionary <- tibble::tibble(
    feature = feature_names,
    question = question,
    response_option = retained_options,
    measurement = "asymmetric binary",
    feature_weight = 1 / length(feature_names),
    selected_n = colSums(encoded, na.rm = TRUE),
    selected_percent = round(
      100 * colSums(encoded, na.rm = TRUE) / sum(!missing_response), 1
    )
  )

  list(data = as.data.frame(encoded), dictionary = dictionary)
}

encode_nominal <- function(x, question) {
  values <- factor(x)
  response_options <- levels(values)
  encoded <- matrix(
    0L, nrow = length(values), ncol = length(response_options),
    dimnames = list(NULL, response_options)
  )
  missing_response <- is.na(values)
  encoded[missing_response, ] <- NA_integer_

  for (row_index in which(!missing_response)) {
    encoded[row_index, as.character(values[row_index])] <- 1L
  }

  feature_names <- make.unique(paste(question, colnames(encoded), sep = "__"))
  colnames(encoded) <- feature_names

  distance_dictionary <- tibble::tibble(
    feature = question,
    question = question,
    response_option = NA_character_,
    measurement = "nominal factor",
    feature_weight = 1,
    selected_n = sum(!missing_response),
    selected_percent = round(100 * mean(!missing_response), 1)
  )
  interpretation_dictionary <- tibble::tibble(
    feature = feature_names,
    question = question,
    response_option = response_options,
    measurement = "nominal category indicator",
    feature_weight = 1 / length(feature_names),
    selected_n = colSums(encoded, na.rm = TRUE),
    selected_percent = round(
      100 * colSums(encoded, na.rm = TRUE) / sum(!missing_response), 1
    )
  )

  list(
    data = stats::setNames(data.frame(values), question),
    interpretation_data = as.data.frame(encoded),
    distance_dictionary = distance_dictionary,
    interpretation_dictionary = interpretation_dictionary
  )
}

audit_value_pair <- function(df, question) {
  value_question <- paste0(question, "__value")
  paired <- df %>%
    dplyr::select(
      label = dplyr::all_of(question),
      value = dplyr::all_of(value_question)
    ) %>%
    dplyr::filter(!is.na(label), !is.na(value)) %>%
    dplyr::distinct()

  maximum_values_per_label <- paired %>%
    dplyr::count(label) %>%
    dplyr::summarize(maximum = max(n, 0)) %>%
    dplyr::pull(maximum)

  maximum_labels_per_value <- paired %>%
    dplyr::count(value) %>%
    dplyr::summarize(maximum = max(n, 0)) %>%
    dplyr::pull(maximum)

  tibble::tibble(
    question = question,
    value_question = value_question,
    complete_pair_n = sum(stats::complete.cases(df[c(question, value_question)])),
    distinct_mapping_n = nrow(paired),
    maximum_values_per_label = maximum_values_per_label,
    maximum_labels_per_value = maximum_labels_per_value,
    mapping_status = ifelse(
      maximum_values_per_label <= 1 && maximum_labels_per_value <= 1,
      "one-to-one", "not one-to-one"
    ),
    interpretation = ifelse(
      real_data, "audit before analysis", "expected failure after column shuffling"
    )
  )
}

prepare_ordinal <- function(df, question, fallback_levels) {
  values <- df[[question]]

  if (real_data) {
    value_question <- paste0(question, "__value")
    mapping <- tibble::tibble(
      label = values,
      value = suppressWarnings(as.numeric(df[[value_question]]))
    ) %>%
      dplyr::filter(!is.na(label), !is.na(value)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(value)

    levels <- mapping$label
  } else {
    observed <- sort(unique(values[!is.na(values)]))
    levels <- c(
      intersect(fallback_levels, observed),
      setdiff(observed, fallback_levels)
    )
  }

  ordered(values, levels = unique(levels))
}

adjusted_rand_index <- function(reference, candidate) {
  contingency <- table(reference, candidate)
  choose_two <- function(x) x * (x - 1) / 2
  total_pairs <- choose_two(sum(contingency))

  if (total_pairs == 0) {
    return(NA_real_)
  }

  observed <- sum(choose_two(contingency))
  row_pairs <- sum(choose_two(rowSums(contingency)))
  column_pairs <- sum(choose_two(colSums(contingency)))
  expected <- row_pairs * column_pairs / total_pairs
  maximum <- (row_pairs + column_pairs) / 2

  if (maximum == expected) {
    return(1)
  }

  (observed - expected) / (maximum - expected)
}

fit_feature_vectors <- function(scores, data, ordination, permutations = 999) {
  purrr::map_dfr(names(data), function(feature) {
    feature_values <- data[[feature]]
    if (is.ordered(feature_values)) {
      feature_values <- as.numeric(feature_values)
    }

    complete_rows <- stats::complete.cases(scores, feature_values)
    feature_data <- stats::setNames(
      data.frame(feature_values[complete_rows]), feature
    )
    feature_fit <- vegan::envfit(
      scores[complete_rows, , drop = FALSE], feature_data,
      permutations = permutations
    )
    coordinates <- vegan::scores(feature_fit, display = "vectors")

    tibble::tibble(
      ordination = ordination,
      feature = feature,
      complete_n = sum(complete_rows),
      axis_1 = unname(coordinates[1, 1]),
      axis_2 = unname(coordinates[1, 2]),
      r_squared = unname(feature_fit$vectors$r[1]),
      permutation_p = unname(feature_fit$vectors$pvals[1])
    )
  })
}

scale_vector_arrows <- function(vector_data, scores, proportion = 0.75) {
  axis_1_multiplier <- max(abs(scores[, 1])) / max(abs(vector_data$axis_1))
  axis_2_multiplier <- max(abs(scores[, 2])) / max(abs(vector_data$axis_2))
  multiplier <- proportion * min(axis_1_multiplier, axis_2_multiplier)

  vector_data %>%
    dplyr::mutate(
      arrow_1 = axis_1 * multiplier,
      arrow_2 = axis_2 * multiplier
    )
}

## -------------------------------------------- ##
# Load and Audit Data ----
## -------------------------------------------- ##

survey_v01 <- read.csv(
  data_path, check.names = FALSE, stringsAsFactors = FALSE,
  na.strings = c("", "NA")
) %>%
  dplyr::mutate(dplyr::across(
    .cols = dplyr::where(is.character),
    .fns = ~ dplyr::na_if(trimws(.), "")
  ))

# Preserve a stable row reference when a de-identified real file omits its
# respondent identifier. This key has no meaning outside the current input file.
if (real_data && !"ResponseId" %in% names(survey_v01)) {
  survey_v01$analysis_row_id <- seq_len(nrow(survey_v01))
}

required_questions <- c(
  active_ordinal_questions,
  paste0(active_ordinal_questions, "__value"),
  active_multiselect_questions,
  active_nominal_questions
)
missing_questions <- setdiff(required_questions, names(survey_v01))

if (length(missing_questions) > 0) {
  stop(
    "Required columns are absent: ", paste(missing_questions, collapse = ", ")
  )
}

input_respondent_n <- nrow(survey_v01)
opposed_respondent_n <- sum(
  survey_v01$Gen_Attitude == "Opposed to GenAI", na.rm = TRUE
)

if (exclude_opposed) {
  # Keep missing attitudes: absence of an answer is not evidence of opposition.
  survey_v01 <- survey_v01 %>%
    dplyr::filter(
      is.na(Gen_Attitude) | Gen_Attitude != "Opposed to GenAI"
    )
}
excluded_opposed_n <- input_respondent_n - nrow(survey_v01)

if (!real_data && analysis_scope == "all-substantive") {
  fixture_substantive_questions <- setdiff(
    names(survey_v01),
    c("fake_row", grep("__value$", names(survey_v01), value = TRUE))
  )
  unclassified_fixture_questions <- setdiff(
    fixture_substantive_questions, configured_questions$question
  )
  if (length(unclassified_fixture_questions) > 0) {
    stop(
      "Substantive fixture columns lack an analysis type: ",
      paste(unclassified_fixture_questions, collapse = ", ")
    )
  }
}

respondent_id_question <- if (real_data && "ResponseId" %in% names(survey_v01)) {
  "ResponseId"
} else if (real_data) {
  "analysis_row_id"
} else {
  "fake_row"
}
if (!respondent_id_question %in% names(survey_v01)) {
  stop("Expected respondent identifier '", respondent_id_question, "' is absent")
}
if (anyDuplicated(survey_v01[[respondent_id_question]]) > 0) {
  stop("Respondent identifiers must be unique")
}

paired_questions <- names(survey_v01)[stringr::str_ends(names(survey_v01), "__value")]
paired_questions <- sub("__value$", "", paired_questions)
paired_questions <- paired_questions[paired_questions %in% names(survey_v01)]

pairing_audit <- purrr::map_dfr(
  paired_questions, ~ audit_value_pair(survey_v01, .x)
) %>%
  dplyr::left_join(question_metadata, by = "question") %>%
  dplyr::relocate(question_label, question_text, .after = question)

if (real_data && any(
  pairing_audit$question %in% active_ordinal_questions &
    pairing_audit$mapping_status != "one-to-one"
)) {
  stop(
    "At least one active ordinal label/value pair is not one-to-one. ",
    "Review the 'pairing_audit' object."
  )
}

## -------------------------------------------- ##
# Encode Active Questions ----
## -------------------------------------------- ##

# Ordinal questions use response labels ordered by their verified `__value`
# companion in real mode. Fake mode uses documented fallback order because the
# independently shuffled companions cannot establish a valid mapping.
ordinal_data <- purrr::map_dfc(active_ordinal_questions, function(question) {
  result <- prepare_ordinal(
    survey_v01, question, fallback_ordinal_levels[[question]]
  )
  stats::setNames(tibble::tibble(result), question)
})

ordinal_dictionary <- tibble::tibble(
  feature = active_ordinal_questions,
  question = active_ordinal_questions,
  response_option = NA_character_,
  measurement = "ordered factor",
  feature_weight = 1,
  selected_n = vapply(ordinal_data, function(x) sum(!is.na(x)), integer(1)),
  selected_percent = round(
    100 * selected_n / nrow(ordinal_data), 1
  )
)

multiselect_encoded <- purrr::map(
  active_multiselect_questions,
  ~ encode_multiselect(survey_v01[[.x]], .x, minimum_option_n)
)

multiselect_data <- purrr::map_dfc(multiselect_encoded, "data")
# Nominal questions enter Gower once as factors. Separate one-hot indicators are
# created only for fitted vectors and heatmaps; they do not multiply the
# question's influence on respondent distance.
multiselect_dictionary <- purrr::map_dfr(multiselect_encoded, "dictionary")

nominal_encoded <- purrr::map(
  active_nominal_questions,
  ~ encode_nominal(survey_v01[[.x]], .x)
)
nominal_data <- purrr::map_dfc(nominal_encoded, "data")
nominal_interpretation_data <- purrr::map_dfc(
  nominal_encoded, "interpretation_data"
)
nominal_dictionary <- purrr::map_dfr(
  nominal_encoded, "distance_dictionary"
)
nominal_interpretation_dictionary <- purrr::map_dfr(
  nominal_encoded, "interpretation_dictionary"
)

active_data_v01 <- dplyr::bind_cols(
  ordinal_data, multiselect_data, nominal_data
)
interpretation_data_v01 <- dplyr::bind_cols(
  ordinal_data, multiselect_data, nominal_interpretation_data
)
feature_dictionary <- dplyr::bind_rows(
  ordinal_dictionary, multiselect_dictionary, nominal_dictionary
) %>%
  dplyr::left_join(question_metadata, by = "question") %>%
  dplyr::relocate(question_label, question_text, .after = question)
interpretation_dictionary <- dplyr::bind_rows(
  ordinal_dictionary, multiselect_dictionary,
  nominal_interpretation_dictionary
) %>%
  dplyr::left_join(question_metadata, by = "question") %>%
  dplyr::relocate(question_label, question_text, .after = question)

active_question_completion <- as.data.frame(stats::setNames(
  lapply(active_ordinal_questions, function(question) {
    !is.na(survey_v01[[question]])
  }),
  active_ordinal_questions
))

multiselect_completion <- as.data.frame(stats::setNames(
  lapply(active_multiselect_questions, function(question) {
    !is.na(survey_v01[[question]])
  }),
  active_multiselect_questions
))

nominal_completion <- as.data.frame(stats::setNames(
  lapply(active_nominal_questions, function(question) {
    !is.na(survey_v01[[question]])
  }),
  active_nominal_questions
))

question_completion_matrix <- dplyr::bind_cols(
  active_question_completion, multiselect_completion, nominal_completion
)
answered_active_blocks <- rowSums(question_completion_matrix)

# Include respondents meeting the scope-specific shared-information threshold.
included_respondent <- answered_active_blocks >= minimum_active_blocks

question_completion <- tibble::tibble(
  question = names(question_completion_matrix),
  answered_n = colSums(question_completion_matrix),
  answered_percent = round(100 * answered_n / nrow(question_completion_matrix), 1)
) %>%
  dplyr::left_join(question_metadata, by = "question") %>%
  dplyr::relocate(question_label, question_text, .after = question)

if (sum(included_respondent) < 3) {
  stop(
    "Fewer than three respondents meet 'AI_DAY_MIN_BLOCKS'. Lower the threshold ",
    "only after reviewing question completion."
  )
}

active_data <- active_data_v01[included_respondent, , drop = FALSE]
interpretation_data <- interpretation_data_v01[
  included_respondent, , drop = FALSE
]
respondent_ids <- survey_v01[[respondent_id_question]][included_respondent]

constant_feature <- vapply(active_data, function(x) {
  length(unique(x[!is.na(x)])) < 2
}, logical(1))

if (any(constant_feature)) {
  feature_dictionary <- feature_dictionary %>%
    dplyr::mutate(included = !feature %in% names(active_data)[constant_feature])
  active_data <- active_data[, !constant_feature, drop = FALSE]
} else {
  feature_dictionary <- feature_dictionary %>%
    dplyr::mutate(included = TRUE)
}

feature_dictionary <- feature_dictionary %>%
  dplyr::group_by(question) %>%
  dplyr::mutate(
    feature_weight = dplyr::if_else(
      included,
      feature_weight / sum(feature_weight[included]),
      feature_weight
    )
  ) %>%
  dplyr::ungroup()

# Enforce equal total influence per question after any constant encoded feature
# is removed. This prevents long checkbox questions from dominating Gower.
question_weight_audit <- feature_dictionary %>%
  dplyr::filter(included) %>%
  dplyr::group_by(question, question_label, question_text) %>%
  dplyr::summarize(
    included_feature_n = dplyr::n(),
    total_gower_weight = sum(feature_weight),
    .groups = "drop"
  )

if (
  nrow(question_weight_audit) != nrow(configured_questions) ||
    any(abs(question_weight_audit$total_gower_weight - 1) > 1e-10)
) {
  stop("Every configured question must contribute total Gower weight one")
}

constant_interpretation_feature <- vapply(interpretation_data, function(x) {
  length(unique(x[!is.na(x)])) < 2
}, logical(1))
interpretation_dictionary <- interpretation_dictionary %>%
  dplyr::mutate(
    included = !feature %in%
      names(interpretation_data)[constant_interpretation_feature]
  )
interpretation_data <- interpretation_data[
  , !constant_interpretation_feature, drop = FALSE
]

feature_weights <- feature_dictionary$feature_weight[
  match(names(active_data), feature_dictionary$feature)
]
asymmetric_features <- names(active_data)[vapply(
  active_data, function(x) all(stats::na.omit(x) %in% c(0, 1)), logical(1)
)]

## -------------------------------------------- ##
# Gower Dissimilarity ----
## -------------------------------------------- ##

gower_dissimilarity <- cluster::daisy(
  active_data,
  metric = "gower",
  type = list(asymm = asymmetric_features),
  weights = feature_weights
)

if (any(!is.finite(gower_dissimilarity))) {
  stop(
    "Some respondent pairs have no comparable active responses. Increase ",
    "'AI_DAY_MIN_BLOCKS' or inspect question-level missingness."
  )
}

## -------------------------------------------- ##
# Ordination ----
## -------------------------------------------- ##

set.seed(analysis_seed)
pcoa_fit <- stats::cmdscale(
  gower_dissimilarity, k = 2, eig = TRUE, add = TRUE, x.ret = TRUE
)

set.seed(analysis_seed)
nmds_fit <- vegan::metaMDS(
  gower_dissimilarity, k = 2, trymax = 100,
  autotransform = FALSE, trace = FALSE
)

pcoa_positive_eigenvalues <- pcoa_fit$eig[pcoa_fit$eig > 0]
pcoa_axis_percent <- 100 * pcoa_fit$eig[1:2] / sum(pcoa_positive_eigenvalues)

pcoa_eigenvalues <- tibble::tibble(
  axis = seq_along(pcoa_fit$eig),
  eigenvalue = pcoa_fit$eig,
  positive = eigenvalue > 0,
  percent_positive_eigenvalues = dplyr::if_else(
    positive, 100 * eigenvalue / sum(pcoa_positive_eigenvalues), NA_real_
  ),
  cumulative_percent_positive = cumsum(dplyr::if_else(
    positive, 100 * eigenvalue / sum(pcoa_positive_eigenvalues), 0
  ))
)

included_completion <- as.matrix(
  question_completion_matrix[included_respondent, , drop = FALSE]
)
shared_active_blocks <- tcrossprod(included_completion * 1)
mean_shared_active_blocks <- (
  rowSums(shared_active_blocks) - diag(shared_active_blocks)
) / (nrow(shared_active_blocks) - 1)

ordination_scores <- tibble::tibble(
  respondent_id = respondent_ids,
  answered_active_blocks = answered_active_blocks[included_respondent],
  mean_shared_active_blocks = mean_shared_active_blocks,
  mean_shared_active_percent = 100 * mean_shared_active_blocks /
    ncol(question_completion_matrix),
  PCoA1 = pcoa_fit$points[, 1],
  PCoA2 = pcoa_fit$points[, 2],
  NMDS1 = nmds_fit$points[, 1],
  NMDS2 = nmds_fit$points[, 2]
)

fit_statistics <- tibble::tibble(
  statistic = c(
    "input_respondents", "opposed_respondents_in_input",
    "excluded_opposed_respondents", "included_respondents", "active_questions",
    "encoded_features", "minimum_answered_questions", "gower_mean",
    "pcoa_additive_constant", "pcoa_axis_1_percent_positive_eigenvalues",
    "pcoa_axis_2_percent_positive_eigenvalues", "nmds_stress"
  ),
  value = c(
    input_respondent_n, opposed_respondent_n, excluded_opposed_n,
    sum(included_respondent),
    length(active_ordinal_questions) + length(active_multiselect_questions) +
      length(active_nominal_questions),
    ncol(active_data), minimum_active_blocks, mean(gower_dissimilarity),
    pcoa_fit$ac, pcoa_axis_percent, nmds_fit$stress
  )
)

## -------------------------------------------- ##
# Active Feature Associations ----
## -------------------------------------------- ##

association_features <- interpretation_dictionary %>%
  dplyr::filter(
    included,
    measurement == "ordered factor" | selected_n >= minimum_option_n
  ) %>%
  dplyr::pull(feature)
association_data <- interpretation_data[
  , association_features, drop = FALSE
]

set.seed(analysis_seed)
pcoa_feature_vectors <- fit_feature_vectors(
  pcoa_fit$points, association_data, "PCoA"
)
set.seed(analysis_seed)
nmds_feature_vectors <- fit_feature_vectors(
  nmds_fit$points, association_data, "NMDS"
)

active_feature_vectors <- dplyr::bind_rows(
  pcoa_feature_vectors, nmds_feature_vectors
) %>%
  dplyr::left_join(
    interpretation_dictionary %>%
      dplyr::select(
        feature, question, question_label, question_text, response_option, measurement,
        feature_weight
      ),
    by = "feature"
  ) %>%
  dplyr::mutate(vector_label = dplyr::case_when(
    measurement == "ordered factor" ~ question_label,
    measurement == "nominal category indicator" ~ paste0(
      question_label, ": ", response_option
    ),
    TRUE ~ response_option
  )) %>%
  dplyr::relocate(
    question, question_label, question_text, response_option, measurement,
    .after = feature
  )

question_associations <- active_feature_vectors %>%
  dplyr::group_by(ordination, question, question_label, question_text) %>%
  dplyr::summarize(
    weighted_mean_r_squared = stats::weighted.mean(
      r_squared, feature_weight, na.rm = TRUE
    ),
    maximum_feature_r_squared = max(r_squared, na.rm = TRUE),
    strongest_feature = vector_label[which.max(r_squared)],
    feature_n = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(ordination, dplyr::desc(weighted_mean_r_squared))

## -------------------------------------------- ##
# Candidate Learning Profiles ----
## -------------------------------------------- ##

candidate_k <- 2:min(8, nrow(active_data) - 1)
distance_matrix <- as.matrix(gower_dissimilarity)
set.seed(analysis_seed)

profile_fits <- purrr::map(candidate_k, function(profile_count) {
  cluster::pam(gower_dissimilarity, k = profile_count, diss = TRUE)
})
names(profile_fits) <- candidate_k

profile_stability <- purrr::map_dfr(candidate_k, function(profile_count) {
  reference_clusters <- profile_fits[[as.character(profile_count)]]$clustering
  sample_size <- max(profile_count + 1, floor(0.8 * nrow(active_data)))

  ari <- replicate(stability_runs, {
    sampled_rows <- sample(seq_len(nrow(active_data)), sample_size, replace = FALSE)
    sampled_distance <- stats::as.dist(
      distance_matrix[sampled_rows, sampled_rows, drop = FALSE]
    )
    sampled_fit <- cluster::pam(
      sampled_distance, k = profile_count, diss = TRUE
    )
    adjusted_rand_index(
      reference_clusters[sampled_rows], sampled_fit$clustering
    )
  })

  tibble::tibble(
    profile_count = profile_count,
    stability_mean_ari = mean(ari, na.rm = TRUE),
    stability_sd_ari = stats::sd(ari, na.rm = TRUE)
  )
})

profile_diagnostics <- purrr::map_dfr(candidate_k, function(profile_count) {
  fit <- profile_fits[[as.character(profile_count)]]
  profile_sizes <- table(fit$clustering)
  tibble::tibble(
    profile_count = profile_count,
    average_silhouette = mean(fit$silinfo$widths[, "sil_width"]),
    minimum_profile_n = min(profile_sizes),
    minimum_profile_percent = 100 * min(profile_sizes) / nrow(active_data)
  )
}) %>%
  dplyr::left_join(profile_stability, by = "profile_count") %>%
  dplyr::mutate(eligible_profile_size = minimum_profile_percent >= 5)

# Profile-count decision: either select the strongest eligible silhouette result
# or honor a user-specified k for planned comparisons and sensitivity checks.
if (profile_k_setting == "") {
  eligible_diagnostics <- profile_diagnostics %>%
    dplyr::filter(eligible_profile_size)

  if (nrow(eligible_diagnostics) == 0) {
    stop("No candidate profile solution has a minimum profile size of 5 percent")
  }

  selected_profile_k <- eligible_diagnostics %>%
    dplyr::slice_max(average_silhouette, n = 1, with_ties = FALSE) %>%
    dplyr::pull(profile_count)
} else {
  selected_profile_k <- suppressWarnings(as.integer(profile_k_setting))
  if (is.na(selected_profile_k) || !selected_profile_k %in% candidate_k) {
    stop("'AI_DAY_PROFILE_K' must be one of: ", paste(candidate_k, collapse = ", "))
  }
}

selected_profile_fit <- profile_fits[[as.character(selected_profile_k)]]
ordination_scores$profile <- factor(selected_profile_fit$clustering)

profile_feature_summary <- interpretation_data %>%
  dplyr::mutate(
    dplyr::across(dplyr::where(is.ordered), as.numeric),
    profile = ordination_scores$profile
  ) %>%
  tidyr::pivot_longer(
    cols = -profile, names_to = "feature", values_to = "feature_value"
  ) %>%
  dplyr::group_by(profile, feature) %>%
  dplyr::summarize(
    nonmissing_n = sum(!is.na(feature_value)),
    mean_feature_value = mean(feature_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    interpretation_dictionary %>%
      dplyr::select(
        feature, question, question_label, question_text, response_option,
        measurement, selected_n
      ),
    by = "feature"
  ) %>%
  dplyr::relocate(
    question, question_label, question_text, response_option, measurement,
    .after = feature
  )

feature_scaling <- tibble::tibble(
  feature = names(interpretation_data),
  feature_minimum = vapply(interpretation_data, function(x) {
    if (is.ordered(x)) 1 else 0
  }, numeric(1)),
  feature_maximum = vapply(interpretation_data, function(x) {
    if (is.ordered(x)) nlevels(x) else 1
  }, numeric(1)),
  overall_mean = vapply(interpretation_data, function(x) {
    mean(if (is.ordered(x)) as.numeric(x) else x, na.rm = TRUE)
  }, numeric(1))
)

profile_heatmap_data <- profile_feature_summary %>%
  dplyr::left_join(feature_scaling, by = "feature") %>%
  dplyr::mutate(
    scaled_profile_mean = (mean_feature_value - feature_minimum) /
      (feature_maximum - feature_minimum),
    scaled_overall_mean = (overall_mean - feature_minimum) /
      (feature_maximum - feature_minimum),
    deviation_from_overall = scaled_profile_mean - scaled_overall_mean,
    display_value = dplyr::if_else(
      measurement %in% c(
        "asymmetric binary", "nominal category indicator"
      ),
      paste0(round(100 * mean_feature_value), "%"),
      format(round(mean_feature_value, 1), nsmall = 1)
    ),
    feature_label = dplyr::if_else(
      is.na(response_option), "Ordered response (higher rank)", response_option
    )
  ) %>%
  dplyr::group_by(feature) %>%
  dplyr::mutate(feature_contrast = max(scaled_profile_mean, na.rm = TRUE) -
    min(scaled_profile_mean, na.rm = TRUE)) %>%
  dplyr::ungroup()

profile_heatmap_features <- profile_heatmap_data %>%
  dplyr::filter(selected_n >= minimum_option_n) %>%
  dplyr::distinct(question, feature, feature_contrast) %>%
  dplyr::group_by(question) %>%
  dplyr::slice_max(feature_contrast, n = 2, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::pull(feature)

profile_heatmap_data <- profile_heatmap_data %>%
  dplyr::filter(feature %in% profile_heatmap_features) %>%
  dplyr::mutate(
    question_label = factor(
      question_label,
      levels = question_metadata$question_label[
        match(
          unique(interpretation_dictionary$question),
          question_metadata$question
        )
      ]
    ),
    feature_label = stringr::str_wrap(feature_label, width = 38)
  )

## -------------------------------------------- ##
# Figures ----
## -------------------------------------------- ##

pcoa_graph <- ggplot2::ggplot(
  ordination_scores,
  ggplot2::aes(x = PCoA1, y = PCoA2, color = profile)
) +
  ggplot2::geom_point(alpha = 0.7, size = 2.5) +
  ggplot2::scale_color_viridis_d(option = "C", end = 0.9) +
  ggplot2::labs(
    x = paste0("Corrected PCoA 1 (", round(pcoa_axis_percent[1], 1), "%)"),
    y = paste0("Corrected PCoA 2 (", round(pcoa_axis_percent[2], 1), "%)"),
    color = "Candidate profile", caption = analysis_scenario
  ) +
  supportR::theme_lyon(text_size = 16)

select_plot_vectors <- function(association_data, ordination_name, scores) {
  association_data %>%
    dplyr::filter(.data$ordination == .env$ordination_name) %>%
    dplyr::group_by(question) %>%
    dplyr::slice_max(r_squared, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(r_squared, n = vector_label_n, with_ties = FALSE) %>%
    scale_vector_arrows(scores) %>%
    dplyr::mutate(vector_label = stringr::str_wrap(vector_label, width = 24))
}

pcoa_plot_vectors <- select_plot_vectors(
  active_feature_vectors, "PCoA", pcoa_fit$points
)
nmds_plot_vectors <- select_plot_vectors(
  active_feature_vectors, "NMDS", nmds_fit$points
)

pcoa_biplot <- pcoa_graph +
  ggplot2::geom_segment(
    data = pcoa_plot_vectors,
    ggplot2::aes(x = 0, y = 0, xend = arrow_1, yend = arrow_2),
    inherit.aes = FALSE, color = "#252525", linewidth = 0.5,
    arrow = grid::arrow(length = grid::unit(0.12, "inches"))
  ) +
  ggrepel::geom_text_repel(
    data = pcoa_plot_vectors,
    ggplot2::aes(x = arrow_1, y = arrow_2, label = vector_label),
    inherit.aes = FALSE, size = 3, color = "#252525",
    box.padding = 0.4, max.overlaps = Inf, seed = analysis_seed
  ) +
  ggplot2::labs(
    title = "Feature gradients in corrected PCoA",
    subtitle = "Strongest fitted response per question; arrows show post hoc associations"
  )

nmds_graph <- ggplot2::ggplot(
  ordination_scores,
  ggplot2::aes(x = NMDS1, y = NMDS2, color = profile)
) +
  ggplot2::geom_point(alpha = 0.7, size = 2.5) +
  ggplot2::scale_color_viridis_d(option = "C", end = 0.9) +
  ggplot2::labs(
    x = "NMDS 1", y = "NMDS 2", color = "Candidate profile",
    subtitle = paste0("Stress = ", round(nmds_fit$stress, 3)),
    caption = analysis_scenario
  ) +
  supportR::theme_lyon(text_size = 16)

nmds_biplot <- nmds_graph +
  ggplot2::geom_segment(
    data = nmds_plot_vectors,
    ggplot2::aes(x = 0, y = 0, xend = arrow_1, yend = arrow_2),
    inherit.aes = FALSE, color = "#252525", linewidth = 0.5,
    arrow = grid::arrow(length = grid::unit(0.12, "inches"))
  ) +
  ggrepel::geom_text_repel(
    data = nmds_plot_vectors,
    ggplot2::aes(x = arrow_1, y = arrow_2, label = vector_label),
    inherit.aes = FALSE, size = 3, color = "#252525",
    box.padding = 0.4, max.overlaps = Inf, seed = analysis_seed
  ) +
  ggplot2::labs(
    title = "Feature gradients in NMDS",
    subtitle = paste0(
      "Strongest fitted response per question; stress = ",
      round(nmds_fit$stress, 3)
    )
  )

pcoa_scree_graph <- pcoa_eigenvalues %>%
  dplyr::filter(positive, axis <= 20) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = axis, y = percent_positive_eigenvalues
  )) +
  ggplot2::geom_col(fill = "#00798c", color = "#252525") +
  ggplot2::scale_x_continuous(breaks = 1:20) +
  ggplot2::labs(
    x = "Corrected PCoA axis",
    y = "Percent of positive eigenvalues (%)",
    caption = analysis_scenario
  ) +
  supportR::theme_lyon(text_size = 16)

completeness_graph <- ggplot2::ggplot(
  ordination_scores,
  ggplot2::aes(
    x = PCoA1, y = PCoA2, color = mean_shared_active_percent,
    size = answered_active_blocks
  )
) +
  ggplot2::geom_point(alpha = 0.75) +
  ggplot2::scale_color_viridis_c(option = "B", end = 0.9) +
  ggplot2::labs(
    x = paste0("Corrected PCoA 1 (", round(pcoa_axis_percent[1], 1), "%)"),
    y = paste0("Corrected PCoA 2 (", round(pcoa_axis_percent[2], 1), "%)"),
    color = "Mean shared active\nquestions (%)",
    size = "Answered active\nquestions",
    caption = analysis_scenario
  ) +
  supportR::theme_lyon(text_size = 16)

question_association_graph <- ggplot2::ggplot(
  question_associations,
  ggplot2::aes(
    x = weighted_mean_r_squared,
    y = forcats::fct_reorder(question_label, weighted_mean_r_squared),
    color = ordination
  )
) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.5), size = 3
  ) +
  ggplot2::scale_color_manual(values = c("NMDS" = "#c44536", "PCoA" = "#00798c")) +
  ggplot2::labs(
    x = expression("Question-weighted " * R^2), y = "", color = "Ordination",
    caption = analysis_scenario
  ) +
  supportR::theme_lyon(text_size = 16)

profile_heatmap_graph <- ggplot2::ggplot(
  profile_heatmap_data,
  ggplot2::aes(x = profile, y = feature_label, fill = deviation_from_overall)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.5) +
  ggplot2::geom_text(ggplot2::aes(label = display_value), size = 3) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(question_label), scales = "free_y", space = "free_y",
    switch = "y"
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0
  ) +
  ggplot2::labs(
    x = "Candidate profile", y = "",
    fill = "Deviation from\noverall mean",
    caption = analysis_scenario
  ) +
  supportR::theme_lyon(text_size = 14) +
  ggplot2::theme(
    strip.placement = "outside",
    strip.text.y.left = ggplot2::element_text(angle = 0),
    axis.text.y = ggplot2::element_text(size = 9)
  )

profile_diagnostic_graph <- profile_diagnostics %>%
  dplyr::select(
    profile_count, average_silhouette, stability_mean_ari
  ) %>%
  tidyr::pivot_longer(
    cols = -profile_count, names_to = "diagnostic", values_to = "value"
  ) %>%
  dplyr::mutate(diagnostic = dplyr::recode(
    diagnostic,
    average_silhouette = "Average silhouette",
    stability_mean_ari = "Subsampling stability (ARI)"
  )) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = profile_count, y = value, color = diagnostic
  )) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_x_continuous(breaks = candidate_k) +
  ggplot2::scale_color_manual(values = c("#00798c", "#c44536")) +
  ggplot2::labs(
    x = "Candidate number of profiles", y = "Diagnostic value", color = "",
    caption = analysis_scenario
  ) +
  supportR::theme_lyon(text_size = 16)

ggplot2::ggsave(
  ordination_plot_path("pcoa-biplot"), pcoa_biplot,
  height = 10, width = 12, units = "in"
)
ggplot2::ggsave(
  ordination_plot_path("nmds-biplot"), nmds_biplot,
  height = 10, width = 12, units = "in"
)
ggplot2::ggsave(
  ordination_plot_path("pcoa-scree"), pcoa_scree_graph,
  height = 7, width = 9, units = "in"
)
ggplot2::ggsave(
  ordination_plot_path("pcoa-completeness"), completeness_graph,
  height = 7, width = 9, units = "in"
)
ggplot2::ggsave(
  ordination_plot_path("question-associations"),
  question_association_graph, height = 8, width = 10, units = "in"
)
ggplot2::ggsave(
  ordination_plot_path("profile-response-heatmap"),
  profile_heatmap_graph,
  height = max(10, length(profile_heatmap_features) * 0.48),
  width = 14, units = "in"
)
ggplot2::ggsave(
  ordination_plot_path("profile-diagnostics"),
  profile_diagnostic_graph, height = 7, width = 9, units = "in"
)

grDevices::png(
  ordination_plot_path("nmds-shepard"),
  width = 1800, height = 1600, res = 220
)
vegan::stressplot(
  nmds_fit,
  main = paste("NMDS Shepard plot", analysis_scenario, sep = "\n")
)
grDevices::dev.off()

message(
  "Ordination workflow complete in ", ifelse(real_data, "REAL", "FAKE"),
  " mode (", analysis_scenario, "). Excluded ", excluded_opposed_n,
  " opposed respondents; included ", sum(included_respondent), " of ",
  nrow(survey_v01),
  " respondents; selected exploratory PAM k = ", selected_profile_k, "."
)

if (!real_data) {
  message(
    "Fake-data coordinates, associations, profiles, and pairing audits are not ",
    "interpretable because columns were shuffled independently."
  )
}

# End ----