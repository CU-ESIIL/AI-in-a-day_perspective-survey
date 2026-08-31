#' @title AI-Focused Closed-Response Ordination and Learning Profiles
#'
#' @description
#' Run the AI in a Day Gower ordination and PAM workflow using only closed
#' questions directly concerning generative AI. Career, education, professional,
#' disciplinary, and demographic questions do not contribute to respondent
#' distances or candidate profiles in this analysis.
#'
#' @details
#' This is a separate entry point for the shared workflow implemented in
#' `_sandbox/survey_earl-explore_ordination.r`. It sets
#' `AI_DAY_ANALYSIS_SCOPE=ai-focused` for the duration of the run and restores
#' the caller's prior setting afterward.
#'
#' The 11 active question blocks are GenAI use frequency, reasons affecting use,
#' general attitude, institutional policies and resources, promising
#' opportunities, challenges, research-task interests, technical-skill
#' interests, training received, and training desired. Each question contributes
#' total Gower weight one. Data-science frequency is excluded because it measures
#' broader professional practice rather than GenAI specifically.
#'
#' Free-response columns that occur only in the real data remain outside this
#' analysis until a validated text-coding workflow is available.
#'
#' @section Configuration:
#' This entry point honors the shared workflow settings, including
#' `AI_DAY_REAL_DATA`, `AI_DAY_SURVEY_DATA`, `AI_DAY_QUESTION_LOOKUP`,
#' `AI_DAY_EXCLUDE_OPPOSED`, `AI_DAY_MIN_OPTION_N`,
#' `AI_DAY_STABILITY_RUNS`, `AI_DAY_PROFILE_K`, `AI_DAY_VECTOR_LABELS`, and
#' `AI_DAY_SEED`. The default completion threshold is 7 of 11 active questions;
#' `AI_DAY_MIN_BLOCKS` can override it.
#'
#' @section Outputs:
#' Labeled PNG files are written to `graphs_fake/` or `graphs/`. Filenames begin
#' with `ordination_ai-focused_` and end with the attitude scenario, allowing
#' them to coexist with all-substantive outputs. Analytical tables remain in the
#' R session and are not written as intermediary CSV files.
#'
#' @return
#' The script writes labeled figures and leaves the shared workflow's analytical
#' objects in the interactive R session when sourced.
#'
#' @examples
#' \dontrun{
#' # Fake structural fixture, retaining all attitudes:
#' Sys.setenv(AI_DAY_REAL_DATA = "false", AI_DAY_EXCLUDE_OPPOSED = "false")
#' source(file.path(
#'   "_sandbox", "survey_earl-explore_ordination-ai-focused.r"
#' ))
#'
#' # Real data, excluding respondents opposed to GenAI:
#' Sys.setenv(AI_DAY_REAL_DATA = "true", AI_DAY_EXCLUDE_OPPOSED = "true")
#' source(file.path(
#'   "_sandbox", "survey_earl-explore_ordination-ai-focused.r"
#' ))
#' }

# This entry point makes exactly one decision: it fixes the analysis scope to
# `ai-focused`. All other decisions remain configurable through the shared
# environment variables documented above:
#
# * AI_DAY_REAL_DATA: fake fixture (`false`) or intact real data (`true`).
# * AI_DAY_EXCLUDE_OPPOSED: retain (`false`) or remove (`true`) opposed responses.
# * AI_DAY_MIN_BLOCKS: default 7/11, or a user-specified positive integer.
# * AI_DAY_MIN_OPTION_N: rare-option threshold, default 5.
# * AI_DAY_STABILITY_RUNS: fast development or precise final stability runs.
# * AI_DAY_PROFILE_K: automatic selection when blank, or forced k = 2,...,8.
# * AI_DAY_VECTOR_LABELS: maximum labeled vectors, default 8.
# * AI_DAY_SEED: reproducible non-negative integer seed.
#
# The previous scope is restored after execution so this wrapper cannot change a
# later all-substantive run in the same R process.
local({
  previous_scope <- Sys.getenv("AI_DAY_ANALYSIS_SCOPE", unset = NA_character_)
  on.exit({
    if (is.na(previous_scope)) {
      Sys.unsetenv("AI_DAY_ANALYSIS_SCOPE")
    } else {
      Sys.setenv(AI_DAY_ANALYSIS_SCOPE = previous_scope)
    }
  }, add = TRUE)

  Sys.setenv(AI_DAY_ANALYSIS_SCOPE = "ai-focused")
  source(
    file.path("_sandbox", "survey_earl-explore_ordination.r"),
    local = .GlobalEnv
  )
})

# End ----
