# -------------------------------------------------
# survey_elmendorf_survey_stats.R
# -------------------------------------------------
# Frequentist cumulative probit models (ordinal::clm, probit link) for the
# ordinal questions, and binomial GLMMs (lme4::glmer) for the
# select-all-that-apply questions, each crossed with demographic predictors.
#
# Structure:
#   1-3. setup, data read, factor releveling, ordinal-model helpers
#   4.   helper functions for the select-all models
#   5.   specs, cell diagnostics, and the select-all grid
#   6.   ordinal grid (+ factor-vs-slope comparisons)
#   6b.  table of every exclusion rule and its n
#   7.   cross-tabs of demographics against career stage
#   8.   pairwise associations among all demographics (polychoric /
#        bias-corrected Cramer's V), plus gen_attitude x AI use frequency
#
# INFERENCE: everything here is EXPLORATORY. Each cell of the grids is
# tested at alpha = .05 with NO correction across cells (~27 ordinal and
# select-all cells, each with several tests), so some "significant" cells
# are expected by chance alone. The only multiplicity adjustments are
# WITHIN a cell: letter displays are mvt-adjusted within each item, and
# per-item slopes are FDR-adjusted across the items of that cell. Report
# p-values as descriptive strength of evidence, not confirmatory tests.
#
# Models are MARGINAL (one predictor at a time) for the figures. Each
# cell is refitted with career stage added as a covariate (a slope on
# career_stage_value, 1..4) as a sensitivity check; those results are in
# the *_adj columns of the summary tables, next to the unadjusted test on
# the SAME respondents (*_cc) so a change can be attributed to the
# adjustment rather than to the respondents lost with missing career stage.
#
# Direction: ai_use_freq and ds_freq are re-ordered so HIGHER = MORE use
# (Never / "do not use" is level 1, Daily the top level).
# -------------------------------------------------

# -------------------------------------------------
# Project setup
# -------------------------------------------------

source(file.path("-setup.r"))          # clears environment, creates folders
gc()

set.seed(1)                            # reproducibility for anything stochastic

# Controls the select-all models, which take hours. Each finished fit is saved
# to fit_path as soon as it completes. The ordinal models ignore this setting
# and refit on every run.
#   FALSE  reuse every saved fit and fit only the cells with no saved file.
#          Use this to adjust figures or to resume an interrupted run.
#   TRUE   refit every cell and overwrite its saved file, whether or not one
#          exists. No need to delete anything first.
# FALSE does not check whether a saved fit is out of date. After changing the
# data or the model code, use TRUE, or delete the affected files in fit_path to
# refit only those cells.
rerun_mods <- FALSE

# ------------------------------------------------------------------
# CRAN mirror, so librarian::shelf installs without prompting
# ------------------------------------------------------------------
options(repos = c(CRAN = "https://cran.r-project.org"))

# -------------------------------------------------
# Load required libraries
# -------------------------------------------------
if (!requireNamespace("librarian", quietly = TRUE)) {
  install.packages("librarian")
}
librarian::shelf(
  scales,
  janitor,
  marginaleffects,
  emmeans,
  multcomp,          # cld() for the compact letter display
  lme4,
  performance,
  ordinal,
  polycor,           # polychoric correlations among ordinal demographics
  effectsize,        # bias-corrected Cramer's V
  lmtest,
  sandwich,
  tidyverse          # loaded last so dplyr wins the masking conflicts
)
conflicted::conflict_scout()
# dplyr verbs are masked by others in this stack
if (requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflict_prefer("count",     "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("filter",    "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("select",    "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("mutate",    "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("summarise", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("chisq.test", "stats", quiet = TRUE)
  conflicted::conflict_prefer("col_factor", "scales", quiet = TRUE)
  conflicted::conflict_prefer("discard", "purrr", quiet = TRUE)
  conflicted::conflict_prefer("expand", "tidyr", quiet = TRUE)
  conflicted::conflict_prefer("fisher.test", "stats", quiet = TRUE)
  conflicted::conflict_prefer("geyser", "MASS", quiet = TRUE)
  conflicted::conflict_prefer("lag", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("mad", "stats", quiet = TRUE)
  conflicted::conflict_prefer("match", "base", quiet = TRUE)
  conflicted::conflict_prefer("ngrps", "lme4", quiet = TRUE)
  conflicted::conflict_prefer("pack", "tidyr", quiet = TRUE)
  conflicted::conflict_prefer("sd", "stats", quiet = TRUE)
  conflicted::conflict_prefer("unpack", "tidyr", quiet = TRUE)
  conflicted::conflict_prefer("var", "stats", quiet = TRUE)
  conflicted::conflict_prefer("slice", "ordinal", quiet = TRUE)
  conflicted::conflict_prefer_all("purrr", "rlang", quiet = TRUE)
}

# -------------------------------------------------
# Source reusable functions (all tools/ scripts)
# -------------------------------------------------
purrr::walk(
  dir("tools", pattern = "\\.r$", full.names = TRUE),
  source
)

# -------------------------------------------------
# Paths
# -------------------------------------------------
data_path  <- file.path("data", "1_tidied-responses_no-free-text.csv")
graph_path <- "graphs"
fig_path   <- file.path(graph_path, "analysis_figs_ordinal")
fit_path   <- "fits_ordinal"

dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)
dir.create(fit_path, recursive = TRUE, showWarnings = FALSE)


# -------------------------------------------------
# Read and clean
# -------------------------------------------------
# Read CSV and convert empty strings to NA.
survey_raw <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    dplyr::across(.cols = dplyr::everything(),
                  .fns  = ~ ifelse(nchar(.) == 0, NA, .))
  )

survey <- survey_raw %>%
  clean_names() %>%   # snake_case, removes spaces, etc.
  # no free text removed responseID, but it in here simply as the row number
  mutate(response_id = row_number())

# ------------------------------------------------------------------
# Re-level the Likert columns so factor level order matches the
# numeric ordering carried in the matching *_value column. unique()
# after order() guards against duplicate label text in the raw data
# causing duplicated factor levels.
# ------------------------------------------------------------------
relevel_by_value <- function(d, col, value_col = paste0(col, "_value")) {
  lv <- unique(d[[col]][order(d[[value_col]])])
  lv <- lv[!is.na(lv)]
  d[[col]] <- factor(d[[col]], levels = lv, ordered = TRUE)
  d
}

for (v in c("gen_attitude", "ds_freq", "ai_use_freq", "career_stage", "policies")) {
  survey <- relevel_by_value(survey, v)
}

# The Qualtrics coding runs Daily (1) -> Never / "do not use" (5), so a
# positive slope would mean LESS use. Reverse both so higher = more use.
# Note: the *_value columns are NOT reversed and are not used below.
for (v in c("ai_use_freq", "ds_freq")) {
  survey[[v]] <- factor(survey[[v]], levels = rev(levels(survey[[v]])),
                        ordered = TRUE)
}
stopifnot(grepl("^Never", levels(survey$ai_use_freq)[1]),
          grepl("^Daily", levels(survey$ai_use_freq)[nlevels(survey$ai_use_freq)]),
          grepl("^I do not use", levels(survey$ds_freq)[1]))

# gen_attitude sensitivity version: "Indifferent" merged into the middle
# level ("A mix of caution and enthusiasm") instead of excluded. "Other"
# is excluded in both versions. The original gen_attitude column is kept,
# so figures can show which respondents in the merged level were
# Indifferent (stacked bar, see .add_split_bars()).
ga_mix_level <- grep("^A mix", levels(survey$gen_attitude), value = TRUE)
stopifnot(length(ga_mix_level) == 1,
          "Indifferent" %in% levels(survey$gen_attitude))
survey$gen_attitude_mix <- local({
  g <- as.character(survey$gen_attitude)
  g[g %in% "Indifferent"] <- ga_mix_level
  g[g %in% "Other"]       <- NA
  factor(g, levels = setdiff(levels(survey$gen_attitude),
                             c("Indifferent", "Other")),
         ordered = TRUE)
})

# ------------------------------------------------------------------
# Short level names for FIGURE AXES ONLY. The data, models and summary
# tables keep the full Qualtrics text; fig_label() is applied inside the
# axis label functions of the plotting helpers, and passes any label not
# in the map through unchanged. Built from the releveled factor (ordered
# by career_stage_value) rather than retyped, because the full labels
# contain en-dashes.
# ------------------------------------------------------------------
stopifnot(nlevels(survey$career_stage) == 4,
          grepl("^Preparation", levels(survey$career_stage)[1]),
          grepl("^Mature",      levels(survey$career_stage)[4]))
fig_short_labels <- stats::setNames(c("prep", "early", "mid", "mature"),
                                    levels(survey$career_stage))

# ds_freq "I do not use data science in my research/role" -> "no use".
# Looked up by pattern rather than typed, so it tracks the exact text.
ds_none <- grep("^I do not use data science", levels(survey$ds_freq), value = TRUE)
stopifnot(length(ds_none) == 1)
fig_short_labels <- c(fig_short_labels, stats::setNames("no use", ds_none))

fig_label <- function(x) {
  x <- as.character(x)
  hit <- x %in% names(fig_short_labels)
  x[hit] <- fig_short_labels[x[hit]]
  x
}

# Human-readable variable names for FIGURE axis titles and captions,
# from the `label` field of demo_specs (defined in section 5a; looked up
# at plot time, so the order of definition does not matter). Falls back
# to the column name for anything not in demo_specs.
#   fig_var("ai_use_freq")        -> "AI use frequency"   (axis title)
#   fig_var("ai_use_freq", FALSE) -> "AI use frequency"   (mid-sentence)
#   fig_var("career_stage")       -> "Career stage" / "career stage"
fig_var <- function(x, capitalise = TRUE) {
  lab <- if (exists("demo_specs") && x %in% names(demo_specs) &&
             !is.null(demo_specs[[x]]$label)) demo_specs[[x]]$label else x
  if (capitalise) paste0(toupper(substring(lab, 1, 1)), substring(lab, 2))
  else lab
}

# Ensure the ordinal helper columns are numeric.
survey <- survey %>%
  dplyr::mutate(dplyr::across(dplyr::ends_with("_value"), as.numeric))

survey <-survey %>%
  dplyr::mutate(prof_work_sector =
    ifelse(grepl('Student', prof_role), 'Student', work_sector)
  )

# 13 ambiguous on whether they are students or not
# bc they say they are students but not in academia.
# the reassigning assumes they are students and will drop from the
# professionals only analysis 

# ambiguous <- survey %>%
#   filter(work_sector != "Academia (Higher Education)"& prof_work_sector == 'Student')
# View (ambiguous %>% select(work_sector, prof_role, prof_work_sector))

survey <- survey %>%
  mutate(caregiver_IO = 
    case_when(grepl('^Not|^Caregiver', caregiver) ~ 1,
  grepl('^Primary|^Contributor|^Share', caregiver) ~ 0,
  TRUE ~ NA_real_)) %>%
    mutate(neuro_IO = 
    case_when(neurodiverse == 'Yes' ~ 0,
  neurodiverse == 'No' ~ 1,
  TRUE ~ NA_real_)) %>%
    mutate(firstgen_IO = 
    case_when(first_gen == 'Yes' ~ 0,
  first_gen == 'No' ~ 1,
  TRUE ~ NA_real_)) %>%
    mutate(race_IO = 
    case_when(
      race_ethnicity == "White / Caucasian" ~ 1,
      race_ethnicity == 'Prefer not to answer' | is.na(race_ethnicity) ~ NA_real_,
    TRUE ~ 0)
) %>%
  mutate(IO = caregiver_IO + neuro_IO + firstgen_IO + race_IO) 


# ---- p-value formatting -------------------------------------------
# Returns the comparison sign WITH the number, so format strings say
# "p %s" and never "p = %s" (which gave "p = < .001"). APA style: no
# leading zero, since p cannot exceed 1.
#   fmt_p(0.0342) -> "= .034"    fmt_p(0.0001) -> "< .001"
fmt_p <- function(p) {
  if (length(p) != 1 || is.na(p)) return("= NA")
  if (p < .001) return("< .001")
  paste("=", sub("^0", "", sprintf("%.3f", p)))
}

# ---- convergence helpers ------------------------------------------
# clm's convergence$code can be a VECTOR when more than one condition
# fires, so it must be collapsed before use in a scalar test.
#   0  clean
#   1  parameters may have few correct decimals (imprecise, usually fine)
#  -3  thresholds not increasing -> the fit is INVALID, never interpret it
.clm_usable <- function(f) {
  if (inherits(f, "try-error")) return(FALSE)
  cc <- f$convergence$code
  !is.null(cc) && length(cc) > 0 && !any(cc == -3)
}

# convergence code as one string: "0", "1", "-3", "0/1", or "error"
.clm_code <- function(f) {
  if (inherits(f, "try-error") || is.null(f)) return("error")
  paste(f$convergence$code, collapse = "/")
}

# readable flag for a summary table; "" when the fit is clean
.conv_flag <- function(code, name) {
  if (is.null(code) || length(code) != 1 || is.na(code) || code == "0")
    return(character(0))
  sprintf("%s %s", name,
          if (code == "error") "failed"
          else if (grepl("-3", code)) sprintf("INVALID (code %s)", code)
          else sprintf("imprecise (code %s)", code))
}

# ---- assumption checks for a fitted clm --------------------------
# Fitted EXPLICITLY (rather than via ordinal::scale_test / nominal_test)
# so each auxiliary model's convergence code is kept: an invalid (-3)
# auxiliary fit gives a meaningless LRT, and its p is returned as NA.
#   PO     y ~ 1, nominal = ~ x  vs  y ~ x   (nominal_test() does not
#          support the probit link, hence the explicit partial-PO fit)
#   scale  y ~ x, scale = ~ x    vs  y ~ x   (does x change the latent SD?)
.assumption_checks <- function(fit, dat, x, y, link = "probit") {
  nom <- try(suppressWarnings(ordinal::clm(
    stats::as.formula(sprintf("%s ~ 1", y)),
    nominal = stats::as.formula(paste("~", x)), data = dat, link = link)),
    silent = TRUE)
  scl <- try(suppressWarnings(ordinal::clm(
    stats::as.formula(sprintf("%s ~ %s", y, x)),
    scale = stats::as.formula(paste("~", x)), data = dat, link = link)),
    silent = TRUE)

  cmp <- function(alt) {
    if (!.clm_usable(fit) || !.clm_usable(alt)) return(NULL)
    t <- try(anova(fit, alt), silent = TRUE)
    if (inherits(t, "try-error")) NULL else t
  }
  t_po <- cmp(nom)
  t_sc <- cmp(scl)
  pp <- function(t) if (is.null(t)) NA_real_ else t[["Pr(>Chisq)"]][2]

  list(p_po = pp(t_po), p_scale = pp(t_sc),
       conv_po = .clm_code(nom), conv_scale = .clm_code(scl),
       test_po = t_po, test_scale = t_sc)
}

# ---- career-stage-adjusted sensitivity fit ------------------------
# On the respondents with a career stage, fits
#   y ~ x                 unadjusted, SAME respondents  (p_cc, b_cc)
#   y ~ x + career        adjusted                     (p_adj, b_adj)
# Career stage enters as a SLOPE on career_stage_value (1..4). x is used
# exactly as in the model it accompanies (factor or numeric slope).
.adjusted_clm <- function(dat, x, y, adj = "career_stage_value",
                          link = "probit") {
  stopifnot(adj %in% names(dat))
  d <- droplevels(dat[!is.na(dat[[adj]]), , drop = FALSE])
  f <- function(rhs) ordinal::clm(stats::as.formula(sprintf("%s ~ %s", y, rhs)),
                                  data = d, link = link)
  f_1  <- f("1")
  f_x  <- f(x)
  f_0  <- f(adj)
  f_xa <- f(paste(x, "+", adj))

  b <- function(m) if (is.numeric(d[[x]])) unname(stats::coef(m)[x]) else NA_real_

  list(n      = nrow(d),
       p_cc   = anova(f_1, f_x)[["Pr(>Chisq)"]][2],
       p_adj  = anova(f_0, f_xa)[["Pr(>Chisq)"]][2],
       b_cc   = b(f_x), b_adj = b(f_xa),
       b_career = unname(stats::coef(f_xa)[adj]),
       conv_adj = .clm_code(f_xa),
       fit_adj = f_xa, error = NULL)
}

 
# =================================================================
# fit_ordinal_by(): ordinal outcome ~ CATEGORICAL predictor
#   1. drops predictor levels with < min_n respondents
#   2. fits the clm problit model of y as a function of x
# =================================================================
fit_ordinal_by <- function(data, x, y,
                           min_n       = 20,
                           link        = "probit",
                           wrap_axis   = 12,
                           wrap_facet  = 20,
                           verbose     = TRUE) {
 
  stopifnot(all(c(x, y) %in% names(data)))
 
  # ---- 1. screen predictor levels -------------------------------
  dat <- data %>%
    dplyr::filter(!is.na(.data[[x]]), !is.na(.data[[y]])) %>%
    droplevels()
 
  if (!is.factor(dat[[x]])) {
    warning(x, " is not a factor; coercing with alphabetical level order.")
    dat[[x]] <- factor(dat[[x]])
  }
 
  lvl_n <- dat %>% dplyr::count(.data[[x]], name = "n")
  small <- lvl_n %>% dplyr::filter(n < min_n)
 
  if (nrow(small)) {
    if (verbose)
      message("Dropping ", nrow(small), " level(s) with n < ", min_n, ": ",
              paste(sprintf("%s (n = %d)", small[[x]], small$n), collapse = ", "))
    dat <- dat %>%
      dplyr::filter(!.data[[x]] %in% small[[x]]) %>%
      droplevels()
  }
 
  if (nlevels(dat[[x]]) < 2)
    stop("Fewer than 2 levels of ", x, " remain after screening.")
 
  # ---- 2. model: y as a function of x ---------------------------------------
  fit <- ordinal::clm(stats::as.formula(sprintf("%s ~ %s", y, x)),
                      data = dat, link = link)
 
  if (!.clm_usable(fit))
    warning("Final model convergence code ",
            paste(fit$convergence$code, collapse = "/"),
            ": code -3 means the thresholds are not increasing and the fit ",
            "is INVALID. Usually a sparsely populated outcome category - ",
            "check table(dat[[y]]) and collapse it, or refit with brms.",
            call. = FALSE)
 
  # proportional-odds and scale checks, with their own convergence codes
  assump <- .assumption_checks(fit, dat, x = x, y = y, link = link)

  lrt <- drop1(fit, test = "Chisq")
 
  # latent-scale contrasts: differences in latent residual SD units, so they
  # read as Cohen's d-like effects (Gambarota & Altoe 2024). The CI matters more than the p-value - it says how
  # large an effect can still be ruled out.
  emm_link  <- emmeans::emmeans(fit, stats::as.formula(paste("~", x)),
                                mode = "linear.predictor")
  contrasts <- confint(pairs(emm_link, adjust = "tukey"))
 
  if (verbose) {
    print(summary(fit)); print(lrt)
    cat("\nlatent-scale contrasts:\n"); print(contrasts)
  }
 
  # ---- predicted category probabilities + observed ---------------
  lv_y <- levels(dat[[y]])
 
  emm <- as.data.frame(
    marginaleffects::avg_predictions(fit, by = x, type = "prob"))
  emm[[y]] <- factor(as.character(emm$group), levels = lv_y)
 
  raw <- dat %>%
    dplyr::count(.data[[x]], .data[[y]], .drop = FALSE) %>%
    dplyr::group_by(.data[[x]]) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup()
  raw[[y]] <- factor(as.character(raw[[y]]), levels = lv_y)
 
  p <- ggplot2::ggplot(emm, ggplot2::aes(.data[[x]], estimate)) +
    ggplot2::geom_col(data = raw, ggplot2::aes(y = prop),
                      fill = "grey85", width = .7) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      colour = "firebrick", size = .3) +
    ggplot2::geom_text(data = raw, ggplot2::aes(y = Inf, label = n),
                       vjust = 1.4, size = 2.6, colour = "grey40") +
    ggplot2::facet_wrap(stats::as.formula(paste("~", y)), nrow = 1,
                        labeller = ggplot2::label_wrap_gen(wrap_facet)) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(.05, .12))) +
    ggplot2::scale_x_discrete(
      labels = function(z) scales::label_wrap(wrap_axis)(fig_label(z))) +
    ggplot2::labs(x = fig_var(x), y = "P(category)",
                  caption = "grey = observed; red = model estimate") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
 
  structure(list(
    x = x, y = y, n = nrow(dat), K_x = nlevels(dat[[x]]),
    dropped = small, level_n = lvl_n,
    data = dat, fit = fit, lrt = lrt,
    scale_test = assump$test_scale, po_test = assump$test_po, assump = assump,
    emm = emm, raw = raw, contrasts = contrasts, plot = p
  ), class = "ordinal_by")
}
 
 
print.ordinal_by <- function(z, ...) {
 
  cat(sprintf("\n%s ~ %s   (n = %d, %d levels)\n", z$y, z$x, z$n, z$K_x))
  if (nrow(z$dropped))
    cat("dropped:", paste(z$dropped[[z$x]], collapse = ", "), "\n")
 
  p <- z$lrt[["Pr(>Chi)"]][2]
  cat(sprintf("main effect   LRT chi2 = %.2f, df = %d, p %s\n",
              z$lrt[["LRT"]][2], z$lrt[["Df"]][2], fmt_p(p)))
 
  cat("\nlatent-scale contrasts (probit SD units):\n")
  print(as.data.frame(z$contrasts), row.names = FALSE, digits = 2)
  cat("\n")
  invisible(z)
}
 
 
# =================================================================
# fit_ordinal_by_continuous(): ordinal outcome ~ CONTINUOUS predictor
#
#   1. screens for sample size and predictor variability
#   2. fits the slope model on x alone (all respondents)
#   3. tests the EQUAL-SPACING assumption the slope makes: on one dataset
#      with sparse x values (n < min_n_level) dropped, a linear model and
#      a factor model are fitted and compared with anova(slope, factor).
#      The linear model is the factor model with the gaps between adjacent
#      x values constrained to be equal, so the two are nested and the LRT
#      (df = K - 2) tests exactly that constraint.
#   4. if the factor model is significantly better, the figure ($plot)
#      comes from the factor model (sparse levels dropped) and
#      $model_used = "factor"; the slope results are still returned.
#
# POWER: the equal-spacing test has LITTLE power here. Several x values
# hold only 20-40 respondents, and the test spends K - 2 df on the
# departures, so a non-significant result (p = .2-.9 in this survey) is
# weak evidence that spacing is equal - only that a large departure is
# unlikely. Treat it as a diagnostic, and read the figure's bars against
# the fitted curve as well.
#
# The model is a single continuous slope, but predictions are evaluated
# at the values of x that actually occur, so the figure matches the one
# from fit_ordinal_by(): bars = observed, points = model estimates,
# faceted by outcome category.
#
# The slope is on the predictor's own scale: a one-unit increase in x
# shifts the latent outcome by b latent SDs. If that is too small to
# read, rescale x to a meaningful unit before fitting (years / 10, say,
# reported per decade) rather than standardising - the SD of a predictor
# is a property of the sample, not of the quantity.
# =================================================================

fit_ordinal_by_continuous <- function(data, x, y,
                                      link            = "probit",
                                      min_n           = 20,
                                      min_distinct_x  = 6,
                                      wrap_facet      = 20,
                                      wrap_axis       = 12,
                                      discrete_axis   = TRUE,
                                      # ---- NEW ---------------------------
                                      # draw the model's fitted curve over
                                      # the observed bars
                                      show_line       = TRUE,
                                      # grid resolution for the smooth line;
                                      # only used when discrete_axis = FALSE
                                      n_grid          = 100,
                                      # the fitted line is drawn SOLID when
                                      # the LRT for x is below this and
                                      # DOTTED when it is not. Also the
                                      # level for the equal-spacing test.
                                      sig_level       = 0.05,
                                      # ---- equal-spacing test ------------
                                      # x values with fewer respondents than
                                      # this are dropped from BOTH models in
                                      # the spacing test (and from the factor
                                      # figure), never from the slope model
                                      min_n_level     = 20,
                                      # above this many distinct x values a
                                      # factor model is not sensible (x is
                                      # genuinely continuous); not tested
                                      spacing_max_levels = 10,
                                      # ------------------------------------
                                      verbose         = TRUE) {

  stopifnot(all(c(x, y) %in% names(data)))

  # ---- 1. prepare and screen ------------------------------------
  dat <- data %>%
    dplyr::filter(!is.na(.data[[x]]), !is.na(.data[[y]])) %>%
    droplevels()

  # x_labels keeps the original factor labels for the axis, so a coerced
  # ordinal predictor still reads legibly in the figure.
  x_labels <- NULL

  if (!is.numeric(dat[[x]])) {

    if (!is.ordered(dat[[x]]) && is.factor(dat[[x]]))
      stop(x, " is an UNORDERED factor. There is no defensible numeric ",
           "scoring - use fit_ordinal_by() instead, or make it ordered ",
           "first if the ordering is real.")

    num <- suppressWarnings(as.numeric(as.character(dat[[x]])))

    if (!anyNA(num)) {
      # labels are themselves numbers: use them directly
      message(x, " coerced from factor labels to numeric.")
      x_labels <- stats::setNames(as.character(dat[[x]]), as.character(num))
      x_labels <- x_labels[!duplicated(names(x_labels))]
      dat[[x]] <- num

    } else {
      # text labels on an ordered factor: score by level POSITION (1..K).
      # This imposes EQUAL SPACING between adjacent levels, which is
      # exactly the assumption the monotonic model exists to avoid - so
      # it is a deliberate simplification, not a neutral default.
      warning(x, " is an ordered factor with non-numeric labels; scoring it ",
              "by level position (1..", nlevels(dat[[x]]), "). This ASSUMES ",
              "EQUAL SPACING between adjacent levels. Consider the monotonic ",
              "model via mo(), which estimates the spacing instead of ",
              "assuming it, and compare the two.", call. = FALSE)
      x_labels <- stats::setNames(levels(dat[[x]]),
                                  as.character(seq_len(nlevels(dat[[x]]))))
      dat[[x]] <- as.integer(dat[[x]])
    }
  }

  if (!is.ordered(dat[[y]]))
    stop(y, " must be an ordered factor.")

  if (nrow(dat) < min_n)
    stop("Only ", nrow(dat), " complete cases; min_n = ", min_n, ".")

  n_distinct_x <- dplyr::n_distinct(dat[[x]])
  if (n_distinct_x < min_distinct_x)
    stop(x, " takes only ", n_distinct_x, " distinct values (min_distinct_x = ",
         min_distinct_x, "). Below this the equal-spacing assumption a slope ",
         "makes is doing real work - use fit_ordinal_by() with mo() or ",
         "factor coding instead, or lower min_distinct_x deliberately.")

  # NOTE on linearity. The cumulative model assumes x shifts the latent
  # variable LINEARLY. For a predictor with a handful of discrete values
  # that is the same as assuming EQUAL SPACING between adjacent values,
  # which is tested in step 3 against a factor model. (A spline test was
  # dropped earlier: it needs more distinct x values than this survey
  # has.) A non-significant spacing test is not proof of linearity - with
  # small groups it has little power - so the fitted curve is still drawn
  # over the observed proportions for a visual check.

  # ---- 2. model: x alone ----------------------------------------
  fit <- ordinal::clm(stats::as.formula(sprintf("%s ~ %s", y, x)),
                      data = dat, link = link)

  if (!.clm_usable(fit))
    warning("Final model convergence code ",
            paste(fit$convergence$code, collapse = "/"),
            ": code -3 means the thresholds are not increasing and the fit ",
            "is INVALID. Usually a sparsely populated outcome category - ",
            "check table(dat[[y]]) and collapse it, or refit with brms.",
            call. = FALSE)

  # proportional-odds and scale checks, with their own convergence codes
  assump <- .assumption_checks(fit, dat, x = x, y = y, link = link)

  lrt <- drop1(fit, test = "Chisq")

  # ---- effect size -----------------------------------------------
  b  <- stats::coef(fit)[x]
  ci <- stats::confint(fit)[x, ]
  slope <- tibble::tibble(term     = x,
                          estimate = unname(b),
                          conf.low = ci[[1]], conf.high = ci[[2]])

  # average marginal effect on the probability scale, per outcome category
  ame <- try(
    as.data.frame(marginaleffects::avg_slopes(fit, variables = x,
                                              type = "prob")),
    silent = TRUE)

  # ---- 3. equal-spacing test: anova(slope, factor) on ONE dataset ----
  # Sparse x values are dropped first, so the factor model is not
  # estimating a level from a handful of people. The linear model is
  # refitted on that SAME reduced dataset purely for this test - the
  # headline slope above keeps every respondent.
  spacing <- list(tested = FALSE, test = NULL, p = NA_real_, df = NA_integer_,
                  violated = FALSE, n = NA_integer_, dropped = NULL,
                  fit_linear = NULL, fit_factor = NULL, reason = NA_character_)

  x_n     <- dat %>% dplyr::count(.data[[x]], name = "n")
  small_x <- dplyr::filter(x_n, n < min_n_level)
  dat_sp  <- dplyr::filter(dat, !.data[[x]] %in% small_x[[x]])
  k_sp    <- dplyr::n_distinct(dat_sp[[x]])

  # dropped levels reported by their original labels when x was coerced
  lab_of <- function(v) if (is.null(x_labels)) as.character(v)
                        else unname(x_labels[as.character(v)])
  spacing$dropped <- tibble::tibble(level = lab_of(small_x[[x]]),
                                    n     = small_x$n)

  if (n_distinct_x > spacing_max_levels) {
    spacing$reason <- sprintf("not tested: %d distinct values > spacing_max_levels",
                              n_distinct_x)
  } else if (k_sp < 3) {
    # with 2 values a line and a factor are the same model (df = 0)
    spacing$reason <- sprintf(
      "not tested: only %d value(s) of %s with n >= %d", k_sp, x, min_n_level)
  } else {
    lv_x <- sort(unique(dat_sp[[x]]))
    dat_sp$.x_fac <- factor(dat_sp[[x]], levels = lv_x, labels = lab_of(lv_x))

    fit_lin_sp <- try(ordinal::clm(stats::as.formula(sprintf("%s ~ %s", y, x)),
                                   data = dat_sp, link = link), silent = TRUE)
    fit_fac_sp <- try(ordinal::clm(stats::as.formula(sprintf("%s ~ .x_fac", y)),
                                   data = dat_sp, link = link), silent = TRUE)

    if (.clm_usable(fit_lin_sp) && .clm_usable(fit_fac_sp)) {
      tst  <- anova(fit_lin_sp, fit_fac_sp)
      p_sp <- tst[["Pr(>Chisq)"]][2]
      spacing <- utils::modifyList(spacing, list(
        tested = TRUE, test = tst, p = p_sp, df = as.integer(tst$df[2]),
        violated = !is.na(p_sp) && p_sp < sig_level, n = nrow(dat_sp),
        fit_linear = fit_lin_sp, fit_factor = fit_fac_sp))
    } else {
      spacing$reason <- "not tested: linear or factor model did not converge"
      warning("Equal-spacing test for ", x, " not run: a model did not ",
              "converge.", call. = FALSE)
    }
  }

  # ---- 4. if equal spacing is rejected, use the factor model ----------
  # Same reduced dataset as the test, x as a factor carrying the original
  # labels. fit_ordinal_by() supplies the categorical fit, contrasts and
  # figure; its own min_n screen has nothing left to drop.
  factor_fit <- NULL
  if (spacing$violated) {
    dat_f <- dat_sp
    dat_f[[x]] <- dat_f$.x_fac
    dat_f$.x_fac <- NULL
    factor_fit <- try(fit_ordinal_by(dat_f, x = x, y = y,
                                     min_n = min_n_level, link = link,
                                     wrap_axis = wrap_axis,
                                     wrap_facet = wrap_facet,
                                     verbose = FALSE), silent = TRUE)
    if (inherits(factor_fit, "try-error")) {
      warning("Equal spacing rejected for ", x, " but the factor model ",
              "failed; falling back to the slope figure. ",
              conditionMessage(attr(factor_fit, "condition")), call. = FALSE)
      factor_fit <- NULL
    }
  }
  model_used <- if (is.null(factor_fit)) "slope" else "factor"

  spacing_txt <- if (spacing$tested)
    sprintf("equal spacing: LRT slope vs factor chi2 = %.2f, df = %d, p %s%s",
            spacing$test[["LR.stat"]][2], spacing$df, fmt_p(spacing$p),
            if (nrow(spacing$dropped))
              sprintf(" (n < %d dropped: %s)", min_n_level,
                      paste(spacing$dropped$level, collapse = ", "))
            else "")
  else paste("equal spacing:", spacing$reason)

  if (verbose) {
    print(summary(fit)); print(lrt)
    cat(sprintf("\nslope: %.3f [%.3f, %.3f] probit SD per unit of %s\n",
                slope$estimate, slope$conf.low, slope$conf.high, x))
    cat(spacing_txt, "\n")
    if (model_used == "factor")
      cat("-> factor model is significantly better; figure uses the factor model\n")
  }

  # ---- predicted probabilities at the observed values of x -------
  # do.call splices a named list in as an ordinary argument. datagrid()
  # does not support dynamic dots, so `!!x := grid` errors there.
  lv_y  <- levels(dat[[y]])
  x_val <- sort(unique(dat[[x]]))

  nd <- do.call(marginaleffects::datagrid,
                c(list(model = fit), stats::setNames(list(x_val), x)))

  emm <- as.data.frame(
    marginaleffects::predictions(fit, newdata = nd, type = "prob"))
  emm[[y]] <- factor(as.character(emm$group), levels = lv_y)

  raw <- dat %>%
    dplyr::count(.data[[x]], .data[[y]], .drop = FALSE) %>%
    dplyr::group_by(.data[[x]]) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup()
  raw[[y]] <- factor(as.character(raw[[y]]), levels = lv_y)

  # discrete_axis = TRUE spaces the values evenly, like the categorical
  # figure. Set FALSE when the numeric gaps are uneven (0, 1, 2, 5, 10),
  # since equal spacing would then misrepresent them.
  if (discrete_axis) {
    emm$.x_plot <- factor(emm[[x]], levels = x_val)
    raw$.x_plot <- factor(raw[[x]], levels = x_val)
  } else {
    emm$.x_plot <- emm[[x]]
    raw$.x_plot <- raw[[x]]
  }

  # ---- fitted curve ----------------------------------------------
  # This is both the model summary and the linearity diagnostic now that
  # the spline test is gone: systematic departure of the bars from the
  # curve is what a formal test would have been looking for.
  #
  # On a DISCRETE axis there is nowhere to put intermediate x values, so
  # the curve can only join the predictions at the observed values. The
  # pointranges already carry the interval at exactly those points, so no
  # ribbon is drawn - it would double-encode the same uncertainty.
  #
  # On a CONTINUOUS axis a fine grid shows the real shape. Note that a
  # probit model is linear on the LATENT scale but S-shaped in
  # probability, so the curve is not expected to be straight.
  # Line style encodes whether the effect of x is distinguishable from
  # zero, using the same LRT reported in $lrt rather than a separate test.
  # A dotted line still shows the model's fitted shape but signals that
  # the slope is not supported, so a reader is not invited to read a
  # trend into it.
  p_x       <- lrt[["Pr(>Chi)"]][2]
  x_sig     <- !is.na(p_x) && p_x < sig_level
  line_type <- if (x_sig) "solid" else "dotted"

  emm_line <- NULL
  if (show_line) {
    if (discrete_axis) {
      emm_line <- emm
    } else {
      x_grid <- seq(min(x_val), max(x_val), length.out = n_grid)
      nd_g <- do.call(marginaleffects::datagrid,
                      c(list(model = fit), stats::setNames(list(x_grid), x)))
      emm_line <- as.data.frame(
        marginaleffects::predictions(fit, newdata = nd_g, type = "prob"))
      emm_line[[y]]    <- factor(as.character(emm_line$group), levels = lv_y)
      emm_line$.x_plot <- emm_line[[x]]
    }
  }

  p <- ggplot2::ggplot(emm, ggplot2::aes(.x_plot, estimate)) +
    ggplot2::geom_col(data = raw, ggplot2::aes(y = prop),
                      fill = "grey85", width = .7)

  # added before the pointranges so the points sit on top of the curve
  if (!is.null(emm_line)) {

    if (!discrete_axis)
      p <- p + ggplot2::geom_ribbon(
        data = emm_line,
        ggplot2::aes(x = .x_plot, ymin = conf.low, ymax = conf.high,
                     group = 1),
        fill = "firebrick", alpha = .15, inherit.aes = FALSE)

    # group = 1 is required on a discrete axis: without it ggplot treats
    # each x position as its own group and draws no line at all
    # linetype is set OUTSIDE aes(): it is one fixed style for the whole
    # layer, not a mapping, so no legend is produced
    p <- p + ggplot2::geom_line(
      data = emm_line,
      ggplot2::aes(x = .x_plot, y = estimate, group = 1),
      colour = "firebrick", linewidth = .5, linetype = line_type,
      inherit.aes = FALSE)
  }

  p <- p +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      colour = "firebrick", size = .3) +
    ggplot2::geom_text(data = raw, ggplot2::aes(y = Inf, label = n),
                       vjust = 1.4, size = 2.6, colour = "grey40") +
    ggplot2::facet_wrap(stats::as.formula(paste("~", y)), nrow = 1,
                        labeller = ggplot2::label_wrap_gen(wrap_facet)) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(.05, .12))) +
    ggplot2::labs(
      x = fig_var(x), y = "P(category)",
      # p-values and the equal-spacing test are reported in the summary
      # table, not in the figure
      caption = sprintf(
        paste("grey = observed; red points = model estimate at the",
              "observed values of %s\nfitted line: solid if p < %s,",
              "dotted otherwise"),
        fig_var(x, FALSE), sub("^0", "", format(sig_level)))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # put the original factor labels back on the axis when x was coerced
  if (discrete_axis && !is.null(x_labels))
    p <- p + ggplot2::scale_x_discrete(
      labels = scales::label_wrap(wrap_axis)(
        fig_label(x_labels[as.character(x_val)])))

  # the figure that gets saved: the factor model's when equal spacing was
  # rejected, otherwise the slope figure. Both are returned.
  plot_out <- if (model_used == "factor")
    factor_fit$plot +
      ggplot2::labs(
        x = fig_var(x),
        caption = sprintf(
          paste("grey = observed; red = FACTOR-model estimate",
                "(equal spacing rejected, so %s is treated as categorical;",
                "see the summary table)"),
          fig_var(x, FALSE)))
  else p

  structure(list(
    x = x, y = y, n = nrow(dat), n_distinct_x = n_distinct_x,
    x_labels = x_labels,
    data = dat, fit = fit,
    slope = slope, ame = ame, lrt = lrt,
    # what the figure's line style was based on, so it can be reported
    p_x = p_x, x_sig = x_sig, line_type = line_type, sig_level = sig_level,
    scale_test = assump$test_scale, po_test = assump$test_po, assump = assump,
    # equal-spacing test and, when it was rejected, the factor model
    spacing = spacing, model_used = model_used, factor_fit = factor_fit,
    discrete_axis = discrete_axis,
    emm = emm, emm_line = emm_line, raw = raw,
    plot = plot_out, plot_slope = p
  ), class = "ordinal_by_cont")
}


print.ordinal_by_cont <- function(z, ...) {

  cat(sprintf("\n%s ~ %s   (n = %d, %d distinct values of %s)\n",
              z$y, z$x, z$n, z$n_distinct_x, z$x))

  s <- z$slope
  cat(sprintf("slope   b = %.3f [%.3f, %.3f]  (probit SD per unit of %s)\n",
              s$estimate, s$conf.low, s$conf.high, z$x))

  p <- z$lrt[["Pr(>Chi)"]][2]
  cat(sprintf("        LRT chi2 = %.2f, df = %d, p %s\n",
              z$lrt[["LRT"]][2], z$lrt[["Df"]][2], fmt_p(p)))

  cat("\n")
  sp <- z$spacing
  if (sp$tested) {
    cat(sprintf("equal spacing   LRT slope vs factor chi2 = %.2f, df = %d, p %s (n = %d)\n",
                sp$test[["LR.stat"]][2], sp$df, fmt_p(sp$p), sp$n))
    if (nrow(sp$dropped))
      cat("                dropped for the test:",
          paste(sprintf("%s (n = %d)", sp$dropped$level, sp$dropped$n),
                collapse = ", "), "\n")
  } else {
    cat("equal spacing  ", sp$reason, "\n")
  }
  cat("model used for figure:", z$model_used, "\n\n")

  invisible(z)
}


# =================================================================
# 4. HELPERS: select-all-that-apply (Y/N) outcomes
# =================================================================
# Each respondent contributes one row per option. The person-level random
# intercept captures a general propensity to tick boxes; question is kept
# FIXED because the options are substantively distinct, not exchangeable
# indicators of one construct. Structurally related to a Rasch model, but
# unidimensionality is not assumed and IRT quantities are not reported.

# ---- expand to long, filling in the implicit "No" ----------------
expand_select_all <- function(survey, q, id_col = "response_id", x = NULL) {

  svy <- survey %>% dplyr::rename(ResponseId = dplyr::all_of(id_col))

  # the chosen options, one row per respondent x selected option
  yes <- prep_select_all(svy, q = q, summarize = FALSE) %>%
    dplyr::rename(!!q := value) %>%
    dplyr::left_join(svy %>% dplyr::select(-dplyr::all_of(q)), by = "ResponseId") %>%
    dplyr::mutate(response = "Yes")

  if (nrow(yes) == 0) stop("prep_select_all() returned no rows for q = ", q)

  # Respondent universe: people who ticked at least one box. Appropriate
  # here because every question offered a "none of the above" option, so
  # someone who selected nothing skipped the question.
  ids <- unique(yes$ResponseId)

  no <- tidyr::expand_grid(
      ResponseId = ids,
      !!q := unique(yes[[q]])
    ) %>%
    dplyr::anti_join(yes, by = c("ResponseId", q)) %>%
    dplyr::mutate(response = "No") %>%
    dplyr::left_join(svy %>% dplyr::select(-dplyr::all_of(q)), by = "ResponseId")

  out <- dplyr::bind_rows(yes, no) %>%
    dplyr::mutate(
      question_label   = as.character(.data[[q]]),            # original text
      question         = factor(make.names(question_label)),  # model-safe name
      response_numeric = as.integer(response == "Yes"))

  if (!is.null(x))
    out <- out %>% dplyr::filter(!is.na(.data[[x]])) %>% droplevels()

  out
}


# ---- strip the "none" option and resolve conflicts ---------------
# "None of the above" is logically exclusive: selecting it entails No on
# every other item, so it is a deterministic function of the others and
# carries no extra information. It is dropped from the item set rather
# than reverse-coded, and respondents whose answers contradict it are
# removed. See https://doi.org/10.1111/j.0006-341X.2004.00147.x
# on why the assumptoin of independence is wrong so need more complicated models.
clean_select_all <- function(long, q, none_pattern, quiet = FALSE) {

  none_rows <- long %>%
    dplyr::filter(grepl(none_pattern, .data[[q]]), response == "Yes")

  long <- long %>% dplyr::filter(!grepl(none_pattern, .data[[q]]))

  yes_ids   <- long %>% dplyr::filter(response == "Yes") %>% dplyr::pull(ResponseId)
  conflicts <- unique(none_rows$ResponseId[none_rows$ResponseId %in% yes_ids])

  if (length(conflicts)) {
    if (!quiet)
      message("Dropping ", length(conflicts),
              " respondent(s) with conflicting yes/none answers.")
    long <- long %>% dplyr::filter(!ResponseId %in% conflicts)
  }

  attr(long, "conflicts") <- conflicts
  long
}


# ---- omnibus test on total endorsements per person ----------------
# Collapses the item-level data to counts: "how many boxes did you tick,
# out of those offered?" rather than "which ones". A sum of Bernoullis
# sharing one probability IS a Binomial, so this is the same likelihood
# as the item-level model minus any item-specific term. cbind(k, n - k)
# supplies the correct denominator when types hold different numbers of
# items - a raw count would make the larger type look higher by default.
# For a continuous x the term is a slope.
#
# `adj` (a person-level column, e.g. centred career stage) is added as a
# covariate for the sensitivity fits; with q_type it is allowed its own
# effect per type (adj * q_type), mirroring question * adj item-level.
.omnibus_counts <- function(data, x, id, q_type = NULL, adj = NULL,
                            quiet = FALSE) {

  cnt <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id, x, adj, q_type)))) %>%
    dplyr::summarise(k = sum(response_numeric), n = dplyr::n(),
                     .groups = "drop")

  if (is.null(q_type)) {

    rhs <- paste(c(x, adj), collapse = " + ")

    # one row per person, so no clustering remains and a plain glm is right
    m_o <- stats::glm(stats::as.formula(sprintf("cbind(k, n - k) ~ %s", rhs)),
                      family = binomial, data = cnt)

    # person-level heterogeneity still makes counts overdispersed relative
    # to binomial, which would make the SEs too small
    disp <- sum(stats::residuals(m_o, type = "pearson")^2) /
            stats::df.residual(m_o)

    omni <- list(kind = "binomial_glm", x = x, adj = adj, counts = cnt,
                 fit = m_o, test = drop1(m_o, test = "Chisq"),
                 dispersion = disp)

    if (disp > 1.5) {
      if (!quiet)
        warning("Omnibus counts are overdispersed (dispersion = ",
                round(disp, 2), "); the quasibinomial test is the one to ",
                "report. See $omni$test_quasi.", call. = FALSE)
      m_q <- stats::update(m_o, family = stats::quasibinomial)
      omni$fit_quasi  <- m_q
      omni$test_quasi <- drop1(m_q, test = "F")
    }
    return(omni)
  }

  a <- if (is.null(adj)) "" else sprintf(" + %s * %s", adj, q_type)
  f_type <- stats::as.formula(sprintf("cbind(k, n - k) ~ %s%s", q_type, a))
  f_add  <- stats::as.formula(sprintf("cbind(k, n - k) ~ %s + %s%s", x, q_type, a))
  f_int  <- stats::as.formula(sprintf("cbind(k, n - k) ~ %s * %s%s", x, q_type, a))

  m_type <- stats::glm(f_type, family = binomial, data = cnt)
  m_add  <- stats::glm(f_add,  family = binomial, data = cnt)
  m_int  <- stats::glm(f_int,  family = binomial, data = cnt)

  # Each person contributes one row per type, so the rows are clustered.
  # Cluster-robust SEs make no assumption about the within-person
  # correlation structure and always converge, unlike the GLMM. They
  # also absorb the overdispersion from person heterogeneity.
  # NOTE: likelihood ratio tests are NOT valid under a robust vcov, so
  # the omnibus tests here are Wald tests.
  V_int <- sandwich::vcovCL(m_int, cluster = cnt[[id]])
  V_add <- sandwich::vcovCL(m_add, cluster = cnt[[id]])

  list(
    kind    = "clustered_glm", x = x, adj = adj,
    counts  = cnt, fit = m_int, fit_add = m_add, vcov = V_int,
    coefs   = lmtest::coeftest(m_int, vcov. = V_int),
    test_x  = lmtest::waldtest(m_type, m_add, vcov = V_add, test = "Chisq"),
    test    = lmtest::waldtest(m_add,  m_int, vcov = V_int, test = "Chisq"))
}

# p-values out of an .omnibus_counts() result:
#   p_total    does total endorsement differ with x?
#   p_valence  (q_type only) does the positive/negative balance shift?
# The quasibinomial test reports an F column, the binomial a Chisq one,
# so the p column is found by prefix; the row is found by NAME, since an
# adjusted fit has more than one term.
.omni_p <- function(o) {
  if (is.null(o)) return(list(p_total = NA_real_, p_valence = NA_real_))
  if (o$kind == "binomial_glm") {
    tst <- if (is.null(o$test_quasi)) o$test else o$test_quasi
    list(p_total   = tst[o$x, grep("^Pr", names(tst))[1]],
         p_valence = NA_real_)
  } else {
    list(p_total   = o$test_x[["Pr(>Chisq)"]][2],
         p_valence = o$test[["Pr(>Chisq)"]][2])
  }
}

# glmer convergence / singularity as one string; "" when clean
.glmer_flag <- function(m) {
  if (inherits(m, "try-error") || is.null(m)) return("failed")
  msg <- m@optinfo$conv$lme4$messages
  paste(c(if (lme4::isSingular(m)) "singular", msg), collapse = "; ")
}


# ---- shared select-all caption text --------------------------------
sa_dagger_note <- paste0(
  "\n† specific examples removed from the panel title here; ",
  "see the figure caption for the full wording")

sa_cap_model <- function(pred_re_form) {
  if (is.null(pred_re_form))
    "red = model estimate (population average)"
  # The typical-respondent estimate is a valid quantity, but it is NOT the
  # same scale as the bars: averaging over people pulls the observed
  # proportions TOWARDS 50%, so the red points sit further from 50% than
  # the bars by construction, and gaps between groups look larger than in
  # the raw data. Kept out of the caption for space; state it in the
  # methods / figure legends of the paper.
  else if (length(pred_re_form) == 1 && is.na(pred_re_form))
    "red = model estimate for a typical respondent (random effect = 0)"
  else
    "red = model estimate"
}


# ---- figure for an ordered / continuous x ---------------------------
# Draws from a fit_select_all() result without touching the model, so
# run_select_all() can redraw cached fits when the styling changes.
# Line style: solid if the item's slope has p < .05 (FDR across the items of
# this figure), dotted otherwise. The slopes, as odds ratios, are in $trends.
plot_select_all_ordered <- function(res,
                                    wrap_facet = 38, facet_max_lines = 2,
                                    strip_size = 12, wrap_axis = 20) {
  x        <- res$x
  x_labels <- res$x_labels
  lab_map  <- res$labels
  raw      <- res$raw

  # no slope available (emtrends failed): solid, so nothing reads as null
  emm_line <- res$emm_line
  p_item   <- if (is.null(res$trends)) rep(NA_real_, nrow(emm_line)) else
    res$trends$p.value[match(as.character(emm_line$question),
                             as.character(res$trends$question))]
  emm_line$.line <- ifelse(is.na(p_item) | p_item < .05, "solid", "dotted")

  # bar width from the tightest gap between observed values, so bars
  # neither overlap nor leave the axis looking empty
  ux <- sort(unique(res$data[[x]]))
  bw <- if (length(ux) > 1) 0.7 * min(diff(ux)) else 0.7

  # smallest wrap width >= wrap_facet at which every title fits in
  # facet_max_lines lines (label_wrap() and strwrap() break the same way)
  facet_w <- max(wrap_facet, vapply(lab_map, function(s) {
    w <- wrap_facet
    while (length(strwrap(s, w)) > facet_max_lines) w <- w + 1
    w
  }, numeric(1)))

  # Axis: a coerced ordered factor keeps its original level text at the
  # positions it was scored to; a genuinely numeric x gets pretty breaks.
  x_scale <- if (!is.null(x_labels))
    ggplot2::scale_x_continuous(
      breaks       = as.numeric(names(x_labels)),
      labels       = scales::label_wrap(wrap_axis)(fig_label(unname(x_labels))),
      minor_breaks = NULL)
  else
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(5))

  p <- ggplot2::ggplot(emm_line, ggplot2::aes(.data[[x]], prob)) +
    ggplot2::geom_col(data = raw, ggplot2::aes(y = prop),
                      fill = "grey85", width = bw) +
    # a ribbon + line rather than pointranges: the model is a smooth
    # function of x, and drawing it as isolated points would hide that.
    # Drawn from the fine grid, so it is the model's actual curve rather
    # than straight segments between the observed values.
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = asymp.LCL, ymax = asymp.UCL),
      fill = "firebrick", alpha = .18) +
    ggplot2::geom_line(ggplot2::aes(linetype = .line),
                       colour = "firebrick", linewidth = .6) +
    ggplot2::scale_linetype_identity()

  # model estimate at each observed value, to read against its bar
  if (res$pred_at_observed)
    p <- p + ggplot2::geom_point(data = res$emm, colour = "firebrick", size = 1.4)

  p <- p +
    ggplot2::geom_text(data = raw, ggplot2::aes(y = Inf, label = n),
                       vjust = 1.4, size = 2.6, colour = "grey40") +
    ggplot2::facet_wrap(
      ~ question,
      labeller = ggplot2::labeller(
        question = function(z) scales::label_wrap(facet_w)(lab_map[z]))) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(.05, .12))) +
    # continuous, NOT scale_x_discrete
    x_scale +
    ggplot2::labs(x = fig_var(x), y = "P(yes)",
                  caption = paste0(
                    "grey = observed; ", sa_cap_model(res$pred_re_form),
                    "\nsolid line = item's slope p < .05 (FDR across items); ",
                    "dotted = p >= .05",
                    if (any(grepl("†", lab_map))) sa_dagger_note)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = strip_size, lineheight = .9,
                                         margin = ggplot2::margin(b = 4)))

  # level text is usually long; angle it like the categorical figure
  if (!is.null(x_labels))
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  p
}


# ---- GLMM + summaries + plot -------------------------------------
fit_select_all <- function(data, x, id = "ResponseId",
                           q_type = NULL,          # column grouping questions
                           re_structure = NULL,
                           # "continuous"  force continuous; an ordered or
                           #               unordered factor is coerced with a
                           #               warning (see .as_numeric_x below)
                           # "categorical" force categorical; a numeric is
                           #               coerced to an unordered factor
                           x_type = c("continuous", "categorical"),
                           # below this many distinct values a continuous x is
                           # warned about but still fitted as continuous
                           min_distinct_x = 4,
                           # at or below this many distinct values, re.form is
                           # honoured and points are drawn at the OBSERVED x
                           # values; above it, only the smooth curve is drawn
                           # and re.form = NA is used (see PREDICTIONS)
                           grid_threshold = 15,
                           # points on the smooth prediction curve. n_grid is
                           # used when random effects are set to zero (cheap);
                           # n_grid_avg when predictions are averaged over every
                           # respondent at each grid point (n_rows x n_grid_avg
                           # predictions, so keep it modest)
                           n_grid = 100,
                           n_grid_avg = 30,
                           # ------------------------------------------------

                           # panel titles: wrapped at wrap_facet characters,
                           # widened just enough that no title needs more
                           # than facet_max_lines lines; strip_size in pt.
                           # 38 characters at 12 pt fills a panel of a
                           # 4-column figure saved at 18 x 14 in, scale 0.8.
                           wrap_facet = 38, facet_max_lines = 2,
                           strip_size = 12,
                           # replace "(e.g. ...)" in panel titles with a dagger and
                           # say so in the caption, to keep titles short
                           strip_examples = TRUE,
                           wrap_axis = 20,
                           order_facets = TRUE,
                           # ignored when x is continuous: a compact letter
                           # display compares groups, and a covariate has
                           # none. Per-question slopes are returned in
                           # $trends instead, and $cld stays NULL.
                           add_letters  = TRUE,
                           letter_adjust = "mvt",  # accounts for the
                           # correlation among contrasts; "sidak" assumes
                           # independence and is conservative. Tukey is
                           # unavailable once `by` splits this into one
                           # family per question.
                           seed_mvt = 12345,
                           re.form  = NULL,
                           # equal-spacing diagnostic for a continuous x:
                           # x values with fewer respondents than
                           # spacing_min_n are dropped from both models of
                           # the test; not run above spacing_max_levels
                           spacing_test       = TRUE,
                           spacing_min_n      = 20,
                           spacing_max_levels = 10,
                           verbose  = TRUE) {

  stopifnot(all(c("question", "response_numeric", x, id) %in% names(data)))
  data <- data %>% dplyr::filter(!is.na(.data[[x]])) %>% droplevels()

  # =================================================================
  # DECIDE AND ENFORCE THE TYPE OF x
  # =================================================================
  # Everything downstream - the empty-cell check, emmeans, the letter
  # join, the x scale - branches on this one flag. Previously the model
  # was fitted continuously while every summary treated x as a factor,
  # and `levels()` of a numeric being NULL turned the join key to NA.
  x_type <- match.arg(x_type)

  # numeric-looking levels ("0", "1", "2") become those numbers; otherwise
  # the level INDEX is used, which assumes equal spacing between levels
  .as_numeric_x <- function(v, nm) {
    if (is.numeric(v)) return(v)
    lab <- as.character(v)
    num <- suppressWarnings(as.numeric(lab))
    if (!any(is.na(num))) {
      warning(nm, " is a ", class(v)[1],
              " with numeric labels; using those values as the scale.",
              call. = FALSE)
      return(num)
    }
    warning(nm, " is a ", class(v)[1],
            " without numeric labels; using the level index (1..",
            nlevels(factor(v)), ") and so ASSUMING equal spacing ",
            "between adjacent levels.", call. = FALSE)
    as.integer(factor(v, levels = levels(factor(v))))
  }

  x_was_coerced <- FALSE

  # number -> original label, so a coerced ordered factor keeps its level
  # text on the x axis instead of showing 1..K. NULL when x was numeric to
  # begin with (the axis then uses ordinary pretty breaks).
  x_labels <- NULL

  if (x_type == "continuous") {
    if (!is.numeric(data[[x]])) {
      x_orig    <- data[[x]]
      data[[x]] <- .as_numeric_x(x_orig, x)
      x_map     <- unique(data.frame(num = data[[x]],
                                     lab = as.character(x_orig)))
      x_map     <- x_map[order(x_map$num), ]
      x_labels  <- stats::setNames(x_map$lab, x_map$num)
      x_was_coerced <- TRUE
    }
    n_distinct_x <- dplyr::n_distinct(data[[x]])
    if (n_distinct_x < min_distinct_x)
      warning(x, " takes only ", n_distinct_x, " distinct values; a ",
              "continuous slope is fitted but x_type = \"categorical\" ",
              "may describe the data better.", call. = FALSE)
  } else {
    if (is.numeric(data[[x]])) {
      data[[x]] <- factor(data[[x]])          # ordered -> unordered on purpose:
      x_was_coerced <- TRUE                   # treatment contrasts, not poly
    } else if (is.ordered(data[[x]])) {
      # an ordered factor in a glmer gets POLYNOMIAL contrasts, so the
      # "linear" coefficient is an orthogonal polynomial term, not a
      # per-level contrast. Drop the ordering to get treatment contrasts.
      # this affects only the coefficients tables not the predictions or overall
      # model stats itsself
      data[[x]] <- factor(data[[x]], levels = levels(data[[x]]),
                          ordered = FALSE)
      x_was_coerced <- TRUE
    } else {
      data[[x]] <- factor(data[[x]])
    }
    n_distinct_x <- nlevels(data[[x]])
  }

  is_cont <- identical(x_type, "continuous")

  # convenience: NULL for continuous, so the old `levels(data[[x]])` calls
  # can be replaced by this and guarded on it
  x_levels <- if (is_cont) NULL else levels(data[[x]])

  # =================================================================
  # SANITY CHECK BEFORE FITTING
  # =================================================================
  # A crosstab is only meaningful for a factor. For a continuous x the
  # equivalent question is whether every question spans enough of the x
  # range to estimate a slope.
  if (is_cont) {
    tab <- data %>%
      dplyr::group_by(question) %>%
      dplyr::summarise(n = dplyr::n(),
                       n_distinct_x = dplyr::n_distinct(.data[[x]]),
                       min_x = min(.data[[x]]), max_x = max(.data[[x]]),
                       n_yes = sum(response_numeric),
                       .groups = "drop")

    thin <- dplyr::filter(tab, n_distinct_x < 2)
    if (nrow(thin))
      warning("No variation in ", x, " within: ",
              paste(thin$question, collapse = ", "),
              "; the question-specific slope is not estimable.",
              call. = FALSE)

    # a question answered "yes" by everyone or no one gives a separated
    # slope, which is the usual cause of a huge SE or a Hessian warning
    flat <- dplyr::filter(tab, n_yes == 0 | n_yes == n)
    if (nrow(flat))
      warning("No variation in the RESPONSE within: ",
              paste(flat$question, collapse = ", "),
              "; the slope is separated.", call. = FALSE)
  } else {
    # make sure there are some individuals in every x group
    tab <- table(data[[x]], data$question)
    if (any(tab == 0))
      warning("Empty cells in ", x, " x question; estimates may be unstable.")
  }

  # `question * x` rather than `0 + question + question:x`: identical fit,
  # but prediction helpers, drop1() and type-III tests all behave.
  # form <- stats::as.formula(
  #   sprintf("response_numeric ~ question * %s + (1 | %s)", x, id))

  re_choices <- c("nested", "correlated", "uncorrelated")

  if (is.null(q_type)) {
    # the RE term is always (1 | id) with no q_type, so no structure applies
    if (!is.null(re_structure))
      warning("re_structure is ignored when q_type is NULL; the ",
              "random-effect term is (1 | ", id, ").", call. = FALSE)
    re_structure <- NA_character_

  } else {
    if (is.null(re_structure))
      stop("q_type = \"", q_type, "\" requires an explicit re_structure, ",
           "one of: ", paste(re_choices, collapse = ", "),
           ".\nThey differ in the between-type correlation they permit, ",
           "which is a substantive choice, so there is no default.",
          call. = FALSE)

    if (!is.character(re_structure) || length(re_structure) != 1L ||
        is.na(re_structure) || !re_structure %in% re_choices)
      stop("re_structure must be exactly one of: ",
           paste(re_choices, collapse = ", "), ".\nGot: ",
           paste(deparse(re_structure), collapse = " "), call. = FALSE)
  }

  # with correlated you get  (0 + q_type | ResponseId), which I think is what you want
  # eg we expect people with a lot of neg reasons will find fewer positive reasons
  # with uncorrelated you get  (0 + q_type || ResponseId)

  # engagement plus idiosyncratic leanings,
  # ie some people are just box tickers
  # with nested you get  (1 | ResponseId) + (1 | ResponseId:q_type)

  re_term <- if (is.null(q_type)) {
    sprintf("(1 | %s)", id)
  } else {
    stopifnot(q_type %in% names(data))
    # each question must belong to exactly one type, or the nesting is
    # not well defined
    tt <- table(data$question, data[[q_type]])
    if (any(apply(tt, 1, \(r) sum(r > 0)) != 1))
      stop("Each question must map to exactly one level of ", q_type, ".")

    switch(re_structure,
           nested       = sprintf("(1 | %s) + (1 | %s:%s)", id, id, q_type),
           correlated   = sprintf("(0 + %s | %s)",  q_type, id),
           uncorrelated = sprintf("(0 + %s || %s)", q_type, id),
           #switch() returns
           # NULL invisibly on no match, which would fail obscurely later
            stop("unhandled re_structure: ", re_structure, call. = FALSE))
  }

  form <- stats::as.formula(
    sprintf("response_numeric ~ question * %s + %s", x, re_term))

  fit <- lme4::glmer(form, family = binomial, data = data,
                     control = lme4::glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 2e5)))

  # LRT. drop1 respects marginality, so it tests
  # only the interaction.
  chisq_test <- drop1(fit, test = "Chisq")

  # ---- omnibus test on total endorsements per person -------------
  # see .omnibus_counts() for the model and why
  omni <- .omnibus_counts(data, x = x, id = id, q_type = q_type)

  # ---- equal-spacing diagnostic (continuous x only) --------------
  # A coerced ordered factor is scored 1..K, which ASSUMES equal spacing
  # between adjacent levels. As in fit_ordinal_by_continuous(): on one
  # dataset with sparse x values (< spacing_min_n respondents) dropped,
  # the item-level slope model (question * x) is compared with the factor
  # model (question * factor(x)) by LRT; df = n_items * (K - 2).
  # DIAGNOSTIC ONLY - the slope model is still the one reported and
  # plotted. Low power, as there: a non-significant p is weak evidence.
  spacing <- list(tested = FALSE, test = NULL, p = NA_real_, df = NA_integer_,
                  n_id = NA_integer_, dropped = tibble::tibble(level = character(0),
                                                               n = integer(0)),
                  conv = NA_character_, reason = NA_character_)

  if (is_cont && spacing_test) {
    x_n <- data %>%
      dplyr::distinct(.data[[id]], .data[[x]]) %>%
      dplyr::count(.data[[x]], name = "n")
    small_x <- dplyr::filter(x_n, n < spacing_min_n)
    d_sp    <- dplyr::filter(data, !.data[[x]] %in% small_x[[x]]) %>% droplevels()
    k_sp    <- dplyr::n_distinct(d_sp[[x]])

    lab_of <- function(v) if (is.null(x_labels)) as.character(v)
                          else unname(x_labels[as.character(v)])
    spacing$dropped <- tibble::tibble(level = lab_of(small_x[[x]]),
                                      n     = small_x$n)

    if (n_distinct_x > spacing_max_levels) {
      spacing$reason <- sprintf("not tested: %d distinct values > spacing_max_levels",
                                n_distinct_x)
    } else if (k_sp < 3) {
      spacing$reason <- sprintf("not tested: only %d value(s) of %s with n >= %d",
                                k_sp, x, spacing_min_n)
    } else {
      d_sp$.x_fac <- factor(d_sp[[x]])
      ctrl <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
      fit_lin_sp <- if (nrow(small_x) == 0) fit else
        try(lme4::glmer(form, family = binomial, data = d_sp, control = ctrl),
            silent = TRUE)
      fit_fac_sp <- try(lme4::glmer(
        stats::as.formula(sprintf("response_numeric ~ question * .x_fac + %s",
                                  re_term)),
        family = binomial, data = d_sp, control = ctrl), silent = TRUE)

      if (!inherits(fit_lin_sp, "try-error") && !inherits(fit_fac_sp, "try-error")) {
        # LRT by hand: anova() refuses fits whose calls name different
        # data objects (data vs d_sp), even when the rows are identical
        stopifnot(stats::nobs(fit_lin_sp) == stats::nobs(fit_fac_sp))
        ll_l <- stats::logLik(fit_lin_sp); ll_f <- stats::logLik(fit_fac_sp)
        df_sp <- attr(ll_f, "df") - attr(ll_l, "df")
        chi   <- max(0, 2 * (as.numeric(ll_f) - as.numeric(ll_l)))
        tst   <- tibble::tibble(chisq = chi, df = df_sp,
                                p = stats::pchisq(chi, df_sp, lower.tail = FALSE))
        spacing <- utils::modifyList(spacing, list(
          tested = TRUE, test = tst, p = tst$p,
          df = as.integer(df_sp),
          n_id = dplyr::n_distinct(d_sp[[id]]),
          conv = .glmer_flag(fit_fac_sp)))
      } else {
        spacing$reason <- "not tested: linear or factor model failed"
      }
    }
    if (verbose)
      cat(if (spacing$tested)
            sprintf("\nequal spacing (diagnostic): LRT slope vs factor df = %d, p %s\n",
                    spacing$df, fmt_p(spacing$p))
          else paste("\nequal spacing:", spacing$reason, "\n"))
  }

  if (verbose) {
    cat("\n--- x treated as ", x_type,
        if (x_was_coerced) " (coerced) " else " ",
        "---\n", sep = "")
    print(summary(fit))
    print(chisq_test)
    print(lme4::VarCorr(fit))
    cat("singular:", lme4::isSingular(fit), "\n")
    # ICC assumes a single random intercept. With per-type slopes or a
    # nested term the variance decomposition is not a single number, so
    # report VarCorr instead and interpret the correlations directly.
    if (is.null(q_type)) print(performance::icc(fit))

    cat("\n--- omnibus test on total endorsements (", omni$kind, ") ---\n", sep = "")
    if (omni$kind == "binomial_glm") {
      cat("dispersion:", round(omni$dispersion, 2), "\n")
      print(if (is.null(omni$test_quasi)) omni$test else omni$test_quasi)
    } else {
      print(omni$coefs)
      cat("\nmain effect of ", x, ":\n", sep = ""); print(omni$test_x)
      cat("\n", x, " x ", q_type, " interaction:\n", sep = ""); print(omni$test)
    }
  }

  # =================================================================
  # PREDICTIONS
  # =================================================================
  # re.form = NULL uses the fitted random effects and averages, giving the
  # probability that is comparable to the observed bars.
  # re.form = NA sets them to zero, giving the prediction for a TYPICAL
  # respondent, which on a nonlinear link is pulled away from the raw
  # proportions (more so the further from 50%).
  # Caveat: with re.form = NULL the intervals reflect fixed-effect
  # uncertainty only - marginaleffects warns about this.
  #
  # CONTINUOUS x: why the curve is not a straight line, and how it is drawn
  # -------------------------------------------------------------------
  # The model is linear in x on the LOGIT scale. On the probability scale
  # plogis(a + b * x) is an S-curve, so a straight line is not expected:
  # it is steepest near 50% and flattens towards 0% and 100%.
  #
  # Curve is evaluated on a fine grid that includes the observed
  # values (so the points sit exactly on it):
  #   re.form = NA    fixed effects only: the typical-respondent curve,
  #                   exactly linear on the logit scale.
  #   re.form = NULL  counterfactual (standardised) average: EVERY
  #                   respondent is set to each grid value in turn and the
  #                   predictions are averaged, so each point averages over
  #                   the same people. The population-averaged curve is
  #                   flatter than the typical-respondent one and is not
  #                   exactly linear on the logit scale either - that is a
  #                   property of averaging a nonlinear function, not noise.
  # With more than grid_threshold distinct values only the curve is drawn,
  # with re.form = NA.
  .re_zero <- function(r) !is.null(r) && !inherits(r, "formula") &&
    length(r) == 1L && is.na(r)

  pred_at_observed <- !is_cont || n_distinct_x <= grid_threshold
  emm_line <- NULL   # continuous only: the smooth curve

  .tidy_pred <- function(p)
    as.data.frame(p) %>%
      dplyr::rename(prob = estimate, asymp.LCL = conf.low, asymp.UCL = conf.high)

  if (!is_cont) {
    emm <- .tidy_pred(
      marginaleffects::avg_predictions(
        fit, by = c("question", x), re.form = re.form))
    pred_re_form <- re.form

  } else {
    x_obs        <- sort(unique(data[[x]]))
    pred_re_form <- if (pred_at_observed) re.form else NA
    grid_n       <- if (.re_zero(pred_re_form)) n_grid else n_grid_avg
    x_grid       <- sort(unique(c(x_obs, seq(min(x_obs), max(x_obs),
                                             length.out = grid_n))))

    if (.re_zero(pred_re_form)) {
      nd <- do.call(
        marginaleffects::datagrid,
        c(list(model = fit, question = unique(as.character(data$question))),
          stats::setNames(list(x_grid), x)))
      emm_line <- .tidy_pred(
        marginaleffects::predictions(fit, newdata = nd, re.form = NA))
    } else {
      emm_line <- .tidy_pred(
        marginaleffects::avg_predictions(
          fit, variables = stats::setNames(list(x_grid), x),
          by = c("question", x), re.form = pred_re_form))
    }

    # points at the observed values, taken from the same curve
    emm <- if (pred_at_observed)
      emm_line[round(emm_line[[x]], 10) %in% round(x_obs, 10), , drop = FALSE]
    else emm_line
  }

  # =================================================================
  # CONTRASTS: letters for categorical, slopes for continuous
  # =================================================================
  # Mutually exclusive by design: cld_df is populated only for a
  # categorical x, trends_df only for a continuous one.
  cld_df    <- NULL     # categorical: letter display, joined onto `raw`
  trends_df <- NULL     # continuous: one slope per question, annotated

  if (add_letters && !is_cont) {

    # Pairwise contrasts WITHIN each question; groups sharing a letter do
    # not differ. No adjustment ACROSS questions - each item is treated as
    # its own family, which must be stated in the methods.
    emm_link <- emmeans::emmeans(
      fit, stats::as.formula(paste("~", x, "| question")),
      nesting = NULL)                   # treat x and question as crossed

    set.seed(seed_mvt)                  # mvt is simulation-based
    cld_df <- try(
      multcomp::cld(emm_link, adjust = letter_adjust, Letters = letters,
                    alpha = 0.05, sort = FALSE),
      silent = TRUE)

    # to verify which family the adjustment was applied over:
    # contrast(emm_link, "pairwise", adjust = letter_adjust)

    if (inherits(cld_df, "try-error")) {
      warning("cld() failed; skipping letters. ",
              attr(cld_df, "condition")$message)
      cld_df <- NULL
    } else {
      cld_df <- as.data.frame(cld_df) |>
        dplyr::mutate(
          .group = trimws(.group),
          dplyr::across(dplyr::all_of(x),
                        \(z) factor(as.character(z), levels = x_levels))) |>
        dplyr::group_by(question) |>
        # blank the letters where nothing differs, so panels stay readable
        dplyr::mutate(.group = if (dplyr::n_distinct(.group) == 1) "" else .group) |>
        dplyr::ungroup()
    }
  }

  if (is_cont) {
    # A compact letter display is meaningless against a covariate - there
    # are no groups to compare. The analogous quantity is the slope of the
    # logit on x WITHIN each question, which is exactly the question:x
    # interaction re-expressed per question.
    tr <- try(
      emmeans::emtrends(fit, ~ question, var = x, nesting = NULL),
      silent = TRUE)

    if (inherits(tr, "try-error")) {
      warning("emtrends() failed; skipping per-question slopes. ",
              attr(tr, "condition")$message)
    } else {
      trends_df <- as.data.frame(
        summary(tr, infer = c(TRUE, TRUE), adjust = "fdr")) |>
        dplyr::rename(slope = dplyr::matches("\\.trend$")) |>
        dplyr::mutate(
          question = as.character(question),
          # odds ratio per one-unit step in x, which is what to report
          OR    = exp(slope))

      # No compact letter display here. A CLD answers "which GROUPS of x
      # differ", and a covariate has no groups. Letters on the slopes would
      # answer a different question entirely (which ITEMS respond most
      # steeply to x), so putting them in $cld would invite them to be read
      # as if they meant the same thing as in the categorical case.
      # $trends carries the per-question slope and its test instead.
    }
  }

  # =================================================================
  # OBSERVED PROPORTIONS
  # =================================================================
  raw <- data %>%
    dplyr::group_by(question, .data[[x]]) %>%
    dplyr::summarise(prop = mean(response_numeric), n = dplyr::n(),
                     .groups = "drop")

  # Cast both key columns to the same type before joining: cld() returns a
  # plain factor and character question, raw has ordered / factor.
  # CATEGORICAL ONLY 
  if (!is_cont && !is.null(cld_df)) {
    harmonise <- function(d) {
      d[[x]] <- factor(as.character(d[[x]]), levels = x_levels)
      d$question <- as.character(d$question)
      d
    }
    raw    <- harmonise(raw)
    cld_df <- harmonise(cld_df)
    emm    <- harmonise(emm)

    raw <- raw %>%
      dplyr::left_join(cld_df %>% dplyr::select(question, dplyr::all_of(x), .group),
                       by = c("question", x))
  } else if (is_cont) {
    # keep the join keys aligned without touching the numeric x
    raw$question      <- as.character(raw$question)
    emm$question      <- as.character(emm$question)
    emm[[x]]          <- as.numeric(emm[[x]])
    emm_line$question <- as.character(emm_line$question)
    emm_line[[x]]     <- as.numeric(emm_line[[x]])
  }

  # ---- facet order: highest overall P(yes) first -----------------
  ord <- data %>%
    dplyr::group_by(question) %>%
    dplyr::summarise(overall = mean(response_numeric), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(overall))

  # ---- facet labels: original text (commas intact), line-wrapped --
  lab_map <- data %>%
    dplyr::distinct(question, question_label) %>%
    dplyr::mutate(question = as.character(question)) %>%
    tibble::deframe()                  # make.names -> original text
  lab_full <- lab_map

  # "(e.g. ...)" / "(e.g., ...)" examples -> dagger; the full wording stays in
  # $labels_full for the paper's figure caption
  eg_pat <- "\\s*\\(e\\.g\\.?,?[^)]*\\)"
  has_eg <- strip_examples && any(grepl(eg_pat, lab_map))
  if (has_eg) lab_map <- stats::setNames(sub(eg_pat, "\u2020", lab_map), names(lab_map))

  # smallest wrap width >= wrap_facet at which every title fits in
  # facet_max_lines lines (label_wrap() and strwrap() break the same way)
  facet_w <- max(wrap_facet, vapply(lab_map, function(s) {
    w <- wrap_facet
    while (length(strwrap(s, w)) > facet_max_lines) w <- w + 1
    w
  }, numeric(1)))
  facet_theme <- ggplot2::theme(
    strip.text = ggplot2::element_text(size = strip_size, lineheight = .9,
                                       margin = ggplot2::margin(b = 4)))

  if (order_facets) {
    lv_q <- as.character(ord$question)
    emm$question <- factor(as.character(emm$question), levels = lv_q)
    raw$question <- factor(as.character(raw$question), levels = lv_q)
    if (!is.null(emm_line))
      emm_line$question <- factor(as.character(emm_line$question), levels = lv_q)
    if (!is.null(trends_df))
      trends_df$question <- factor(trends_df$question, levels = lv_q)
  } else if (!is.null(trends_df)) {
    trends_df$question <- factor(trends_df$question,
                                 levels = levels(factor(raw$question)))
  }

  cap_model <- sa_cap_model(pred_re_form)

  # =================================================================
  # PLOT
  # =================================================================
  # ordered x: drawn by plot_select_all_ordered() from the finished result,
  # below, so cached fits can be redrawn without refitting
  p <- NULL
  if (!is_cont) {

    p <- ggplot2::ggplot(emm, ggplot2::aes(.data[[x]], prob)) +
      ggplot2::geom_col(data = raw, ggplot2::aes(y = prop),
                        fill = "grey85", width = .7) +
      ggplot2::geom_pointrange(
        ggplot2::aes(ymin = asymp.LCL, ymax = asymp.UCL),
        colour = "firebrick", size = .3) +
      ggplot2::geom_text(data = raw, ggplot2::aes(y = Inf, label = n),
                         vjust = 1.4, size = 2.6, colour = "grey40") +
      ggplot2::facet_wrap(
        ~ question,
        labeller = ggplot2::labeller(
          question = function(z) scales::label_wrap(facet_w)(lab_map[z]))) +
      ggplot2::scale_y_continuous(
        labels = scales::percent,
        expand = ggplot2::expansion(mult = c(.05, .20))) +  # room for 2 label rows
      ggplot2::scale_x_discrete(
      labels = function(z) scales::label_wrap(wrap_axis)(fig_label(z))) +
      ggplot2::labs(x = fig_var(x), y = "P(yes)",
                    caption = paste("grey = observed;", cap_model,
                                    "\ngroups sharing a letter do not differ",
                                    "(within item); no letters = none differ")) +
      ggplot2::theme_minimal() +
      facet_theme +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    # letters on a fixed line below the counts, so heights are consistent
    if (!is.null(cld_df))
      p <- p + ggplot2::geom_text(
        data = raw, ggplot2::aes(y = Inf, label = .group),
        vjust = 2.8, size = 2.4, colour = "grey20", fontface = "bold")
  }

  if (has_eg && !is.null(p))
    p <- p + ggplot2::labs(caption = paste0(p$labels$caption, sa_dagger_note))

  res <- structure(
    list(x = x, x_type = x_type, x_coerced = x_was_coerced,
         x_labels = x_labels,
         n_distinct_x = n_distinct_x,
         n_obs = nrow(data), n_id = dplyr::n_distinct(data[[id]]),
         re.form = re.form, pred_re_form = pred_re_form,
         pred_at_observed = pred_at_observed,
         data = data, fit = fit,
         emm = emm, emm_line = emm_line,
         raw = raw, cld = cld_df, trends = trends_df, plot = p,
         chisq_test = chisq_test, omni = omni, spacing = spacing,
         q_type = q_type, re_structure = re_structure, re_term = re_term,
         id = id,
         order = ord, labels = lab_map, labels_full = lab_full, cells = tab),
    class = "select_all")

  if (is_cont)
    res$plot <- plot_select_all_ordered(
      res, wrap_facet = wrap_facet, facet_max_lines = facet_max_lines,
      strip_size = strip_size, wrap_axis = wrap_axis)
  res
}


# ---- career-stage-adjusted sensitivity fit ------------------------
# Takes a fit_select_all() result (its $data already has x coded exactly
# as the main model used it) and, on the respondents with a career stage:
#   item level  question * x + question * career   vs  question + x + question * career
#               (the adjusted analogue of p_item_int), and the same
#               comparison WITHOUT career on the same respondents (_cc)
#   omnibus     .omnibus_counts() with and without career
# Career stage is a slope on career_stage_value, centred at the middle of
# its range to help the optimiser. Four extra glmer fits per cell, so this
# is slow; results are cached alongside the main fit by run_select_all().
fit_select_all_adjusted <- function(res, adj = "career_stage_value") {
  x  <- res$x
  id <- res$id
  stopifnot(adj %in% names(res$data))

  d <- res$data %>% dplyr::filter(!is.na(.data[[adj]])) %>% droplevels()
  d$.adj <- d[[adj]] - mean(range(d[[adj]]))

  ctrl <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  g <- function(rhs) try(lme4::glmer(
    stats::as.formula(paste("response_numeric ~", rhs, "+", res$re_term)),
    family = binomial, data = d, control = ctrl), silent = TRUE)
  lrt_p <- function(m0, m1) {
    if (inherits(m0, "try-error") || inherits(m1, "try-error")) return(NA_real_)
    anova(m0, m1)[["Pr(>Chisq)"]][2]
  }

  m_cc   <- g(sprintf("question * %s", x))
  m_cc0  <- g(sprintf("question + %s", x))
  m_adj  <- g(sprintf("question * %s + question * .adj", x))
  m_adj0 <- g(sprintf("question + %s + question * .adj", x))

  o_cc  <- try(.omnibus_counts(d, x = x, id = id, q_type = res$q_type,
                               quiet = TRUE), silent = TRUE)
  o_adj <- try(.omnibus_counts(d, x = x, id = id, q_type = res$q_type,
                               adj = ".adj", quiet = TRUE), silent = TRUE)
  op <- function(o) if (inherits(o, "try-error")) .omni_p(NULL) else .omni_p(o)

  list(n_id = dplyr::n_distinct(d[[id]]), n_obs = nrow(d),
       p_item_int_cc  = lrt_p(m_cc0,  m_cc),
       p_item_int_adj = lrt_p(m_adj0, m_adj),
       p_total_cc     = op(o_cc)$p_total,
       p_total_adj    = op(o_adj)$p_total,
       p_valence_cc   = op(o_cc)$p_valence,
       p_valence_adj  = op(o_adj)$p_valence,
       conv_adj       = .glmer_flag(m_adj),
       fit_adj = m_adj, omni_adj = o_adj)
}


# =================================================================
# 
# -----------------------------------------------------------------
# Every question crossed with every demographic predictor, driven by
# two spec lists. Each cell is wrapped
# in try() so one separation failure does not stop the run, and the
# failures are collected into a table at the end.
#
# Demographics: career_stage, IO, prof_work_sector, gender
# Select-all outcomes: promising_opps, challenges, ai_use_reasons
# Ordinal outcomes:    ai_use_freq, gen_attitude
# =================================================================


# =================================================================
# 5a. SPECS
# =================================================================

# ---- demographic predictors --------------------------------------
# `drop` removes levels on SUBSTANTIVE grounds (non-responses, "Other").
# Levels that are merely small are handled by the min_n screen inside
# fit_ordinal_by(), so they are not listed here - that keeps the two
# kinds of exclusion separable in the methods section.
#
# `type`:
#   "categorical" -> factor everywhere; fit_ordinal_by()
#   "count"       -> ordered factor for the select-all models (which need
#                    levels() and scale_x_discrete), numeric slope for the
#                    ordinal models via fit_ordinal_by_continuous()
demo_specs <- list(
  career_stage = list(
    var   = "career_stage",
    label = "career stage",
    drop  = character(0),
    #type  = "categorical"),
    type  = "count"),

  gender = list(
    var   = "gender",
    label = "gender",
    # Non-binary is dropped because it is not estimable alongside career
    # stage (max 11 in any one stage). State this restriction explicitly.
    drop  = c("Prefer not to answer", "Prefer to self-identify", "Non-binary"),
    type  = "categorical"),

  prof_work_sector = list(
    var   = "prof_work_sector",
    label = "work sector",
    drop  = c("Student", "Other"),
    type  = "categorical"),

  IO = list(
    var   = "IO",
    label = "index of opportunity",
    drop  = character(0),
    type  = "count"),

  ds_freq = list(
    var   = "ds_freq",
    label = "data science frequency",
    drop  = character(0),
    type  = "count"),
    
  policies = list(
    var   = "policies",
    label = "institutional policies",
    drop  = c("There are not any policies or guidelines at my institution", "Other"),
    type  = "count"),

    ai_use_freq = list(
    var   = "ai_use_freq",
    label = "AI use frequency",
    drop  = character(0),
    type  = "count")
)


# ---- prepare `survey` for one demographic -------------------------
prep_demo <- function(survey, spec) {
  v <- spec$var

  d <- survey %>%
    dplyr::filter(!is.na(.data[[v]]), !.data[[v]] %in% spec$drop)

  if (nrow(d) == 0)
    stop("No rows left after filtering ", v, ".")

  # IO is a numeric count. Career stage is ordinal,
  # Every select-all helper treats x as a factor
  # (levels(), scale_x_discrete), so it has to be one there; the ordering
  # is preserved so the axis reads 0, 1, 2, ... rather than alphabetically.
  if (spec$type == "count") {
    d[[v]] <- factor(d[[v]], levels = sort(unique(d[[v]])), ordered = TRUE)
  } else if (!is.factor(d[[v]])) {
    d[[v]] <- factor(d[[v]])
  }
  droplevels(d)
}


# ---- select-all questions -----------------------------------------
# free-text "Other" answers are resolved upstream, by consensus, in the
# tidying step, so only the "none" option is handled here

clean_opps <- function(long)
  clean_select_all(long, q = "promising_opps", none_pattern = "None")

clean_challenges <- function(long)
  clean_select_all(long, q = "challenges", none_pattern = "^No")

clean_reasons <- function(long)
  long %>%
    dplyr::filter(ai_use_reasons != "Other") %>%
    dplyr::mutate(q_type = dplyr::case_when(
      grepl("^Desire|^Get|^Expand", ai_use_reasons)                       ~ "positive",
      grepl("^Lack|^Worried|^Concerns|^The|^Security|^Anxiety|^Financial",
            ai_use_reasons)                                               ~ "negative",
      TRUE                                                                ~ "neutral")) %>%
    # the single neutral item has no replication within person, so its
    # variance component in (0 + q_type | ResponseId) is not identified
    dplyr::filter(q_type != "neutral") %>%
    dplyr::mutate(q_type = factor(q_type, levels = c("positive", "negative"))) %>%
    droplevels()

sa_specs <- list(
  opportunity = list(q = "promising_opps", clean = clean_opps,
                     q_type = NULL, label = "opportunities",
                     x_text_size = 11, x_title_size = 14),
  challenges  = list(q = "challenges",     clean = clean_challenges,
                     q_type = NULL, label = "challenges",
                     x_text_size = 11, x_title_size = 14),
  reasons     = list(q = "ai_use_reasons", clean = clean_reasons,
                     q_type = "q_type", label = "reasons for use",
                     x_text_size = 11, x_title_size = 14,
                     re_structure = "correlated")
)


# ---- ordinal outcomes ---------------------------------------------
ord_specs <- list(
  ai_use_freq = list(
    y      = "ai_use_freq",
    label  = "AI use frequency",
    filter = NULL),

  # primary version: Indifferent (and Other) excluded
  gen_attitude = list(
    y      = "gen_attitude",
    label  = "general attitude",
    filter = function(d)
      dplyr::filter(d, !gen_attitude %in% c("Other", "Indifferent"))),

  # sensitivity version: Indifferent merged into "A mix ..." (Other still
  # excluded). `split` draws the merged level's bar stacked, so the
  # Indifferent respondents inside it stay visible.
  gen_attitude_mix = list(
    y      = "gen_attitude_mix",
    label  = "general attitude (Indifferent = A mix)",
    filter = function(d) dplyr::filter(d, !is.na(gen_attitude_mix)),
    split  = list(col = "gen_attitude", level = "Indifferent",
                  into = ga_mix_level))
)


# =================================================================
# 5b. CELL DIAGNOSTICS (run before fitting, recorded either way)
# =================================================================
check_cells <- function(long, x, min_n = 5,

                        x_type = c("continuous", "categorical"),
                        # continuous only: a question needs at least this
                        # many distinct x values for a slope to exist, and
                        # this many respondents overall to be worth fitting
                        min_distinct_x = 3,
                        min_n_item     = 20
                        # ----------------------------------------------
                        ) {

  x_type <- match.arg(x_type)
  is_cont <- identical(x_type, "continuous")

  # An ordered factor arrives here un-coerced (fit_select_all() does the
  # coercion later). Score it the same way - numeric labels as numbers,
  # otherwise level position - so the range checks below compare numbers.
  # Without this, an all-yes or all-no item makes the x_yes/x_no columns
  # mix <ordered> and <double> across groups and summarise() errors.
  if (is_cont && !is.numeric(long[[x]])) {
    num <- suppressWarnings(as.numeric(as.character(long[[x]])))
    long[[x]] <- if (anyNA(num)) as.integer(factor(long[[x]])) else num
  }

  # .drop = FALSE materialises unused FACTOR combinations so that genuinely
  # empty cells appear as n == 0 rows. A numeric has no levels, so it does
  # nothing there - which is why `empty` was already always 0 rows for a
  # continuous x.
  tab <- long %>%
    dplyr::group_by(question, .data[[x]], .drop = !is_cont) %>%
    dplyr::summarise(n   = dplyr::n(),
                     yes = sum(response_numeric),
                     p   = mean(response_numeric),
                     .groups = "drop")

  item_rates <- long %>%
    dplyr::group_by(question) %>%
    dplyr::summarise(n = dplyr::n(), p = mean(response_numeric),
                     .groups = "drop") %>%
    dplyr::arrange(p)

  if (!is_cont) {

    # ---- CATEGORICAL: each x level has its own parameter -----------
    # An empty cell means a non-estimable cell mean, and an all-yes or
    # all-no cell means complete separation, so cell-level checks are the
    # right granularity.
    out <- list(
      empty     = dplyr::filter(tab, n == 0),
      # all-yes or all-no cells cause complete separation, which is what
      # makes glmer fail. See Gelman et al. 2008, 10.1214/08-AOAS191
      separated = dplyr::filter(tab, n > 0, yes == 0 | yes == n),
      thin      = dplyr::filter(tab, n > 0, n < min_n))

  } else {

    # ---- CONTINUOUS: one slope pools across all x values -----------
    # A gap at any single x value is harmless: the model interpolates
    # through it. Nothing is estimated per cell, so cell-level emptiness,
    # thinness and all-yes/all-no are all irrelevant - with ~400 people
    # spread over question x x, most cells are thin and many are constant
    # by chance, which made the old checks fire on nearly every question.
    #
    # What matters instead, per question:
    #   1. is there any spread in x at all?            (no spread, no slope)
    #   2. is the response constant?                   (complete separation)
    #   3. do the yes and no x-ranges overlap?         (separation along x)
    item <- long %>%
      dplyr::group_by(question) %>%
      dplyr::summarise(
        n            = dplyr::n(),
        yes          = sum(response_numeric),
        p            = mean(response_numeric),
        n_distinct_x = dplyr::n_distinct(.data[[x]]),
        min_x        = min(.data[[x]]),
        max_x        = max(.data[[x]]),
        # x range covered by each response, used for the overlap test.
        # NA rather than Inf when one response never occurs, so the
        # comparisons below return NA and are caught by the constant test.
        x_yes_min = if (any(response_numeric == 1))
                      min(.data[[x]][response_numeric == 1]) else NA_real_,
        x_yes_max = if (any(response_numeric == 1))
                      max(.data[[x]][response_numeric == 1]) else NA_real_,
        x_no_min  = if (any(response_numeric == 0))
                      min(.data[[x]][response_numeric == 0]) else NA_real_,
        x_no_max  = if (any(response_numeric == 0))
                      max(.data[[x]][response_numeric == 0]) else NA_real_,
        .groups = "drop") %>%
      dplyr::mutate(
        # the response never varies -> the intercept is separated
        const_y  = yes == 0 | yes == n,
        # every "yes" sits above every "no" (or below): the slope is
        # separated and diverges to +-Inf. This is the continuous
        # analogue of an all-yes/all-no cell, and the thing worth
        # catching. Gelman et al. 2008, 10.1214/08-AOAS191 applies here
        # too - a weakly informative prior is the fix if you need the fit.
        sep_x    = !const_y &
                   ((x_yes_min > x_no_max) | (x_no_min > x_yes_max)),
        # no spread in x -> the question-specific slope is not estimable
        flat_x   = n_distinct_x < 2,
        thin_x   = n_distinct_x < min_distinct_x,
        thin_n   = n < min_n_item)

    out <- list(
      # kept for interface compatibility, deliberately always 0 rows:
      # an unobserved (question, x) combination is not a problem here
      empty     = tab[0, ],
      separated = dplyr::filter(item, const_y | sep_x | flat_x),
      thin      = dplyr::filter(item, !(const_y | sep_x | flat_x),
                                thin_x | thin_n),
      # full question-level diagnostics, continuous only
      item      = item)
  }

  c(out,
    list(item_rates = item_rates,
         full       = tab,
         x          = x,
         x_type     = x_type,
         # so a caller can tell at a glance whether `separated` / `thin`
         # are cell-level (categorical) or question-level (continuous)
         grain      = if (is_cont) "question" else "cell"))
}


# ---- drop items that cannot be modelled ---------------------------
# Two independent reasons an item has to go:
#
#   RATE   endorsed by almost nobody (or almost everybody) overall, so
#          there is no estimable interaction. Report it as a plain
#          percentage instead of modelling it.
#
#   SEP    flagged by check_cells() as separated. For a continuous x this
#          means every "yes" sits above every "no", so the slope diverges
#          to +-Inf. glmer WARNS rather than errors on this, so try() will
#          not catch it and a garbage fit gets recorded as a success -
#          which is why it has to be caught here.
#
# `chk` is the list returned by check_cells(). Passing NULL reproduces the
# old behaviour exactly (rate filter only).
#
# drop_separated:
#   "auto"  drop only when chk is question-level (continuous x). For a
#           categorical x a single all-yes/all-no CELL separates one
#           coefficient but leaves the rest of the model usable, so the
#           whole item is kept and the marginal rate filter decides.
#   TRUE    always drop anything in chk$separated
#   FALSE   never
drop_unmodellable <- function(long, chk = NULL,
                              lo = 0.03, hi = 0.97,
                              drop_separated = c("auto", "yes", "no"),
                              quiet = FALSE) {
 
  drop_separated <- match.arg(drop_separated)
 
  # ---- reason 1: marginal endorsement rate ----------------------
  rates <- long %>%
    dplyr::group_by(question) %>%
    dplyr::summarise(p = mean(response_numeric), .groups = "drop")
 
  keep_rate <- rates %>%
    dplyr::filter(p >= lo, p <= hi) %>%
    dplyr::pull(question) %>%
    as.character()
 
  gone_rate <- setdiff(as.character(rates$question), keep_rate)
 
  # ---- reason 2: separation flagged by check_cells() ------------
  # grain is absent on a pre-update check_cells() result, in which case
  # treat it as cell-level so nothing new gets dropped
  grain <- if (is.null(chk) || is.null(chk$grain)) "cell" else chk$grain
 
  use_sep <- !is.null(chk) && !is.null(chk$separated) &&
    (drop_separated == "yes" ||
       (drop_separated == "auto" && identical(grain, "question")))
 
  gone_sep <- if (use_sep)
    setdiff(unique(as.character(chk$separated$question)), gone_rate)
  else character(0)
 
  gone <- union(gone_rate, gone_sep)
 
  if (!quiet) {
    if (length(gone_rate))
      message("  dropping ", length(gone_rate), " item(s) outside [", lo,
              ", ", hi, "]: ", paste(gone_rate, collapse = ", "))
    if (length(gone_sep))
      message("  dropping ", length(gone_sep), " separated item(s): ",
              paste(gone_sep, collapse = ", "))
  }
 
  out <- long %>%
    dplyr::filter(!as.character(question) %in% gone) %>%
    droplevels()
 
  # set attributes last: filter() and droplevels() do not preserve them
  attr(out, "dropped_items")     <- gone        # union, as before
  attr(out, "dropped_rate")      <- gone_rate
  attr(out, "dropped_separated") <- gone_sep
  out
}


# =================================================================
# 5c. SELECT-ALL GRID
# =================================================================

# ---- fit both re.form variants, cache, save figures ---------------
# re.form = NULL  -> population average, comparable to the observed bars
# re.form = NA    -> typical respondent (random effects = 0)

run_select_all <- function(long, x, tag,
                           rerun     = rerun_mods,
                           # re_forms  = list(pop = NULL, subj = NA),
                           re_forms  = list(subj = NA),
                           # career-stage-adjusted sensitivity fit, cached
                           # as res_<tag>_adj.rds; returned as $adj
                           adjust    = TRUE,
                           # x-axis label and title sizes in pt; NULL keeps
                           # the default
                           x_text_size = NULL, x_title_size = NULL,
                           width = 18, height = 14, scale = 0.8, ...) {

  out <- list()

  for (nm in names(re_forms)) {
    suffix <- if (nm == "pop") "" else paste0("_", nm)
    f      <- file.path(fit_path, paste0("res_", tag, suffix, ".rds"))

    if (rerun || !file.exists(f))
      saveRDS(fit_select_all(long, x = x, re.form = re_forms[[nm]], ...), f)

    out[[nm]] <- readRDS(f)

    # redrawn here, not read from the cache, so styling changes need no refit
    if (!is.null(out[[nm]]$emm_line))
      out[[nm]]$plot <- plot_select_all_ordered(out[[nm]])
    if (!is.null(x_text_size))
      out[[nm]]$plot <- out[[nm]]$plot +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = x_text_size))
    if (!is.null(x_title_size))
      out[[nm]]$plot <- out[[nm]]$plot +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = x_title_size))

    ggsave(file.path(fig_path, paste0(tag, suffix, ".jpg")), out[[nm]]$plot,
           width = width, height = height, scale = scale)
  }

  # the adjustment does not depend on re.form (it only uses $data, $x and
  # the random-effect term), so it is fitted once, from the first variant
  adj <- NULL
  if (adjust) {
    f_adj <- file.path(fit_path, paste0("res_", tag, "_adj.rds"))
    if (rerun || !file.exists(f_adj)) {
      a <- try(fit_select_all_adjusted(out[[1]]), silent = TRUE)
      if (inherits(a, "try-error"))
        a <- list(error = conditionMessage(attr(a, "condition")))
      saveRDS(a, f_adj)
    }
    adj <- if (file.exists(f_adj)) readRDS(f_adj) else list(error = "no cached fit")
  }

  list(fits = out, adj = adj)
}


# ---- one cell of the grid ------------------------------------------
run_sa_cell <- function(survey, sa, demo,
                        min_n_level = 20, verbose = FALSE, ...) {
 
  tag <- paste0(sa$label_tag, "_by_", demo$var)
  message("\n=== ", sa$q, " by ", demo$var, " ===")
 
  dat <- prep_demo(survey, demo)
 
  long <- expand_select_all(dat, q = sa$q, x = demo$var) %>%
    sa$clean()
 
  # one decision, read by every branch below. demo$type is the spec's own
  # label and is honoured when it says "count"/"continuous"; otherwise the
  # column itself decides.
  is_cont <- if (!is.null(demo$type) &&
                 demo$type %in% c("count", "continuous"))
    TRUE else is.numeric(long[[demo$var]])
 
  # respondents per level / per x value - recorded either way
  lvl_n <- long %>%
    dplyr::distinct(ResponseId, .data[[demo$var]]) %>%
    dplyr::count(.data[[demo$var]], name = "n_id")

  small <- NULL                      # levels dropped for small n (categorical)

  if (!is_cont) {
 
    # CATEGORICAL: each level carries its own parameter, so a level with
    # too few respondents gives an unstable estimate and is dropped.
    small <- dplyr::filter(lvl_n, n_id < min_n_level)
 
    if (nrow(small)) {
      message("  dropping level(s) with < ", min_n_level, " respondents: ",
              paste(sprintf("%s (n = %d)", small[[demo$var]], small$n_id),
                    collapse = ", "))
      long <- long %>%
        dplyr::filter(!.data[[demo$var]] %in% small[[demo$var]]) %>%
        droplevels()
    }
 
  } else {
 
    # CONTINUOUS: nothing is estimated at an individual x value - one
    # slope pools across all of them - so dropping sparse values would
    # truncate the predictor's range rather than remove a weak group.
    # Noted, not acted on.
    thin_x <- dplyr::filter(lvl_n, n_id < min_n_level)
    if (nrow(thin_x))
      message("  ", nrow(thin_x), " value(s) of ", demo$var, " have < ",
              min_n_level, " respondents (",
              paste(sprintf("%s: %d", thin_x[[demo$var]], thin_x$n_id),
                    collapse = ", "),
              "); kept, since the slope pools across values")
  }
 
  # chk describes the data BEFORE dropping, so it records WHY items went.
  # drop_unmodellable() now reads it, so separated items are removed
  # instead of being handed to glmer.
  chk  <- check_cells(long, x = demo$var,
                      x_type = if (is_cont) "continuous" else "categorical")
  long <- drop_unmodellable(long, chk = chk)
 
  # nlevels() is 0 for a numeric column, here counts number regardless
  n_x <- if (is_cont) dplyr::n_distinct(long[[demo$var]])
         else nlevels(long[[demo$var]])
 
  base <- list(tag = tag, x = demo$var, cells = chk, lvl_n = lvl_n,
               is_cont = is_cont, n_x = n_x,
               dropped_levels    = if (!is.null(small) && nrow(small)) small,
               dropped_items     = attr(long, "dropped_items"),
               dropped_rate      = attr(long, "dropped_rate"),
               dropped_separated = attr(long, "dropped_separated"))
 
  if (n_x < 2 || dplyr::n_distinct(long$question) < 2)
    return(c(base, list(
      error = sprintf("fewer than 2 %s or 2 items remain",
                      if (is_cont) "distinct x values" else "levels"))))
                   
  # x_type must be passed explicitly: fit_select_all() defaults to
  # "continuous", so without this gender and work sector were silently
  # recoded to 1..K and fitted as a slope.
  fit <- try(
    run_select_all(long, x = demo$var, tag = tag,
                   q_type = sa$q_type, verbose = verbose,
                   re_structure = sa$re_structure,
                   x_type = if (is_cont) "continuous" else "categorical",
                   # no career-stage adjustment of career stage itself
                   adjust = demo$var != "career_stage",
                   x_text_size = sa$x_text_size,
                   x_title_size = sa$x_title_size,
                   ...),
    silent = TRUE)
 
  if (inherits(fit, "try-error")) {
    msg <- conditionMessage(attr(fit, "condition"))
 
    # A package-conflict, missing-function or missing-object error is a
    # script configuration problem, not a property of this cell's data.
    # Recording it as a cell error empties every downstream table, so stop.
    if (grepl("\\[conflicted\\]|could not find function|object '.*' not found",
              msg))
      stop("configuration error in cell ", tag, ":\n", msg, call. = FALSE)
 
    message("  FAILED: ", msg)
    return(c(base, list(long = long, error = msg)))
  }
 
  c(base, list(long = long, res = fit, error = NA_character_))
}


# ---- run the whole grid --------------------------------------------
# sa_grid <- tidyr::expand_grid(sa = names(sa_specs), demo = names(demo_specs))
sa_grid <- tidyr::expand_grid(sa = c("opportunity", "challenges"),
 demo = c("career_stage","gender", "prof_work_sector", "IO","ds_freq","policies"))

#examine reasons not be demo but by AI use freq
sa_grid <- sa_grid %>%
  bind_rows(., tibble (sa = "reasons", demo = c("ai_use_freq")))

sa_results <- lapply(seq_len(nrow(sa_grid)), function(i) {
  sa   <- sa_grid$sa[[i]]
  demo <- sa_grid$demo[[i]]
  spec <- sa_specs[[sa]]
  spec$label_tag <- sa
  # re_structure only matters when q_type is supplied; correlated is expected due to 
  # expecting inverse
  # robust default (see the single-neutral-item problem in clean_reasons)
  run_sa_cell(survey, spec, demo_specs[[demo]])
})
names(sa_results) <- paste(sa_grid$sa, sa_grid$demo, sep = "__")


# ---- summary table --------------------------------------------------

# one string per cell: "<item>: OR 0.55 (95% CI 0.37-0.81, p = 0.038); ...", in
# figure order. p is FDR-adjusted across the items of the cell, as for the line
# style. The CI is an UNADJUSTED Wald interval from the slope and its SE, not
# the $trends limits: emmeans cannot FDR-adjust intervals and falls back to
# Bonferroni ones. So a CI can exclude 1 while the FDR p is above .05.
# NA for a categorical x (no slope).
.item_or <- function(f, tr = f$trends) {
  if (is.null(tr)) return(NA_character_)
  tr <- tr[order(tr$question), ]
  item <- sub("\u2020", "", f$labels[as.character(tr$question)])
  z    <- stats::qnorm(.975)
  paste(sprintf("%s: OR %.2f (95%% CI %.2f-%.2f, p = %.3f)", item, tr$OR,
                exp(tr$slope - z * tr$SE), exp(tr$slope + z * tr$SE), tr$p.value),
        collapse = "; ")
}

# the same string from the career-stage-adjusted fit, read from the saved model
# so no refit is needed. Slopes are conditional on career stage (slope, varying
# by item), on the respondents with a career stage. p is FDR-adjusted as above.
# The figures stay unadjusted, so their line style follows item_or, not this.
.item_or_adj <- function(f, a) {
  if (is.null(f$trends) || is.null(a$fit_adj)) return(NA_character_)
  tr <- try(as.data.frame(summary(
    emmeans::emtrends(a$fit_adj, ~ question, var = f$x, nesting = NULL),
    infer = c(TRUE, TRUE), adjust = "fdr")), silent = TRUE)
  if (inherits(tr, "try-error")) return(NA_character_)
  tr <- tr |>
    dplyr::rename(slope = dplyr::matches("\\.trend$")) |>
    dplyr::mutate(question = factor(as.character(question),
                                    levels = levels(f$trends$question)),
                  OR = exp(slope))
  .item_or(f, tr)
}

if (length(sa_results)) {
sa_summary <- purrr::imap_dfr(sa_results, function(z, nm) {

  base <- tibble::tibble(
    cell    = nm,
    tag     = z$tag,
    n_sep   = nrow(z$cells$separated),
    n_empty = nrow(z$cells$empty),
    dropped = paste(z$dropped_items, collapse = "; "),
    error   = z$error)

  if (!is.na(z$error)) return(base)

  # whichever re.form variants were fitted, prefer pop when present
  fits <- z$res$fits
  f <- if (!is.null(fits$pop)) fits$pop else fits[[1]]
  if (is.null(f) || is.null(f$fit))
    return(dplyr::mutate(base, error = "no fitted model in $res"))

  # p_total: does total endorsement differ across the demographic?
  # p_valence (q_type questions only): does the valence balance shift?
  op <- .omni_p(f$omni)

  # equal-spacing diagnostic (continuous x only; NULL on old cached fits)
  sp <- f$spacing

  # career-stage-adjusted sensitivity fit (NULL / $error when not run)
  a  <- z$res$adj
  a_ok <- !is.null(a) && is.null(a$error)
  ga <- function(nm) if (a_ok) a[[nm]] else NA_real_

  dplyr::bind_rows(base) %>%
    dplyr::mutate(
      n_obs     = f$n_obs,
      n_id      = f$n_id,
      singular  = if (inherits(f$fit, "merMod"))
                    lme4::isSingular(f$fit) else NA,
      conv_flag = .glmer_flag(f$fit),
      p_total   = op$p_total,
      p_valence = op$p_valence,
      # item-level: does the demographic effect differ by item?
      p_item_int = if (!is.null(f$chisq_test))
                     f$chisq_test[["Pr(Chi)"]][2] else NA_real_,
      # odds ratio per one-level step in x, for each item
      item_or = .item_or(f),
      # ---- equal spacing (diagnostic; low power, see fit_select_all) ----
      spacing_df      = if (!is.null(sp)) sp$df else NA_integer_,
      spacing_p       = if (!is.null(sp)) sp$p  else NA_real_,
      spacing_n       = if (!is.null(sp)) sp$n_id else NA_integer_,
      spacing_dropped = if (!is.null(sp) && nrow(sp$dropped))
                          paste(sprintf("%s (n = %d)", sp$dropped$level,
                                        sp$dropped$n), collapse = "; ")
                        else "",
      spacing_note    = if (is.null(sp) || !f$x_type == "continuous") "n/a: categorical x"
                        else if (sp$tested) sp$conv else sp$reason,
      # ---- career-stage adjustment (sensitivity) ----------------------
      # *_cc  = unadjusted, on the same respondents as the adjusted fit
      n_id_adj       = if (a_ok) a$n_id else NA_integer_,
      p_total_cc     = ga("p_total_cc"),
      p_total_adj    = ga("p_total_adj"),
      p_valence_cc   = ga("p_valence_cc"),
      p_valence_adj  = ga("p_valence_adj"),
      p_item_int_cc  = ga("p_item_int_cc"),
      p_item_int_adj = ga("p_item_int_adj"),
      item_or_adj    = if (a_ok) .item_or_adj(f, a) else NA_character_,
      adj_note       = if (is.null(a)) "not run (x is career stage)"
                       else if (!a_ok) paste("failed:", a$error)
                       else a$conv_adj)
})

cat("\nNOTE: exploratory. p-values are NOT adjusted across the cells of this",
    "\ngrid (see the INFERENCE note in the header). *_adj = career stage added",
    "\nas a covariate; *_cc = unadjusted on those same respondents.\n\n")
print(as.data.frame(sa_summary), row.names = FALSE)

readr::write_csv(sa_summary, file.path(fig_path, "select_all_summary.csv"))

# cells that failed, with the reason
dplyr::filter(sa_summary, !is.na(error))
}


# =================================================================
# 6. ORDINAL GRID: every ordinal outcome x every demographic
# =================================================================
run_ord_cell <- function(survey, ord, demo, min_n = 20, verbose = FALSE) {

  tag <- paste0(ord$y, "_by_", demo$var)
  message("\n=== ", ord$y, " by ", demo$var, " ===")

  dat <- prep_demo(survey, demo)
  if (!is.null(ord$filter)) dat <- ord$filter(dat)
  dat <- droplevels(dat)

  fit <- if (demo$type == "count") {
    # IO/ds_freq/career_stage is an ordered count: a single slope on its own scale. The
    # min_distinct_x guard is lowered deliberately 
    try(fit_ordinal_by_continuous(dat, x = demo$var, y = ord$y,
                                  min_distinct_x = 3,
                                  verbose = verbose), silent = TRUE)
  } else {
    try(fit_ordinal_by(dat, x = demo$var, y = ord$y,
                       min_n = min_n,
                       verbose = verbose), silent = TRUE)
  }

  if (inherits(fit, "try-error")) {
    message("  FAILED: ", conditionMessage(attr(fit, "condition")))
    return(list(tag = tag, error = conditionMessage(attr(fit, "condition"))))
  }

  # ---- career-stage-adjusted sensitivity fits -----------------------
  # adj: for the model behind p_main (the slope, for a count-type x).
  # adj_factor: also for the factor model, when it replaced the slope.
  .try_adj <- function(d) {
    a <- try(.adjusted_clm(d, x = demo$var, y = ord$y), silent = TRUE)
    if (inherits(a, "try-error"))
      list(error = conditionMessage(attr(a, "condition"))) else a
  }
  adj <- adj_factor <- NULL
  if (demo$var != "career_stage") {
    adj <- .try_adj(fit$data)
    if (inherits(fit, "ordinal_by_cont") && !is.null(fit$factor_fit))
      adj_factor <- .try_adj(fit$factor_fit$data)
  }

  # merged-level composition in the figure (gen_attitude_mix)
  if (!is.null(ord$split)) fit$plot <- .add_split_bars(fit, ord$split)

  ggsave(file.path(fig_path, paste0(tag, ".jpg")), fit$plot,
         width = 10, height = 5)

  list(tag = tag, res = fit, adj = adj, adj_factor = adj_factor,
       error = NA_character_)
}

# ---- stacked bar for a merged outcome level ------------------------
# In the merged level's panel, the observed bar is split into the
# respondents who gave the original answer (split$level, darker grey, on
# top) and the rest. The layer goes in directly above the grey bars, so
# the model points and line still sit on top.
.add_split_bars <- function(res, split) {
  src <- if (inherits(res, "ordinal_by_cont") && !is.null(res$factor_fit))
    res$factor_fit else res
  d <- src$data; x <- src$x; y <- src$y

  tot <- dplyr::count(d, .data[[x]], name = "n_x")
  parts <- d %>%
    dplyr::filter(.data[[y]] == split$into) %>%
    dplyr::mutate(.part = ifelse(.data[[split$col]] %in% split$level,
                                 split$level, "rest")) %>%
    dplyr::count(.data[[x]], .part) %>%
    dplyr::left_join(tot, by = x) %>%
    dplyr::mutate(prop = n / n_x,
                  # first level stacks on TOP in position_stack()
                  .part = factor(.part, levels = c(split$level, "rest")),
                  .fill = ifelse(.part == split$level, "grey55", "grey85"))
  parts[[y]] <- factor(split$into, levels = levels(d[[y]]))

  # the slope figure maps x to .x_plot; the categorical one to x itself
  if (inherits(src, "ordinal_by_cont"))
    parts$.x_plot <- if (isTRUE(src$discrete_axis))
      factor(parts[[x]], levels = sort(unique(d[[x]]))) else parts[[x]]

  p <- res$plot
  bars <- ggplot2::geom_col(data = parts,
                            ggplot2::aes(y = prop, fill = .fill, group = .part),
                            width = .7, position = "stack")
  p$layers <- append(p$layers, list(bars), after = 1)
  n_lvl <- sum(parts$n[parts$.part == split$level])
  p + ggplot2::scale_fill_identity() +
    ggplot2::labs(caption = paste0(
      p$labels$caption,
      sprintf("\ndarker grey in the '%s' panel = the %d respondents who answered '%s' (merged into it)",
              split$into, n_lvl, split$level)))
}

ord_grid <- tidyr::expand_grid(ord = names(ord_specs), demo = names(demo_specs)) %>%
  dplyr::filter(!ord == demo,
                # attitude x AI use is two OUTCOMES, so the direction cannot
                # be identified; it is reported as a polychoric correlation
                # in section 8 instead of as a model
                !(ord %in% c("gen_attitude", "gen_attitude_mix") &
                    demo == "ai_use_freq"))

ord_results <- purrr::pmap(ord_grid, function(ord, demo)
  run_ord_cell(survey, ord_specs[[ord]], demo_specs[[demo]]))
names(ord_results) <- paste(ord_grid$ord, ord_grid$demo, sep = "__")


# ---- summary table --------------------------------------------------
ord_summary <- purrr::imap_dfr(ord_results, function(z, nm) {

  base <- tibble::tibble(cell = nm, tag = z$tag, error = z$error)
  if (!is.na(z$error)) return(base)

  f <- z$res
  asm <- f$assump
  a  <- z$adj
  a_ok <- !is.null(a) && is.null(a$error)
  ga <- function(nm) if (a_ok) a[[nm]] else NA_real_

  out <- dplyr::bind_rows(base) %>%
    dplyr::mutate(
      n        = f$n,
      # convergence code -3 means the thresholds are not increasing and
      # the fit is INVALID - do not interpret those rows
      conv     = .clm_code(f$fit),
      p_main   = f$lrt[["Pr(>Chi)"]][2],
      dropped  = if (!is.null(f$dropped) && nrow(f$dropped))
                   paste(f$dropped[[f$x]], collapse = "; ") else "",
      # ---- assumption checks on the main fit (LRT p; NA when the
      # auxiliary fit was invalid - see conv_flag) ---------------------
      # p_po small    -> x's effect differs across outcome thresholds
      # p_scale small -> x changes the latent SD, not only its location
      p_po     = asm$p_po,
      p_scale  = asm$p_scale,
      # ---- career-stage adjustment (sensitivity) ----------------------
      # p_main_cc  = unadjusted on the respondents WITH a career stage
      # p_main_adj = adjusted for career stage (slope), same respondents
      # b_cc vs b_adj: not directly comparable, because adding a covariate
      # rescales probit coefficients (Karlson et al. 2012)
      # b_*        = slope (latent SD per unit of x), count-type x only
      n_adj      = if (a_ok) a$n else NA_integer_,
      p_main_cc  = ga("p_cc"),
      p_main_adj = ga("p_adj"),
      b_main     = if (inherits(f, "ordinal_by_cont")) f$slope$estimate else NA_real_,
      b_cc       = ga("b_cc"),
      b_adj      = ga("b_adj"),
      adj_note   = if (is.null(a)) "not run (x is career stage)"
                   else if (!a_ok) paste("failed:", a$error) else "",
      # every fit that was not clean, in one readable column
      conv_flag  = paste(c(.conv_flag(conv, "main"),
                           .conv_flag(asm$conv_po, "PO check"),
                           .conv_flag(asm$conv_scale, "scale check"),
                           if (a_ok) .conv_flag(a$conv_adj, "adjusted")),
                         collapse = "; "))

  # count-type predictors: the equal-spacing test, and which model the
  # figure uses. p_main stays the slope (all respondents); p_factor is the
  # factor model's main effect on the reduced data, filled in only when
  # the factor model replaced the slope.
  if (inherits(f, "ordinal_by_cont")) {
    sp  <- f$spacing
    out <- dplyr::mutate(out,
      model_used      = f$model_used,
      spacing_df      = sp$df,
      spacing_p       = sp$p,
      spacing_n       = sp$n,
      spacing_dropped = if (nrow(sp$dropped))
                          paste(sprintf("%s (n = %d)", sp$dropped$level,
                                        sp$dropped$n), collapse = "; ")
                        else "",
      spacing_note    = if (sp$tested) "" else sp$reason,
      p_factor        = if (is.null(f$factor_fit)) NA_real_
                        else f$factor_fit$lrt[["Pr(>Chi)"]][2],
      p_factor_adj    = if (!is.null(z$adj_factor) && is.null(z$adj_factor$error))
                          z$adj_factor$p_adj else NA_real_)
  } else {
    out <- dplyr::mutate(out, model_used = "categorical")
  }
  out
})

if (nrow(ord_summary)) {
cat("\nNOTE: exploratory. p-values are NOT adjusted across the cells of this",
    "\ngrid (see the INFERENCE note in the header). The equal-spacing test",
    "\n(spacing_p) has low power. *_adj = career stage added as a covariate;",
    "\n*_cc = unadjusted on those same respondents. ai_use_freq / ds_freq:",
    "\nhigher = MORE use, so a positive b means more use goes with a higher",
    "\noutcome category.\n\n")
print(as.data.frame(ord_summary), row.names = FALSE)

readr::write_csv(ord_summary, file.path(fig_path, "ordinal_summary.csv"))

# anything that did not converge cleanly
dplyr::filter(ord_summary, !is.na(error) | conv_flag != "")
}

# Equal spacing for the count-type predictors (IO, career_stage, ds_freq,
# policies, ai_use_freq) is now tested inside fit_ordinal_by_continuous():
# see the spacing_* and model_used columns of ord_summary above.


# =================================================================
# 6b. EXCLUSIONS: every rule that removes respondents (or items), with n
# =================================================================
# One row per rule. `scope` says where it applies:
#   variable         whenever that variable is used (predictor or outcome)
#   select-all       whenever that question is the outcome
#   ordinal cell /   one cell of a grid only
#   select-all cell
# Counts are of respondents unless `unit` says otherwise. Rules overlap
# (one person can be missing several variables), so rows do not sum.
# The target-audience filter is applied upstream in 01_tidy.r and is not
# counted here; n_start is the data as read.
n_start <- nrow(survey)

.excl <- function(scope, variable, cell, reason, level, n,
                  unit = "respondents")
  tibble::tibble(scope = scope, variable = variable, cell = cell,
                 reason = reason, level = as.character(level),
                 n = as.integer(n), unit = unit)

# ---- variable level -------------------------------------------------
excl_demo <- purrr::map_dfr(demo_specs, function(s) {
  v <- as.character(survey[[s$var]])
  dplyr::bind_rows(
    .excl("variable", s$var, NA, "missing / blank / prefer not (-> NA)", NA,
          sum(is.na(v))),
    if (length(s$drop))
      .excl("variable", s$var, NA, "level excluded on substantive grounds (spec$drop)",
            s$drop, vapply(s$drop, function(l) sum(v %in% l), integer(1))))
})

# IO is NA when ANY component is missing; the components overlap
excl_io <- purrr::map_dfr(c("caregiver_IO", "neuro_IO", "firstgen_IO", "race_IO"),
  function(v) .excl("variable", "IO", NA,
                    "component missing or prefer not (IO set to NA; overlaps)",
                    v, sum(is.na(survey[[v]]))))

ga_chr <- as.character(survey$gen_attitude)
# (ai_use_freq is already counted above, as a demographic spec)
excl_out <- dplyr::bind_rows(
  .excl("variable", "gen_attitude", NA, "missing / blank (-> NA)", NA, sum(is.na(ga_chr))),
  .excl("variable", "gen_attitude", NA, "level excluded (primary version)",
        c("Other", "Indifferent"),
        c(sum(ga_chr %in% "Other"), sum(ga_chr %in% "Indifferent"))),
  .excl("variable", "gen_attitude_mix", NA, "level excluded (sensitivity version)",
        "Other", sum(ga_chr %in% "Other")),
  .excl("variable", "gen_attitude_mix", NA,
        paste0("NOT excluded: merged into '", ga_mix_level, "'"),
        "Indifferent", sum(ga_chr %in% "Indifferent")))

# ---- select-all questions ------------------------------------------
excl_sa <- purrr::map_dfr(sa_specs, function(sa) {
  long0 <- expand_select_all(survey, q = sa$q)
  cl    <- suppressMessages(sa$clean(long0))
  items_gone <- setdiff(unique(long0$question_label), unique(cl$question_label))
  dplyr::bind_rows(
    .excl("select-all", sa$q, NA, "no option ticked (question skipped)", NA,
          n_start - dplyr::n_distinct(long0$ResponseId)),
    .excl("select-all", sa$q, NA, "ticked 'none' AND another option (conflict)",
          NA, length(attr(cl, "conflicts"))),
    if (length(items_gone))
      .excl("select-all", sa$q, NA,
            "option not modelled ('none' / 'Other' / single neutral item)",
            items_gone, NA, "items"))
})

# ---- ordinal cells ---------------------------------------------------
excl_ord <- purrr::imap_dfr(ord_results, function(z, nm) {
  if (!is.na(z$error))
    return(.excl("ordinal cell", NA, nm, paste("cell failed:", z$error), NA, NA, "cell"))
  f <- z$res
  dplyr::bind_rows(
    if (!is.null(f$dropped) && nrow(f$dropped))
      .excl("ordinal cell", f$x, nm, "predictor level with n < min_n excluded",
            f$dropped[[f$x]], f$dropped$n),
    if (inherits(f, "ordinal_by_cont") && nrow(f$spacing$dropped))
      .excl("ordinal cell", f$x, nm,
            paste("x value with n < min_n_level excluded from the equal-spacing",
                  "test (and the factor figure, if used); kept in the slope"),
            f$spacing$dropped$level, f$spacing$dropped$n),
    if (!is.null(z$adj) && is.null(z$adj$error))
      .excl("ordinal cell", "career_stage", nm,
            "career stage missing: excluded from the adjusted fit only", NA,
            f$n - z$adj$n))
})

# ---- select-all cells ------------------------------------------------
excl_sa_cell <- purrr::imap_dfr(sa_results, function(z, nm) {
  f <- if (!is.null(z$res)) z$res$fits[[1]]
  dplyr::bind_rows(
    if (!is.null(z$dropped_levels))
      .excl("select-all cell", z$x, nm,
            "predictor level with < min_n_level respondents excluded",
            z$dropped_levels[[z$x]], z$dropped_levels$n_id),
    if (length(z$dropped_rate))
      .excl("select-all cell", z$x, nm, "item endorsed by < 3% or > 97%: not modelled",
            z$dropped_rate, NA, "items"),
    if (length(z$dropped_separated))
      .excl("select-all cell", z$x, nm, "item separated along x: not modelled",
            z$dropped_separated, NA, "items"),
    if (!is.null(f$spacing) && nrow(f$spacing$dropped))
      .excl("select-all cell", z$x, nm,
            "x value with < spacing_min_n respondents excluded from the equal-spacing test only",
            f$spacing$dropped$level, f$spacing$dropped$n),
    if (!is.null(f) && !is.null(z$res$adj) && is.null(z$res$adj$error))
      .excl("select-all cell", "career_stage", nm,
            "career stage missing: excluded from the adjusted fit only", NA,
            f$n_id - z$res$adj$n_id),
    if (!is.na(z$error))
      .excl("select-all cell", z$x, nm, paste("cell failed:", z$error), NA, NA, "cell"))
})

exclusions <- dplyr::bind_rows(
  .excl("data", NA, NA, "rows read (after the upstream target-audience filter)",
        NA, n_start),
  excl_demo, excl_io, excl_out, excl_sa, excl_ord, excl_sa_cell)

print(as.data.frame(exclusions), row.names = FALSE)
readr::write_csv(exclusions, file.path(fig_path, "exclusions.csv"))


# =================================================================
# 7. CROSS-TABS: are the demographics confounded with career stage?
# =================================================================
# Every model above pools over career stage. These tables say whether
# that pooling is hiding a compositional difference.
demo_crosstabs <- purrr::map(demo_specs[names(demo_specs) != "career_stage"],
                             function(spec) {
  d <- prep_demo(survey, spec) %>%
    dplyr::filter(!is.na(career_stage)) %>%
    droplevels()

  tab <- table(d[[spec$var]], d$career_stage)
  list(table = tab,
       expected = tryCatch(chisq.test(tab)$expected, error = function(e) NULL),
       # >20% of expected counts < 5 -> use simulate.p.value
       test = tryCatch(chisq.test(tab, simulate.p.value = TRUE, B = 1e4),
                       error = function(e) NULL))
})

purrr::iwalk(demo_crosstabs, function(z, nm) {
  cat("\n---", nm, "x career stage ---\n"); print(z$table)
  if (!is.null(z$test)) print(z$test)
})

# for gender, we have differences by career stage
# rest are pretty even


# =================================================================
# 8. ASSOCIATIONS AMONG ALL THE DEMOGRAPHIC PREDICTORS
# =================================================================
# Section 7 only crosses each demographic with career stage; this covers
# every pair. Spearman is a poor fit here: with 3-5 categories its
# attainable maximum is well below 1 and depends on the margins, so values
# are not comparable across pairs, and gender / work sector are unordered.
#
#   ordinal x ordinal -> POLYCHORIC correlation (two-step, polycor). The
#                        correlation of the latent normal variables behind
#                        the two ordered responses - the same latent model
#                        as the cumulative probit fits above. Ties are not
#                        an issue: it is estimated from the contingency
#                        table. The two-step estimator is used because full
#                        ML (ML = TRUE) fails more readily on sparse tables.
#   nominal x any     -> BIAS-CORRECTED Cramer's V, effectsize::cramers_v()
#                        with adjust = TRUE (Bergsma 2013, J Korean Stat Soc
#                        42:323-328, doi:10.1016/j.jkss.2012.10.002). Plain V
#                        is biased upward in small / sparse tables.
#
# "ordinal" / "nominal" is read from demo_specs$type, and each variable
# loses the same `drop` levels it loses in the models. Every pair then uses
# its own complete cases, so n differs by pair (reported in the table).

assoc_specs <- demo_specs
assoc_kind  <- vapply(assoc_specs, function(s)
  if (s$type == "count") "ordinal" else "nominal", character(1))

# one column per demographic, dropped levels set to NA (not removed rows,
# which would make every pair lose every other variable's exclusions)
assoc_dat <- purrr::map_dfc(assoc_specs, function(s) {
  v <- survey[[s$var]]
  v[v %in% s$drop] <- NA
  if (s$type == "count")
    factor(v, levels = sort(unique(v[!is.na(v)])), ordered = TRUE)
  else
    droplevels(factor(v))
})

# ---- the two measures ----------------------------------------------
# both return one row in the same shape, so the pairs bind together
assoc_polychoric <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x  <- droplevels(x[ok]); y <- droplevels(y[ok])
  r  <- polycor::polychor(x, y, std.err = TRUE)
  se <- sqrt(r$var[1, 1])
  tibble::tibble(
    measure  = "polychoric", estimate = r$rho, se = se,
    # Wald 95% CI, clipped to the possible range
    ci_low   = max(-1, r$rho - stats::qnorm(.975) * se),
    ci_high  = min( 1, r$rho + stats::qnorm(.975) * se),
    # Wald test of rho = 0
    p_value  = 2 * stats::pnorm(-abs(r$rho / se)), n = r$n,
    # test of bivariate normality of the latent pair; a small p says the
    # latent-normal model behind rho is doubtful for this pair
    normality_p = if (r$df > 0)
      stats::pchisq(r$chisq, r$df, lower.tail = FALSE) else NA_real_)
}

assoc_cramers_v <- function(x, y, B = 1e4) {
  tab <- table(x, y)                               # NA pairs excluded
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  # two-sided CI: the default ("greater") fixes the upper bound at 1
  v <- as.data.frame(effectsize::cramers_v(tab, adjust = TRUE, ci = 0.95,
                                           alternative = "two.sided"))
  tibble::tibble(
    measure  = "cramers_v_bc",
    estimate = v$Cramers_v_adjusted, se = NA_real_,
    ci_low   = v$CI_low, ci_high = v$CI_high,
    # simulated p, as in section 7: many expected counts are < 5
    p_value  = stats::chisq.test(tab, simulate.p.value = TRUE, B = B)$p.value,
    n = as.integer(sum(tab)), normality_p = NA_real_)
}

# ---- every pair ----------------------------------------------------
assoc_pairs <- as.data.frame(t(utils::combn(names(assoc_specs), 2)),
                             stringsAsFactors = FALSE) %>%
  stats::setNames(c("var1", "var2"))

assoc_tbl <- purrr::pmap_dfr(assoc_pairs, function(var1, var2) {
  both_ord <- assoc_kind[[var1]] == "ordinal" && assoc_kind[[var2]] == "ordinal"
  res <- tryCatch(
    if (both_ord) assoc_polychoric(assoc_dat[[var1]], assoc_dat[[var2]])
    else          assoc_cramers_v(assoc_dat[[var1]], assoc_dat[[var2]]),
    error = function(e) tibble::tibble(
      measure = if (both_ord) "polychoric" else "cramers_v_bc",
      error = conditionMessage(e)))
  dplyr::bind_cols(tibble::tibble(var1 = var1, var2 = var2), res)
})
if (!"error" %in% names(assoc_tbl)) assoc_tbl$error <- NA_character_
assoc_tbl$set <- "demographics"

# ---- gen_attitude x AI use frequency --------------------------------
# Both are OUTCOMES, so this pair is not modelled as y ~ x in section 6
# (the direction cannot be identified); it is reported here as a
# correlation, in both gen_attitude versions. In the table only - the
# heatmaps stay demographics x demographics.
ga_versions <- list(
  gen_attitude     = droplevels(factor(
    ifelse(survey$gen_attitude %in% c("Other", "Indifferent"), NA,
           as.character(survey$gen_attitude)),
    levels = levels(survey$gen_attitude), ordered = TRUE)),
  gen_attitude_mix = survey$gen_attitude_mix)

assoc_ga <- purrr::imap_dfr(ga_versions, function(g, nm)
  dplyr::bind_cols(
    tibble::tibble(var1 = nm, var2 = "ai_use_freq"),
    assoc_polychoric(g, assoc_dat$ai_use_freq),
    tibble::tibble(error = NA_character_, set = "outcome pair")))

assoc_tbl <- dplyr::bind_rows(assoc_tbl, assoc_ga)

print(as.data.frame(dplyr::mutate(assoc_tbl,
        dplyr::across(c(estimate, se, p_value, normality_p), ~ round(.x, 3)))),
      row.names = FALSE)

readr::write_csv(assoc_tbl, file.path(fig_path, "demographic_associations.csv"))

# ---- heatmaps ------------------------------------------------------
# Two panels, not one: rho is SIGNED (-1..1, diverging scale with a grey
# zero) and V is a MAGNITUDE (0..1, one-hue sequential scale). Sharing a
# colour scale would make V = 0.3 look like rho = 0.3.
assoc_label <- vapply(assoc_specs, `[[`, character(1), "label")

assoc_long <- assoc_tbl %>%
  dplyr::filter(is.na(error), set == "demographics") %>%
  dplyr::mutate(lab = sprintf("%.2f\nn = %d", estimate, n),
                # ink flips to the light surface on the darkest fills
                ink = ifelse(abs(estimate) > .6, "#fcfcfb", "#0b0b0b"))

# polychoric: lower triangle, in demo_specs order. AI use frequency is left
# out of the figure because it is an outcome here, not a demographic; its
# correlations stay in demographic_associations.csv.
ord_names <- setdiff(names(assoc_kind)[assoc_kind == "ordinal"], "ai_use_freq")
poly_plot_dat <- assoc_long %>%
  dplyr::filter(measure == "polychoric",
                var1 %in% ord_names, var2 %in% ord_names) %>%
  dplyr::mutate(
    # put the later variable in demo_specs order on the rows
    row = factor(assoc_label[var2], levels = rev(assoc_label[ord_names])),
    col = factor(assoc_label[var1], levels = assoc_label[ord_names]))

p_poly <- ggplot2::ggplot(poly_plot_dat, ggplot2::aes(col, row, fill = estimate)) +
  ggplot2::geom_tile(colour = "#fcfcfb", linewidth = 1) +
  ggplot2::geom_text(ggplot2::aes(label = lab, colour = ink), size = 3) +
  ggplot2::scale_colour_identity() +
  ggplot2::scale_fill_gradient2(low = "#e34948", mid = "#f0efec",
                                high = "#1c5cab", midpoint = 0,
                                limits = c(-1, 1), name = "polychoric\nrho") +
  ggplot2::scale_x_discrete(labels = scales::label_wrap(12)) +
  ggplot2::scale_y_discrete(labels = scales::label_wrap(16)) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Ordinal x ordinal demographics",
                caption = "two-step polychoric correlation; pairwise complete cases") +
  ggplot2::coord_equal() +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid = ggplot2::element_blank())

ggsave(file.path(fig_path, "demographic_assoc_polychoric.jpg"), p_poly,
       width = 7, height = 6)

# Cramer's V: each nominal variable (rows) against every other variable,
# again without AI use frequency (see above)
nom_names <- names(assoc_kind)[assoc_kind == "nominal"]
cv_plot_dat <- assoc_long %>%
  dplyr::filter(measure == "cramers_v_bc",
                var1 != "ai_use_freq", var2 != "ai_use_freq") %>%
  # orient each pair so the nominal variable is the row; a nominal x
  # nominal pair is shown once, in combn() order
  dplyr::mutate(
    flip = !var1 %in% nom_names,
    row_var = ifelse(flip, var2, var1),
    col_var = ifelse(flip, var1, var2),
    row = factor(assoc_label[row_var], levels = rev(assoc_label[nom_names])),
    col = factor(assoc_label[col_var],
                 levels = assoc_label[names(assoc_specs)]),
    ink = ifelse(estimate > .6, "#fcfcfb", "#0b0b0b")) %>%
  droplevels()

p_cv <- ggplot2::ggplot(cv_plot_dat, ggplot2::aes(col, row, fill = estimate)) +
  ggplot2::geom_tile(colour = "#fcfcfb", linewidth = 1) +
  ggplot2::geom_text(ggplot2::aes(label = lab, colour = ink), size = 3) +
  ggplot2::scale_colour_identity() +
  ggplot2::scale_fill_gradient(low = "#cde2fb", high = "#104281",
                               limits = c(0, 1), name = "Cramer's V\n(bias-corr.)") +
  ggplot2::scale_x_discrete(labels = scales::label_wrap(12)) +
  ggplot2::scale_y_discrete(labels = scales::label_wrap(16)) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Nominal demographics x every other demographic",
                caption = "bias-corrected Cramer's V (Bergsma 2013); pairwise complete cases") +
  ggplot2::coord_equal() +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid = ggplot2::element_blank())

ggsave(file.path(fig_path, "demographic_assoc_cramersv.jpg"), p_cv,
       width = 9, height = 3.5)

# -------------------------------------------------
# Session info for the methods section
# -------------------------------------------------
  sessionInfo()
