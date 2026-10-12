# -------------------------------------------------
# survey_elmendorf_survey_stats.R
# -------------------------------------------------
# Bayesian cumulative probit models with monotonic predictors where both
# x and y are Likert (questions 1-3), GLMMs for select-all-that-apply
# questions, and ordinal (clm) models for the remaining questions.
#
# Structure:
#   1. setup, data read, factor releveling
#   2. helper functions for the Likert-pair models
#   3. Likert-pair analyses (Q1-Q3)
#   4. helper functions for the select-all models
#   5. select-all analyses
#   6. gender / AI-use analysis
# -------------------------------------------------

# -------------------------------------------------
# Project setup
# -------------------------------------------------
# TO DO
# deal with the inconsistencies in the re.formula implementation
# across brms vs emm in the predictions
# https://github.com/easystats/modelbased/issues/579

source(file.path("-setup.r"))          # clears environment, creates folders
gc()

set.seed(1)                            # reproducibility for anything stochastic

# models are slow to run; set FALSE to reload cached fits while fiddling
# with plots. Either way the objects you analyse are built by the same
# summarise_likert_pair() call, so the two paths cannot drift apart.
rerun_mods <- TRUE

check_lin

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
  brms,
  marginaleffects,
  emmeans,
  multcomp,          # cld() for the compact letter display
  loo,
  posterior,
  bayestestR,
  lme4,
  performance,
  ordinal,
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
  conflicted::conflict_prefer("ar", "brms", quiet = TRUE)
  conflicted::conflict_prefer("chisq.test", "stats", quiet = TRUE)
  conflicted::conflict_prefer("col_factor", "scales", quiet = TRUE)
  conflicted::conflict_prefer("discard", "purrr", quiet = TRUE)
  conflicted::conflict_prefer("expand", "tidyr", quiet = TRUE)
  conflicted::conflict_prefer("fisher.test", "stats", quiet = TRUE)
  conflicted::conflict_prefer("geyser", "MASS", quiet = TRUE)
  conflicted::conflict_prefer("kidney", "brms", quiet = TRUE)
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
data_path  <- file.path("data", "01_tidied-responses.csv")
graph_path <- "graphs"
fig_path   <- file.path(graph_path, "analysis_figs_ordinal")
fit_path   <- "fits_ordinal"

dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)
dir.create(fit_path, recursive = TRUE, showWarnings = FALSE)


check_linearity = TRUE
spline_df       = 3
min_n           = 20

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
  clean_names()   # snake_case, removes spaces, etc.

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

# Ensure the ordinal helper columns are numeric.
survey <- survey %>%
  dplyr::mutate(dplyr::across(dplyr::ends_with("_value"), as.numeric))

unique (survey$work_sector)
table(survey$work_sector)

survey <-survey %>%
  dplyr::mutate(prof_work_sector =
    ifelse(grepl('Student', prof_role), 'Student', work_sector)
  )

# 13 ambiguous, let's drop for now
# ambiguous <- survey %>%
#   filter(work_sector != "Academia (Higher Education)"& prof_work_sector == 'Student')

# View (ambiguous %>% select(work_sector, prof_role, prof_work_sector))


#View (survey %>% select(caregiver, caregiver_IO) %>% distinct())

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


# ---- frequentist pre-check on the covariate -----------------------
# Cheap clm test of whether the x effect differs across the covariate.
# Both models are fitted to ONE dataset with complete cases on all three
# variables, so the LRT compares genuinely nested models.
covariate_check <- function(dat, x, y, covariate, link = "probit") {

  if (is.null(covariate) || !covariate %in% names(dat)) return(NULL)

  d <- dat %>%
    dplyr::filter(!is.na(.data[[covariate]])) %>% droplevels()

  f_add <- stats::as.formula(sprintf("%s ~ %s + %s", y, x, covariate))
  f_int <- stats::as.formula(sprintf("%s ~ %s * %s", y, x, covariate))

  m_add <- try(ordinal::clm(f_add, data = d, link = link), silent = TRUE)
  m_int <- try(ordinal::clm(f_int, data = d, link = link), silent = TRUE)

 ok <- function(f) {
    if (inherits(f, "try-error")) return(FALSE)
    cc <- paste(f$convergence$code, collapse = "/")
    !is.null(cc) && length(cc) > 0 && all(cc == 0)
  }

  if (!ok(m_add) || !ok(m_int)) {
    warning("Covariate interaction model did not converge; not tested.",
            call. = FALSE)
    return(NULL)
  }

  anova(m_add, m_int)
}


 
# ---- convergence helpers ------------------------------------------
# clm's convergence$code can be a VECTOR when more than one condition
# fires, so it must be collapsed before use in a scalar test.
#   0  clean
#   1  parameters may have few correct decimals (imprecise, usually fine)
#  -3  thresholds not increasing -> the fit is INVALID, never interpret it
.clm_ok <- function(f) {
  if (inherits(f, "try-error")) return(FALSE)
  cc <- f$convergence$code
  !is.null(cc) && length(cc) > 0 && all(cc == 0)
}
 
.clm_usable <- function(f) {
  if (inherits(f, "try-error")) return(FALSE)
  cc <- f$convergence$code
  !is.null(cc) && length(cc) > 0 && !any(cc == -3)
}
 
 
# =================================================================
# fit_ordinal_by(): ordinal outcome ~ CATEGORICAL predictor
#   1. drops predictor levels with < min_n respondents
#   2. reports the predictor x covariate interaction (diagnostic only)
#   3. fits the final model on x alone, pooled over the covariate
# =================================================================
fit_ordinal_by <- function(data, x, y,
                           drop_levels = character(0),
                           min_n       = 20,
                           covariate   = "career_stage",
                           link        = "probit",
                           wrap_axis   = 12,
                           wrap_facet  = 20,
                           verbose     = TRUE) {
 
  stopifnot(all(c(x, y) %in% names(data)))
 
  # ---- 1. screen predictor levels -------------------------------
  dat <- data %>%
    dplyr::filter(!is.na(.data[[x]]), !is.na(.data[[y]]),
                  !.data[[x]] %in% drop_levels) %>%
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
 
  # cross-tab against the covariate, for the record
  cells <- if (!is.null(covariate) && covariate %in% names(dat))
    table(dat[[x]], dat[[covariate]]) else NULL
 
  # ---- 2. interaction with the covariate (diagnostic) -----------
  # Fitted on ONE dataset with complete cases on all three variables,
  # so the two models in the LRT are genuinely nested. Fitting them to
  # different subsets is a common and silent error.
  int_test <- NULL; fit_int <- NULL
  if (!is.null(covariate) && covariate %in% names(dat)) {
    dat_int <- dat %>% dplyr::filter(!is.na(.data[[covariate]])) %>% droplevels()
 
    f_int <- stats::as.formula(sprintf("%s ~ %s * %s", y, x, covariate))
    f_add <- stats::as.formula(sprintf("%s ~ %s + %s", y, x, covariate))
 
    fit_int <- try(ordinal::clm(f_int, data = dat_int, link = link), silent = TRUE)
    fit_add <- try(ordinal::clm(f_add, data = dat_int, link = link), silent = TRUE)
 
    if (.clm_ok(fit_int) && .clm_ok(fit_add)) {
      int_test <- anova(fit_add, fit_int)
    } else {
      warning("Interaction model did not converge; interaction not tested. ",
              "Usually sparse cells in the three-way cross-classification. ",
              "See $cells.", call. = FALSE)
      fit_int <- NULL
    }
  }
 
  # ---- 3. final model: x alone, pooled over the covariate -------
  # Uses the larger dataset (covariate NAs allowed) to maximise n.
  fit <- ordinal::clm(stats::as.formula(sprintf("%s ~ %s", y, x)),
                      data = dat, link = link)
 
  if (!.clm_usable(fit))
    warning("Final model convergence code ",
            paste(fit$convergence$code, collapse = "/"),
            ": code -3 means the thresholds are not increasing and the fit ",
            "is INVALID. Usually a sparsely populated outcome category - ",
            "check table(dat[[y]]) and collapse it, or refit with brms.",
            call. = FALSE)
 
  # assumption checks. nominal_test() does not support the probit link,
  # so the partial-PO model is fitted explicitly and compared.
  scale_chk <- try(ordinal::scale_test(fit), silent = TRUE)
  po_chk <- try(
    anova(fit, ordinal::clm(stats::as.formula(sprintf("%s ~ 1", y)),
                            nominal = stats::as.formula(paste("~", x)),
                            data = dat, link = link)),
    silent = TRUE)
 
  lrt <- drop1(fit, test = "Chisq")
 
  # latent-scale contrasts: differences in latent SD units, so they read
  # like a Cohen's d. The CI matters more than the p-value - it says how
  # large an effect can still be ruled out.
  emm_link  <- emmeans::emmeans(fit, stats::as.formula(paste("~", x)),
                                mode = "linear.predictor")
  contrasts <- confint(pairs(emm_link, adjust = "tukey"))
 
  if (verbose) {
    print(summary(fit)); print(lrt)
    if (!is.null(int_test)) {
      cat("\ninteraction with ", covariate, ":\n", sep = ""); print(int_test)
    }
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
    ggplot2::scale_x_discrete(labels = scales::label_wrap(wrap_axis)) +
    ggplot2::labs(x = NULL, y = "P(category)",
                  caption = "grey = observed; red = model estimate") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
 
  structure(list(
    x = x, y = y, n = nrow(dat), K_x = nlevels(dat[[x]]),
    dropped = small, level_n = lvl_n, cells = cells,
    data = dat, fit = fit, fit_int = fit_int,
    int_test = int_test, lrt = lrt,
    scale_test = scale_chk, po_test = po_chk,
    emm = emm, raw = raw, contrasts = contrasts, plot = p
  ), class = "ordinal_by")
}
 
 
print.ordinal_by <- function(z, ...) {
 
  cat(sprintf("\n%s ~ %s   (n = %d, %d levels)\n", z$y, z$x, z$n, z$K_x))
  if (nrow(z$dropped))
    cat("dropped:", paste(z$dropped[[z$x]], collapse = ", "), "\n")
 
  p <- z$lrt[["Pr(>Chi)"]][2]
  cat(sprintf("main effect   LRT chi2 = %.2f, df = %d, p = %s\n",
              z$lrt[["LRT"]][2], z$lrt[["Df"]][2],
              if (p < .001) "< .001" else sprintf("%.3f", p)))
 
  if (!is.null(z$int_test)) {
    pi <- z$int_test[["Pr(>Chisq)"]][2]
    cat(sprintf("x covariate   LRT chi2 = %.2f, df = %d, p = %s\n",
                z$int_test[["LR.stat"]][2], z$int_test[["df"]][2],
                if (pi < .001) "< .001" else sprintf("%.3f", pi)))
  }
 
  cat("\nlatent-scale contrasts (probit SD units):\n")
  print(as.data.frame(z$contrasts), row.names = FALSE, digits = 2)
  cat("\n")
  invisible(z)
}
 
 
# =================================================================
# fit_ordinal_by_continuous(): ordinal outcome ~ CONTINUOUS predictor
#
#   1. screens for sample size and predictor variability
#   2. checks linearity on the latent scale (linear vs natural spline)
#   3. reports the predictor x covariate interaction (diagnostic only)
#   4. fits the final model on x alone, pooled over the covariate
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
                                      covariate       = "career_stage",
                                      link            = "probit",
                                      check_linearity = TRUE,
                                      spline_df       = 3,
                                      min_n           = 20,
                                      min_distinct_x  = 6,
                                      wrap_facet      = 20,
                                      wrap_axis       = 12,
                                      discrete_axis   = TRUE,
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
 
  # ---- 2. linearity on the latent scale --------------------------
  # The cumulative model assumes x shifts the latent variable linearly.
  # Compare against a natural spline; a significant improvement means the
  # single slope is hiding curvature. This tests the latent-scale SHAPE,
  # not the proportional-odds assumption (po_test, below).
  lin_test <- NULL; fit_spline <- NULL
  if (check_linearity && n_distinct_x > spline_df + 2) {
    f_lin <- stats::as.formula(sprintf("%s ~ %s", y, x))
    f_spl <- stats::as.formula(
      sprintf("%s ~ splines::ns(%s, df = %d)", y, x, spline_df))
 
    fit_l      <- try(ordinal::clm(f_lin, data = dat, link = link), silent = TRUE)
    fit_spline <- try(ordinal::clm(f_spl, data = dat, link = link), silent = TRUE)
 
    if (.clm_ok(fit_l) && .clm_ok(fit_spline)) {
      lin_test <- anova(fit_l, fit_spline)
    } else {
      warning("Spline model did not converge; linearity not tested.",
              call. = FALSE)
      fit_spline <- NULL
    }
  }
 
  # ---- 3. interaction with the covariate (diagnostic) ------------
  int_test <- NULL; fit_int <- NULL
  if (!is.null(covariate) && covariate %in% names(dat)) {
    dat_int <- dat %>%
      dplyr::filter(!is.na(.data[[covariate]])) %>% droplevels()
 
    f_int <- stats::as.formula(sprintf("%s ~ %s * %s", y, x, covariate))
    f_add <- stats::as.formula(sprintf("%s ~ %s + %s", y, x, covariate))
 
    fit_int <- try(ordinal::clm(f_int, data = dat_int, link = link), silent = TRUE)
    fit_add <- try(ordinal::clm(f_add, data = dat_int, link = link), silent = TRUE)
 
    if (.clm_ok(fit_int) && .clm_ok(fit_add)) {
      int_test <- anova(fit_add, fit_int)
    } else {
      warning("Interaction model did not converge; interaction not tested. ",
              "Usually sparse cells in the three-way cross-classification.",
              call. = FALSE)
      fit_int <- NULL
    }
  }
 
  # ---- 4. final model: x alone, pooled over the covariate --------
  fit <- ordinal::clm(stats::as.formula(sprintf("%s ~ %s", y, x)),
                      data = dat, link = link)
 
  if (!.clm_usable(fit))
    warning("Final model convergence code ",
            paste(fit$convergence$code, collapse = "/"),
            ": code -3 means the thresholds are not increasing and the fit ",
            "is INVALID. Usually a sparsely populated outcome category - ",
            "check table(dat[[y]]) and collapse it, or refit with brms.",
            call. = FALSE)
 
  scale_chk <- try(ordinal::scale_test(fit), silent = TRUE)
  po_chk <- try(
    anova(fit, ordinal::clm(stats::as.formula(sprintf("%s ~ 1", y)),
                            nominal = stats::as.formula(paste("~", x)),
                            data = dat, link = link)),
    silent = TRUE)
 
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
 
  if (verbose) {
    print(summary(fit)); print(lrt)
    cat(sprintf("\nslope: %.3f [%.3f, %.3f] probit SD per unit of %s\n",
                slope$estimate, slope$conf.low, slope$conf.high, x))
    if (!is.null(lin_test)) {
      cat("\nlinearity (linear vs spline):\n"); print(lin_test)
    }
    if (!is.null(int_test)) {
      cat("\ninteraction with ", covariate, ":\n", sep = ""); print(int_test)
    }
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
 
  p <- ggplot2::ggplot(emm, ggplot2::aes(.x_plot, estimate)) +
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
    ggplot2::labs(x = x, y = "P(category)",
                  caption = "grey = observed; red = model estimate") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
 
  # put the original factor labels back on the axis when x was coerced
  if (discrete_axis && !is.null(x_labels))
    p <- p + ggplot2::scale_x_discrete(
      labels = scales::label_wrap(wrap_axis)(x_labels[as.character(x_val)]))
 
  structure(list(
    x = x, y = y, n = nrow(dat), n_distinct_x = n_distinct_x,
    x_labels = x_labels,
    data = dat, fit = fit, fit_int = fit_int, fit_spline = fit_spline,
    slope = slope, ame = ame, lrt = lrt,
    lin_test = lin_test, int_test = int_test,
    scale_test = scale_chk, po_test = po_chk,
    emm = emm, raw = raw, plot = p
  ), class = "ordinal_by_cont")
}
 
 
print.ordinal_by_cont <- function(z, ...) {
 
  cat(sprintf("\n%s ~ %s   (n = %d, %d distinct values of %s)\n",
              z$y, z$x, z$n, z$n_distinct_x, z$x))
 
  s <- z$slope
  cat(sprintf("slope   b = %.3f [%.3f, %.3f]  (probit SD per unit of %s)\n",
              s$estimate, s$conf.low, s$conf.high, z$x))
 
  p <- z$lrt[["Pr(>Chi)"]][2]
  cat(sprintf("        LRT chi2 = %.2f, df = %d, p = %s\n",
              z$lrt[["LRT"]][2], z$lrt[["Df"]][2],
              if (p < .001) "< .001" else sprintf("%.3f", p)))
 
  fmt <- function(tst, lab, pcol = "Pr(>Chisq)") {
    if (is.null(tst)) {
      cat(sprintf("%-16s not tested\n", lab)); return(invisible())
    }
    pv <- tst[[pcol]][2]
    cat(sprintf("%-16s p = %s%s\n", lab,
                if (pv < .001) "< .001" else sprintf("%.3f", pv),
                if (pv < .05) "   <- flagged" else ""))
  }
 
  cat("\n")
  fmt(z$lin_test, "linearity")
  fmt(z$int_test, "x covariate")
  cat("\n")
 
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
expand_select_all <- function(survey, q, id_col = "response_id",
                              keep_na_x = FALSE, x = NULL,
                              all_respondents = FALSE) {

  svy <- survey %>% dplyr::rename(ResponseId = dplyr::all_of(id_col))

  # the chosen options, one row per respondent x selected option
  yes <- prep_select_all(svy, q = q, summarize = FALSE) %>%
    dplyr::rename(!!q := value) %>%
    dplyr::left_join(svy %>% dplyr::select(-dplyr::all_of(q)), by = "ResponseId") %>%
    dplyr::mutate(response = "Yes")

  if (nrow(yes) == 0) stop("prep_select_all() returned no rows for q = ", q)

  # Respondent universe. all_respondents = TRUE includes people who
  # selected nothing (their row of zeros is real data); FALSE restricts
  # to people who ticked at least one box, which is appropriate here
  # because every question offered a "none of the above" option.
  ids <- if (all_respondents) unique(svy$ResponseId) else unique(yes$ResponseId)

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

  if (!is.null(x) && !keep_na_x)
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
clean_select_all <- function(long, q, none_pattern,
                             text_col  = NULL,
                             none_text = character(0),
                             some_text = character(0),
                             quiet = FALSE) {

  none_rows <- long %>%
    dplyr::filter(grepl(none_pattern, .data[[q]]), response == "Yes")

  long <- long %>% dplyr::filter(!grepl(none_pattern, .data[[q]]))

  # free-text "Other" answers that actually mean "none"
  if (!is.null(text_col) && length(none_text)) {
    ids <- long %>%
      dplyr::filter(.data[[text_col]] %in% none_text) %>%
      dplyr::distinct(ResponseId) %>% dplyr::pull(ResponseId)
    long <- long %>%
      dplyr::mutate(
        response = ifelse(.data[[q]] == "Other" & ResponseId %in% ids,
                          "No", response),
        response_numeric = ifelse(.data[[q]] == "Other" & ResponseId %in% ids,
                                  0L, response_numeric))
  }

  # free-text answers that actually mean "some" (so not a real conflict)
  if (!is.null(text_col) && length(some_text)) {
    ids <- long %>%
      dplyr::filter(.data[[text_col]] %in% some_text) %>%
      dplyr::distinct(ResponseId) %>% dplyr::pull(ResponseId)
    none_rows <- none_rows %>% dplyr::filter(!ResponseId %in% ids)
  }

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


# ---- GLMM + summaries + plot -------------------------------------
#fit_select_all(data = reason_long_career, x = "career_stage",
                       #    q_type = "q_type", re_structure = "correlated")


fit_select_all <- function(data, x, id = "ResponseId",
                        q_type = NULL,          # column grouping questions
                         re_structure = c("nested", "correlated", "uncorrelated"),
                           wrap_facet = 25, wrap_axis = 20,
                           order_facets = TRUE,
                           add_letters  = TRUE,
                           letter_adjust = "mvt",  # accounts for the
                           # correlation among contrasts; "sidak" assumes
                           # independence and is conservative. Tukey is
                           # unavailable once `by` splits this into one
                           # family per question.
                           seed_mvt = 12345,
                           re.form  = NULL,
                           verbose  = TRUE) {

  stopifnot(all(c("question", "response_numeric", x, id) %in% names(data)))
  data <- data %>% dplyr::filter(!is.na(.data[[x]])) %>% droplevels()

  # sanity check before fitting
  # make sure there are some individuals in every x group
  tab <- table(data[[x]], data$question)
  if (any(tab == 0))
    warning("Empty cells in ", x, " x question; estimates may be unstable.")

  # `question * x` rather than `0 + question + question:x`: identical fit,
  # but prediction helpers, drop1() and type-III tests all behave.
  # form <- stats::as.formula(
  #   sprintf("response_numeric ~ question * %s + (1 | %s)", x, id))
  
  re_structure <- match.arg(re_structure)

  # with correlated you get  (0 + q_type | ResponseId), which I think is what you want
  # eg we expect people with a lot of neg reasons will find fewer positive reasons
  # with uncorrelated you get  (0 + q_type || ResponseId)

  # engagement plus idiosyncratic leanings,
  # ie some people are just box tickers b
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
      uncorrelated = sprintf("(0 + %s || %s)", q_type, id))
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
  # Collapses the item-level data to counts: "how many boxes did you tick,
  # out of those offered?" rather than "which ones". A sum of Bernoullis
  # sharing one probability IS a Binomial, so this is the same likelihood
  # as the item-level model minus any item-specific term. cbind(k, n - k)
  # supplies the correct denominator when types hold different numbers of
  # items - a raw count would make the larger type look higher by default.
  omni <- NULL

  if (is.null(q_type)) {

    cnt <- data %>%
      dplyr::group_by(.data[[id]], .data[[x]]) %>%
      dplyr::summarise(k = sum(response_numeric), n = dplyr::n(),
                       .groups = "drop")

    # one row per person, so no clustering remains and a plain glm is right
    m_o <- stats::glm(stats::as.formula(sprintf("cbind(k, n - k) ~ %s", x)),
                      family = binomial, data = cnt)

    # person-level heterogeneity still makes counts overdispersed relative
    # to binomial, which would make the SEs too small
    disp <- sum(stats::residuals(m_o, type = "pearson")^2) /
            stats::df.residual(m_o)

    omni <- list(kind = "binomial_glm", counts = cnt, fit = m_o,
                 test = drop1(m_o, test = "Chisq"), dispersion = disp)

    if (disp > 1.5) {
      warning("Omnibus counts are overdispersed (dispersion = ",
              round(disp, 2), "); the quasibinomial test is the one to ",
              "report. See $omni$test_quasi.", call. = FALSE)
      m_q <- stats::update(m_o, family = stats::quasibinomial)
      omni$fit_quasi  <- m_q
      omni$test_quasi <- drop1(m_q, test = "F")
    }

  } else {

    cnt <- data %>%
      dplyr::group_by(.data[[id]], .data[[x]], .data[[q_type]]) %>%
      dplyr::summarise(k = sum(response_numeric), n = dplyr::n(),
                       .groups = "drop")

    f_type <- stats::as.formula(sprintf("cbind(k, n - k) ~ %s", q_type))
    f_add  <- stats::as.formula(sprintf("cbind(k, n - k) ~ %s + %s", x, q_type))
    f_int  <- stats::as.formula(sprintf("cbind(k, n - k) ~ %s * %s", x, q_type))

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

    omni <- list(
      kind    = "clustered_glm",
      counts  = cnt, fit = m_int, fit_add = m_add, vcov = V_int,
      coefs   = lmtest::coeftest(m_int, vcov. = V_int),
      test_x  = lmtest::waldtest(m_type, m_add, vcov = V_add, test = "Chisq"),
      test    = lmtest::waldtest(m_add,  m_int, vcov = V_int, test = "Chisq"))
  }

  if (verbose) {
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

  # Predictions. re.form = NULL uses the fitted random effects and averages,
  # giving the POPULATION-AVERAGE probability that is comparable to the
  # observed bars. re.form = NA sets them to zero, giving the prediction
  # for a TYPICAL respondent, which on a nonlinear link is pulled away from
  # the raw proportions (more so the further from 50%).
  # Caveat: with re.form = NULL the intervals reflect fixed-effect
  # uncertainty only - marginaleffects warns about this.
  emm <- as.data.frame(
    marginaleffects::avg_predictions(
      fit, by = c("question", x), re.form = re.form)) %>%
    dplyr::rename(prob = estimate, asymp.LCL = conf.low, asymp.UCL = conf.high)

  # ---- compact letter display per question -----------------------
  # Pairwise contrasts WITHIN each question; groups sharing a letter do
  # not differ. No adjustment ACROSS questions - each item is treated as
  # its own family, which must be stated in the methods.
  cld_df <- NULL
  if (add_letters) {
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
      lv <- levels(data[[x]])
      cld_df <- as.data.frame(cld_df) |>
        dplyr::mutate(
          .group = trimws(.group),
          dplyr::across(dplyr::all_of(x),
                        \(z) factor(as.character(z), levels = lv))) |>
        dplyr::group_by(question) |>
        # blank the letters where nothing differs, so panels stay readable
        dplyr::mutate(.group = if (dplyr::n_distinct(.group) == 1) "" else .group) |>
        dplyr::ungroup()
    }
  }

  raw <- data %>%
    dplyr::group_by(question, .data[[x]]) %>%
    dplyr::summarise(prop = mean(response_numeric), n = dplyr::n(),
                     .groups = "drop")

  # cast both key columns to the same type before joining: cld() returns
  # a plain factor and character question, raw has ordered / factor
  if (!is.null(cld_df)) {
    lv <- levels(data[[x]])
    harmonise <- function(d) {
      d[[x]] <- factor(as.character(d[[x]]), levels = lv)
      d$question <- as.character(d$question)
      d
    }
    raw    <- harmonise(raw)
    cld_df <- harmonise(cld_df)
    emm    <- harmonise(emm)

    raw <- raw %>%
      dplyr::left_join(cld_df %>% dplyr::select(question, dplyr::all_of(x), .group),
                       by = c("question", x))
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

  if (order_facets) {
    lv_q <- as.character(ord$question)
    emm$question <- factor(as.character(emm$question), levels = lv_q)
    raw$question <- factor(as.character(raw$question), levels = lv_q)
  }

  cap_model <- if (is.null(re.form))
    "red = model estimate (population average)"
  else if (length(re.form) == 1 && is.na(re.form))
    "red = model estimate (typical respondent, random effects = 0)"
  else
    "red = model estimate"

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
        question = function(z) scales::label_wrap(wrap_facet)(lab_map[z]))) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(.05, .20))) +  # room for 2 label rows
    ggplot2::scale_x_discrete(labels = scales::label_wrap(wrap_axis)) +
    ggplot2::labs(x = NULL, y = "P(yes)",
                  caption = paste("grey = observed;", cap_model,
                                  "\nstages sharing a letter do not differ",
                                  "(within item); no letters = none differ")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # letters on a fixed line below the counts, so heights are consistent
  if (!is.null(cld_df))
    p <- p + ggplot2::geom_text(
      data = raw, ggplot2::aes(y = Inf, label = .group),
      vjust = 2.8, size = 2.4, colour = "grey20", fontface = "bold")

  list(x = x, n_obs = nrow(data), n_id = dplyr::n_distinct(data[[id]]),
       re.form = re.form, data = data, fit = fit,
       emm = emm, raw = raw, cld = cld_df, plot = p,
       chisq_test = chisq_test, omni = omni,
       q_type = q_type, re_structure = re_structure,
       order = ord, labels = lab_map, cells = tab)
}


# ---- fit both re.form variants, cache, save both figures ---------
run_select_all <- function(long, x, tag, rerun = rerun_mods,
                           width = 15, height = 14, scale = 0.8, ...) {

  f_pop  <- file.path(fit_path, paste0("res_", tag, ".rds"))
  f_subj <- file.path(fit_path, paste0("res_", tag, "_1.rds"))

  if (rerun) {
    saveRDS(fit_select_all(long, x = x, re.form = NULL, ...), f_pop)
    saveRDS(fit_select_all(long, x = x, re.form = NA,   ...), f_subj)
  }

  out <- list(pop = readRDS(f_pop), subj = readRDS(f_subj))

  ggsave(file.path(fig_path, paste0(tag, ".jpg")),   out$pop$plot,
         width = width, height = height, scale = scale)
  ggsave(file.path(fig_path, paste0(tag, "_1.jpg")), out$subj$plot,
         width = width, height = height, scale = scale)

  out
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
# free-text "Other" answers that need reassigning on the opportunities item
opps_none_text <- c(
  "Currently I feel none, but I would be hesitantly open to being convinced of a specific benefit of AI",
  "There is no ethical way to use generative AI given its current ownership model",
  "Having said all these nasty things about AI and about the amoral engineers who are leading us down the garden path, I will admit that several of my colleagues, who have large data sets, have used several programs, and have clearly drunk the coolaid."
)
opps_some_text <- "faster work (does not equal quality work)."

clean_opps <- function(long)
  clean_select_all(long, q = "promising_opps", none_pattern = "None",
                   text_col  = "promising_opps_14_text",
                   none_text = opps_none_text,
                   some_text = opps_some_text)

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
                     q_type = NULL, label = "opportunities"),
  challenges  = list(q = "challenges",     clean = clean_challenges,
                     q_type = NULL, label = "challenges"),
  reasons     = list(q = "ai_use_reasons", clean = clean_reasons,
                     q_type = "q_type", label = "reasons for use")
)


# ---- ordinal outcomes ---------------------------------------------
ord_specs <- list(
  ai_use_freq = list(
    y      = "ai_use_freq",
    label  = "AI use frequency",
    filter = NULL),

  gen_attitude = list(
    y      = "gen_attitude",
    label  = "general attitude",
    filter = function(d)
      dplyr::filter(d, !gen_attitude %in% c("Other", "Indifferent")))
)


# =================================================================
# 5b. CELL DIAGNOSTICS (run before fitting, recorded either way)
# =================================================================
check_cells <- function(long, x, min_n = 5) {

  tab <- long %>%
    dplyr::group_by(question, .data[[x]], .drop = FALSE) %>%
    dplyr::summarise(n   = dplyr::n(),
                     yes = sum(response_numeric),
                     p   = mean(response_numeric),
                     .groups = "drop")

  list(
    empty     = dplyr::filter(tab, n == 0),
    # all-yes or all-no cells cause complete separation, which is what
    # makes glmer fail. See Gelman et al. 2008, 10.1214/08-AOAS191
    separated = dplyr::filter(tab, n > 0, yes == 0 | yes == n),
    thin      = dplyr::filter(tab, n > 0, n < min_n),
    item_rates = long %>%
      dplyr::group_by(question) %>%
      dplyr::summarise(n = dplyr::n(), p = mean(response_numeric),
                       .groups = "drop") %>%
      dplyr::arrange(p),
    full = tab
  )
}


# ---- drop items that cannot be modelled ---------------------------
# An item endorsed by almost nobody (or almost everybody) within a
# demographic produces separated cells and no estimable interaction.
# Report those items as plain percentages instead of modelling them.
drop_unmodellable <- function(long, lo = 0.03, hi = 0.97, quiet = FALSE) {

  rates <- long %>%
    dplyr::group_by(question) %>%
    dplyr::summarise(p = mean(response_numeric), .groups = "drop")

  keep <- rates %>% dplyr::filter(p >= lo, p <= hi) %>% dplyr::pull(question)
  gone <- setdiff(as.character(rates$question), as.character(keep))

  if (length(gone) && !quiet)
    message("  dropping ", length(gone), " item(s) outside [", lo, ", ", hi,
            "]: ", paste(gone, collapse = ", "))

  out <- long %>% dplyr::filter(question %in% keep) %>% droplevels()
  attr(out, "dropped_items") <- gone
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
                           re_forms  = list(pop = NULL, subj = NA),
                           width = 15, height = 14, scale = 0.8, ...) {

  out <- list()

  for (nm in names(re_forms)) {
    suffix <- if (nm == "pop") "" else paste0("_", nm)
    f      <- file.path(fit_path, paste0("res_", tag, suffix, ".rds"))

    if (rerun)
      saveRDS(fit_select_all(long, x = x, re.form = re_forms[[nm]], ...), f)

    out[[nm]] <- readRDS(f)

    ggsave(file.path(fig_path, paste0(tag, suffix, ".jpg")), out[[nm]]$plot,
           width = width, height = height, scale = scale)
  }

  out
}


# ---- one cell of the grid ------------------------------------------
run_sa_cell <- function(survey, sa, demo,
                        min_n_level = 20, verbose = FALSE, ...) {

  tag <- paste0(sa$label_tag, "_by_", demo$var)
  message("\n=== ", sa$q, " by ", demo$var, " ===")

  dat <- prep_demo(survey, demo)

  long <- expand_select_all(dat, q = sa$q, x = demo$var) %>%
    sa$clean()

  # drop demographic levels too small to support an item-level model
  lvl_n <- long %>%
    dplyr::distinct(ResponseId, .data[[demo$var]]) %>%
    dplyr::count(.data[[demo$var]], name = "n_id")
  small <- lvl_n %>% dplyr::filter(n_id < min_n_level)

  if (nrow(small)) {
    message("  dropping level(s) with < ", min_n_level, " respondents: ",
            paste(sprintf("%s (n = %d)", small[[demo$var]], small$n_id),
                  collapse = ", "))
    long <- long %>%
      dplyr::filter(!.data[[demo$var]] %in% small[[demo$var]]) %>%
      droplevels()
  }

  chk  <- check_cells(long, x = demo$var)
  long <- drop_unmodellable(long)

  if (nlevels(long[[demo$var]]) < 2 || dplyr::n_distinct(long$question) < 2)
    return(list(tag = tag, cells = chk, lvl_n = lvl_n,
                error = "fewer than 2 levels or 2 items remain"))

  fit <- try(
    run_select_all(long, x = demo$var, tag = tag,
                   q_type = sa$q_type, verbose = verbose, ...),
    silent = TRUE)

  if (inherits(fit, "try-error")) {
    message("  FAILED: ", conditionMessage(attr(fit, "condition")))
    return(list(tag = tag, cells = chk, lvl_n = lvl_n, long = long,
                error = conditionMessage(attr(fit, "condition"))))
  }

  list(tag = tag, cells = chk, lvl_n = lvl_n, long = long,
       dropped_items = attr(long, "dropped_items"),
       res = fit, error = NA_character_)
}


# ---- run the whole grid --------------------------------------------
sa_grid <- tidyr::expand_grid(sa = names(sa_specs), demo = names(demo_specs))

sa_results <- purrr::pmap(sa_grid, function(sa, demo) {
  spec <- sa_specs[[sa]]
  spec$label_tag <- sa
  # re_structure only matters when q_type is supplied; "nested" is the
  # robust default (see the single-neutral-item problem in clean_reasons)
  run_sa_cell(survey, spec, demo_specs[[demo]],
              re_structure = if (is.null(spec$q_type)) "nested" else "nested")
})
names(sa_results) <- paste(sa_grid$sa, sa_grid$demo, sep = "__")


# ---- summary table --------------------------------------------------

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

  f <- z$res$pop
  o <- f$omni

  dplyr::bind_rows(base) %>%
    dplyr::mutate(
      n_obs     = f$n_obs,
      n_id      = f$n_id,
      singular  = lme4::isSingular(f$fit),
      # omnibus: does total endorsement differ across the demographic?
      p_total   = if (o$kind == "binomial_glm") {
                    t <- if (is.null(o$test_quasi)) o$test else o$test_quasi
                    t[[grep("^Pr", names(t))[1]]][2]
                  } else o$test_x[["Pr(>Chisq)"]][2],
      # only for q_type questions: does the valence balance shift?
      p_valence = if (o$kind == "clustered_glm")
                    o$test[["Pr(>Chisq)"]][2] else NA_real_,
      # item-level: does the demographic effect differ by item?
      p_item_int = if (!is.null(f$chisq_test))
                     f$chisq_test[["Pr(Chi)"]][2] else NA_real_)
})

print(as.data.frame(sa_summary), row.names = FALSE)

readr::write_csv(sa_summary, file.path(fig_path, "select_all_summary.csv"))

# cells that failed, with the reason
dplyr::filter(sa_summary, !is.na(error))
}

# a single plot
# sa_results$opportunity__career_stage$res$pop$plot


# =================================================================
# 6. ORDINAL GRID: every ordinal outcome x every demographic
# =================================================================
#survey
#run_ord_cell(survey, ord_specs[[ord]], demo_specs[[demo]]))
#gen_attitude career_stage 

run_ord_cell <- function(survey, ord, demo, min_n = 20, verbose = FALSE) {

  tag <- paste0(ord$y, "_by_", demo$var)
  message("\n=== ", ord$y, " by ", demo$var, " ===")

  dat <- prep_demo(survey, demo)
  if (!is.null(ord$filter)) dat <- ord$filter(dat)
  dat <- droplevels(dat)

  # The interaction diagnostic uses career_stage as the covariate, which
  # is meaningless when career_stage IS the predictor.
  cov <- if (demo$var == "career_stage") NULL else "career_stage"

  fit <- if (demo$type == "count") {
    # IO is an ordered count: a single slope on its own scale. The
    # min_distinct_x guard is lowered deliberately - IO takes 5 values,
    # below the default 6, so the equal-spacing assumption is doing real
    # work and should be checked against the factor version below.
    try(fit_ordinal_by_continuous(dat, x = demo$var, y = ord$y,
                                  covariate = cov, min_distinct_x = 3,
                                  verbose = verbose), silent = TRUE)
  } else {
    try(fit_ordinal_by(dat, x = demo$var, y = ord$y,
                       covariate = cov, min_n = min_n,
                       verbose = verbose), silent = TRUE)
  }

  if (inherits(fit, "try-error")) {
    message("  FAILED: ", conditionMessage(attr(fit, "condition")))
    return(list(tag = tag, error = conditionMessage(attr(fit, "condition"))))
  }

  ggsave(file.path(fig_path, paste0(tag, ".jpg")), fit$plot,
         width = 10, height = 5)

  list(tag = tag, res = fit, error = NA_character_)
}


ord_grid <- tidyr::expand_grid(ord = names(ord_specs), demo = names(demo_specs))



ord_results <- purrr::pmap(ord_grid, function(ord, demo)
  run_ord_cell(survey, ord_specs[[ord]], demo_specs[[demo]]))
names(ord_results) <- paste(ord_grid$ord, ord_grid$demo, sep = "__")


# ---- summary table --------------------------------------------------
ord_summary <- purrr::imap_dfr(ord_results, function(z, nm) {

  base <- tibble::tibble(cell = nm, tag = z$tag, error = z$error)
  if (!is.na(z$error)) return(base)

  f <- z$res

  dplyr::bind_rows(base) %>%
    dplyr::mutate(
      n        = f$n,
      # convergence code -3 means the thresholds are not increasing and
      # the fit is INVALID - do not interpret those rows
      conv     = paste(f$fit$convergence$code, collapse = "/"),
      p_main   = f$lrt[["Pr(>Chi)"]][2],
      p_int    = if (is.null(f$int_test)) NA_real_
                 else f$int_test[["Pr(>Chisq)"]][2],
      p_linear = if (is.null(f$lin_test)) NA_real_
                 else f$lin_test[["Pr(>Chisq)"]][2],
      dropped  = if (!is.null(f$dropped) && nrow(f$dropped))
                   paste(f$dropped[[f$x]], collapse = "; ") else "")
})

if (nrow(ord_summary)) {
print(as.data.frame(ord_summary), row.names = FALSE)

readr::write_csv(ord_summary, file.path(fig_path, "ordinal_summary.csv"))

# anything that did not converge cleanly
dplyr::filter(ord_summary, !is.na(error) | conv != 0)
}

# ---- IO as a factor, for comparison ---------------------------------
# IO takes 5 values, so the slope version assumes equal spacing between
# adjacent counts. Fitting it as a factor says whether that matters.
io_factor <- purrr::map(names(ord_specs), function(o) {
  ord <- ord_specs[[o]]
  dat <- prep_demo(survey, demo_specs$IO)
  if (!is.null(ord$filter)) dat <- ord$filter(dat)
  try(fit_ordinal_by(droplevels(dat), x = "IO", y = ord$y,
                     covariate = "career_stage", min_n = 20, verbose = FALSE),
      silent = TRUE)
})
names(io_factor) <- names(ord_specs)

# compare AIC: if the factor version is no better, the slope is fine
purrr::imap_dfr(io_factor, function(z, nm) {
  slope <- ord_results[[paste0(nm, "__IO")]]$res
  if (inherits(z, "try-error") || is.null(slope)) return(tibble::tibble(y = nm))
  tibble::tibble(y = nm,
                 AIC_slope  = AIC(slope$fit),
                 AIC_factor = AIC(z$fit),
                 diff       = AIC(z$fit) - AIC(slope$fit))
})

#SCE need to refit this with the same data this is not the correct comparison
nrow (io_factor$ai_use_freq$data) == nrow (ord_results$ai_use_freq__IO$res$data)

# ---- Career stage as a factor, for comparison ---------------------------------
# Career takes 4 values, so the slope version assumes equal spacing between
# adjacent counts. Fitting it as a factor says whether that matters.
career_factor <- purrr::map(names(ord_specs), function(o) {
  ord <- ord_specs[[o]]
  dat <- prep_demo(survey, demo_specs$career_stage)
  if (!is.null(ord$filter)) dat <- ord$filter(dat)
  try(fit_ordinal_by(droplevels(dat), x = "career_stage", y = ord$y,
                     covariate = NULL, min_n = 20, verbose = FALSE),
      silent = TRUE)
})
names(career_factor) <- names(ord_specs)

# compare AIC: if the factor version is no better, the slope is fine
purrr::imap_dfr(career_factor, function(z, nm) {
  slope <- ord_results[[paste0(nm, "__career_stage")]]$res
  if (inherits(z, "try-error") || is.null(slope)) return(tibble::tibble(y = nm))
  tibble::tibble(y = nm,
                 AIC_slope  = AIC(slope$fit),
                 AIC_factor = AIC(z$fit),
                 diff       = AIC(z$fit) - AIC(slope$fit))
})

# sanity check: the factor and slope versions should have the same number of rows
# nrow (career_factor$ai_use_freq$data) == nrow (ord_results$ai_use_freq__career_stage$res$data)





# =================================================================
# 7. CROSS-TABS: are the demographics confounded with career stage?
# =================================================================
# Every model above pools over career stage. These tables say whether
# that pooling is hiding a compositional difference, and the interaction
# p-values in ord_summary say whether it matters for the outcome.
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

# -------------------------------------------------
# Session info for the methods section
# -------------------------------------------------
  sessionInfo()