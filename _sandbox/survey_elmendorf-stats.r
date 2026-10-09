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

# sampler settings, applied to every Likert-pair model
n.iter        <- 6000
n.adapt_delta <- 0.99

# prior on the slope for the Likert-pair models:
#   "scaled"   -> normal(0, 1/(K-1)) on b, so the implied prior on the
#                 lowest-to-highest contrast is normal(0, 1) and matches
#                 the dummy-coded model. RECOMMENDED.
#   "unscaled" -> normal(0, 1) on b itself; the mono model's prior on the
#                 full range is then (K-1) times wider than the fac model's
#   "flat"     -> brms default (improper). No prior draws, so no Bayes
#                 factor and no prior-predictive checks.
b_prior_mode <- "scaled"

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
fig_path   <- file.path(graph_path, "analysis_figs")
fit_path   <- "fits"

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


# =================================================================
# 2. HELPERS: Likert predictor (x) and Likert outcome (y)
# -----------------------------------------------------------------
# Bayesian cumulative probit with a monotonic predictor.
# Dirichlet prior on the simplex (category spacing); normal prior on
# the probit-scale monotonic coefficient b.
# Reference: Burkner & Charpentier 2020, https://doi.org/10.1111/bmsp.12195
# =================================================================

# ---- prior vs posterior draws, matched by parameter name ---------
# Thresholds are ordered, so the single unordered `prior_Intercept`
# column stored by sample_prior = "yes" is NOT the right reference.
# Both arguments here come from full model fits (the second with
# sample_prior = "only"), so every parameter matches by name and the
# ordering constraint is respected.
prior_post_draws <- function(fit_post, fit_prior,
                             pattern = "^(b_|bsp_|simo_)") {
  post_vars  <- grep(pattern, posterior::variables(fit_post),  value = TRUE)
  prior_vars <- grep(pattern, posterior::variables(fit_prior), value = TRUE)
  pars <- intersect(post_vars, prior_vars)

  missing <- setdiff(post_vars, pars)
  if (length(missing))
    warning("No prior-only draws for: ", paste(missing, collapse = ", "))

  get_draws <- function(fit) {
    m <- posterior::as_draws_matrix(fit, variable = pars)
    tibble::as_tibble(unclass(m)[, pars, drop = FALSE])
  }

  dplyr::bind_rows(
    get_draws(fit_prior) |> dplyr::mutate(dist = "prior"),
    get_draws(fit_post)  |> dplyr::mutate(dist = "posterior")
  ) |>
    tidyr::pivot_longer(-dist, names_to = "parameter", values_to = "value") |>
    dplyr::mutate(parameter = factor(parameter, levels = pars),
                  dist      = factor(dist, levels = c("prior", "posterior")))
}


plot_prior_post <- function(d, ncol = 3, trim = c(.005, .995)) {

  if (is.null(d) || !nrow(d)) {
    message("No prior draws available (flat prior); nothing to plot.")
    return(invisible(NULL))
  }

  # trim the heavy prior tails for display only, per parameter
  d <- d |>
    dplyr::group_by(parameter) |>
    dplyr::mutate(lo = quantile(value[dist == "prior"], trim[1]),
                  hi = quantile(value[dist == "prior"], trim[2])) |>
    dplyr::filter(value >= pmin(lo, min(value[dist == "posterior"])),
                  value <= pmax(hi, max(value[dist == "posterior"]))) |>
    dplyr::ungroup()

  ggplot2::ggplot(d, ggplot2::aes(value, fill = dist)) +
    ggplot2::geom_density(alpha = .45, colour = NA) +
    ggplot2::facet_wrap(~ parameter, scales = "free", ncol = ncol) +
    ggplot2::scale_fill_manual(values = c(prior = "grey65",
                                          posterior = "#2c7fb8")) +
    ggplot2::labs(x = NULL, y = "density", fill = NULL,
                  title = "Prior vs posterior") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}


# ---- per-model convergence summary -------------------------------
# Tags each row with the model name so an ESS or divergence warning
# can be traced to mono / fac / ls rather than appearing anonymously.
diagnose_fit <- function(fit, tag, bulk_min = 400, tail_min = 400,
                         rhat_max = 1.01) {

  s <- posterior::summarise_draws(
    posterior::as_draws(fit),
    "rhat", "ess_bulk", "ess_tail")

  np  <- brms::nuts_params(fit)
  div <- sum(np$Value[np$Parameter == "divergent__"])

  max_td <- fit$fit@stan_args[[1]]$control$max_treedepth
  if (is.null(max_td)) max_td <- 10
  tree <- sum(np$Value[np$Parameter == "treedepth__"] >= max_td)

  bad <- s |>
    dplyr::filter(rhat > rhat_max | ess_bulk < bulk_min | ess_tail < tail_min) |>
    dplyr::arrange(ess_tail)

  tibble::tibble(
    model      = tag,
    divergent  = div,
    max_tree   = tree,
    max_rhat   = max(s$rhat, na.rm = TRUE),
    min_ess_b  = min(s$ess_bulk, na.rm = TRUE),
    min_ess_t  = min(s$ess_tail, na.rm = TRUE),
    n_flagged  = nrow(bad),
    worst      = if (nrow(bad)) bad$variable[1] else NA_character_
  )
}


# ---- everything derived from fitted objects ----------------------
# Called by load_likert_pair(). fit_likert_pair() only fits and caches,
# so the two entry points cannot produce different objects.
summarise_likert_pair <- function(fits, fits_prior, x, y, data = NULL) {

  fit_mono   <- fits$mono
  prior_mono <- fits_prior$mono
  if (is.null(fit_mono)) stop("No monotonic fit supplied.")

  dat <- if (is.null(data)) fit_mono$data else data

  slope_row <- grep("^mo", rownames(brms::fixef(fit_mono)), value = TRUE)
  if (length(slope_row) != 1)
    stop("Expected exactly one monotonic term, found: ",
         paste(slope_row, collapse = ", "))

  has_priors <- length(fits_prior) > 0

  # Savage-Dickey BF for the slope only. Thresholds and simplex elements
  # have no meaningful point null (the simplex is bounded on (0,1) and
  # constrained to sum to 1), so testing them against 0 is meaningless.
  bf_slope <- if (!is.null(prior_mono))
    bayestestR::bayesfactor_parameters(fit_mono, prior = prior_mono,
                                       parameters = paste0("bsp_", slope_row))
  else NULL

  diag_tab <- purrr::imap_dfr(fits, diagnose_fit)

  prior_post <- if (has_priors)
    purrr::map2(fits[names(fits_prior)], fits_prior, prior_post_draws) else NULL

  fits_loo <- purrr::map(fits, brms::add_criterion, "loo")
  # pass a NAMED list so loo_compare labels rows mono/fac/ls rather than
  # deparsing whole brmsfit objects into the row names
  loo_tab  <- if (length(fits_loo) > 1)
    loo::loo_compare(purrr::map(fits_loo, \(f) f$criteria$loo)) else NULL

  ce <- brms::conditional_effects(fit_mono, categorical = TRUE)
  # no random effects in these models. With ranefs you would need
  # re_formula = NULL here for population-average predictions, since
  # conditional_effects defaults to re_formula = NA.
  ce_plot <- plot(ce, plot = FALSE)[[1]] +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x, y = paste0("P(", y, " = k)")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  sa <- fit_mono$fit@stan_args[[1]]

  structure(list(
    x = x, y = y, n = nrow(dat),
    K_x = nlevels(dat[[x]]), K_y = nlevels(dat[[y]]),
    sampling = list(iter = sa$iter, warmup = sa$warmup,
                    chains = length(fit_mono$fit@stan_args),
                    adapt_delta = sa$control$adapt_delta, seed = sa$seed),
    b_prior = if (has_priors) "set" else "flat",
    diagnostics = diag_tab,
    data = dat,
    priors = brms::prior_summary(fit_mono),
    fits = fits_loo, fits_prior = fits_prior,
    prior_post = prior_post,
    prior_pc = if (has_priors)
      purrr::imap(fits_prior, \(f, nm)
        brms::pp_check(f, type = "bars", ndraws = 200) +
          ggplot2::ggtitle(paste0("Prior predictive: ", nm))) else NULL,
    post_pc = purrr::imap(fits, \(f, nm)
      brms::pp_check(f, type = "bars", ndraws = 200) +
        ggplot2::ggtitle(paste0("Posterior predictive: ", nm))),
    loo = loo_tab,
    bf_slope = bf_slope,
    slope_summary = brms::fixef(fit_mono)[slope_row, , drop = FALSE],
    ce = ce, ce_plot = ce_plot
  ), class = "likert_pair")
}


# ---- fit and cache three competing models ------------------------
#   mono : cumulative probit, monotonic predictor (primary)
#   fac  : cumulative probit, dummy-coded predictor (no monotonicity)
#   lin  : linear model, no monotonicity
#   ls   : location-scale, latent variance also modelled (robustness)
fit_likert_pair <- function(data, x, y,
                            b_sd_fac    = 1,
                            b_prior     = c("scaled", "unscaled", "flat"),
                            disc_sd     = 0.25,
                            run_lin     = TRUE,    #
                            run_ls      = FALSE,
                            adapt_delta = 0.99,
                            iter        = 2000,
                            warmup      = NULL,   # defaults to iter/2
                            chains      = 4,
                            cores = 4, seed = 1,
                            cache_dir   = NULL,
                            file_prefix = NULL,
                            overwrite   = FALSE,
                            quiet = FALSE) {

  b_prior <- match.arg(b_prior)

  stopifnot(is.character(x), is.character(y), length(x) == 1, length(y) == 1)
  if (!all(c(x, y) %in% names(data)))
    stop("Columns not found in data: ",
         paste(setdiff(c(x, y), names(data)), collapse = ", "))

  if (is.null(warmup)) warmup <- floor(iter / 2)
  stopifnot(warmup < iter)

  # ---- data prep -------------------------------------------------
  dat <- data[!is.na(data[[x]]) & !is.na(data[[y]]), , drop = FALSE]

  # mo() needs an ordered factor or integer; cumulative() needs ordered y
  if (!is.ordered(dat[[x]])) {
    warning(x, " is not an ordered factor; coercing with its current level order.")
    dat[[x]] <- factor(dat[[x]], ordered = TRUE)
  }
  if (!is.ordered(dat[[y]])) {
    warning(y, " is not an ordered factor; coercing with its current level order.")
    dat[[y]] <- factor(dat[[y]], ordered = TRUE)
  }

  # drop levels left empty by upstream filtering, or brms estimates
  # thresholds for categories nobody chose
  dat <- droplevels(dat)

  K_x <- nlevels(dat[[x]]); K_y <- nlevels(dat[[y]])
  if (K_x < 3) stop(x, " has fewer than 3 levels after filtering; mo() needs >= 3.")
  if (K_y < 3) stop(y, " has fewer than 3 levels after filtering.")

  # Unordered copy of x for the dummy-coded model. An ORDERED factor on
  # the RHS gets polynomial contrasts, so normal(0, s) would land on
  # linear/quadratic/cubic trend coefficients rather than on category
  # differences. Same fit either way, different prior.
  dat_fac <- dat
  dat_fac[[x]] <- factor(as.character(dat[[x]]),
                         levels = levels(dat[[x]]), ordered = FALSE)

                         #########
  
  # sequential difference coding, per Burkner & Charpentier's case study:
 # coefficients become adjacent-category differences, so the same prior on
  # b applies to the monotonic, linear and categorical models alike
  contrasts(dat_fac[[x]]) <- MASS::contr.sdif(K_x)

 # linear coding: as.integer() on a factor gives level position 1..K, which
  # is the correct ordering. as.numeric(as.character()) would give NA for
  # text labels.
  dat_lin <- dat
  dat_lin[[x]] <- as.integer(dat[[x]])
  
  #############

  # ---- names and prior scaling -----------------------------------
  simo_coef <- paste0("mo", x, "1")

  # b in the monotonic model is the AVERAGE per-step effect; the total
  # lowest-to-highest effect is b * (K_x - 1). The factor model's dummy
  # coefficients are already full-range contrasts. "scaled" therefore
  # puts the same prior on the full-range contrast in both models.
  b_sd_mono <- switch(b_prior,
                      scaled   = b_sd_fac / (K_x - 1),
                      unscaled = b_sd_fac,
                      flat     = NA_real_)

  f_mono <- brms::bf(stats::as.formula(sprintf("%s ~ mo(%s)", y, x)))
  f_fac  <- brms::bf(stats::as.formula(sprintf("%s ~ %s",     y, x)))
  f_lin  <- brms::bf(stats::as.formula(sprintf("%s ~ %s",     y, x)))
  # disc ~ 0 + ... fixes disc = 1 at the first category. Without the 0 +,
  # the disc intercept competes with the thresholds for the same scale
  # information and the model is not identified.
  f_ls   <- brms::bf(stats::as.formula(sprintf("%s ~ mo(%s)", y, x)),
                     stats::as.formula(sprintf("disc ~ 0 + mo(%s)", x)))

  # ---- priors ----------------------------------------------------
  # Thresholds and simplex use brms defaults, stated explicitly for the
  # record. brms's default on class "b" is FLAT (improper), so the normal
  # priors below are a deliberate addition: they regularise, and they are
  # what makes sample_prior = "only" and the Savage-Dickey BF possible.
  th_prior   <- brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept")
  simo_prior <- brms::set_prior("dirichlet(1)", class = "simo", coef = simo_coef)

  b_prior_mono <- if (b_prior == "flat") NULL else
    brms::set_prior(sprintf("normal(0, %g)", b_sd_mono), class = "b")
  b_prior_fac  <- if (b_prior == "flat") NULL else
    brms::set_prior(sprintf("normal(0, %g)", b_sd_fac),  class = "b")

  priors_mono <- th_prior + simo_prior
if (!is.null(b_prior_mono)) priors_mono <- priors_mono + b_prior_mono
 
 priors_fac <- th_prior
 if (!is.null(b_prior_fac)) priors_fac <- priors_fac + b_prior_fac

  # disc is modelled on the log scale; normal(0, 0.25/(K-1)) implies a
  # 95% prior interval of roughly 0.25 to 4 for the ratio of latent SDs
  # between the predictor's extremes.
  priors_ls <- priors_mono +
    brms::set_prior("dirichlet(1)", class = "simo",
                    coef = simo_coef, dpar = "disc")
  if (b_prior != "flat")
    priors_ls <- priors_ls +
      brms::set_prior(sprintf("normal(0, %g)", disc_sd / (K_x - 1)),
                      class = "b", dpar = "disc")

  # ---- resolve the cache path ------------------------------------
  if (is.null(file_prefix) && !is.null(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    file_prefix <- file.path(cache_dir, make.names(paste0(y, "__by__", x)))
  }

  tags <- c("mono", "fac", if (run_lin) "lin", if (run_ls) "ls")
  if (b_prior != "flat") tags <- c(tags, paste0(tags, "_prior"))

  if (!is.null(file_prefix)) {
    paths    <- paste0(file_prefix, "_", tags, ".rds")
    existing <- paths[file.exists(paths)]

    if (length(existing) && !overwrite) {
      stop("Fit files already exist:\n  ",
           paste(existing, collapse = "\n  "),
           "\n\nPass overwrite = TRUE to refit and replace them, ",
           "or choose a different file_prefix / cache_dir.",
           call. = FALSE)
    }

    if (length(existing) && overwrite) {
      if (!quiet) message("Removing ", length(existing), " existing fit file(s).")
      file.remove(existing)
    }
  }

  # ---- fitting ---------------------------------------------------
  fp   <- function(tag) if (is.null(file_prefix)) NULL else
                          paste0(file_prefix, "_", tag)
  ctrl <- list(adapt_delta = adapt_delta)

  # withCallingHandlers tags any sampler warning with the model name, so
  # an ESS or divergence message says WHICH of the three models produced it
  run <- function(formula, d, prior, tag) {
    if (!quiet) message("Fitting ", tag, " ...")
    withCallingHandlers(
      brms::brm(formula, data = d, family = brms::cumulative("probit"),
                prior = prior, cores = cores, seed = seed,
                iter = iter, warmup = warmup, chains = chains,
                control = ctrl, file = fp(tag)),
      warning = function(w) {
        message("[", tag, "] ", conditionMessage(w))
        invokeRestart("muffleWarning")
      })
  }

  fit_mono <- run(f_mono, dat,     priors_mono, "mono")
  fit_fac  <- run(f_fac,  dat_fac, priors_fac,  "fac")
  fit_lin  <- if (run_lin) run(f_lin, dat_lin, priors_fac, "lin") else NULL
  fit_ls   <- if (run_ls) run(f_ls, dat, priors_ls, "ls") else NULL

  # update() does NOT inherit sampling arguments, so they are repeated here
  prior_only <- function(fit, tag) {
    if (is.null(fit)) return(NULL)
    if (b_prior == "flat") return(NULL)   # improper prior: nothing to sample
    if (!quiet) message("Fitting ", tag, " (prior only) ...")
    withCallingHandlers(
      stats::update(fit, sample_prior = "only", seed = seed,
                    cores = cores, iter = iter, warmup = warmup,
                    chains = chains, file = fp(paste0(tag, "_prior"))),
      warning = function(w) {
        message("[", tag, "_prior] ", conditionMessage(w))
        invokeRestart("muffleWarning")
      })
  }

  invisible(purrr::map2(list(fit_mono, fit_fac, fit_lin, fit_ls),
                       c("mono", "fac", "lin", "ls"), prior_only))

  # ---- quick diagnostic report; the rest is built on load ---------
  fits     <- purrr::compact(list(mono = fit_mono, fac = fit_fac, ls = fit_ls,
                                  lin = fit_lin, ls = fit_ls))
  diag_tab <- purrr::imap_dfr(fits, diagnose_fit)

  if (!quiet) {
    print(diag_tab)
    bad <- dplyr::filter(diag_tab, n_flagged > 0 | divergent > 0)
    if (nrow(bad))
      warning("Diagnostic issues in: ", paste(bad$model, collapse = ", "),
              call. = FALSE)
  }

  invisible(file_prefix)
}


# ---- read cached fits and build the summary object ---------------
load_likert_pair <- function(x, y,
                             cache_dir   = NULL,
                             file_prefix = NULL,
                             data = NULL) {

  if (is.null(file_prefix) && !is.null(cache_dir))
    file_prefix <- file.path(cache_dir, make.names(paste0(y, "__by__", x)))
  if (is.null(file_prefix)) stop("Give cache_dir or file_prefix.")

  rd <- function(tag) {
    p <- paste0(file_prefix, "_", tag, ".rds")
    if (file.exists(p)) readRDS(p) else NULL
  }

  fits       <- purrr::compact(list(mono = rd("mono"),
                                    fac  = rd("fac"),
                                    lin  = rd("lin"),
                                    ls   = rd("ls")))
  fits_prior <- purrr::compact(list(mono = rd("mono_prior"),
                                    fac  = rd("fac_prior"),
                                    lin  = rd("lin_prior"),
                                    ls   = rd("ls_prior")))

  if (is.null(fits$mono))
    stop("No monotonic fit found at ", file_prefix, "_mono.rds")

  summarise_likert_pair(fits, fits_prior, x = x, y = y, data = data)
}


# ---- fit if asked, then ALWAYS load ------------------------------
# Guarantees the returned object is identical whether or not the models
# were refitted this session.
run_likert_pair <- function(data, x, y, file_prefix,
                            rerun = rerun_mods,
                            b_prior = b_prior_mode, ...) {
  if (rerun)
    fit_likert_pair(data, x = x, y = y, file_prefix = file_prefix,
                    overwrite = TRUE, b_prior = b_prior,
                    iter = n.iter, adapt_delta = n.adapt_delta, ...)
  load_likert_pair(x = x, y = y, file_prefix = file_prefix, data = NULL)
}


# ---- compact console summary -------------------------------------
print.likert_pair <- function(z, ...) {
  cat(sprintf("\n%s ~ %s   (n = %d, K_x = %d, K_y = %d)\n",
              z$y, z$x, z$n, z$K_x, z$K_y))

  s <- z$slope_summary[1, ]
  cat(sprintf("slope   b = %.2f [%.2f, %.2f]\n",
              s[["Estimate"]], s[["Q2.5"]], s[["Q97.5"]]))

  # BF is unavailable under a flat prior: there is no prior density at
  # the null to form the Savage-Dickey ratio with
  if (is.null(z$bf_slope)) {
    cat("BF10    not available (flat prior on b)\n")
  } else {
    logbf <- as.data.frame(z$bf_slope)$log_BF[1]
    # cap the extremes: the density estimate at 0 is unstable when the
    # posterior sits far from it, so the exact value is not meaningful
    cat(sprintf("BF10    %s\n",
                if (logbf > log(100)) "> 100"
                else if (logbf < log(1 / 100)) "< 0.01"
                else sprintf("%.1f", exp(logbf))))
  }

  if (!is.null(z$loo)) {
    l <- unclass(z$loo)[, c("elpd_diff", "se_diff", "p_loo"), drop = FALSE]
    cat("\nmodel comparison (best first):\n")
    print(round(l, 2))
  }

  if (!is.null(z$diagnostics)) {
    bad <- dplyr::filter(z$diagnostics, n_flagged > 0 | divergent > 0)
    if (nrow(bad)) {
      cat("\ndiagnostic issues:\n")
      print(as.data.frame(bad[, c("model", "divergent", "max_rhat",
                                  "min_ess_t", "worst")]), row.names = FALSE)
    }
  }

  cat("\n")
  invisible(z)
}


# ---- observed proportions + model intervals ----------------------
make.likert.plot <- function(fit,
                             palette    = NULL,   # named vector, or f(n)
                             pal_option = "D",    # viridis option if NULL
                             pal_end    = 0.9,
                             wrap_axis  = 20,
                             dodge      = 0.8) {

  yvar <- fit$y
  xvar <- fit$x

  # .drop = FALSE keeps zero-count combinations, so a category nobody
  # chose still occupies its slot instead of letting the dodge stretch
  # the remaining bars across the whole group
  raw <- fit$data %>%
    dplyr::count(.data[[xvar]], .data[[yvar]], .drop = FALSE) %>%
    dplyr::group_by(.data[[xvar]]) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(effect1__ = !!xvar, effect2__ = !!yvar)

  lvl <- levels(raw$effect2__)
  pal <- if (is.null(palette)) {
    scales::viridis_pal(option = pal_option, end = pal_end)(length(lvl))
  } else if (is.function(palette)) {
    palette(length(lvl))
  } else {
    palette
  }

  # name by level so colours cannot drift if level order differs between
  # the raw counts and the conditional-effects frame
  if (is.null(names(pal))) names(pal) <- lvl
  if (!all(lvl %in% names(pal)))
    stop("palette is missing colours for: ",
         paste(setdiff(lvl, names(pal)), collapse = ", "))

  tot <- raw %>%
    dplyr::group_by(effect1__) %>%
    dplyr::summarise(n = sum(n), .groups = "drop")

  ggplot2::ggplot() +
    ggplot2::geom_col(
      data = raw,
      ggplot2::aes(effect1__, prop, fill = effect2__),
      position = ggplot2::position_dodge(width = dodge), width = .7,
      alpha = .35, colour = NA) +
    # y = Inf pins the labels to a single line at the panel top, so they
    # cannot collide with a bar at 100%
    ggplot2::geom_text(
      data = tot,
      ggplot2::aes(effect1__, y = Inf, label = paste0("n = ", n)),
      vjust = 1.4, size = 3, colour = "grey30") +
    ggplot2::geom_linerange(
      data = as.data.frame(fit$ce[[1]]),
      ggplot2::aes(effect1__, estimate__, ymin = lower__, ymax = upper__,
                   color = effect2__),
      position = ggplot2::position_dodge(width = dodge), linewidth = .3) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                expand = ggplot2::expansion(mult = c(.05, .12))) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::scale_x_discrete(labels = scales::label_wrap(wrap_axis)) +
    ggplot2::labs(x = xvar, y = paste0("P(", yvar, " = k)"),
                  fill = yvar, colour = yvar) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


# =================================================================
# 3. LIKERT-PAIR ANALYSES
# =================================================================

# ---- Q1: general attitude as a function of career stage ----------
dat_q1 <- survey %>%
  dplyr::filter(!gen_attitude %in% c("Other", "Indifferent"))

res_1 <- run_likert_pair(dat_q1, x = "career_stage", y = "gen_attitude",
                         file_prefix = file.path(fit_path, "attitude_by_stage"),
                         run_lin = TRUE, run_ls = FALSE)

res_1_plot <- make.likert.plot(res_1)
ggsave(file.path(fig_path, "gen_attitude_by_career.jpg"), plot = res_1_plot,
       width = 8, height = 6)

res_1                                  # compact summary via print method
res_1$loo                              # top row is the best-fitting model
summary(res_1$fits$mono)
plot_prior_post(res_1$prior_post$mono) # posteriors over priors
res_1$prior_pc$mono                    # prior predictive
res_1$diagnostics

# Sanity check per the Kurz blog: with disc ~ 0 + mo(), there should be
# NO b_disc_Intercept. If one appears the model is not identified.
# https://solomonkurz.netlify.app/blog/2021-12-29-notes-on-the-bayesian-cumulative-probit/
# grep("disc", posterior::variables(res_1$fits$ls), value = TRUE)

# check whether you gain information from the mono models vs linear by examining the
# posteriors on the simplex
# all 3 straddle the 1/(K-1) so not too much info from running as likert-predictor
# model equivalent between linear and mono
# for mod_1, currently linear is fine, could drop down to clm::ordinal or just keep
# the linear
posterior::summarise_draws(posterior::as_draws(res_1$fits$mono),
  "mean", ~quantile(.x, probs = c(0.05, 0.95))
) |>
  dplyr::filter(grepl("^simo_", variable))

# ---- Q2: AI use frequency as a function of data-science frequency ----
dat_q2 <- survey %>%
  dplyr::filter(!is.na(career_stage), !is.na(ai_use_freq), !is.na(ds_freq))

# counts first: are there cells large enough to work with?
tab_ds_freq_career <- dat_q2 %>%
  droplevels() %>%
  dplyr::group_by(ds_freq, career_stage, .drop = FALSE) %>%
  dplyr::tally() %>%
  tidyr::pivot_wider(names_from = career_stage, values_from = n) %>%
  tibble::column_to_rownames("ds_freq") %>%
  as.matrix()

tab_ds_freq_career
any(tab_ds_freq_career == 0)   # sum() would not detect zeros

# Does the ds_freq effect differ by career stage? If not, pool across
# stages to maximise n. NOTE: ordered and unordered factors span the same
# column space in clm, so an ordered-vs-unordered comparison is NOT a
# test of anything (zero df difference) - only the marginal test below is.
res_2_int <- clm(ai_use_freq ~ ds_freq * career_stage,
                 data = dat_q2, link = "probit")
anova(clm(ai_use_freq ~ ds_freq + career_stage, data = dat_q2, link = "probit"),
      res_2_int)

res_2 <- run_likert_pair(dat_q2, x = "ds_freq", y = "ai_use_freq",
                         file_prefix = file.path(fit_path, "dsfreq_by_ai_usefreq"))

res_2
res_2$loo        # differences within ~2 SE: report the simplest (mono)
res_2$ce_plot
summary(res_2$fits$mono)
plot_prior_post(res_2$prior_post$mono)
res_2$prior_pc$mono
res_2$diagnostics

# for mod2, very slight evidence that the first group is bigger, so might keep mono
# though it's similar to linear
posterior::summarise_draws(posterior::as_draws(res_2$fits$mono),
  "mean", ~quantile(.x, probs = c(0.05, 0.95))
) |>
  dplyr::filter(grepl("^simo_", variable))


# people who do data-science tasks more often use AI more frequently
res_2_plot <- make.likert.plot(res_2, pal_option = "C")
ggsave(file.path(fig_path, "dsfreq_by_ai_usefreq.jpg"), plot = res_2_plot,
       width = 8, height = 6)

# to set colours by hand instead, e.g. a diverging scale for an ordered item:
# make.likert.plot(res_2, palette = c(
#   "Daily" = "#4575b4", "Weekly" = "#91bfdb", "Monthly" = "#ffffbf",
#   "Yearly" = "#fc8d59", "Never" = "#d73027"))


# ---- Q3: AI use frequency as a function of institutional policy ----
dat_q3 <- survey %>%
  dplyr::filter(
    !is.na(policies), !is.na(ai_use_freq),
    !policies %in% c("Other",
                     "There are not any policies or guidelines at my institution"))

res_3 <- run_likert_pair(dat_q3, x = "policies", y = "ai_use_freq",
                         file_prefix = file.path(fit_path, "policies_by_ai_usefreq"))

res_3
res_3$loo
res_3$ce_plot
summary(res_3$fits$mono)
plot_prior_post(res_3$prior_post$mono)
res_3$prior_pc$mono
res_3$diagnostics

# for mod3, linear probably fine
  posterior::summarise_draws(posterior::as_draws(res_3$fits$mono),
    "mean", ~quantile(.x, probs = c(0.05, 0.95))
  ) |>
    dplyr::filter(grepl("^simo_", variable))



res_3_plot <- make.likert.plot(res_3, pal_option = "A")
ggsave(file.path(fig_path, "policies_by_ai_usefreq.jpg"), plot = res_3_plot,
       width = 8, height = 6)


# ---- optional: prior sensitivity ---------------------------------
# Refits Q1 under all three slope priors. The flat version is the useful
# anchor since it shows what the likelihood alone says; it carries no
# Bayes factor and no prior-predictive check.
#
sens_1 <- purrr::map(c("scaled", "unscaled", "flat"), \(m)
  run_likert_pair(dat_q1, x = "career_stage", y = "gen_attitude",
                  file_prefix = file.path(fit_path,
                                          paste0("attitude_by_stage_", m)),
                  b_prior = m)) |>
  rlang::set_names(c("scaled", "unscaled", "flat"))

# totally insensitive to the prior
purrr::map_dfr(sens_1, \(z) tibble::as_tibble(z$slope_summary),
               .id = "b_prior")


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
                           # re.form  = NULL,
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
# 5. SELECT-ALL ANALYSES
# =================================================================

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


# ---- Q1B: opportunities by career stage --------------------------
opps_long_career <- expand_select_all(survey, q = "promising_opps",
                                      x = "career_stage") %>%
  clean_opps()


tst <- fit_select_all(reason_long_career, x = "career_stage",
                           q_type = "q_type", re_structure = "correlated")

reason_long_career <- expand_select_all(survey, q = "ai_use_reasons",
                                      x = "career_stage") %>%
  filter(ai_use_reasons != "Other") %>%
                                        mutate(q_type =
                                        case_when (
                                          grepl('^Desire|^Get|^Expand', ai_use_reasons) ~ 'positive',
                                          grepl('^Get', ai_use_reasons) ~ 'positive',
                                          grepl('^Lack|^Worried|^Concerns|^The|^Security|^Anxiety|^Financial', ai_use_reasons) ~ 'negative',
                                          TRUE ~ 'neutral'
                                        )) %>%
                                          filter(q_type != 'neutral') %>%
  droplevels()

tst <- fit_select_all(reason_long_career, x = "career_stage",
                           q_type = "q_type", re_structure = "correlated")


tst$omni$coefs      # coefficients with cluster-robust SEs
tst$omni$test       # does the career-stage effect differ by type?
tst$omni$test_x     # does total endorsement differ by career stage?
tst$omni$counts     # the aggregated data, if you want to plot it

res_opps_career <- run_select_all(opps_long_career, x = "career_stage",
                                  tag = "opportunity_by_career")
res_opps_career$pop$plot

# overall career-stage effect, averaging over questions
emmeans(res_opps_career$pop$fit, ~ career_stage) |> pairs()


# ---- Q1A: challenges by career stage -----------------------------
challenges_long_career <- expand_select_all(survey, q = "challenges",
                                            x = "career_stage") %>%
  clean_challenges()

res_challenges_career <- run_select_all(challenges_long_career,
                                        x = "career_stage",
                                        tag = "challenges_by_career")
res_challenges_career$pop$plot
emmeans(res_challenges_career$pop$fit, ~ career_stage) |> pairs()


# ---- opportunities by general attitude ---------------------------
survey_att <- survey %>%
  dplyr::filter(!gen_attitude %in% c("Other", "Indifferent"))

opps_long_attitude <- expand_select_all(survey_att, q = "promising_opps",
                                        x = "gen_attitude") %>%
  clean_opps()

res_opps_attitude <- run_select_all(opps_long_attitude, x = "gen_attitude",
                                    tag = "opportunity_by_attitude")
res_opps_attitude$pop$plot


# ---- challenges by general attitude ------------------------------
challenges_long_attitude <- expand_select_all(survey_att, q = "challenges",
                                              x = "gen_attitude") %>%
  clean_challenges()

res_challenges_attitude <- run_select_all(challenges_long_attitude,
                                          x = "gen_attitude",
                                          tag = "challenges_by_attitude")
res_challenges_attitude$pop$plot

# ---- opportunities by prof role ---------------------------
survey_prof <- survey %>%
  dplyr::filter(prof_work_sector != 'Student' &
  prof_work_sector != 'Other' & !is.na(prof_work_sector)) %>%
  dplyr::mutate(prof_work_sector = factor(prof_work_sector)) %>%
  droplevels()

opps_long_prof <- expand_select_all(survey_prof, q = "promising_opps",
                                        x = "prof_work_sector") %>%
  clean_opps() %>%
  # sce to check if we definitly need this after any recoding but at the
  # moment we have empty cells which break this so
  dplyr::filter(!promising_opps %in% c('Other', 'Beneficial job transformation'))%>%
  droplevels()

check_cells <- function(long, x, min_n = 5) {

  tab <- long %>%
    dplyr::group_by(question, .data[[x]], .drop = FALSE) %>%
    dplyr::summarise(n   = dplyr::n(),
                     yes = sum(response_numeric),
                     p   = mean(response_numeric),
                     .groups = "drop")

  list(
    # cells with no respondents at all
    empty = dplyr::filter(tab, n == 0),

    # cells that are all-yes or all-no -> separation
    separated = dplyr::filter(tab, n > 0, yes == 0 | yes == n),

    # thin cells, estimable but unstable
    thin = dplyr::filter(tab, n > 0, n < min_n),

    # items too rare or too universal to model
    item_rates = long %>%
      dplyr::group_by(question) %>%
      dplyr::summarise(n = dplyr::n(), p = mean(response_numeric),
                       .groups = "drop") %>%
      dplyr::arrange(p),

    full = tab
  )
}

chk <- check_cells(opps_long_prof, x = "prof_work_sector")   # 

# this paper explains about what to do with complete separation
# which cases glm to fail. That is the case here where the nonprofit
# ngo people found no job opportnities or other
# 10.1214/08-AOAS191

# run fails without removing the separated cases
res_opps_prof <- run_select_all(opps_long_prof, x = "prof_work_sector",
                                    tag = "opportunity_by_prof")
res_opps_prof$pop$plot

# this one fix, but didn't work
# we could revert to full bayesian if we want to keep this
# library (blme)
# nf <- ncol(model.matrix(~ question * prof_work_sector, data = res_opps_prof$pop$data))
# fit <- bglmer(response_numeric ~ question * prof_work_sector + (1 | ResponseId),
#               family = binomial, data = res_opps_prof$pop$data,
#               fixef.prior = normal(cov = diag(9, nf)),   # nf = n fixed effects
#               control = glmerControl(optimizer = "bobyqa",
#                                      optCtrl = list(maxfun = 2e5)))

# res_opps_prof$pop$data %>%
#   dplyr::count(question, prof_work_sector, wt = response_numeric, name = "yes") %>%
#   dplyr::left_join(dplyr::count(res_opps_prof$pop$data, question, prof_work_sector),
#                    by = c("question", "prof_work_sector")) %>%
#   dplyr::filter(yes == 0 | yes == n)

# ---- challenges by prof role ------------------------------
challenges_long_prof <- expand_select_all(survey_prof, q = "challenges",
                                              x = "prof_work_sector") %>%
  clean_challenges()

res_challenges_prof <- run_select_all(challenges_long_prof
                                          x = "prof_work_sector",
                                          tag = "challenges_by_prof")
res_challenges_prof$pop$plot

# ---- opportunities by IO ---------------------------
survey_IO <- survey %>%
  dplyr::filter(!is.na(IO))

opps_long_IO <- expand_select_all(survey_IO, q = "promising_opps",
                                        x = "IO") %>%
  clean_opps()

res_opps_IO <- run_select_all(opps_long_IO, x = "IO_work_sector",
                                    tag = "opportunity_by_IO")
res_opps_IO$pop$plot


# ---- challenges by IO ------------------------------
challenges_long_IO <- expand_select_all(survey_IO, q = "challenges",
                                              x = "IO") %>%
  clean_challenges()

res_challenges_IO <- run_select_all(challenges_long_IO
                                          x = "IO",
                                          tag = "challenges_by_IO")
res_challenges_IO$pop$plot


# =================================================================
# 6. ORDINAL MODELS BY CATEGORICAL VARIABLES
# =================================================================

# -------------------------------------------------
# fit_ordinal_by(): ordinal outcome ~ categorical predictor
#   1. drops predictor levels with < min_n respondents
#   2. reports the predictor x career_stage interaction (diagnostic only)
#   3. fits the final model on x alone, pooled over career stage
# -------------------------------------------------
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
  # so the two models in the LRT are genuinely nested.
  int_test <- NULL; fit_int <- NULL
  if (!is.null(covariate) && covariate %in% names(dat)) {
    dat_int <- dat %>% dplyr::filter(!is.na(.data[[covariate]])) %>% droplevels()

    f_int <- stats::as.formula(sprintf("%s ~ %s * %s", y, x, covariate))
    f_add <- stats::as.formula(sprintf("%s ~ %s + %s", y, x, covariate))

    fit_int <- try(ordinal::clm(f_int, data = dat_int, link = link), silent = TRUE)
    fit_add <- try(ordinal::clm(f_add, data = dat_int, link = link), silent = TRUE)

    if (!inherits(fit_int, "try-error") && !inherits(fit_add, "try-error")) {
      int_test <- anova(fit_add, fit_int)
    } else {
      warning("Interaction model failed; likely sparse cells. See $cells.")
    }
  }

  # ---- 3. final model: x alone, pooled over the covariate -------
  # Uses the larger dataset (covariate NAs allowed) to maximise n.
  fit <- ordinal::clm(stats::as.formula(sprintf("%s ~ %s", y, x)),
                      data = dat, link = link)

  # assumption checks. nominal_test() does not support probit, so the
  # partial-PO model is fitted explicitly and compared.
  scale_chk <- try(ordinal::scale_test(fit), silent = TRUE)
  po_chk <- try(
    anova(fit, ordinal::clm(stats::as.formula(sprintf("%s ~ 1", y)),
                            nominal = stats::as.formula(paste("~", x)),
                            data = dat, link = link)),
    silent = TRUE)

  lrt <- drop1(fit, test = "Chisq")

  # latent-scale contrasts: differences in latent SD units (Cohen's d)
  emm_link <- emmeans::emmeans(fit, stats::as.formula(paste("~", x)),
                               mode = "linear.predictor")
  contrasts <- confint(pairs(emm_link, adjust = "tukey"))

  if (verbose) {
    print(summary(fit)); print(lrt)
    if (!is.null(int_test)) { cat("\ninteraction with ", covariate, ":\n", sep = ""); print(int_test) }
    cat("\nlatent-scale contrasts:\n"); print(contrasts)
  }

  # ---- predicted category probabilities + observed ---------------
  lv_y <- levels(dat[[y]])

  emm <- as.data.frame(
    marginaleffects::avg_predictions(fit, by = x, type = "prob")) %>%
    dplyr::mutate(!!y := factor(as.character(group), levels = lv_y))

  raw <- dat %>%
    dplyr::count(.data[[x]], .data[[y]], .drop = FALSE) %>%
    dplyr::group_by(.data[[x]]) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!y := factor(as.character(.data[[y]]), levels = lv_y))

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
    cat(sprintf("x career_stage  LRT chi2 = %.2f, df = %d, p = %s\n",
                z$int_test[["LR.stat"]][2], z$int_test[["df"]][2],
                if (pi < .001) "< .001" else sprintf("%.3f", pi)))
  }

  cat("\nlatent-scale contrasts (probit SD units):\n")
  print(as.data.frame(z$contrasts), row.names = FALSE, digits = 2)
  cat("\n")
  invisible(z)
}

# =================================================================
# 6. AI USE FREQUENCY BY GENDER, PROF WORK SECTOR
# =================================================================
res_gender <- fit_ordinal_by(
  survey, x = "gender", y = "ai_use_freq",
  drop_levels = c("Prefer not to answer", "Prefer to self-identify"))

res_gender$plot


res_work <- fit_ordinal_by(
  survey, x = "prof_work_sector", y = "ai_use_freq",
  drop_levels = c("Student", "Other"))

res_work$fit$convergence
# can't fit the interaction... probaly ok
res_work$fit_int$convergence 

res_work$plot

# =================================================================
# 7. AI USE ATTITUDE BY GENDER, PROF WORK SECTOR
# =================================================================
res_gender_att <- fit_ordinal_by(
  survey %>%
    filter(!gen_attitude %in% c('Indifferent', 'Other')), x = "gender", y = "gen_attitude",
  drop_levels = c("Prefer not to answer", "Prefer to self-identify"))

res_gender_att$plot


res_work_att <- fit_ordinal_by(
  survey%>%
    filter(!gen_attitude %in% c('Indifferent', 'Other')),
   x = "prof_work_sector", y = "ai_use_freq",
  drop_levels = c("Student", "Other"))

res_work$fit$convergence
# can't fit the interaction... probaly ok
res_work$fit_int$convergence 

res_work_att$plot
print(res_work_att)

# =================================================================
# 6. ORDINAL MODELS BY CONTINUOUS-ISH VARIABLES
# =================================================================

# -------------------------------------------------
# fit_ordinal_by_continuous(): ordinal outcome ~ continuous predictor
#
#   1. screens for sample size and predictor variability
#   2. checks linearity on the latent scale (linear vs natural spline)
#   3. reports the predictor x covariate interaction (diagnostic only)
#   4. fits the final model on x alone, pooled over the covariate
#
# The slope is reported on the predictor's own scale: a one-unit increase
# in x shifts the latent outcome by b latent SDs. If that is too small to
# read, rescale x to a meaningful unit before fitting (e.g. divide years
# by 10 and report per decade) rather than standardising, since SDs of a
# predictor are a property of the sample, not of the quantity.
#
# Companion to fit_ordinal_by() for categorical predictors; the returned
# objects have the same shape so downstream code can treat them alike.
# -------------------------------------------------
# -------------------------------------------------
# fit_ordinal_by_continuous(): ordinal outcome ~ continuous predictor
#
#   1. screens for sample size and predictor variability
#   2. checks linearity on the latent scale (linear vs natural spline)
#   3. reports the predictor x covariate interaction (diagnostic only)
#   4. fits the final model on x alone, pooled over the covariate
#
# The model is a single continuous slope, but predictions are evaluated at
# the values of x that actually occur, so the figure matches the one from
# fit_ordinal_by(): bars = observed proportions, points = model estimates,
# faceted by outcome category.
#
# The slope is reported on the predictor's own scale: a one-unit increase
# in x shifts the latent outcome by b latent SDs. If that is too small to
# read, rescale x to a meaningful unit before fitting (e.g. divide years
# by 10 and report per decade) rather than standardising, since the SD of
# a predictor is a property of the sample, not of the quantity.
#
# Companion to fit_ordinal_by() for categorical predictors; the returned
# objects have the same shape so downstream code can treat them alike.
# -------------------------------------------------
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
      # This imposes EQUAL SPACING between adjacent levels, which is exactly
      # the assumption the monotonic model exists to avoid - so this is a
      # deliberate simplification, not a neutral default.
      warning(x, " is an ordered factor with non-numeric labels; scoring it ",
              "by level position (1..", nlevels(dat[[x]]), "). This ASSUMES ",
              "EQUAL SPACING between adjacent levels. Consider ",
              "fit_ordinal_by() with mo(), which estimates the spacing ",
              "instead of assuming it, and compare the two.", call. = FALSE)
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

  ok <- function(f) !inherits(f, "try-error") &&
                    !is.null(f$convergence$code) && f$convergence$code == 0

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

    if (ok(fit_l) && ok(fit_spline)) {
      lin_test <- anova(fit_l, fit_spline)
    } else {
      warning("Spline model did not converge; linearity not tested.")
      fit_spline <- NULL
    }
  }

  # ---- 3. interaction with the covariate (diagnostic) ------------
  # Both models are fitted to ONE dataset with complete cases on x, y AND
  # the covariate, so the LRT compares genuinely nested models. Fitting
  # them to different subsets is a common and silent error.
  int_test <- NULL; fit_int <- NULL
  if (!is.null(covariate) && covariate %in% names(dat)) {
    dat_int <- dat %>%
      dplyr::filter(!is.na(.data[[covariate]])) %>% droplevels()

    f_int <- stats::as.formula(sprintf("%s ~ %s * %s", y, x, covariate))
    f_add <- stats::as.formula(sprintf("%s ~ %s + %s", y, x, covariate))

    fit_int <- try(ordinal::clm(f_int, data = dat_int, link = link), silent = TRUE)
    fit_add <- try(ordinal::clm(f_add, data = dat_int, link = link), silent = TRUE)

    if (ok(fit_int) && ok(fit_add)) {
      int_test <- anova(fit_add, fit_int)
    } else {
      warning("Interaction model did not converge; interaction not tested. ",
              "Usually sparse cells in the three-way cross-classification.")
      fit_int <- NULL
    }
  }

  # ---- 4. final model: x alone, pooled over the covariate --------
  # Uses the larger dataset (covariate NAs allowed) to maximise n, since
  # the final model does not use the covariate.
  fit <- ordinal::clm(stats::as.formula(sprintf("%s ~ %s", y, x)),
                      data = dat, link = link)

  if (fit$convergence$code != 0)
    warning("Final model convergence code ", fit$convergence$code, ": ",
            paste(fit$convergence$messages, collapse = "; "),
            "\nCode -3 means the thresholds are not increasing and the fit ",
            "is INVALID - usually a sparsely populated outcome category. ",
            "Collapse it, or refit with brms.")

  # assumption checks. nominal_test() does not support the probit link,
  # so the partial-PO model is fitted explicitly and compared.
  scale_chk <- try(ordinal::scale_test(fit), silent = TRUE)
  po_chk <- try(
    anova(fit, ordinal::clm(stats::as.formula(sprintf("%s ~ 1", y)),
                            nominal = stats::as.formula(paste("~", x)),
                            data = dat, link = link)),
    silent = TRUE)

  lrt <- drop1(fit, test = "Chisq")

  # ---- effect size -----------------------------------------------
  # b is the shift in the latent outcome, in its SD units, per one-unit
  # increase in x.
  b  <- stats::coef(fit)[x]
  ci <- stats::confint(fit)[x, ]
  slope <- tibble::tibble(term     = x,
                          estimate = unname(b),
                          conf.low = ci[[1]], conf.high = ci[[2]])

  # average marginal effect on the probability scale, per outcome category
  ame <- try(
    as.data.frame(marginaleffects::avg_slopes(fit, variables = x, type = "prob")),
    silent = TRUE)

  if (verbose) {
    print(summary(fit)); print(lrt)
    cat(sprintf("\nslope: %.3f [%.3f, %.3f] probit SD per unit of %s\n",
                slope$estimate, slope$conf.low, slope$conf.high, x))
    if (!is.null(lin_test)) { cat("\nlinearity (linear vs spline):\n"); print(lin_test) }
    if (!is.null(int_test)) { cat("\ninteraction with ", covariate, ":\n", sep = ""); print(int_test) }
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
  # figure. Set FALSE when the numeric gaps are uneven (e.g. 0,1,2,5,10),
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


# -------------------------------------------------
# compact console summary
# -------------------------------------------------
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
    if (is.null(tst)) { cat(sprintf("%-16s not tested\n", lab)); return(invisible()) }
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

# -------------------------------------------------
# compact console summary
# -------------------------------------------------
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
    if (is.null(tst)) { cat(sprintf("%-16s not tested\n", lab)); return(invisible()) }
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


res_exp <- fit_ordinal_by_continuous(survey, x = "IO",
                                     y = "ai_use_freq",
                                    min_distinct_x = 5)

res_exp$fit$convergence
res_exp

res_att<- fit_ordinal_by_continuous(survey %>%
  filter(!gen_attitude %in% c("Indifferent", "Other")), x = "career_stage",
                                     y = "gen_attitude", covariate = NULL,
                                    min_distinct_x = 3)

table(res_att$data[[res_att$y]])
res_att$plot
res_exp$fit$convergence
res_exp

#sce start HERE cleaning things up



#



drop_gender <- c("Prefer not to answer", "Prefer to self-identify")

# are gender splits even across career stages?
tab_gend_career <- survey %>%
  dplyr::filter(!gender %in% drop_gender,
                !is.na(gender), !is.na(career_stage), !is.na(ai_use_freq)) %>%
  droplevels() %>%
  dplyr::group_by(gender, career_stage, .drop = FALSE) %>%
  dplyr::tally() %>%
  tidyr::pivot_wider(names_from = career_stage, values_from = n) %>%
  tibble::column_to_rownames("gender") %>%
  as.matrix()

tab_gend_career
any(tab_gend_career == 0)
chisq.test(tab_gend_career)$expected   # >20% of cells < 5 -> simulate.p.value

# The non-binary category is vanishingly small (max 11 in ECR) and not
# estimable alongside career stage, so this step is restricted to the two
# largest categories. Report that restriction explicitly.
dat_q4a <- survey %>%
  dplyr::filter(!is.na(career_stage), !is.na(ai_use_freq), !is.na(gender),
                !gender %in% c(drop_gender, "Non-binary")) %>%
  droplevels()

# does the gender effect differ by career stage?
res_4_int <- clm(ai_use_freq ~ gender * career_stage, data = dat_q4a,
                 link = "probit")
anova(clm(ai_use_freq ~ gender + career_stage, data = dat_q4a, link = "probit"),
      res_4_int)
# NOTE: a non-significant LRT is not evidence the effects are equal. With
# these cell counts the test has limited power - report the interaction
# estimate and its CI alongside, not just the p-value.

# no interaction, so pool over career stages to maximise n
dat_q4 <- survey %>%
  dplyr::filter(!is.na(ai_use_freq), !is.na(gender),
                !gender %in% c(drop_gender, "Non-binary")) %>%
  droplevels()

res_4 <- clm(ai_use_freq ~ gender, data = dat_q4, link = "probit")
drop1(res_4, test = "Chisq")
summary(res_4)

# proportional odds and equal-variance checks. nominal_test() does not
# support the probit link, so the partial-PO model is fitted explicitly.
ordinal::scale_test(res_4)
anova(res_4, clm(ai_use_freq ~ 1, nominal = ~ gender,
                 data = dat_q4, link = "probit"))

# Effect size on the latent scale: a difference in latent SD units,
# interpretable as a Cohen's d. The CI matters more than the p-value -
# it says how large an effect can still be ruled out.
confint(pairs(emmeans(res_4, ~ gender, mode = "linear.predictor")))

# ---- model estimates --------------------------------------------
emm_gender <- as.data.frame(
  marginaleffects::avg_predictions(res_4, by = "gender", type = "prob")) %>%
  dplyr::mutate(ai_use_freq = factor(as.character(group),
                                     levels = levels(dat_q4$ai_use_freq)))

# emmeans gives the same answer here: with a single predictor and no
# covariates, the reference grid and the row-wise average coincide.
# as.data.frame(emmeans(res_4, ~ ai_use_freq | gender, mode = "prob"))

# ---- observed proportions ---------------------------------------
raw_gender <- dat_q4 %>%
  dplyr::count(gender, ai_use_freq) %>%
  dplyr::group_by(gender) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ai_use_freq = factor(as.character(ai_use_freq),
                                     levels = levels(dat_q4$ai_use_freq)))

# ---- plot --------------------------------------------------------
# n labels come from raw (cell counts), not the gender totals, so they
# differ across facets as they should
gender_plot <- ggplot(emm_gender, aes(gender, estimate)) +
  geom_col(data = raw_gender, aes(y = prop), fill = "grey85", width = .7) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  colour = "firebrick", size = .3) +
  geom_text(data = raw_gender, aes(y = Inf, label = n),
            vjust = 1.4, size = 2.6, colour = "grey40") +
  facet_wrap(~ ai_use_freq, nrow = 1, labeller = label_wrap_gen(20)) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(.05, .12))) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  labs(x = NULL, y = "P(category)",
       caption = "grey = observed; red = model estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_path, "ai_use_by_gender.jpg"), plot = gender_plot,
       width = 8, height = 6)





# =================================================================
# 6. AI USE FREQUENCY BY PROF WORK SECTOR
# =================================================================

drop_work <- c("Student", "Other")

# are work splits even across career stages?
tab_work_career <- survey %>%
  dplyr::filter(!prof_work_sector %in% drop_work,
                !is.na(prof_work_sector), !is.na(career_stage), !is.na(ai_use_freq)) %>%
  droplevels() %>%
  dplyr::group_by(prof_work_sector, career_stage, .drop = FALSE) %>%
  dplyr::tally() %>%
  tidyr::pivot_wider(names_from = career_stage, values_from = n) %>%
  tibble::column_to_rownames("prof_work_sector") %>%
  as.matrix()

tab_work_career
any(tab_work_career == 0)
chisq.test(tab_work_career)$expected   # >20% of cells < 5 -> simulate.p.value

# SCE test for any categories that are is vanishingly small (max 11 in ECR) and not
# estimable alongside career stage, so this step is restricted to the two
# largest categories. Report that restriction explicitly.
dat_ai_work <- survey %>%
  dplyr::filter(!is.na(career_stage), !is.na(ai_use_freq), !is.na(gender),
                !gender %in% c(drop_gender, "Non-binary")) %>%
  droplevels()

# does the work  effect differ by career stage?
res_4_int <- clm(ai_use_freq ~ prof_work_sector * career_stage, data = dat_q4a,
                 link = "probit")
anova(clm(ai_use_freq ~ prof_work_sector + career_stage, data = dat_ai_work, link = "probit"),
      res_4_int)
# NOTE: a non-significant LRT is not evidence the effects are equal. With
# these cell counts the test has limited power - report the interaction
# estimate and its CI alongside, not just the p-value.

# no interaction, so pool over career stages to maximise n
dat_ai_work <- survey %>%
  dplyr::filter(!is.na(ai_use_freq), !is.na(prof_work_sector),
                !prof_work_sector %in% c(drop_work, "Non-binary")) %>%
  droplevels()

res_4 <- clm(ai_use_freq ~ prof_work_sector, data = dat_q4, link = "probit")
drop1(res_4, test = "Chisq")
summary(res_4)

# proportional odds and equal-variance checks. nominal_test() does not
# support the probit link, so the partial-PO model is fitted explicitly.
ordinal::scale_test(res_4)
anova(res_4, clm(ai_use_freq ~ 1, nominal = ~ prof_work_sector,
                 data = dat_ai_work, link = "probit"))

# Effect size on the latent scale: a difference in latent SD units,
# interpretable as a Cohen's d. The CI matters more than the p-value -
# it says how large an effect can still be ruled out.
confint(pairs(emmeans(res_4, ~ prof_work_sector, mode = "linear.predictor")))

# ---- model estimates --------------------------------------------
emm_work <- as.data.frame(
  marginaleffects::avg_predictions(res_4, by = "prof_work_sector", type = "prob")) %>%
  dplyr::mutate(ai_use_freq = factor(as.character(group),
                                     levels = levels(dat_ai_work$ai_use_freq)))

# emmeans gives the same answer here: with a single predictor and no
# covariates, the reference grid and the row-wise average coincide.
# as.data.frame(emmeans(res_4, ~ ai_use_freq | gender, mode = "prob"))

# ---- observed proportions ---------------------------------------
raw_work <- dat_ai_work %>%
  dplyr::count(prof_work_sector, ai_use_freq) %>%
  dplyr::group_by(prof_work_sector) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ai_use_freq = factor(as.character(ai_use_freq),
                                     levels = levels(dat_ai_work$ai_use_freq)))

# ---- plot --------------------------------------------------------
# n labels come from raw (cell counts), not the work totals, so they
# differ across facets as they should
work_plot <- ggplot(emm_work, aes(prof_work_sector, estimate)) +
  geom_col(data = raw_work, aes(y = prop), fill = "grey85", width = .7) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  colour = "firebrick", size = .3) +
  geom_text(data = raw_work, aes(y = Inf, label = n),
            vjust = 1.4, size = 2.6, colour = "grey40") +
  facet_wrap(~ ai_use_freq, nrow = 1, labeller = label_wrap_gen(20)) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(.05, .12))) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  labs(x = NULL, y = "P(category)",
       caption = "grey = observed; red = model estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_path, "ai_use_by_work.jpg"), plot = work_plot,
       width = 8, height = 6)       


# -------------------------------------------------
# Session info for the methods section
# -------------------------------------------------
sessionInfo()

