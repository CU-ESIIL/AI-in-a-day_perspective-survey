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
# confirm we want re.formula as NA (I think so)
# note differs across brms vs emm in the predictions
# https://github.com/easystats/modelbased/issues/579

# remove excess packges (brms tests now ported into different code)
# deal with different counts in the lin vs fact ordinal models
# the AIC comparison is not correct bc fit to different data

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
  fits     <- purrr::compact(list(mono = fit_mono, fac = fit_fac,
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
# REPLACEMENT FOR SECTION 3: LIKERT-PAIR ANALYSES
# -----------------------------------------------------------------
# Three copy-pasted blocks become one spec list plus a runner. The
# per-question decision you were making by eye - "is mo() earning its
# keep over a linear coding?" - is computed instead, from the simplex
# posteriors and the LOO comparison together.
#
# Burkner & Charpentier (2020) compare exactly three codings of an
# ordinal predictor: monotonic, linear and categorical. Their own case
# study rests on p_loo as much as elpd_diff - the monotonic model wins
# by being more parsimonious than the categorical one, not by fitting
# better - so p_loo is carried through here.
# =================================================================


# =================================================================
# 3a. SPECS
# =================================================================
# tag        -> cache file prefix and figure name
# filter     -> rows to keep, applied to `survey`
# covariate  -> optional frequentist pre-check: does the x effect differ
#               across this variable? If it does, pooling is hiding
#               something and the Bayesian model should be reconsidered.
# pal_option -> viridis option, so the three figures are distinguishable
likert_specs <- list(

  attitude_by_stage = list(
    x = "career_stage", y = "gen_attitude",
    tag = "attitude_by_stage",
    label = "attitude by career stage",
    filter = function(d)
      dplyr::filter(d, !gen_attitude %in% c("Other", "Indifferent")),
    covariate  = NULL,          # x IS the covariate here
    pal_option = "D"),

  dsfreq_by_ai_usefreq = list(
    x = "ds_freq", y = "ai_use_freq",
    tag = "dsfreq_by_ai_usefreq",
    label = "AI use by data-science frequency",
    filter = function(d)
      dplyr::filter(d, !is.na(career_stage), !is.na(ai_use_freq),
                    !is.na(ds_freq)),
    covariate  = "career_stage",
    pal_option = "C"),

  policies_by_ai_usefreq = list(
    x = "policies", y = "ai_use_freq",
    tag = "policies_by_ai_usefreq",
    label = "AI use by institutional policy",
    filter = function(d)
      dplyr::filter(d, !is.na(policies), !is.na(ai_use_freq),
                    !policies %in% c(
                      "Other",
                      "There are not any policies or guidelines at my institution")),
    covariate  = "career_stage",
    pal_option = "A")
)


# =================================================================
# 3b. HELPERS
# =================================================================

# ---- cell counts, before fitting anything -------------------------
# A cumulative model needs every outcome category populated: a category
# with a handful of observations pushes its thresholds around and is the
# usual cause of clm/brms convergence code -3 ("thresholds not increasing").
likert_cells <- function(dat, x, y) {
  tab <- table(dat[[x]], dat[[y]])
  list(table   = tab,
       zeros   = sum(tab == 0),
       y_total = table(dat[[y]]),
       x_total = table(dat[[x]]),
       thin_y  = names(which(table(dat[[y]]) < 10)))
}


# ---- is the monotonic spacing telling us anything? ----------------
# Under equal spacing every simplex element equals 1/(K-1). An element
# whose 90% interval EXCLUDES that value is evidence of uneven spacing,
# which is the only thing mo() buys over a linear coding.
#
# Burkner & Charpentier note that when b is near zero there is little to
# learn about the simplex, so its posterior stays close to its prior.
# A flat-looking simplex under a weak effect is expected, not a failure.
#
# NOTE the 90% interval here is narrower than the 95% used for the slope.
# A 95% interval on a simplex element almost never excludes 1/(K-1), so
# the test would never fire. State the width when reporting.
simplex_summary <- function(fit, K_x, x_levels = NULL) {
 
  eq <- 1 / (K_x - 1)
 
  s <- posterior::summarise_draws(
    posterior::as_draws(fit),
    "mean", ~quantile(.x, probs = c(0.05, 0.95))) |>
    dplyr::filter(grepl("^simo_", variable)) |>
    dplyr::rename(q5 = `5%`, q95 = `95%`) |>
    dplyr::mutate(step  = dplyr::row_number(),
                  equal = eq,
                  off   = q5 > eq | q95 < eq) |>   # interval excludes equal spacing
    dplyr::relocate(step, .before = 1)
 
  # Label each element by the pair of x levels whose gap it scales, so the
  # CSV is readable without decoding simo_mo<x>1[i]. Element i is the
  # share of the total effect spent moving from level i to level i+1.
  s$step_label <- if (!is.null(x_levels) && length(x_levels) == K_x)
    paste(x_levels[s$step], x_levels[s$step + 1L], sep = " -> ")
  else NA_character_
 
  attr(s, "n_off") <- sum(s$off)
  s
}


# ---- which coding to report ---------------------------------------
# -----------------------------------------------------------------
# Combines the two pieces of evidence rather than eyeballing them:
#   - does any simplex element depart from equal spacing?
#   - does the linear model lose meaningfully on LOO?
# A difference under ~2 ELPD is not meaningful, and se_diff is optimistic
# for nested models (Sivula, Magnusson & Vehtari 2022), so the threshold
# is deliberately conservative.
recommend_coding <- function(z, elpd_tol = 2) {
 
  # x_levels lets the simplex table name the step each element scales
  x_lev <- if (!is.null(z$data) && !is.null(z$data[[z$x]]))
    levels(z$data[[z$x]]) else NULL
 
  sx    <- simplex_summary(z$fits$mono, z$K_x, x_levels = x_lev)
  n_off <- attr(sx, "n_off")
 
  l <- if (!is.null(z$loo)) unclass(z$loo) else NULL
  best <- if (!is.null(l)) rownames(l)[1] else NA_character_
 
  # how far behind the best model is the linear coding, and the MCSE on
  # that gap. se_diff is 0 in the top row because it is the reference.
  has_lin <- !is.null(l) && "lin" %in% rownames(l)
  lin_gap <- if (has_lin) abs(l["lin", "elpd_diff"]) else NA_real_
  lin_se  <- if (has_lin) l["lin", "se_diff"]        else NA_real_
 
  rec <- if (is.na(lin_gap)) {
    "lin not fitted"
  } else if (n_off == 0 && lin_gap < elpd_tol) {
    "linear - spacing is even and LOO cannot separate them"
  } else if (n_off > 0) {
    sprintf("monotonic - %d simplex element(s) depart from equal spacing", n_off)
  } else {
    sprintf("monotonic - linear loses %.1f ELPD", lin_gap)
  }
 
  list(simplex   = sx,
       n_off     = n_off,
       # which steps are uneven, as one readable string for the CSV
       off_steps = if (n_off > 0)
                     paste(stats::na.omit(sx$step_label[sx$off]), collapse = "; ")
                   else NA_character_,
       # the flattest and steepest step, to show the spread at a glance
       simo_min  = min(sx$mean),
       simo_max  = max(sx$mean),
       best_loo  = best,
       lin_gap   = lin_gap,
       lin_se    = lin_se,
       recommendation = rec)
}



# =================================================================
# 3c. RUNNER
# =================================================================
run_likert_cell <- function(survey, spec, rerun = rerun_mods,
                            b_prior = b_prior_mode,
                            width = 8, height = 6, ...) {

  message("\n=== ", spec$y, " by ", spec$x, " ===")

  dat <- spec$filter(survey) %>%
    dplyr::filter(!is.na(.data[[spec$x]]), !is.na(.data[[spec$y]])) %>%
    droplevels()

  cells <- likert_cells(dat, spec$x, spec$y)
  if (length(cells$thin_y))
    message("  sparse outcome categories (n < 10): ",
            paste(cells$thin_y, collapse = ", "),
            " - a likely cause of convergence code -3")

  cov_test <- covariate_check(dat, spec$x, spec$y, spec$covariate)

  res <- try(
    run_likert_pair(dat, x = spec$x, y = spec$y,
                    file_prefix = file.path(fit_path, spec$tag),
                    rerun = rerun, b_prior = b_prior,
                    run_lin = TRUE, run_ls = FALSE, ...),
    silent = TRUE)

  if (inherits(res, "try-error")) {
    msg <- conditionMessage(attr(res, "condition"))

    # A package-conflict, missing-object or missing-function error is a
    # script configuration problem, not a property of this cell's data.
    # Swallowing it silently empties every downstream table, so stop.
    if (grepl("\\[conflicted\\]|could not find function|object '.*' not found",
              msg)) {
      stop("configuration error while fitting ", spec$tag, ":\n", msg,
           call. = FALSE)
    }

    message("  FAILED: ", msg)
    return(list(spec = spec, cells = cells, cov_test = cov_test,
                error = msg))
  }

  coding <- recommend_coding(res)
  message("  -> report the ", coding$recommendation)

  p <- make.likert.plot(res, pal_option = spec$pal_option)
  ggsave(file.path(fig_path, paste0(spec$tag, ".jpg")), plot = p,
         width = width, height = height)

  list(spec = spec, cells = cells, cov_test = cov_test,
       res = res, plot = p, coding = coding, error = NA_character_)
}


# =================================================================
# 3d. RUN ALL THREE
# =================================================================
likert_results <- purrr::map(likert_specs, \(s) run_likert_cell(survey, s))


# ---- summary table --------------------------------------------------
# One row per cell. Carries a digest of both the LOO comparison and the
# simplex spacing, so this file alone answers "which coding did we report
# and why". The full per-model and per-element detail goes to the two
# long CSVs written in step 4.
likert_summary <- purrr::imap_dfr(likert_results, function(z, nm) {
 
  base <- tibble::tibble(cell = nm, x = z$spec$x, y = z$spec$y,
                         error = z$error)
  if (!is.na(z$error)) return(base)
 
  r  <- z$res
  s  <- r$slope_summary[1, ]
  l  <- if (!is.null(r$loo)) unclass(r$loo) else NULL
 
  # pull one LOO cell by model name, NA if that model was not fitted
  loo_cell <- function(model, col) {
    if (!is.null(l) && model %in% rownames(l)) l[model, col] else NA_real_
  }
 
  logbf <- if (is.null(r$bf_slope)) NA_real_
           else as.data.frame(r$bf_slope)$log_BF[1]
 
  dplyr::bind_rows(base) %>%
    dplyr::mutate(
      n        = r$n, K_x = r$K_x, K_y = r$K_y,
      b        = s[["Estimate"]],
      b_lo     = s[["Q2.5"]], b_hi = s[["Q97.5"]],
      # capped: the density estimate at zero is unstable when the
      # posterior sits far from it, so an exact large BF is not meaningful
      BF10     = if (is.na(logbf)) NA_character_
                 else if (logbf > log(100)) "> 100"
                 else if (logbf < log(1/100)) "< 0.01"
                 else sprintf("%.1f", exp(logbf)),
 
      # ---- LOO comparison digest --------------------------------
      best_loo = z$coding$best_loo,
      # elpd_diff is relative to the best model, so the top row is 0
      elpd_diff_mono = loo_cell("mono", "elpd_diff"),
      elpd_diff_fac  = loo_cell("fac",  "elpd_diff"),
      elpd_diff_lin  = loo_cell("lin",  "elpd_diff"),
      se_diff_mono   = loo_cell("mono", "se_diff"),
      se_diff_fac    = loo_cell("fac",  "se_diff"),
      se_diff_lin    = loo_cell("lin",  "se_diff"),
      # how far behind the best model the linear coding sits, and its se
      lin_gap  = z$coding$lin_gap,
      lin_se   = z$coding$lin_se,
      # effective parameters: mono wins over fac by parsimony, not fit
      p_loo_mono = loo_cell("mono", "p_loo"),
      p_loo_fac  = loo_cell("fac",  "p_loo"),
      p_loo_lin  = loo_cell("lin",  "p_loo"),
 
      # ---- simplex spacing digest -------------------------------
      # K_x-1 simplex elements, each equal to 1/(K_x-1) under even spacing
      equal_spacing = 1 / (r$K_x - 1),
      n_simplex_off = z$coding$n_off,
      simplex_off_steps = z$coding$off_steps,
      simo_min      = z$coding$simo_min,
      simo_max      = z$coding$simo_max,
 
      coding      = z$coding$recommendation,
      p_covariate = if (is.null(z$cov_test)) NA_real_
                    else z$cov_test[["Pr(>Chisq)"]][2],
      divergent   = sum(r$diagnostics$divergent),
      n_flagged   = sum(r$diagnostics$n_flagged))
})
 
if (nrow(likert_summary)) {
  print(as.data.frame(likert_summary), row.names = FALSE)
  readr::write_csv(likert_summary,
                   file.path(fig_path, "likert_pair_summary.csv"))
}


# ---- per-question detail, on demand ---------------------------------
# likert_results$attitude_by_stage$res                 # compact print method
# likert_results$attitude_by_stage$coding$simplex      # spacing vs 1/(K-1)
# likert_results$attitude_by_stage$res$loo             # full LOO table
# likert_results$attitude_by_stage$res$diagnostics
# likert_results$attitude_by_stage$plot
# summary(likert_results$attitude_by_stage$res$fits$mono)
# plot_prior_post(likert_results$attitude_by_stage$res$prior_post$mono)
# likert_results$attitude_by_stage$res$prior_pc$mono   # prior predictive

# all three prior-predictive checks at once
purrr::iwalk(likert_results, function(z, nm) {
  if (is.na(z$error)) print(z$res$prior_pc$mono + ggplot2::ggtitle(nm))
})


# ---- full LOO comparison, one row per cell x model ----------------
# The long form of what print.likert_pair() shows per cell. Rows are
# ordered best-first within a cell, which is what `rank` records.
loo_tab <- purrr::imap_dfr(likert_results, function(z, nm) {
  if (!is.na(z$error) || is.null(z$res$loo)) return(NULL)
  l <- unclass(z$res$loo)
  tibble::tibble(
    cell        = nm,
    x           = z$spec$x,
    y           = z$spec$y,
    rank        = seq_len(nrow(l)),      # 1 = best
    model       = rownames(l),
    elpd_diff   = l[, "elpd_diff"],      # vs the best model
    se_diff     = l[, "se_diff"],
    elpd_loo    = l[, "elpd_loo"],
    se_elpd_loo = l[, "se_elpd_loo"],
    p_loo       = l[, "p_loo"],
    looic       = l[, "looic"],
    best_loo    = z$coding$best_loo,
    coding      = z$coding$recommendation)
})
 
if (nrow(loo_tab)) {
  loo_tab |>
    dplyr::select(cell, rank, model, elpd_diff, se_diff, p_loo) |>
    as.data.frame() |>
    print(row.names = FALSE, digits = 3)
  readr::write_csv(loo_tab, file.path(fig_path, "likert_loo_compare.csv"))
} else {
  message("No LOO tables - every cell errored. ",
          "See purrr::map_chr(likert_results, 'error').")
}
 
 
# ---- full simplex spacing, one row per cell x step ----------------
# Each element is the share of the total lowest-to-highest effect spent
# on one step between adjacent x levels. Equal spacing = 1/(K_x-1).
# off = TRUE means the 90% interval excludes that value, which is the
# evidence for mo() over a linear coding.
simplex_tab <- purrr::imap_dfr(likert_results, function(z, nm) {
  if (!is.na(z$error) || is.null(z$coding)) return(NULL)
  z$coding$simplex |>
    dplyr::mutate(cell = nm, x = z$spec$x, y = z$spec$y, .before = 1)
})
 
if (nrow(simplex_tab)) {
  simplex_tab |>
    dplyr::select(cell, step, step_label, mean, q5, q95, equal, off) |>
    as.data.frame() |>
    print(row.names = FALSE, digits = 3)
  readr::write_csv(simplex_tab, file.path(fig_path, "likert_simplex.csv"))
} else {
  message("No simplex tables - every cell errored. ",
          "See purrr::map_chr(likert_results, 'error').")
}

# Sanity check per the Kurz blog: with disc ~ 0 + mo() there should be NO
# b_disc_Intercept. If one appears the location-scale model is not
# identified. Only relevant if you turn run_ls back on.
# https://solomonkurz.netlify.app/blog/2021-12-29-notes-on-the-bayesian-cumulative-probit/
# grep("disc", posterior::variables(likert_results[[1]]$res$fits$ls), value = TRUE)


# =================================================================
# 3e. PRIOR SENSITIVITY
# =================================================================
# Refits under all three slope priors. The flat version is the useful
# anchor - it shows what the likelihood alone says - but it carries no
# Bayes factor and no prior-predictive check, since an improper prior
# has no distribution to draw from.
#
# Each extra mode costs a full set of fits, so this defaults to the
# first question only. Set to names(likert_specs) to run them all.
sens_cells <- "attitude_by_stage"
run_sensitivity <- TRUE

if (run_sensitivity) {

  sens <- purrr::map(sens_cells, function(nm) {
    spec <- likert_specs[[nm]]
    dat  <- spec$filter(survey) %>% droplevels()

    purrr::map(c("scaled", "unscaled", "flat"), function(m) {
      s <- spec
      s$tag <- paste0(spec$tag, "_", m)
      try(run_likert_cell(survey, s, b_prior = m), silent = TRUE)
    }) |> rlang::set_names(c("scaled", "unscaled", "flat"))
  }) |> rlang::set_names(sens_cells)

  # if the slope posterior barely moves across the three, the result is
  # insensitive to the prior - one line for the methods section
  sens_tab <- purrr::imap_dfr(sens, function(cell, cell_nm)
    purrr::imap_dfr(cell, function(z, prior_nm) {
      if (inherits(z, "try-error") || !is.na(z$error))
        return(tibble::tibble(cell = cell_nm, b_prior = prior_nm))
      s <- z$res$slope_summary[1, ]
      tibble::tibble(cell = cell_nm, b_prior = prior_nm,
                     b = s[["Estimate"]], lo = s[["Q2.5"]], hi = s[["Q97.5"]])
    }))

  print(as.data.frame(sens_tab), row.names = FALSE, digits = 3)
  readr::write_csv(sens_tab, file.path(fig_path, "likert_prior_sensitivity.csv"))
}
