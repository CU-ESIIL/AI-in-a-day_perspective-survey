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
# REPLACEMENT FOR SECTIONS 5-7
# -----------------------------------------------------------------
# Every question crossed with every demographic predictor, driven by
# two spec lists rather than copy-pasted blocks. Each cell is wrapped
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
    type  = "categorical"),

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
    label = "identity-overlap count",
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

  # IO is a numeric count. Every select-all helper treats x as a factor
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

#if (nrow(sa_results)) {
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
#}

# a single plot
# sa_results$opportunity__career_stage$res$pop$plot


# =================================================================
# 6. ORDINAL GRID: every ordinal outcome x every demographic
# =================================================================

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


# -------------------------------------------------
# Session info for the methods section
# -------------------------------------------------
  sessionInfo()