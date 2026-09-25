# -------------------------------------------------
# survey_elmendorf_survey_stats.R
# -------------------------------------------------
# Experimenting with fitting bayesian models
# with both x and y as likert for the first 4 questions,
# continuing down the list with ordinal package for simple ordinal models
# as appropriate to the question
# -------------------------------------------------

# -------------------------------------------------
# Project setup
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
# Load required libraries
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
  janitor,            # clean column names
  brms,
  marginaleffects,
  emmeans,
  tidybayes,
  loo,
  ordinal
)


# -------------------------------------------------
# Source reusable functions (all tools/ scripts)
# -------------------------------------------------
purrr::walk(
  dir("tools", pattern = "\\.r$", full.names = TRUE),
  source
)


# -------------------------------------------------
# Choose dataset  and read it with the required logic
# -------------------------------------------------
data_path <- file.path("data", "01_tidied-responses.csv")

# Read CSV, convert empty strings to NA, and add a RespondentId column.
svy_v01 <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    dplyr::across(.cols = dplyr::everything(), .fns = ~ ifelse(nchar(.) == 0, NA, .))
  )

# Use the same variable name as before for downstream code.
survey_raw <- svy_v01

# Define where graphs will be saved based on the data toggle.
graph_path <- "graphs"

# -------------------------------------------------
# Clean column names & basic preparation
# -------------------------------------------------
survey <- survey_raw %>%
  clean_names()   # snake_case, removes spaces, etc.


# ------------------------------------------------------------------
# Re‑level the categorical AI attitude column to reflect the ordered
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
    ai_use_freq = factor(.data[["ai_use_freq"]],
                     levels = unique(.data[["ai_use_freq"]][order(.data[["ai_use_freq_value"]])]),
                     ordered = TRUE),
    career_stage = factor(.data[["career_stage"]],
                          levels = unique(.data[["career_stage"]][order(.data[["career_stage_value"]])]),
                          ordered = TRUE),
    policies = factor(.data[["policies"]],
                      levels = unique(.data[["policies"]][order(.data[["policies_value"]])]),
                      ordered = TRUE)                                        
  )
# Ensure ordinal columns are numeric (they should already be, but be safe).
survey <- survey %>%
  dplyr::mutate(across(ends_with("_value"), as.numeric))

# -------------------------------------------------
# fit_likert_pair(): ordinal model workflow for a
# Likert predictor (x) and Likert outcome (y)
# -------------------------------------------------

prior_post_draws <- function(fit_post, fit_prior,
                             pattern = "^(b_|bsp_|simo_)") {
  post_vars  <- grep(pattern, variables(fit_post),  value = TRUE)
  prior_vars <- grep(pattern, variables(fit_prior), value = TRUE)
  pars <- intersect(post_vars, prior_vars)
  
  missing <- setdiff(post_vars, pars)
  if (length(missing))
    warning("No prior-only draws for: ", paste(missing, collapse = ", "))
  
  get_draws <- function(fit) {
    m <- as_draws_matrix(fit, variable = pars)
    as_tibble(unclass(m)[, pars, drop = FALSE])
  }
  
  bind_rows(
    get_draws(fit_prior) |> mutate(dist = "prior"),
    get_draws(fit_post)  |> mutate(dist = "posterior")
  ) |>
    pivot_longer(-dist, names_to = "parameter", values_to = "value") |>
    mutate(parameter = factor(parameter, levels = pars),
           dist      = factor(dist, levels = c("prior", "posterior")))
}


plot_prior_post <- function(d, ncol = 3, trim = c(.005, .995)) {
  # trim the heavy prior tails for display only, per parameter
  d <- d |>
    group_by(parameter) |>
    mutate(lo = quantile(value[dist == "prior"], trim[1]),
           hi = quantile(value[dist == "prior"], trim[2])) |>
    filter(value >= pmin(lo, min(value[dist == "posterior"])),
           value <= pmax(hi, max(value[dist == "posterior"]))) |>
    ungroup()
  
  ggplot(d, aes(value, fill = dist)) +
    geom_density(alpha = .45, colour = NA) +
    facet_wrap(~ parameter, scales = "free", ncol = ncol) +
    scale_fill_manual(values = c(prior = "grey65", posterior = "#2c7fb8")) +
    labs(x = NULL, y = "density", fill = NULL,
         title = "Prior vs posterior") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")
}

# example use
# pp <- prior_post_draws(fit_mono, fit_prior_mono)
# plot_prior_post(pp)

fit_likert_pair <- function(data, x, y,
                            b_sd_fac    = 1,
                            disc_sd     = 0.25,
                            run_ls      = TRUE,
                            adapt_delta = 0.99,
                            cores = 4, seed = 1,
                            cache_dir   = NULL,
                            file_prefix = NULL,
                            overwrite   = FALSE,
                            quiet = FALSE) {
  
  stopifnot(is.character(x), is.character(y), length(x) == 1, length(y) == 1)
  if (!all(c(x, y) %in% names(data)))
    stop("Columns not found in data: ",
         paste(setdiff(c(x, y), names(data)), collapse = ", "))

  # ---- data prep -------------------------------------------------------
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

  # drop levels left empty by upstream filtering — otherwise brms estimates
  # thresholds for categories nobody chose
  dat <- droplevels(dat)

  K_x <- nlevels(dat[[x]]); K_y <- nlevels(dat[[y]])
  if (K_x < 3) stop(x, " has fewer than 3 levels after filtering; mo() needs >= 3.")
  if (K_y < 3) stop(y, " has fewer than 3 levels after filtering.")

  # unordered copy of x for the dummy-coded model (see notes)
  dat_fac <- dat
  dat_fac[[x]] <- factor(as.character(dat[[x]]),
                         levels = levels(dat[[x]]), ordered = FALSE)

  # ---- names built from the variables ----------------------------------
  simo_coef <- paste0("mo", x, "1")
  b_sd_mono <- b_sd_fac / (K_x - 1)     # b is the per-step effect

  f_mono <- brms::bf(stats::as.formula(sprintf("%s ~ mo(%s)", y, x)))
  f_fac  <- brms::bf(stats::as.formula(sprintf("%s ~ %s",     y, x)))
  f_ls   <- brms::bf(stats::as.formula(sprintf("%s ~ mo(%s)", y, x)),
                     stats::as.formula(sprintf("disc ~ 0 + mo(%s)", x)))

  # ---- priors ----------------------------------------------------------
  th_prior <- brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept")

  priors_mono <- c(
    brms::set_prior(sprintf("normal(0, %g)", b_sd_mono), class = "b"),
    th_prior,
    brms::set_prior("dirichlet(1)", class = "simo", coef = simo_coef)
  )

  priors_fac <- c(
    brms::set_prior(sprintf("normal(0, %g)", b_sd_fac), class = "b"),
    th_prior
  )

  priors_ls <- c(
    priors_mono,
    brms::set_prior(sprintf("normal(0, %g)", disc_sd / (K_x - 1)),
                    class = "b", dpar = "disc"),
    brms::set_prior("dirichlet(1)", class = "simo",
                    coef = simo_coef, dpar = "disc")
  )

  # ---- resolve the cache path -------------------------------------------
  if (is.null(file_prefix) && !is.null(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    file_prefix <- file.path(cache_dir, make.names(paste0(y, "__by__", x)))
  }

  tags <- c("mono", "fac", if (run_ls) "ls")
  tags <- c(tags, paste0(tags, "_prior"))

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

    # ---- fitting ---------------------------------------------------------
  fp <- function(tag) if (is.null(file_prefix)) NULL else paste0(file_prefix, "_", tag)
  ctrl <- list(adapt_delta = adapt_delta)

  run <- function(formula, d, prior, tag) {
    if (!quiet) message("Fitting ", tag, " ...")
    brms::brm(formula, data = d, family = brms::cumulative("probit"),
              prior = prior, cores = cores, seed = seed,
              control = ctrl, file = fp(tag))
  }

  fit_mono <- run(f_mono, dat,     priors_mono, "mono")
  fit_fac  <- run(f_fac,  dat_fac, priors_fac,  "fac")
  fit_ls   <- if (run_ls) run(f_ls, dat, priors_ls, "ls") else NULL

  prior_only <- function(fit, tag) {
    if (is.null(fit)) return(NULL)
    if (!quiet) message("Fitting ", tag, " (prior only) ...")
    stats::update(fit, sample_prior = "only", seed = seed,
                  cores = cores, file = fp(paste0(tag, "_prior")))
  }

  prior_mono <- prior_only(fit_mono, "mono")
  prior_fac  <- prior_only(fit_fac,  "fac")
  prior_ls   <- prior_only(fit_ls,   "ls")

  # ---- checks, comparison, BF -----------------------------------------
  fits       <- purrr::compact(list(mono = fit_mono, fac = fit_fac, ls = fit_ls))
  fits_prior <- purrr::compact(list(mono = prior_mono, fac = prior_fac, ls = prior_ls))

  prior_post <- purrr::map2(fits, fits_prior, prior_post_draws)
  prior_pc   <- purrr::imap(fits_prior, \(f, nm)
    brms::pp_check(f, type = "bars", ndraws = 200) +
      ggplot2::ggtitle(paste0("Prior predictive: ", nm)))
  post_pc    <- purrr::imap(fits, \(f, nm)
    brms::pp_check(f, type = "bars", ndraws = 200) +
      ggplot2::ggtitle(paste0("Posterior predictive: ", nm)))

  fits_loo <- purrr::map(fits, brms::add_criterion, "loo")
  loo_tab  <- loo::loo_compare(purrr::map(fits_loo, \(f) f$criteria$loo))

  # BF for the monotonic slope only; thresholds/simplex have no meaningful null
  slope_row <- grep("^mo", rownames(brms::fixef(fit_mono)), value = TRUE)
  if (length(slope_row) != 1)
    stop("Expected exactly one monotonic term, found: ",
         paste(slope_row, collapse = ", "))
  slope_draw <- paste0("bsp_", slope_row)

  bf_slope <- bayestestR::bayesfactor_parameters(
    fit_mono, prior = prior_mono,
    parameters = paste0("^", gsub("([\\W])", "\\\\\\1", slope_draw, perl = TRUE), "$")
  )

  ce <- brms::conditional_effects(fit_mono, categorical = TRUE)
  ce_plot <- plot(ce, plot = FALSE)[[1]] +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x, y = paste0("P(", y, " = k)")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  structure(list(
    x = x, y = y, n = nrow(dat), K_x = K_x, K_y = K_y,
    data = dat,
    priors = list(mono = priors_mono, fac = priors_fac, ls = priors_ls),
    fits = fits_loo, fits_prior = fits_prior,
    prior_post = prior_post, prior_pc = prior_pc, post_pc = post_pc,
    loo = loo_tab,
    bf_slope = bf_slope,
    slope_summary = brms::fixef(fit_mono)[slope_row, , drop = FALSE],
    ce = ce, ce_plot = ce_plot
  ), class = "likert_pair")
}

# define function to load so you don't have to rerun everything
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

  fits       <- purrr::compact(list(mono = rd("mono"), fac = rd("fac"), ls = rd("ls")))
  fits_prior <- purrr::compact(list(mono = rd("mono_prior"),
                                    fac  = rd("fac_prior"),
                                    ls   = rd("ls_prior")))

  if (is.null(fits$mono)) stop("No monotonic fit found at ", file_prefix, "_mono.rds")

  fit_mono   <- fits$mono
  prior_mono <- fits_prior$mono
  dat        <- if (is.null(data)) fit_mono$data else data

  # ---- recompute the cheap parts -----------------------------------------
  slope_row <- grep("^mo", rownames(brms::fixef(fit_mono)), value = TRUE)
  if (length(slope_row) != 1)
    stop("Expected one monotonic term, found: ", paste(slope_row, collapse = ", "))

  bf_slope <- if (!is.null(prior_mono))
    bayestestR::bayesfactor_parameters(fit_mono, prior = prior_mono,
                                       parameters = paste0("bsp_", slope_row))
  else NULL

  fits_loo <- purrr::map(fits, brms::add_criterion, "loo")
  loo_tab  <- if (length(fits_loo) > 1)
    loo::loo_compare(purrr::map(fits_loo, \(f) f$criteria$loo)) else NULL

  prior_post <- if (length(fits_prior))
    purrr::map2(fits[names(fits_prior)], fits_prior, prior_post_draws) else NULL

  ce      <- brms::conditional_effects(fit_mono, categorical = TRUE)
  ce_plot <- plot(ce, plot = FALSE)[[1]] +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x, y = paste0("P(", y, " = k)")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  structure(list(
    x = x, y = y, n = nrow(dat),
    K_x = nlevels(dat[[x]]), K_y = nlevels(dat[[y]]),
    data = dat,
    fits = fits_loo, fits_prior = fits_prior,
    prior_post = prior_post,
    prior_pc = purrr::imap(fits_prior, \(f, nm)
      brms::pp_check(f, type = "bars", ndraws = 200) +
        ggplot2::ggtitle(paste0("Prior predictive: ", nm))),
    post_pc = purrr::imap(fits, \(f, nm)
      brms::pp_check(f, type = "bars", ndraws = 200) +
        ggplot2::ggtitle(paste0("Posterior predictive: ", nm))),
    loo = loo_tab,
    bf_slope = bf_slope,
    slope_summary = brms::fixef(fit_mono)[slope_row, , drop = FALSE],
    ce = ce, ce_plot = ce_plot
  ), class = "likert_pair")
}


print.likert_pair <- function(z, ...) {
  cat(sprintf("\n%s ~ %s   (n = %d, K_x = %d, K_y = %d)\n",
              z$y, z$x, z$n, z$K_x, z$K_y))

  # slope: one line
  s <- z$slope_summary[1, ]
  cat(sprintf("slope   b = %.2f [%.2f, %.2f]\n",
              s[["Estimate"]], s[["Q2.5"]], s[["Q97.5"]]))

  # Bayes factor: one line, no footnote
  logbf <- as.data.frame(z$bf_slope)$log_BF[1]
  cat(sprintf("BF10    %s\n",
              if (logbf > log(100)) "> 100"
              else if (logbf < log(1/100)) "< 0.01"
              else sprintf("%.1f", exp(logbf))))

  # LOO: just the ranking
  l <- as.matrix(z$loo)[, c("elpd_diff", "se_diff"), drop = FALSE]
  cat("\nmodel comparison (best first):\n")
  print(round(l, 1))

  cat("\n")
  invisible(z)
}

dat_q1 <- survey %>%
  filter(!gen_attitude %in% c("Other", "Indifferent"))

res_1 <- fit_likert_pair(dat_q1, x = "career_stage", y = "gen_attitude",
                       file_prefix = "fits/attitude_by_stage", overwrite = TRUE)

# example can reload without fitting if you want to not rerun
res_1  <-load_likert_pair(x = "career_stage", y = "gen_attitude", 
file_prefix = "fits/attitude_by_stage")

#examine output
res_1$loo
res_1$ce_plot
summary (res_1$fits$mono)
plot_prior_post(res_1$prior_post$mono)
res_1$prior_pc$mono


dat_q2 <- survey %>%
  filter(grepl("Early", career_stage) &
    !is.na(ai_use_freq)&
    !is.na(ds_freq)
  )

res_2 <- fit_likert_pair(dat_q2, x = "ds_freq", y = "ai_use_freq",
                       file_prefix = "fits/early_career_dsfreq_by_ai_usefreq",
                        overwrite = TRUE)

res_2 <-load_likert_pair(x = "ds_freq", y = "ai_use_freq", 
file_prefix = "fits/early_career_dsfreq_by_ai_usefreq")
res_2$loo
res_2$ce_plot
summary (res_2$fits$mono)

#n = 140
dat_q3<- survey %>%
  filter(grepl("Early", career_stage) &
    !is.na(policies) &
         !policies %in% c("Other", "There are not any policies or guidelines at my institution")&
    !is.na(ai_use_freq)
)

res_3 <- fit_likert_pair(dat_q3, x = "policies", y = "ai_use_freq",
                       file_prefix = "fits/early_career_policies_by_ai_usefreq",
                        overwrite = TRUE)

res_3$loo
res_3$ce_plot
summary (res_3$fits$mono)


dat_q4 <- survey %>%
  filter(grepl("Early", career_stage) &
  !is.na(career_stage),
         !gen_attitude %in% c("Other", "Indifferent"),
        !is.na(gender) & gender != "Prefer not to answer") %>%
  droplevels()

# no effects of gender among early c
res_4 <- ordinal::clm(gen_attitude ~ gender, data = dat_q4, link = "probit")
# overall test of the predictor
drop1(m, test = "Chisq")

# Assumption checks
# chsq nonsig so keep the simpler test
res_4_nom  <- ordinal::clm(gen_attitude ~ 1, nominal = ~ gender, data = dat_q4, link = "probit")
anova(res_4, res_4_nom)   

ordinal::scale_test(res_4)     # unequal variance: does the latent SD differ by stage?

# stopped here 25 Sept 2026

