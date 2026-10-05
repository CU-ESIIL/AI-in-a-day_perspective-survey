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
# TO DO 
# deal with the inconsistencies in the re.formula implementation 
# across brms vs emm in the predictions
# https://github.com/easystats/modelbased/issues/579
rm(list = ls())
source(file.path("-setup.r"))          # clears environment, creates folders
gc()                  # start with a clean workspace

# these are slow to run so can run once if fiddling with plots
rerun_mods <-TRUE

# set for fast or slow runs
n.iter = 6000
n.adapt_delta = 0.99

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
  ordinal,
  tidyverse
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
# Define functions for models with
# Likert predictor (x) and Likert outcome (y)
# Baysian monotonic effects with ordinal outcomes
# Similar to Rasch models
# Cumulative probit with a monotonic predictor 
# Dirichlet prior on the simplex (spacing) parameter; weakly-informative
# Normal(0,1) prior on the (probit-scale) monotonic coefficient b.
# 
# reference paper is here: Burkner and Charpentier 2020
# https://doi.org/10.1111/bmsp.12195
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

diagnose_fit <- function(fit, tag, bulk_min = 400, tail_min = 400, rhat_max = 1.01) {
  s <- posterior::summarise_draws(
    posterior::as_draws(fit),
    "rhat", "ess_bulk", "ess_tail")
  
  np <- brms::nuts_params(fit)
  div <- sum(np$Value[np$Parameter == "divergent__"])
  max_td <- fit$fit@stan_args[[1]]$control$max_treedepth

if (is.null(max_td)) {
  max_td <- 10
}

tree <- sum(
  np$Value[np$Parameter == "treedepth__"] >= max_td
)


  # tree <- sum(np$Value[np$Parameter == "treedepth__"] >=
  #               (fit$fit@stan_args[[1]]$control$max_treedepth %||% 10))
  
  bad <- s |>
    dplyr::filter(rhat > rhat_max | ess_bulk < bulk_min | ess_tail < tail_min) |>
    dplyr::arrange(ess_tail)
  
  tibble::tibble(
    model      = tag,
    divergent  = div,
    max_rhat   = max(s$rhat, na.rm = TRUE),
    min_ess_b  = min(s$ess_bulk, na.rm = TRUE),
    min_ess_t  = min(s$ess_tail, na.rm = TRUE),
    n_flagged  = nrow(bad),
    worst      = if (nrow(bad)) bad$variable[1] else NA_character_
  )
}

# example use
# pp <- prior_post_draws(fit_mono, fit_prior_mono)
# plot_prior_post(pp)

fit_likert_pair <- function(data, x, y,
                            b_sd_fac    = 1,
                            disc_sd     = 0.25,
                            run_ls      = TRUE,
                            adapt_delta = 0.99,
                            iter        = 2000,
                            warmup      = NULL,   # defaults to iter/2
                            chains      = 4,
                            cores = 4, seed = 1,
                            cache_dir   = NULL,
                            file_prefix = NULL,
                            overwrite   = FALSE,
                            quiet = FALSE) {
  
  stopifnot(is.character(x), is.character(y), length(x) == 1, length(y) == 1)
  if (!all(c(x, y) %in% names(data)))
    stop("Columns not found in data: ",
         paste(setdiff(c(x, y), names(data)), collapse = ", "))
  
  if (is.null(warmup)) warmup <- floor(iter / 2)
  stopifnot(warmup < iter)

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
  #b_sd_mono <- b_sd_fac / (K_x - 1)     # b is the per-step effect
  b_sd_mono <- b_sd_fac # b is the per-step effect

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
              iter = iter, warmup = warmup, chains = chains,
              control = ctrl, file = fp(tag))
  }

  fit_mono <- run(f_mono, dat,     priors_mono, "mono")
  fit_fac  <- run(f_fac,  dat_fac, priors_fac,  "fac")
  fit_ls   <- if (run_ls) run(f_ls, dat, priors_ls, "ls") else NULL

  prior_only <- function(fit, tag) {
    if (is.null(fit)) return(NULL)
    if (!quiet) message("Fitting ", tag, " (prior only) ...")
    stats::update(fit, sample_prior = "only", seed = seed,
                  cores = cores, iter = iter, warmup = warmup, chains = chains,
                  file = fp(paste0(tag, "_prior")))
  }

  prior_mono <- prior_only(fit_mono, "mono")
  prior_fac  <- prior_only(fit_fac,  "fac")
  prior_ls   <- prior_only(fit_ls,   "ls")

  # ---- checks, comparison, BF -----------------------------------------
  fits       <- purrr::compact(list(mono = fit_mono, fac = fit_fac, ls = fit_ls))
  fits_prior <- purrr::compact(list(mono = prior_mono, fac = prior_fac, ls = prior_ls))
  
  diag_tab <- purrr::imap_dfr(fits, diagnose_fit)
  if (!quiet) {
    print(diag_tab)
    bad <- dplyr::filter(diag_tab, n_flagged > 0 | divergent > 0)
    if (nrow(bad))
      warning("Diagnostic issues in: ", paste(bad$model, collapse = ", "),
                     ". See $diagnostics.", call. = FALSE)
    }

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
    #parameters = paste0("^", gsub("([\\W])", "\\\\\\1", slope_draw, perl = TRUE), "$")
    parameters = slope_draw #)
  )

  #ce <- brms::conditional_effects(fit_mono, categorical = TRUE)
  ce <- brms::conditional_effects(fit_mono, categorical = TRUE, re_formula = NULL)
  
  ce_plot <- plot(ce, plot = FALSE)[[1]] +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x, y = paste0("P(", y, " = k)")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  structure(list(
    x = x, y = y, n = nrow(dat), K_x = K_x, K_y = K_y,
    sampling = list(iter = iter, warmup = warmup, chains = chains,
                    adapt_delta = adapt_delta, seed = seed),
    diagnostics = diag_tab,
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

  ce      <- brms::conditional_effects(fit_mono, categorical = TRUE, re_formula = NULL)
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

# define function for summaries
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

# define function for plots
make.likert.plot <-function(fit,
                            palette   = NULL,   # named vector, or a function of n
                            pal_option = "D",   # viridis option if palette is NULL
                            pal_end    = 0.9,
                            wrap_axis  = 20,
                            dodge      = 0.8){
  
  yvar <- all.vars(fit$fits$mono$formula$formula)[1]
  xvar <- all.vars(fit$fits$mono$formula$formula)[2] 
  
  raw <- fit$data %>%
    dplyr::count(.data[[xvar]], .data[[yvar]], .drop = FALSE) %>%
    group_by(.data[[xvar]]) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    rename(effect1__ = !!xvar, effect2__ = !!yvar)
  
  lvl <- levels(raw$effect2__)
  pal <- if (is.null(palette)) {
    scales::viridis_pal(option = pal_option, end = pal_end)(length(lvl))
  } else if (is.function(palette)) {
    palette(length(lvl))
  } else {
    palette                       # vector user supplied
  }
  
  # name by level so colours can't drift if level order differs between
  # the raw counts and the conditional-effects frame
  if (is.null(names(pal))) names(pal) <- lvl
  if (!all(lvl %in% names(pal)))
    stop("palette is missing colours for: ",
              paste(setdiff(lvl, names(pal)), collapse = ", "))
  
  tot <- raw %>%
    group_by(effect1__) %>%
    summarise(n = sum(n), top = max(prop), .groups = "drop")
  
  res_plot <- ggplot() +
    geom_col(data = raw,
             aes(effect1__, prop, fill = effect2__),
             position = position_dodge(width = dodge), width = .7,
             alpha = .35, colour = NA) +
    geom_text(data = tot,
              aes(effect1__, top, label = paste0("n = ", n)),
              vjust = -0.8, size = 3, colour = "grey30")+
    geom_linerange(data = as.data.frame(fit$ce[[1]]),
                   aes(effect1__, estimate__, ymin = lower__, ymax = upper__,
                       color = effect2__),
                   position = position_dodge(width = dodge), linewidth = .3) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = pal) +
    scale_colour_manual(values = pal) +
    labs(x = fit$x, y = paste0("P(", fit$y, " = k)"),
         fill = fit$y, colour = fit$y) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_x_discrete(labels = scales::label_wrap(20))
  return (res_plot)
}


# -------------------------------------------------
# Run Likert Models
# -------------------------------------------------
# gen attitude as a function of career stage
dat_q1 <- survey %>%
  filter(!gen_attitude %in% c("Other", "Indifferent"))

if (rerun_mods) {
res_1 <- fit_likert_pair(dat_q1, x = "career_stage", y = "gen_attitude",
                       file_prefix = "fits/attitude_by_stage", overwrite = TRUE,
                       iter = n.iter,
                       adapt_delta = n.adapt_delta)
} else {
# example can reload without fitting if you want to not rerun
  res_1  <-load_likert_pair(x = "career_stage", y = "gen_attitude", 
  file_prefix = "fits/attitude_by_stage")
}

res_1_plot <-make.likert.plot(res_1)

ggsave(file.path("graphs","gen_attitude_by_career.jpg"), plot = res_1_plot, 
       width =8, height =6)

#examine output
# top is the best fit model, here is mono
res_1$loo

# see just the effects
res_1$ce_plot

summary (res_1$fits$mono)
# check posteriors overplotted on priors
plot_prior_post(res_1$prior_post$mono)

# yrep plots
res_1$prior_pc$mono

res_1$diagnostics

########################
# sanity check based on the Kurtz blog that you have the reference value right
# otherwise non-identifiable if it's also estimating a b_disc_Intercept
# https://solomonkurz.netlify.app/blog/2021-12-29-notes-on-the-bayesian-cumulative-probit/
# grep("disc", variables(res_1$fits$ls), value = TRUE)

######################################################################
# AI use frequency as a function of data science frequency

# first ask - do we have even ds splits across career stages?
# let's look at counts to see what we have big enough samples to work with

dat_q2 <- survey %>%
  filter(!is.na(career_stage) &
           !is.na(ai_use_freq)&
           !is.na(ds_freq)
  )

tab_ds_freq_career <- dat_q2 %>%
  droplevels() %>%
  group_by(ds_freq, career_stage, .drop = FALSE) %>%
  dplyr::tally()%>%
  tidyr::pivot_wider(names_from = career_stage, values_from = n) %>%
  tibble::column_to_rownames("ds_freq") %>%
  as.matrix()

tab_ds_freq_career

# run polynomial contrasts first, finding none combined across career stages
# with none, combine
res_2_poly <- clm(ai_use_freq ~ ds_freq*career_stage, data = dat_q2,
             link = "probit")

res_2_cat <-clm(ai_use_freq ~ as.factor(ds_freq)*as.factor(career_stage), data = dat_q2,
             link = "probit")

# categorical and ordinal are not treated differently in clm
anova (res_2_poly, res_2_cat)

summary (res_2_cat)
# marginal here
anova(clm(ai_use_freq ~ ds_freq + career_stage, data = dat_q2 , link = "probit"), res_2_cat)


if (rerun_mods) {
res_2 <- fit_likert_pair(dat_q2, x = "ds_freq", y = "ai_use_freq",
                       file_prefix = "fits/dsfreq_by_ai_usefreq",
                       iter = n.iter,
                       adapt_delta = n.adapt_delta,
                        overwrite = TRUE)
} else{ 
res_2 <-load_likert_pair(x = "ds_freq", y = "ai_use_freq", 
file_prefix = "fits/dsfreq_by_ai_usefreq")
}

# if all within ~2 then report the simplest which is mono
res_2$loo
# see just the effects
res_2$ce_plot

summary (res_2$fits$mono)
# check posteriors overplotted on priors
plot_prior_post(res_2$prior_post$mono)

# yrep plots
res_2$prior_pc$mono

res_2$diagnostics

#significant interaction but with some noise
# in general people who do ds tasks more often use
# AI more freq
res_2_plot <-make.likert.plot(res_2, pal_option = "C")

# if want to manually scale colors we can
# make.likert.plot(res_3, palette = c(
#"Daily" = "#4575b4", "Weekly" = "#91bfdb", "Monthly" = "#ffffbf",
#"Yearly" = "#fc8d59", "Never"  = "#d73027"))

ggsave(plot = res_2_plot, filename = file.path("graphs",
                                        "analysis_figs",
                                        "dsfreq_by_ai_usefreq.jpg"),
       width =8, height =6)



# AI use frequency as a function of Institutional Policy
dat_q3<- survey %>%
    dplyr::filter(!is.na(policies) &
         !policies %in% c("Other", "There are not any policies or guidelines at my institution")&
    !is.na(ai_use_freq)
)

if (rerun_mods) {
res_3 <- fit_likert_pair(dat_q3, x = "policies", y = "ai_use_freq",
                       file_prefix = "fits/policies_by_ai_usefreq",
                       iter = n.iter,
                       adapt_delta = n.adapt_delta,
                       overwrite = TRUE)
} else {
res_3 <-load_likert_pair(x = "policies", y = "ai_use_freq", 
                         file_prefix = "fits/policies_by_ai_usefreq")
}
# if all within ~2 then report the simplest which is mono
res_3$loo
# see just the effects
res_3$ce_plot

summary (res_3$fits$mono)
# check posteriors overplotted on priors
plot_prior_post(res_3$prior_post$mono)

# yrep plots
res_3$prior_pc$mono

res_3$diagnostics

#significant interaction but with some noise
# in general people who do ds tasks more often use
# AI more freq
res_3_plot <-make.likert.plot(res_3, pal_option = "A")

# if want to manually scale colors we can
# make.likert.plot(res_3, palette = c(
#"Daily" = "#4575b4", "Weekly" = "#91bfdb", "Monthly" = "#ffffbf",
#"Yearly" = "#fc8d59", "Never"  = "#d73027"))


ggsave(plot = res_3_plot, filename =
         file.path("graphs","analysis_figs", "policies_by_ai_usefreq.jpg"),
       width =8, height =6)


# -------------------------------------------------
# Define functions for models with select all (Y)
# Questions as a function of categorical x
# Tried using monotonic x model but ran forever..
# -------------------------------------------------

# -------------------------------------------------
# expand_select_all(): select-all-that-apply -> long
#   one row per respondent x option, with Yes/No filled in
# note if there are NO answers, it isn't expected they filled
# "no" for all. I think this is right bc there was always a 
# "none of the above" answer but we should confirm
# -------------------------------------------------
expand_select_all <- function(survey, q, id_col = "response_id",
                              keep_na_x = FALSE, x = NULL) {
  
  svy <- survey %>% rename(ResponseId = all_of(id_col))
  
  # the chosen options, one row per respondent x selected option
  yes <- prep_select_all(svy, q = q, summarize = FALSE) %>%
    rename(!!q := value) %>%
    left_join(svy %>% select(-all_of(q)), by = "ResponseId") %>%
    mutate(response = "Yes")
  
  if (nrow(yes) == 0) stop("prep_select_all() returned no rows for q = ", q)
  
  # every respondent x option combination that wasn't chosen
  no <- tidyr::expand_grid(
    ResponseId = unique(yes$ResponseId),
    !!q := unique(yes[[q]])
  ) %>%
    anti_join(yes, by = c("ResponseId", q)) %>%
    mutate(response = "No") %>%
    left_join(svy %>% select(-all_of(q)), by = "ResponseId")
  
  out <- bind_rows(yes, no) %>%
    mutate(question_label   = as.character(.data[[q]]),   # original, with commas
      question         = factor(make.names(as.character(.data[[q]]))),
           response_numeric = as.integer(response == "Yes"))
  
  if (!is.null(x) && !keep_na_x)
    out <- out %>% filter(!is.na(.data[[x]])) %>% droplevels()
  
  out
}

# -------------------------------------------------
# fit_select_all(): GLMM + summaries + plot
# -------------------------------------------------
fit_select_all <- function(data, x, id = "ResponseId",
                           wrap_facet = 25, wrap_axis = 20,
                           order_facets = TRUE,
                           add_letters = TRUE,
                           # letter_adjust = "tukey",
                           letter_adjust = "mvt",
                           seed_mvt = 12345,
                           verbose = TRUE) {
  
  # fit_select_all(challenges_long_attitude,
  #                                         x = "gen_attitude",
  #                                          verbose = TRUE)
  
  
  #fit_select_all(opps_long_career,
  #               x = "career_stage", verbose = TRUE)
  
  
  stopifnot(all(c("question", "response_numeric", x, id) %in% names(data)))
  data <- data %>% filter(!is.na(.data[[x]])) %>% droplevels()
  
  # sanity check before fitting
  # only 16 in the very enthusiastic category which may have some issues
  tab <- table(data[[x]], data$question)
  if (any(tab == 0))
    warning("Empty cells in ", x, " x question; estimates may be unstable.")
  
  form <- stats::as.formula(
    sprintf("response_numeric ~ 0 + question + question:%s + (1 | %s)", x, id))
  
  fit <- lme4::glmer(form, family = binomial, data = data,
                     control = lme4::glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 2e5)))
  #bu<-fit
  if (verbose) {
    print(summary(fit))
    m_red <- update(fit, stats::as.formula(paste(". ~ . - question:", x)))
    print(anova(m_red, fit))
    print(lme4::VarCorr(fit))
    cat("singular:", lme4::isSingular(fit), "\n")
    print(performance::icc(fit))
  }
  
  # predictions, comparable to the observed proportions.
  # re.form = NULL includes all ranefs; predictions conditional on the subjects included
  # will match the actual proportions in the data
  # but is not population estimates assuming random sampling. If we want that we need to use
  # re.form = NA. This is a bit tricky to explain in the methods.
  # emmeans' default (random effects = 0) estimates will be shrunk relative to the raw proportions
   emm <- as.data.frame(
      marginaleffects::avg_predictions(
        fit, by = c("question", x), re.form = NULL)) %>%
        dplyr::rename(prob = estimate, asymp.LCL = conf.low, asymp.UCL = conf.high)
   
     # ---- compact letter display per question -------------------------------
     # Tukey-adjusted pairwise contrasts WITHIN each question; groups sharing
       # a letter do not differ. No adjustment across questions — each item is
       # treated as its own family (state this in the methods).
   # does not work it will ondly do slidak
       #cld_df <- NULL
     # if (add_letters) {
     #     emm_link <- emmeans::emmeans(
     #         fit, stats::as.formula(paste("~", x, "| question")))   # link scale
     # 
     #       cld_df <- try(
     #           multcomp::cld(emm_link, adjust = letter_adjust, Letters = letters) |>
     #               as.data.frame() |>
     #               dplyr::mutate(.group = trimws(.group)),
     #           silent = TRUE)
     #   
     #         if (inherits(cld_df, "try-error")) {
     #             warning("cld() failed; skipping letters. ", attr(cld_df, "condition")$message)
     #             cld_df <- NULL
     #           } else {
     #               # blank the letters where nothing differs, so panels stay uncluttered
     #                 cld_df <- cld_df |>
     #                     dplyr::group_by(question) |>
     #                     dplyr::mutate(.group = if (dplyr::n_distinct(.group) == 1) "" else .group) |>
     #                     dplyr::ungroup()
     #               }
     #   }
   
   cld_df <- NULL
   if (add_letters) {
     emm_link <- emmeans::emmeans(
       fit, stats::as.formula(paste("~", x, "| question")),
       nesting = NULL)                      # treat x and question as crossed
     
     set.seed(seed_mvt)                     # mvt is simulation-based
     cld_df <- try(
       multcomp::cld(emm_link, adjust = letter_adjust, Letters = letters,
                     alpha = 0.05, 
                     sort = FALSE),
       silent = TRUE)
     # verify it's the subset of tests
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
         # at the moment it's blanking all panels with no diffs
         dplyr::mutate(.group = if (dplyr::n_distinct(.group) == 1) "" else .group) |>
         dplyr::ungroup()
       
       #reorder
       cld_df[[x]] <- factor(as.character(cld_df[[x]]),
                             levels = levels(data[[x]]),
                             ordered = TRUE)
     }
   }
  
  raw <- data %>%
    group_by(question, .data[[x]]) %>%
    summarise(prop = mean(response_numeric), n = n(), .groups = "drop")
  
  if (!is.null(cld_df)){
    # raw <- raw %>%
    # dplyr::left_join(cld_df %>% dplyr::select(question, dplyr::all_of(x), .group),
    #                                                by = c("question", x))
    raw <- raw %>%
      dplyr::left_join(., cld_df,
                       by = c("question", x))
  }
  
  
  # ---- facet order: highest overall P(yes) first -------------------------
  ord <- data %>%
    group_by(question) %>%
    summarise(overall = mean(response_numeric), .groups = "drop") %>%
    arrange(desc(overall))
  
   # ---- facet labels: original text, line-wrapped -------------------------
  lab_map <- data %>%
    distinct(question, question_label) %>%
    tibble::deframe() # named vector: make.names -> original
 
    if (order_facets) {
      lv <- as.character(ord$question)
      emm$question <- factor(as.character(emm$question), levels = lv)
      raw$question <- factor(as.character(raw$question), levels = lv)
      }
  
  
  p <- ggplot(emm, aes(.data[[x]], prob)) +
    geom_col(data = raw, aes(y = prop), fill = "grey85", width = .7) +
    geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                    colour = "firebrick", size = .3) +
    geom_text(data = raw, aes(y = Inf, label = n),
              vjust = 1.4, size = 2.6, colour = "grey40") +
    # facet_wrap(~ question, labeller = label_wrap_gen(wrap_facet)) +facet_wrap(~ question,
    facet_wrap(~ question,
              labeller = labeller(question = function(z)
              scales::label_wrap(wrap_facet)(lab_map[z]))) +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = c(.05, .12))) +
    scale_x_discrete(labels = scales::label_wrap(wrap_axis)) +
    labs(x = NULL, y = "P(yes)",
         caption = "grey = observed; red = model estimate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (!is.null(cld_df))
    
  p <- p + geom_text(data = raw,
            aes(y = Inf, label = .group),
            vjust = 2.8, size = 2.4, colour = "grey20", fontface = "bold")
  list(x = x, n_obs = nrow(data), n_id = dplyr::n_distinct(data[[id]]),
       data = data, fit = fit, emm = emm, raw = raw, plot = p,
       order = ord, labels = lab_map,
       cells = tab)
}
# Q1B: How do the opportunities of using genAI vary with career stage?
# SCE comment - not sure whether to leave 'Other' in here or drop it
# "None (No opportunities associated with using GenAI for research and learning)"

# some people say "no promising opps" and 'Other'
 #View (opps_long_career %>% filter(ResponseId == "R_5MiK8tBzS2UfsNy") %>% select(ResponseId, promising_opps, response,
#promising_opps_14_text))
# "R_5MiK8tBzS2UfsNy"

#View (noopps %>% filter(ResponseId == "R_5MiK8tBzS2UfsNy"))

opps_long_career <- expand_select_all(survey, q = "promising_opps",
                                      x = "career_stage")

# fin
noopps <- opps_long_career %>%
  dplyr::filter(grepl("None", promising_opps)&
                  response == 'Yes')

opps_long_career <- opps_long_career %>%
  dplyr::filter(!grepl("None", promising_opps))

noopps_indivs <- opps_long_career %>%
  filter(promising_opps_14_text %in% c(
    "Currently I feel none, but I would be hesitantly open to being convinced of a specific benefit of AI",
    "There is no ethical way to use generative AI given its current ownership model",
    "Having said all these nasty things about AI and about the amoral engineers who are leading us down the garden path, I will admit that several of my colleagues, who have large data sets, have used several programs, and have clearly drunk the coolaid."
  )
  ) %>% dplyr::select(ResponseId) %>%
  distinct() %>%
  dplyr::pull(ResponseId)

some_opps_indivs <- opps_long_career %>%
  filter(promising_opps_14_text %in% c(
    "faster work (does not equal quality work)."  
  ))%>% dplyr::select(ResponseId) %>%
  distinct() %>%
  dplyr::pull(ResponseId)

opps_long_career <- opps_long_career %>%
  mutate(response = 
           ifelse (promising_opps == 'Other' & ResponseId %in% noopps_indivs, 'No', response),
         response_numeric = 
           ifelse (promising_opps == 'Other' & ResponseId %in% noopps_indivs, 0, response_numeric)
)

noopps <- noopps %>%
  filter(!ResponseId %in% some_opps_indivs)
      
yesIDs <- opps_long_career %>% filter(response == "Yes") %>%
  dplyr::pull(ResponseId)

conflicts <- unique(noopps$ResponseId[noopps$ResponseId %in%
                                        yesIDs])

# this person picked 2 and also 'no opportunities'; drop
# View (opps_long_career %>% filter(ResponseId %in% (conflicts))%>%
#         arrange(ResponseId, promising_opps, promising_opps_14_text, response) %>%
#         select(ResponseId, promising_opps, promising_opps_14_text, response)
# )

if(length(conflicts)>0){
  cat ("yes and no values conflict for ", length(conflicts))
  opps_long_career <-opps_long_career %>%
    dplyr::filter(!ResponseId %in% conflicts)
}

# can keep this if we want to report on how many people actually said no opps
#   noopps <- opps_long_career %>%
#     dplyr::filter(grepl("None", promising_opps))%>%
#     dplyr::filter(!ResponseId %in% conflicts)
# } else {
#   noopps <- opps_long_career %>%
#     dplyr::filter(grepl("None", promising_opps))
# }

# # check for any conflicts whether ALL they selected was "Other"
# check_other <- opps_long_career %>%
#   dplyr::filter(ResponseId %in% conflicts) %>%
#   dplyr::group_by(ResponseId) %>%
#   dplyr::summarize(
#     ct_yes   = sum(response_numeric),
#     ct_other = sum(response_numeric[.data[['promising_opps']] == "Other"]),
#     .groups  = "drop"
#   ) %>%
#   left_join(., opps_long_career) %>%
#   arrange(ResponseId, promising_opps, promising_opps_14_text, response) %>%
#   select(ResponseId, promising_opps, promising_opps_14_text, response)
# 
# # sanity check what the 'no opps and other' said
# unique (check_other$promising_opps_14_text)
# 
# noopps_indivs <- opps_long_career %>%
#   filter(promising_opps_14_text %in% c(
#     "Currently I feel none, but I would be hesitantly open to being convinced of a specific benefit of AI",
#     "There is no ethical way to use generative AI given its current ownership model",
#     "Having said all these nasty things about AI and about the amoral engineers who are leading us down the garden path, I will admit that several of my colleagues, who have large data sets, have used several programs, and have clearly drunk the coolaid."
#   )
#   ) %>% dplyr::select(ResponseId) %>%
#   distinct() %>%
#   dplyr::pull(ResponseId)
# 
# opps_long_career <-opps_long_career %>%
#   mutate(response = ifelse(ResponseId %in% (noopps_indivs), 'No', response),
#          response_numeric = ifelse(ResponseID %in% (noopps_indivs), 'No', response_numeric))
# 
# some_opps_indivs <- opps_long_career %>%
#   filter(promising_opps_14_text %in% c(
#     "faster work (does not equal quality work)."  
#   ))%>% dplyr::select(ResponseId) %>%
#     distinct() %>%
#     dplyr::pull(ResponseId)


if (rerun_mods) {
res_opps_career  <- fit_select_all(opps_long_career,
                                   x = "career_stage", verbose = TRUE)
saveRDS(res_opps_career, file.path('fits', 'res_opps_career.rds'))
} else{
  res_opps_career <-readRDS(file.path('fits', 'res_opps_career.rds'))
}

res_opps_career$plot
ggsave(plot = res_opps_career$plot, filename =
         file.path("graphs","analysis_figs", "opportunity_by_career.jpg"),
       width =15, height =14, scale =0.8)


# Q1A: How do the challenges of using genAI vary with career stage?

challenges_long_career <- expand_select_all(survey, q = "challenges",
                                            x = "career_stage")

# find those with no challenges (3)
nochall_indivs <- challenges_long_career %>%
  dplyr::filter(grepl("^No", challenges)&
                  response == 'Yes')

challenges_long_career <- challenges_long_career %>%
  dplyr::filter(!grepl("^No", challenges))

yesIDs <- challenges_long_career %>% filter(response == "Yes") %>%
  dplyr::pull(ResponseId)

conflicts <- unique(nochall_indivs$ResponseId[nochall_indivs$ResponseId %in%
                                        yesIDs])

# View (challenges_long_career %>% filter(ResponseId %in% conflicts) %>%
#   arrange(ResponseId) %>%
#   select(ResponseId, challenges, response_numeric, challenges_15_text))

# none for this one
if(length(conflicts)>0){
  cat ("yes and no values conflict for ", length(conflicts))
  challenges_long_career <- challenges_long_career %>%
    dplyr::filter(!ResponseId %in% conflicts)
}

# as nec can reassign
# View (challenges_long_career %>%
#         filter(challenges == 'Other') %>%
#         select(challenges_15_text))


if (rerun_mods){
res_challenges_career  <- fit_select_all(challenges_long_career,
                                         x = "career_stage")
saveRDS(res_challenges_career, file.path('fits', 'res_challenges_career.rds'))
} else{
  res_challenges_career <-readRDS(file.path('fits', 'res_challenges_career.rds'))
}

res_challenges_career$plot

ggsave(plot = res_challenges_career$plot, filename =
         file.path("graphs","analysis_figs", "challenges_by_career.jpg"),
       width =15, height =14, scale =0.8)


# Q: how do opportunities vary with attitude

opps_long_attitude <- expand_select_all(survey %>%
  filter(!gen_attitude %in% c("Other", "Indifferent")), q = "promising_opps", x = "gen_attitude")

# find conflicts
noopps <- opps_long_attitude %>%
  dplyr::filter(grepl("None", promising_opps)&
                  response == 'Yes')

opps_long_attitude <- opps_long_attitude %>%
  dplyr::filter(!grepl("None", promising_opps))

#reassign a few based on 'other' response
noopps_indivs <- opps_long_attitude %>%
  filter(promising_opps_14_text %in% c(
    "Currently I feel none, but I would be hesitantly open to being convinced of a specific benefit of AI",
    "There is no ethical way to use generative AI given its current ownership model",
    "Having said all these nasty things about AI and about the amoral engineers who are leading us down the garden path, I will admit that several of my colleagues, who have large data sets, have used several programs, and have clearly drunk the coolaid."
  )
  ) %>% dplyr::select(ResponseId) %>%
  distinct() %>%
  dplyr::pull(ResponseId)

some_opps_indivs <- opps_long_attitude %>%
  filter(promising_opps_14_text %in% c(
    "faster work (does not equal quality work)."  
  ))%>% dplyr::select(ResponseId) %>%
  distinct() %>%
  dplyr::pull(ResponseId)

opps_long_attitude  <- opps_long_attitude %>%
  mutate(response = 
           ifelse (promising_opps == 'Other' & ResponseId %in% noopps_indivs, 'No', response),
         response_numeric = 
           ifelse (promising_opps == 'Other' & ResponseId %in% noopps_indivs, 0, response_numeric)
)

noopps <- noopps %>%
  filter(!ResponseId %in% some_opps_indivs)
      
yesIDs <- opps_long_attitude %>% filter(response == "Yes") %>%
  dplyr::pull(ResponseId)

conflicts <- unique(noopps$ResponseId[noopps$ResponseId %in%
                                        yesIDs])

# this person picked 2 and also 'no opportunities'; drop
# View (opps_long_attitude %>% filter(ResponseId %in% (conflicts))%>%
#         arrange(ResponseId, promising_opps, promising_opps_14_text, response) %>%
#         select(ResponseId, promising_opps, promising_opps_14_text, response)
# )

if(length(conflicts)>0){
  cat ("yes and no values conflict for ", length(conflicts))
  opps_long_attitude <-opps_long_attitude %>%
    dplyr::filter(!ResponseId %in% conflicts)
}

# can keep this if we want to report on how many people actually said no opps
#   noopps <- opps_long_attitude %>%
#     dplyr::filter(grepl("None", promising_opps))%>%
#     dplyr::filter(!ResponseId %in% conflicts)
# } else {
#   noopps <- opps_long_attitude %>%
#     dplyr::filter(grepl("None", promising_opps))
# }


if (rerun_mods){
res_opps_attitude <- fit_select_all(opps_long_attitude, x = "gen_attitude",
                                          verbose = TRUE)
saveRDS(res_opps_attitude, file.path('fits', 'res_opps_attitude.rds'))
}else {
  res_opps_attitude <-readRDS(file.path('fits', 'res_opps_attitude.rds'))
}
res_opps_attitude$plot
ggsave(plot = res_opps_attitude$plot, filename =
         file.path("graphs","analysis_figs", "opportunity_by_attitude.jpg"),
       width =15, height =14, scale =0.8)


# how do challenges vary with attitude
# Q: How do the challenges of using genAI vary with attitude?
challenges_long_attitude <- expand_select_all(survey %>%
  filter(!gen_attitude %in% c("Other", "Indifferent")) , q = "challenges",
                                              x = "gen_attitude")

# find those with no challenges (3)
nochall_indivs <- challenges_long_attitude %>%
  dplyr::filter(grepl("^No", challenges)&
                  response == 'Yes')

challenges_long_attitude <- challenges_long_attitude %>%
  dplyr::filter(!grepl("^No", challenges))

yesIDs <- challenges_long_attitude %>% filter(response == "Yes") %>%
  dplyr::pull(ResponseId)

conflicts <- unique(nochall_indivs$ResponseId[nochall_indivs$ResponseId %in%
                                        yesIDs])

# View (challenges_long_attitude %>% filter(ResponseId %in% conflicts) %>%
#   arrange(ResponseId) %>%
#   select(ResponseId, challenges, response_numeric, challenges_15_text))

# none for this one
if(length(conflicts)>0){
  cat ("yes and no values conflict for ", length(conflicts))
  challenges_long_attitude <- challenges_long_attitude %>%
    dplyr::filter(!ResponseId %in% conflicts)
}

if (rerun_mods){
res_challenges_attitude <- fit_select_all(challenges_long_attitude,
                                          x = "gen_attitude",
                                           verbose = TRUE)
saveRDS(res_challenges_attitude, file.path('fits', 'res_challenges_attitude'))
} else {
  res_challenges_attitude <-readRDS(file.path('fits', 'res_challenges_attitude'))
}
res_challenges_attitude$plot
ggsave(plot = res_challenges_attitude$plot, filename =
         file.path("graphs","analysis_figs", "challenges_by_attitude.jpg"),
       width =15, height =14, scale =0.8)

# AI use frequency as a function of gender
# first ask - do we have even gender splits across career stages?
# let's look at counts to see what we have big enough samples to work with

tab_gend_career <- survey %>%
  dplyr::filter(!gender %in% c("Prefer not to answer", "Prefer to self-identify"),
                     !is.na(gender), !is.na(career_stage), !is.na(ai_use_freq)) %>%
              droplevels() %>%
  group_by(gender, career_stage, .drop = FALSE) %>%
  dplyr::tally()%>%
  tidyr::pivot_wider(names_from = career_stage, values_from = n) %>%
  tibble::column_to_rownames("gender") %>%
  as.matrix()

# the Non-binary category is vanishingly small, max 11 in ECR
# so not really estimable
# visualize
tab_gend_career

#step back and just do man/woman
dat_q4a <- survey %>%
  dplyr::filter(!is.na(career_stage)& !is.na(ai_use_freq) &
                !is.na(gender) & !gender%in% c("Prefer not to answer",
                                               "Prefer to self-identify",
                                               "Non-binary" )) %>%
  droplevels()

# no interactions between gender and career stage so can test separately
# using max sample sizes for each
res_4_int <- clm(ai_use_freq ~ gender*career_stage, data = dat_q4a,
             link = "probit")
anova(clm(ai_use_freq ~ gender + career_stage, data = dat_q4a, link = "probit"), res_4_int)

#run combining over career stages to maximize sample n
dat_q4 <- survey %>%
  dplyr::filter(!is.na(ai_use_freq)&
                !is.na(gender) & !gender%in% c("Prefer not to answer",
                                               "Prefer to self-identify",
                                               "Non-binary" )) %>%
  droplevels()

#no significant difference between genders
res_4 <- clm(ai_use_freq ~ gender, data = dat_q4,
             link = "probit")
drop1(res_4, test = "Chisq")

# NS but slightly higher values for women than men
# suggesting they are a little higher into the "never" category
summary (res_4) 
confint(pairs(emmeans(res_4, ~ gender, mode = "linear.predictor")))


# ---- model estimates ----------------------------------------------------
emm <- as.data.frame(
  emmeans(res_4, ~ ai_use_freq | gender, mode = "prob"))
names(emm)   # check: prob + asymp.LCL/asymp.UCL or lower.CL/upper.CL

emm <- as.data.frame(
  marginaleffects::avg_predictions(res_4, ~ ai_use_freq | gender, mode = "prob"))
names(emm)   # check: prob + asymp.LCL/asymp.UCL or lower.CL/upper.CL

emm <-emm %>%
  mutate(ai_use_freq = factor(as.character(ai_use_freq),
 levels = levels(dat_q4$ai_use_freq),
  ordered = TRUE))

gender_tukey <- emmeans(res_4, ~ gender, mode = "linear.predictor")
pairs(gender_tukey, adjust = "tukey")

# ---- observed proportions ----------------------------------------------
raw <- dat_q4 %>%
  dplyr::count(gender, ai_use_freq) %>%
  group_by(gender) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

tot <- raw %>% group_by(gender) %>% summarise(n = sum(n), .groups = "drop")

# ---- plot ---------------------------------------------------------------
gender_plot <- ggplot(emm, aes(gender, prob)) +
  geom_col(data = raw, aes(y = prop), fill = "grey85", width = .7) +
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                  colour = "firebrick", size = .3) +
  geom_text(data = tot, aes(y = Inf, label = paste0("n = ", n)),
            vjust = 1.4, size = 2.6, colour = "grey40") +
  facet_wrap(~ ai_use_freq, nrow = 1, labeller = label_wrap_gen(20)) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(.05, .12))) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  labs(x = NULL, y = "P(category)",
       caption = "grey = observed; red = model estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(plot = gender_plot, filename =
         file.path("graphs","analysis_figs", "ai_use_by_gender.jpg"), width =8, height =6)




