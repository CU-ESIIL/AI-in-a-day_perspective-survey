#' @title Policy and AI attitude: Bayesian ordinal model comparison
#'
#' @description
#' Does the policy environment a respondent reports (restrictive -> permissive)
#' relate to their general attitude toward GenAI (opposed -> very enthusiastic)?
#' Modelled on `survey_elmendorf-stats-bayesian-mod_compare.r`, with the
#' parallelisation and memory changes identified in review of that script.
#'
#' @details
#' ## Workflow
#' 1. `prep_policy_data()` reads the tidied responses, orders the Likert
#'    factors by their `*_value` codes, collapses "Very restrictive" and
#'    "Somewhat restrictive" into "Restrictive" (4 policy levels), and builds
#'    two analysis sets (see below).
#' 2. Descriptive checks: cell counts (`likert_cells()`) and a frequentist
#'    check that the policy effect does not differ by career stage
#'    (`covariate_check()`).
#' 3. `build_tasks()` defines the models; `run_tasks()` fits them in parallel
#'    (one `future` per model) and caches each to disk.
#' 4. Summaries from the cached fits: slope + Bayes factor, LOO comparison,
#'    simplex spacing, probability-scale contrasts, prior sensitivity, plots.
#'
#' ## Models (cumulative probit, `gen_attitude` as the outcome)
#' All fitted to the same complete-case rows (`d_main`) so LOO is comparable:
#' * `mono`     : `mo(policies)`, monotonic predictor (primary)
#' * `fac`      : `policies` as dummy-coded (sequential-difference contrasts)
#' * `lin`      : `policies` as integer 1..K
#' * `mono_adj` : `mo(policies) + career_stage` (adjusted; career stage is a
#'                likely confounder)
#'
#' Fitted to a different row set, so NOT in the LOO comparison:
#' * `grp`      : `policy_group` as an unordered factor that keeps "No policy"
#'                and "Other" as separate groups (reference = Neutral)
#' * `mono_unscaled`, `mono_flat` : prior sensitivity for the slope
#'                (the `scaled` run is `mono` itself; no duplicate fit)
#'
#' ## Analysis sets
#' * `d_all`  : attitude on-scale AND policy answered (6 groups). Used for
#'              descriptives and the `grp` model.
#' * `d_main` : `d_all` restricted to the 4 on-scale policy levels AND a
#'              career stage. Used for the ordinal models.
#'
#' ## Parallelisation
#' Every model is independent, so each is one `future`. The posterior fit and
#' its prior-only refit run in the same worker because `update()` needs the
#' compiled Stan model. Workers = `cores %/% chains`, each using `chains`
#' cores, so the machine is not oversubscribed. Stan compilation (single
#' threaded) overlaps across workers.
#'
#' ## Memory
#' * Formulas are built with `env = globalenv()`. A formula built inside a
#'   function captures that frame, and `saveRDS()` then serialises every
#'   object in it (earlier fits included): the cached fits of the sibling
#'   script are 30-90 MB on disk but about 4 MB in memory.
#' * The compiled Stan model (`fit@stanmodel`, ~130 MB) is dropped after
#'   sampling. Nothing downstream needs it, but a slimmed fit cannot be
#'   `update()`d, so refit rather than update.
#' * brms `file =` caching is not used (it writes before slimming). Fits are
#'   saved by `fit_task()`, keyed by a hash of data + formula + prior +
#'   sampler settings, so unchanged models are never refitted.
#' * LOO is computed in the worker and stored in the fit.
#' * Prior-only fits are made for `mono` only (they feed the Bayes factor and
#'   prior-predictive plot; they are not needed for fac/lin).
#' * Predictions use a fixed subset of draws, one level at a time.
#'
#' ## Handling of `Other` values (NOT implemented; kept as separate groups)
#' Free text (`Policies_8_TEXT`, `Gen_Attitude_21_TEXT`) is never read by
#' this script. Suggestions for a later pass:
#' * Policy "Other" (n ~ 19): mostly "it varies by unit/context". Keep as a
#'   "Mixed/varies" group. Recode into the scale only where the text names a
#'   scale level explicitly (a handful), using two independent coders and a
#'   logged lookup table. Do not code policy from tone: some texts express
#'   attitude ("forcing it down our throats"), which would make the analysis
#'   circular.
#' * Policy "No policy" (n ~ 34): not a point on the scale (code 7 sorts
#'   above "Very permissive" by value). Keep as its own group; consider
#'   splitting "no policy" from "unaware of policy".
#' * Attitude "Other" (n ~ 23): many are scorable ("Cautiously opposed",
#'   "Very against it", "equally cautious and enthusiastic"); others describe
#'   another dimension ("Exasperated"). Candidate for a double-coded
#'   sensitivity analysis. Keep any coding table local; free text can identify
#'   respondents.
#' * Attitude "Indifferent" (n = 5): not a point on the scale; too small to
#'   model. Dropped.
#' * Never use the `*_value` codes as numbers: they are Qualtrics recode IDs
#'   (attitude 11, 12, 17-21; policy 7 and 8 are off-scale).
#'
#' ## Interpretation caveats
#' Policy is perceived (self-reported), the data are cross-sectional, and
#' respondents may be clustered by institution (no institution variable is
#' used). Report associations, not effects of policy.
#'
#' @section Outputs (in `fig_path`):
#' `*_summary.csv`, `*_loo.csv`, `*_simplex.csv`, `*_probs.csv`,
#' `*_contrasts.csv`, `*_sensitivity.csv`, `*_cells.csv`, `*_diagnostics.csv`
#' and figures `*_groups.jpg`, `*_mono.jpg`, `*_probs.jpg`, `*_ppc_*.jpg`.
#'
#' @author Stevan Earl

# ---- 0. setup ---------------------------------------------------------------

source(file.path("-setup.r"))   # clears environment, creates folders

# smoke-test switch: tiny sampler settings, separate cache and output folders
quick     <- FALSE
# TRUE -> data/01_tidied-responses.csv, graphs/;
# FALSE -> data/broken-row-survey-data.csv, graphs_fake/
real_data <- TRUE

#' Run configuration
#'
#' `refit = FALSE` reuses a cached fit whenever its input hash is unchanged;
#' set `TRUE` to force refitting everything.
#' `b_prior`: "scaled" normal(0, 1/(K-1)) per step, so the implied prior on
#' the lowest-to-highest contrast is normal(0, 1); "unscaled" normal(0, 1)
#' per step; "flat" is brms default (no prior draws -> no Bayes factor).
cfg <- list(
  data_path       = if (real_data) file.path("data", "01_tidied-responses.csv")
                    else file.path("data", "broken-row-survey-data.csv"),
  refit           = FALSE,
  iter            = if (quick) 600L else 6000L,
  chains          = if (quick) 2L else 4L,
  adapt_delta     = 0.99,
  seed            = 1L,
  b_prior         = "scaled",
  run_sensitivity = TRUE,
  ndraws_pred     = if (quick) 500L else 2000L,
  prefix          = paste0("policy_attitude", if (real_data) "" else "_fake",
                           if (quick) "_quick" else ""),
  fit_path        = if (quick) "fits_quick" else "fits",
  fig_path        = file.path(if (real_data) "graphs" else "graphs_fake",
                              "analysis_figs",
                              if (quick) "policy_attitude_quick"
                              else "policy_attitude")
)
cfg$warmup  <- cfg$iter %/% 2L
# Each worker compiles a Stan model (peak ~1-2 GB) and then holds its fit, so
# workers are capped by memory as well as cores. Raise `max_workers` only if
# there is headroom; an earlier uncapped run (6 workers) was killed for memory.
max_workers <- 3L
cfg$workers <- max(1L, min(parallel::detectCores() %/% cfg$chains, max_workers))

purrr::walk(c(cfg$fit_path, cfg$fig_path),
            \(p) dir.create(p, recursive = TRUE, showWarnings = FALSE))

set.seed(cfg$seed)

# survey labels, in scale order (verified against the *_value codes on read)
policy_raw_scale <- c("Very restrictive/opposed", "Somewhat restrictive/opposed",
                      "Neutral", "Somewhat permissive/supportive",
                      "Very permissive/supportive")
policy_none      <- "There are not any policies or guidelines at my institution"
policy_levels    <- c("Restrictive", "Neutral", "Somewhat permissive/supportive",
                      "Very permissive/supportive")
policy_groups    <- c(policy_levels, "No policy", "Other")
attitude_levels  <- c("Opposed to GenAI", "Cautious",
                      "A mix of caution and enthusiasm", "Enthusiastic",
                      "Very enthusiastic")


# ---- 1. data ----------------------------------------------------------------

#' Order a Likert column by its matching `*_value` code
#'
#' `unique()` after `order()` guards against duplicate label text producing
#' duplicated factor levels.
#'
#' @param d Data frame with `col` and `paste0(col, "_value")`.
#' @param col Column name (clean_names style).
#' @return `d` with `col` as an ordered factor.
relevel_by_value <- function(d, col) {
  lv <- unique(d[[col]][order(d[[paste0(col, "_value")]])])
  d[[col]] <- factor(d[[col]], levels = lv[!is.na(lv)], ordered = TRUE)
  d
}

#' Read and prepare the policy / attitude / career-stage data
#'
#' Only the six columns needed are kept; free-text columns are not read into
#' the analysis. The off-scale labels ("Other", "No policy", "Indifferent")
#' are handled by filtering BEFORE `droplevels()`, because their codes sort
#' above the scale (e.g. attitude "Indifferent" = 20 > "Very enthusiastic").
#'
#' @param path Path to the tidied responses CSV.
#' @return List: `all` (6 policy groups), `main` (4 on-scale policy levels,
#'   career stage present), `flow` (respondent counts at each exclusion).
prep_policy_data <- function(path) {

  need <- c("policies", "policies_value", "gen_attitude", "gen_attitude_value",
            "career_stage", "career_stage_value")

  raw <- utils::read.csv(path, stringsAsFactors = FALSE,
                         na.strings = c("NA", "")) |>
    janitor::clean_names()
  stopifnot(all(need %in% names(raw)))

  d <- raw[, need] |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("_value"), as.numeric)) |>
    (\(x) purrr::reduce(c("gen_attitude", "policies", "career_stage"),
                        relevel_by_value, .init = x))()

  # the explicit level order used below must agree with the survey codes
  stopifnot(
    identical(intersect(levels(d$policies), policy_raw_scale), policy_raw_scale),
    identical(intersect(levels(d$gen_attitude), attitude_levels), attitude_levels))

  unmatched <- setdiff(levels(d$policies), c(policy_raw_scale, policy_none, "Other"))
  if (length(unmatched))
    warning("Unrecognised policy label(s) set to NA: ",
            paste(unmatched, collapse = "; "), call. = FALSE)

  d <- d |>
    dplyr::mutate(
      policy_group = dplyr::case_when(
        policies %in% policy_raw_scale[1:2] ~ "Restrictive",
        policies %in% policy_raw_scale[3:5] ~ as.character(policies),
        policies == policy_none             ~ "No policy",
        policies == "Other"                 ~ "Other",
        TRUE                                ~ NA_character_),
      policy_group = factor(policy_group, levels = policy_groups),
      policies     = factor(as.character(policy_group), levels = policy_levels,
                            ordered = TRUE),
      attitude_ok  = !is.na(gen_attitude) &
                       !gen_attitude %in% c("Other", "Indifferent"),
      # unordered: an ordered factor on the RHS would get polynomial contrasts
      career_stage = factor(as.character(career_stage),
                            levels = levels(career_stage)))

  d_all  <- d |>
    dplyr::filter(attitude_ok, !is.na(policy_group)) |>
    dplyr::select(-attitude_ok) |>
    droplevels()
  d_main <- d_all |>
    dplyr::filter(!is.na(policies), !is.na(career_stage)) |>
    droplevels()

  flow <- tibble::tibble(
    step = c("rows", "answered policy and attitude",
             "attitude on-scale (drops Other, Indifferent)",
             "+ policy on-scale (drops No policy, Other)  [d_all has both]",
             "+ career stage present  [d_main]"),
    n = c(nrow(d),
          sum(!is.na(d$policy_group) & !is.na(d$gen_attitude)),
          nrow(d_all),
          sum(!is.na(d_all$policies)),
          nrow(d_main)))

  list(all = d_all, main = d_main, flow = flow)
}

#' Cell counts before fitting anything
#'
#' A cumulative model needs every outcome category populated; sparse cells are
#' the usual cause of convergence problems.
#'
#' @param dat Data frame.
#' @param x,y Column names.
#' @return List: cross `table`, number of `zeros`, outcome totals, and outcome
#'   categories with fewer than 10 responses (`thin_y`).
likert_cells <- function(dat, x, y) {
  tab <- table(dat[[x]], dat[[y]])
  list(table   = tab,
       zeros   = sum(tab == 0),
       x_total = table(dat[[x]]),
       y_total = table(dat[[y]]),
       thin_y  = names(which(table(dat[[y]]) < 10)))
}

#' Frequentist pre-check: does the policy effect differ by `covariate`?
#'
#' Documented in `survey_elmendorf-stats.r` (`covariate_check()`). Both `clm`
#' models are fitted to one dataset (complete cases on all three variables) so
#' the likelihood-ratio test compares genuinely nested models. With sparse
#' cells the interaction model often fails to converge; the test is then
#' skipped rather than reported.
#'
#' @param dat Data frame.
#' @param x,y,covariate Column names.
#' @param link `clm` link.
#' @return An `anova.clm` table, or `NULL` if not testable.
covariate_check <- function(dat, x, y, covariate, link = "probit") {

  if (is.null(covariate) || !covariate %in% names(dat)) return(NULL)

  d <- dat[!is.na(dat[[covariate]]), , drop = FALSE] |> droplevels()

  m_add <- try(ordinal::clm(stats::as.formula(sprintf("%s ~ %s + %s", y, x, covariate)),
                            data = d, link = link), silent = TRUE)
  m_int <- try(ordinal::clm(stats::as.formula(sprintf("%s ~ %s * %s", y, x, covariate)),
                            data = d, link = link), silent = TRUE)

  # convergence$code can be a VECTOR; 0 = clean
  ok <- \(f) !inherits(f, "try-error") && all(f$convergence$code == 0)

  if (!ok(m_add) || !ok(m_int)) {
    warning("Covariate interaction model did not converge; not tested.",
            call. = FALSE)
    return(NULL)
  }
  stats::anova(m_add, m_int)
}


# ---- 2. models --------------------------------------------------------------

#' Define the models to fit
#'
#' Each task is a plain list (formula, data, prior, flags) so it can be shipped
#' to a `future` worker. Priors on the slope are per-step for mono, fac
#' (sequential-difference contrasts) and lin, so one prior scale serves all
#' three codings. The `grp` model's coefficients are group-vs-Neutral
#' contrasts, so it uses normal(0, 1). Formulas use `env = globalenv()` so the
#' saved fits do not drag the calling frame along.
#'
#' @param d_main,d_all Analysis sets from `prep_policy_data()`.
#' @param cfg Run configuration.
#' @return Named list of tasks.
build_tasks <- function(d_main, d_all, cfg) {

  x <- "policies"; y <- "gen_attitude"; cov <- "career_stage"
  K_x       <- nlevels(d_main[[x]])
  simo_coef <- paste0("mo", x, "1")
  mo_coef   <- paste0("mo", x)

  frm <- \(rhs) brms::bf(stats::as.formula(paste(y, "~", rhs), env = globalenv()))

  step_sd <- \(mode) switch(mode, scaled = 1 / (K_x - 1), unscaled = 1, flat = NA_real_)

  th_prior   <- brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept")
  simo_prior <- brms::set_prior("dirichlet(1)", class = "simo", coef = simo_coef)
  nrm        <- \(sd, ...) brms::set_prior(sprintf("normal(0, %g)", sd), ...)

  # priors for a model whose slope is `coef` (NULL = every class-b term)
  slope_prior <- \(mode, with_simo, coef = NULL) {
    p <- if (with_simo) th_prior + simo_prior else th_prior
    if (mode == "flat") return(p)
    p + if (is.null(coef)) nrm(step_sd(mode), class = "b")
        else nrm(step_sd(mode), class = "b", coef = coef)
  }

  keep <- \(d, cols) d[, cols, drop = FALSE]

  d_fac <- keep(d_main, c(y, x, cov))
  d_fac[[x]] <- factor(as.character(d_fac[[x]]), levels = levels(d_main[[x]]),
                       ordered = FALSE)
  d_fac[[x]] <- stats::`contrasts<-`(d_fac[[x]], value = MASS::contr.sdif(K_x))

  d_lin <- keep(d_main, c(y, x, cov))
  d_lin[[x]] <- as.integer(d_main[[x]])   # level position 1..K

  d_grp <- keep(d_all, c(y, "policy_group", cov))
  d_grp$policy_group <- factor(as.character(d_grp$policy_group),
                               levels = policy_groups, ordered = FALSE)
  d_grp$policy_group <- stats::`contrasts<-`(
    d_grp$policy_group,
    value = stats::contr.treatment(length(policy_groups), base = 2))  # Neutral

  mono_data <- keep(d_main, c(y, x, cov))

  main_mode <- cfg$b_prior
  tasks <- list(
    mono = list(tag = "mono", formula = frm(sprintf("mo(%s)", x)),
                data = mono_data, prior = slope_prior(main_mode, TRUE, mo_coef),
                prior_only = main_mode != "flat"),
    fac  = list(tag = "fac", formula = frm(x), data = d_fac,
                prior = slope_prior(main_mode, FALSE), prior_only = FALSE),
    lin  = list(tag = "lin", formula = frm(x), data = d_lin,
                prior = slope_prior(main_mode, FALSE), prior_only = FALSE),
    mono_adj = list(
      tag = "mono_adj", formula = frm(sprintf("mo(%s) + %s", x, cov)),
      data = mono_data,
      # class-b normal(0, 1) covers the career-stage dummies; the slope gets
      # its own coef-specific prior
      prior = slope_prior(main_mode, TRUE, mo_coef) +
        if (main_mode == "flat") NULL else nrm(1, class = "b"),
      prior_only = FALSE),
    grp  = list(tag = "grp", formula = frm("policy_group"), data = d_grp,
                prior = th_prior + nrm(1, class = "b"), prior_only = FALSE)
  )

  if (cfg$run_sensitivity) {
    sens <- setdiff(c("scaled", "unscaled", "flat"), main_mode) |>
      rlang::set_names(\(m) paste0("mono_", m)) |>
      purrr::imap(\(m, tag) list(tag = tag, formula = frm(sprintf("mo(%s)", x)),
                                 data = mono_data,
                                 prior = slope_prior(m, TRUE, mo_coef),
                                 prior_only = FALSE))
    tasks <- c(tasks, sens)
  }
  tasks
}

#' Per-fit convergence summary
#'
#' @param fit A `brmsfit`.
#' @param tag Model name, so a warning can be traced to a model.
#' @param bulk_min,tail_min,rhat_max Flagging thresholds.
#' @return One-row tibble.
diagnose_fit <- function(fit, tag, bulk_min = 400, tail_min = 400, rhat_max = 1.01) {

  s  <- posterior::summarise_draws(posterior::as_draws(fit),
                                   "rhat", "ess_bulk", "ess_tail")
  np <- brms::nuts_params(fit)

  max_td <- fit$fit@stan_args[[1]]$control$max_treedepth
  if (is.null(max_td)) max_td <- 10

  bad <- s |>
    dplyr::filter(rhat > rhat_max | ess_bulk < bulk_min | ess_tail < tail_min) |>
    dplyr::arrange(ess_tail)

  tibble::tibble(
    model     = tag,
    divergent = sum(np$Value[np$Parameter == "divergent__"]),
    max_tree  = sum(np$Value[np$Parameter == "treedepth__"] >= max_td),
    max_rhat  = max(s$rhat, na.rm = TRUE),
    min_ess_b = min(s$ess_bulk, na.rm = TRUE),
    min_ess_t = min(s$ess_tail, na.rm = TRUE),
    n_flagged = nrow(bad),
    worst     = if (nrow(bad)) bad$variable[1] else NA_character_)
}

#' Shrink a fit for storage
#'
#' Resets the formula environment and the `terms` attribute of the data (both
#' can capture a calling frame) and drops the compiled Stan model. The result
#' still supports `loo`, `posterior_epred`, `conditional_effects` and
#' `pp_check`, but cannot be `update()`d.
#'
#' @param fit A `brmsfit`.
#' @return The slimmed `brmsfit`.
slim_fit <- function(fit) {
  environment(fit$formula$formula) <- globalenv()
  purrr::walk(seq_along(fit$formula$pforms),
              \(i) environment(fit$formula$pforms[[i]]) <- globalenv())
  attr(fit$data, "terms") <- NULL
  loadNamespace("rstan")
  fit$fit@stanmodel <- methods::new("stanmodel")
  fit
}

#' Fit one task (runs inside a `future` worker)
#'
#' Fits the posterior model, optionally the prior-only refit (before slimming,
#' since `update()` needs the compiled model), computes LOO and diagnostics,
#' slims, and saves. A task whose inputs hash to the cached fit's is skipped.
#'
#' @param task One element of `build_tasks()`.
#' @param cfg Run configuration.
#' @return List: `tag`, `path`, `prior_path`, `diagnostics`, `cached`.
fit_task <- function(task, cfg) {

  path       <- file.path(cfg$fit_path, paste0(cfg$prefix, "_", task$tag, ".rds"))
  prior_path <- if (task$prior_only) sub("\\.rds$", "_prior.rds", path) else NULL

  # the formula is hashed as text: the brmsformula object itself does not
  # hash identically across R sessions
  hash <- rlang::hash(list(
    formula    = paste(deparse(task$formula$formula), collapse = ""),
    data       = task$data,
    prior      = task$prior,
    prior_only = task$prior_only,
    sampler    = cfg[c("iter", "warmup", "chains", "adapt_delta", "seed")]))

  if (!cfg$refit && file.exists(path) &&
      (is.null(prior_path) || file.exists(prior_path))) {
    old <- readRDS(path)
    if (identical(attr(old, "input_hash"), hash))
      return(list(tag = task$tag, path = path, prior_path = prior_path,
                  diagnostics = attr(old, "diagnostics"), cached = TRUE))
  }

  # tag any sampler warning with the model that produced it
  tagged <- \(expr, tag) withCallingHandlers(
    expr, warning = \(w) {
      message("[", tag, "] ", conditionMessage(w))
      invokeRestart("muffleWarning")
    })

  fit <- tagged(
    brms::brm(task$formula, data = task$data, family = brms::cumulative("probit"),
              prior = task$prior, chains = cfg$chains, cores = cfg$chains,
              iter = cfg$iter, warmup = cfg$warmup, seed = cfg$seed,
              control = list(adapt_delta = cfg$adapt_delta), refresh = 0),
    task$tag)

  fit_prior <- if (task$prior_only)
    tagged(stats::update(fit, sample_prior = "only", chains = cfg$chains,
                         cores = cfg$chains, iter = cfg$iter,
                         warmup = cfg$warmup, seed = cfg$seed, refresh = 0),
           paste0(task$tag, "_prior"))

  fit  <- brms::add_criterion(fit, "loo")
  diag <- diagnose_fit(fit, task$tag)

  save_slim <- \(f, p, ...) {
    f <- slim_fit(f)
    attr(f, "input_hash")  <- hash
    attr(f, "diagnostics") <- diag
    saveRDS(f, p)
  }
  save_slim(fit, path)
  if (!is.null(fit_prior)) save_slim(fit_prior, prior_path)

  list(tag = task$tag, path = path, prior_path = prior_path,
       diagnostics = diag, cached = FALSE)
}

#' Run all tasks in parallel
#'
#' One `future` per task on a `multisession` plan; the plan is restored on
#' exit, which also shuts the workers down and frees their memory. A failed
#' task does not discard the others: finished fits are already cached, so
#' fixing the failure and re-running resumes where it stopped.
#'
#' @param tasks Output of `build_tasks()`.
#' @param cfg Run configuration.
#' @return Named list of `fit_task()` results.
run_tasks <- function(tasks, cfg) {

  old_plan <- future::plan(future::multisession,
                           workers = min(cfg$workers, length(tasks)))
  on.exit(future::plan(old_plan), add = TRUE)

  futs <- purrr::map(tasks, \(t) future::future(
    fit_task(t, cfg), seed = TRUE, packages = "brms"))
  res  <- purrr::map(futs, \(f) tryCatch(future::value(f), error = \(e) e))

  failed <- purrr::keep(res, inherits, "error")
  if (length(failed))
    stop("Fit(s) failed: ",
         paste0(names(failed), " (", purrr::map_chr(failed, conditionMessage), ")",
                collapse = "; "), call. = FALSE)
  res
}


# ---- 3. summaries -----------------------------------------------------------

#' LOO comparison table, one row per model, best first
#'
#' `loo::loo_compare()` changed its return type across versions (matrix with
#' row names vs data frame with a `model` column, and `p_loo` is not in the
#' newer output), so the table is assembled from the comparison plus each
#' model's own estimates.
#'
#' @param fits Named list of brmsfits with `loo` stored (see `fit_task()`).
#' @return Tibble: rank, model, elpd_diff, se_diff, elpd_loo, se_elpd_loo,
#'   p_loo, looic.
loo_table <- function(fits) {

  loos <- purrr::map(fits, \(f) f$criteria$loo)
  cmp  <- as.data.frame(loo::loo_compare(loos))
  if (!"model" %in% names(cmp)) cmp$model <- rownames(cmp)

  est <- \(m, k, col) loos[[m]]$estimates[k, col]

  tibble::tibble(
    rank        = seq_len(nrow(cmp)),
    model       = cmp$model,
    elpd_diff   = cmp$elpd_diff,
    se_diff     = cmp$se_diff,
    elpd_loo    = purrr::map_dbl(cmp$model, est, "elpd_loo", "Estimate"),
    se_elpd_loo = purrr::map_dbl(cmp$model, est, "elpd_loo", "SE"),
    p_loo       = purrr::map_dbl(cmp$model, est, "p_loo", "Estimate"),
    looic       = purrr::map_dbl(cmp$model, est, "looic", "Estimate"))
}

#' Spacing of the monotonic simplex versus equal spacing
#'
#' Under equal spacing every simplex element equals 1/(K-1). An element whose
#' 90% interval EXCLUDES that value is evidence of uneven spacing, which is
#' the only thing `mo()` buys over a linear coding. (90%, not 95%: a 95%
#' interval on a simplex element almost never excludes 1/(K-1).)
#'
#' @param fit The `mono` brmsfit.
#' @param K_x Number of predictor levels.
#' @param x_levels Predictor levels, used to label each step.
#' @return Tibble, one row per simplex element; `off` flags uneven steps.
simplex_summary <- function(fit, K_x, x_levels = NULL) {

  eq <- 1 / (K_x - 1)

  s <- posterior::summarise_draws(
    posterior::as_draws(fit), "mean",
    \(x) stats::quantile(x, probs = c(0.05, 0.95))) |>
    dplyr::filter(grepl("^simo_", variable)) |>
    dplyr::rename(q5 = `5%`, q95 = `95%`) |>
    dplyr::mutate(step = dplyr::row_number(), equal = eq,
                  off = q5 > eq | q95 < eq) |>
    dplyr::relocate(step, .before = 1)

  s$step_label <- if (!is.null(x_levels) && length(x_levels) == K_x)
    paste(x_levels[s$step], x_levels[s$step + 1L], sep = " -> ")
  else NA_character_
  s
}

#' Which coding to report
#'
#' Combines the simplex evidence with a PAIRWISE LOO comparison of the
#' monotonic and linear codings. The comparison is deliberately restricted to
#' these two: ranking against every model (e.g. the career-stage-adjusted one)
#' would attribute that model's advantage to the coding choice. A gap under
#' about 2 ELPD is not meaningful, and `se_diff` is optimistic for nested
#' models (Sivula, Magnusson & Vehtari 2022), so the threshold is
#' deliberately conservative.
#'
#' @param fit_mono,fit_lin The `mono` and `lin` brmsfits (LOO stored).
#' @param K_x,x_levels See `simplex_summary()`.
#' @param elpd_tol ELPD difference below which LOO cannot separate the codings.
#' @return List: simplex table, `n_off`, `off_steps`, `lin_gap` (ELPD that
#'   linear loses to monotonic; negative if linear fits better), `lin_se`
#'   (SE of that difference) and the recommendation.
recommend_coding <- function(fit_mono, fit_lin, K_x, x_levels, elpd_tol = 2) {

  sx    <- simplex_summary(fit_mono, K_x, x_levels)
  n_off <- sum(sx$off)

  pair    <- loo_table(list(mono = fit_mono, lin = fit_lin))
  lin_gap <- pair$elpd_loo[pair$model == "mono"] - pair$elpd_loo[pair$model == "lin"]
  lin_se  <- pair$se_diff[pair$model != pair$model[1]]

  rec <- if (n_off > 0) {
    sprintf("monotonic - %d simplex element(s) depart from equal spacing", n_off)
  } else if (abs(lin_gap) < elpd_tol) {
    "linear - spacing is even and LOO cannot separate them"
  } else if (lin_gap > 0) {
    sprintf("monotonic - linear loses %.1f ELPD", lin_gap)
  } else {
    sprintf("linear - fits better than monotonic by %.1f ELPD", -lin_gap)
  }

  list(simplex = sx, n_off = n_off,
       off_steps = if (n_off > 0)
         paste(stats::na.omit(sx$step_label[sx$off]), collapse = "; ")
       else NA_character_,
       lin_gap = lin_gap, lin_se = lin_se, recommendation = rec)
}

#' Probability-scale predictions and contrasts by policy level
#'
#' G-computation: every respondent's covariates are held at their observed
#' values, the policy variable is set to each level in turn, and predicted
#' category probabilities are averaged over respondents. For `mono_adj` this
#' marginalises over the observed career-stage mix. A single fixed subset of
#' draws is used for every level so contrasts are paired draw-by-draw. Levels
#' are processed one at a time to bound memory (draws x respondents x
#' categories per level).
#'
#' Derived outcomes: `negative` = lowest two attitude categories, `positive`
#' = highest two.
#'
#' @param fit A brmsfit with a categorical (factor) predictor `x`.
#' @param x,y Predictor and outcome column names.
#' @param ref Reference level for contrasts (default: first level).
#' @param ndraws Number of posterior draws to use.
#' @param seed Seed for the draw subset.
#' @return List: `probs` (mean, 95% interval per level x outcome) and
#'   `contrasts` (difference vs `ref`, with 95% interval and P(diff > 0)).
policy_probs <- function(fit, x, y, ref = NULL, ndraws = 2000, seed = 1) {

  lv   <- levels(fit$data[[x]])
  y_lv <- levels(fit$data[[y]])
  ref  <- if (is.null(ref)) lv[1] else ref

  set.seed(seed)
  sel <- sort(sample.int(brms::ndraws(fit), min(ndraws, brms::ndraws(fit))))

  derive <- \(m) cbind(m,
                       negative = rowSums(m[, y_lv[1:2], drop = FALSE]),
                       positive = rowSums(m[, utils::tail(y_lv, 2), drop = FALSE]))

  per_level <- rlang::set_names(lv) |>
    purrr::map(\(l) {
      nd <- fit$data
      nd[[x]][] <- l            # `[<-` keeps the factor's contrasts attribute
      ep <- brms::posterior_epred(fit, newdata = nd, draw_ids = sel)
      m  <- apply(ep, c(1, 3), mean)
      colnames(m) <- y_lv
      derive(m)
    })

  summ <- \(v) tibble::tibble(mean = mean(v),
                              lo = stats::quantile(v, 0.025, names = FALSE),
                              hi = stats::quantile(v, 0.975, names = FALSE))
  metrics <- colnames(per_level[[1]])

  probs <- per_level |>
    purrr::imap(\(m, l) purrr::map(rlang::set_names(metrics),
                                   \(k) summ(m[, k])) |>
                  purrr::list_rbind(names_to = "outcome") |>
                  dplyr::mutate(level = l, .before = 1)) |>
    purrr::list_rbind()

  contrasts <- rlang::set_names(setdiff(lv, ref)) |>
    purrr::map(\(l) purrr::map(rlang::set_names(metrics), \(k) {
      d <- per_level[[l]][, k] - per_level[[ref]][, k]
      dplyr::mutate(summ(d), pd = mean(d > 0))
    }) |> purrr::list_rbind(names_to = "outcome")) |>
    purrr::list_rbind(names_to = "level") |>
    dplyr::mutate(reference = ref, .after = level)

  list(probs = probs, contrasts = contrasts)
}


# ---- 4. plots ---------------------------------------------------------------

#' Wrap axis labels, allowing a break after "/" (e.g. "permissive/supportive")
#'
#' @param width Characters per line.
#' @return A labelling function for `scale_x_discrete()`.
wrap_lab <- function(width = 14) {
  \(l) scales::label_wrap(width)(gsub("/", "/ ", l, fixed = TRUE))
}

#' Descriptive plot: attitude by policy group, including the off-scale groups
#'
#' Raw proportions only (no model). Shows "No policy" and "Other" next to the
#' four on-scale levels.
#'
#' @param d_all Analysis set with all six policy groups.
#' @return A ggplot.
plot_policy_groups <- function(d_all) {

  tab <- table(d_all$policy_group, d_all$gen_attitude)
  raw <- as.data.frame(tab, responseName = "n") |>
    rlang::set_names(c("group", "attitude", "n")) |>
    dplyr::mutate(prop = n / stats::ave(n, group, FUN = sum))
  tot <- as.data.frame(table(group = d_all$policy_group), responseName = "n")

  ggplot2::ggplot(raw, ggplot2::aes(group, prop, fill = attitude)) +
    ggplot2::geom_col(width = .7) +
    ggplot2::geom_text(data = tot, inherit.aes = FALSE,
                       ggplot2::aes(group, y = 1.04, label = paste0("n = ", n)),
                       size = 3, colour = "grey30") +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                expand = ggplot2::expansion(mult = c(0, .08))) +
    ggplot2::scale_x_discrete(labels = wrap_lab(14)) +
    ggplot2::scale_fill_viridis_d(option = "D", end = .9) +
    ggplot2::labs(x = NULL, y = "share of respondents", fill = "Attitude") +
    ggplot2::theme_classic()
}

#' Observed proportions with model intervals (mono model)
#'
#' @param dat Model data (`fit$data`).
#' @param ce `brms::conditional_effects(fit, categorical = TRUE)`.
#' @param x,y Column names.
#' @param dodge Dodge width.
#' @return A ggplot.
plot_policy_ce <- function(dat, ce, x, y, dodge = 0.8) {

  tab <- table(dat[[x]], dat[[y]])
  raw <- as.data.frame(tab, responseName = "n") |>
    rlang::set_names(c("effect1__", "effect2__", "n")) |>
    dplyr::mutate(prop = n / stats::ave(n, effect1__, FUN = sum))
  tot <- as.data.frame(table(effect1__ = dat[[x]]), responseName = "n")

  pal <- rlang::set_names(scales::viridis_pal(end = .9)(nlevels(dat[[y]])),
                          levels(dat[[y]]))

  ggplot2::ggplot() +
    ggplot2::geom_col(data = raw,
                      ggplot2::aes(effect1__, prop, fill = effect2__),
                      position = ggplot2::position_dodge(width = dodge),
                      width = .7, alpha = .35, colour = NA) +
    ggplot2::geom_text(data = tot,
                       ggplot2::aes(effect1__, y = Inf, label = paste0("n = ", n)),
                       vjust = 1.4, size = 3, colour = "grey30") +
    ggplot2::geom_linerange(
      data = as.data.frame(ce[[1]]),
      ggplot2::aes(effect1__, estimate__, ymin = lower__, ymax = upper__,
                   colour = effect2__),
      position = ggplot2::position_dodge(width = dodge), linewidth = .3) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                expand = ggplot2::expansion(mult = c(.05, .12))) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::scale_x_discrete(labels = wrap_lab(14)) +
    ggplot2::labs(x = x, y = paste0("P(", y, " = k)"), fill = y, colour = y) +
    ggplot2::theme_classic()
}

#' P(negative) and P(positive) attitude by policy level
#'
#' @param probs `probs` tibble from `policy_probs()`, with a `model` column.
#' @return A ggplot.
plot_probs <- function(probs) {

  d <- probs |>
    dplyr::filter(outcome %in% c("negative", "positive")) |>
    dplyr::mutate(level = factor(level, levels = unique(level)),
                  outcome = dplyr::recode(
                    outcome, negative = "P(opposed or cautious)",
                    positive = "P(enthusiastic or very enthusiastic)"))

  ggplot2::ggplot(d, ggplot2::aes(level, mean, colour = model)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lo, ymax = hi),
                             position = ggplot2::position_dodge(width = .4)) +
    ggplot2::facet_wrap(~ outcome, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_discrete(labels = wrap_lab(14)) +
    ggplot2::labs(x = "Policy", y = NULL, colour = NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "top")
}


# ---- 5. run -----------------------------------------------------------------

dat <- prep_policy_data(cfg$data_path)
print(as.data.frame(dat$flow), row.names = FALSE)

x <- "policies"; y <- "gen_attitude"

cells <- likert_cells(dat$main, x, y)
print(cells$table)
if (length(cells$thin_y))
  message("sparse outcome categories (n < 10): ", paste(cells$thin_y, collapse = ", "))

cov_test <- covariate_check(dat$main, x, y, "career_stage")
p_cov <- if (is.null(cov_test)) NA_real_ else cov_test[["Pr(>Chisq)"]][2]

# --- fit (parallel) ---
tasks <- build_tasks(dat$main, dat$all, cfg)
res   <- run_tasks(tasks, cfg)
rm(tasks); invisible(gc())

diag_tab <- purrr::map(res, "diagnostics") |> purrr::list_rbind()
print(as.data.frame(diag_tab), row.names = FALSE)

# --- load slimmed fits (a few MB each) ---
load_fit <- \(tag, prior = FALSE) readRDS(res[[tag]][[if (prior) "prior_path" else "path"]])

compare_tags <- c("mono", "fac", "lin", "mono_adj")
fits <- rlang::set_names(compare_tags) |> purrr::map(load_fit)
fit_prior_mono <- if (!is.null(res$mono$prior_path)) load_fit("mono", prior = TRUE)

# --- slope, Bayes factor ---
slope_of <- \(fit) {
  fx <- brms::fixef(fit)
  fx[grep("^mo", rownames(fx)), , drop = FALSE][1, ]
}
slope_row <- grep("^mo", rownames(brms::fixef(fits$mono)), value = TRUE)

# Savage-Dickey BF for the slope only; thresholds and simplex elements have no
# meaningful point null
bf_slope <- if (!is.null(fit_prior_mono))
  bayestestR::bayesfactor_parameters(fits$mono, prior = fit_prior_mono,
                                     parameters = paste0("bsp_", slope_row))
log_bf <- if (is.null(bf_slope)) NA_real_ else as.data.frame(bf_slope)$log_BF[1]
# capped: the density estimate at zero is unstable far from zero
bf_txt <- if (is.na(log_bf)) NA_character_ else
  if (log_bf > log(100)) "> 100" else
  if (log_bf < log(1 / 100)) "< 0.01" else sprintf("%.1f", exp(log_bf))

# --- LOO and coding ---
loo_tab <- loo_table(fits)
coding  <- recommend_coding(fits$mono, fits$lin, nlevels(dat$main[[x]]),
                            levels(dat$main[[x]]))
message("-> report the ", coding$recommendation)
print(as.data.frame(loo_tab[, c("rank", "model", "elpd_diff", "se_diff", "p_loo")]),
      row.names = FALSE, digits = 3)

# --- probability-scale results ---
pp_mono <- policy_probs(fits$mono, x, y, ndraws = cfg$ndraws_pred, seed = cfg$seed)
pp_adj  <- policy_probs(fits$mono_adj, x, y, ndraws = cfg$ndraws_pred, seed = cfg$seed)
fit_grp <- load_fit("grp")
pp_grp  <- policy_probs(fit_grp, "policy_group", y, ref = "Neutral",
                        ndraws = cfg$ndraws_pred, seed = cfg$seed)

bind_models <- \(which) purrr::map(list(mono = pp_mono, mono_adj = pp_adj, grp = pp_grp),
                                   which) |>
  purrr::list_rbind(names_to = "model")
probs_tab     <- bind_models("probs")
contrasts_tab <- bind_models("contrasts")

# --- prior sensitivity (slope across prior modes) ---
sens_tags <- grep("^mono(_|$)", names(res), value = TRUE) |>
  setdiff("mono_adj")
sens_tab <- rlang::set_names(sens_tags) |>
  purrr::map(\(tag) {
    s <- slope_of(load_fit(tag))
    tibble::tibble(b = s[["Estimate"]], lo = s[["Q2.5"]], hi = s[["Q97.5"]])
  }) |>
  purrr::list_rbind(names_to = "model")
print(as.data.frame(sens_tab), row.names = FALSE, digits = 3)

# --- one-row summary ---
s_mono <- slope_of(fits$mono)
s_adj  <- slope_of(fits$mono_adj)
loo_cell <- \(m, col) if (m %in% loo_tab$model) loo_tab[[col]][loo_tab$model == m] else NA_real_

summary_tab <- tibble::tibble(
  n = nrow(dat$main), K_x = nlevels(dat$main[[x]]), K_y = nlevels(dat$main[[y]]),
  b = s_mono[["Estimate"]], b_lo = s_mono[["Q2.5"]], b_hi = s_mono[["Q97.5"]],
  BF10 = bf_txt,
  b_adj = s_adj[["Estimate"]], b_adj_lo = s_adj[["Q2.5"]], b_adj_hi = s_adj[["Q97.5"]],
  p_covariate = p_cov,
  best_loo = loo_tab$model[1], lin_gap = coding$lin_gap, lin_se = coding$lin_se,
  elpd_diff_mono_adj = loo_cell("mono_adj", "elpd_diff"),
  n_simplex_off = coding$n_off, simplex_off_steps = coding$off_steps,
  coding = coding$recommendation,
  divergent = sum(diag_tab$divergent), n_flagged = sum(diag_tab$n_flagged))
print(as.data.frame(summary_tab), row.names = FALSE)

# --- figures ---
save_jpg <- \(p, name, width = 8, height = 6)
  ggplot2::ggsave(file.path(cfg$fig_path, paste0(cfg$prefix, "_", name, ".jpg")),
                  plot = p, width = width, height = height)

ce <- brms::conditional_effects(fits$mono, categorical = TRUE)
save_jpg(plot_policy_groups(dat$all), "groups")
save_jpg(plot_policy_ce(fits$mono$data, ce, x, y), "mono")
save_jpg(plot_probs(probs_tab |> dplyr::filter(model != "grp")), "probs")

pc <- \(fit, title) brms::pp_check(fit, type = "bars", ndraws = 200) +
  ggplot2::ggtitle(title)
save_jpg(pc(fits$mono, "Posterior predictive: mono"), "ppc_posterior")
if (!is.null(fit_prior_mono))
  save_jpg(pc(fit_prior_mono, "Prior predictive: mono"), "ppc_prior")

# --- tables ---
out <- list(summary = summary_tab, loo = loo_tab, simplex = coding$simplex,
            probs = probs_tab, contrasts = contrasts_tab,
            sensitivity = sens_tab, diagnostics = diag_tab,
            cells = as.data.frame(cells$table, responseName = "n") |>
              rlang::set_names(c(x, y, "n")))

purrr::iwalk(out, \(d, nm) readr::write_csv(
  d, file.path(cfg$fig_path, paste0(cfg$prefix, "_", nm, ".csv"))))

# ---- per-question detail, on demand -----------------------------------------
# out$contrasts |> dplyr::filter(model == "mono", outcome == "positive")
# out$contrasts |> dplyr::filter(model == "grp")   # No policy / Other vs Neutral
# summary(fits$mono)
# plot(ce)

# End ----
