# -------------------------------------------------
# survey_elmendorf_likert.R
# -------------------------------------------------
# Experimenting with fitting bayesian models
# with both x and y as likert
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
  tidybayes
)


# -------------------------------------------------
# Source reusable functions (all tools/ scripts)
# -------------------------------------------------
purrr::walk(
  dir("tools", pattern = "\\.r$", full.names = TRUE),
  source
)

# define useful helper functions for visualizing bayesian posteriors
# on priors as sanity check that the prior is not driving the 
# distribution for any critical params

# viz posterior on prior
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

# -------------------------------------------------
# Choose dataset (de‑identified vs full) and read it with the required logic
# -------------------------------------------------
real_data <- TRUE   # FALSE → use de‑identified `broken-row‑survey‑data.csv`
data_path <- if (real_data) {
  file.path("data", "01_tidied-responses.csv")
} else {
  file.path("data", "broken-row-survey-data.csv")
}

# Read CSV, convert empty strings to NA, and add a RespondentId column.
svy_v01 <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    dplyr::across(.cols = dplyr::everything(), .fns = ~ ifelse(nchar(.) == 0, NA, .))
  )

if(!real_data){
  svy_v01$ResponseId <- c(1:nrow(svy_v01))
}


# Use the same variable name as before for downstream code.
survey_raw <- svy_v01

# Define where graphs will be saved based on the data toggle.
graph_path <- if (real_data) "graphs" else "graphs_fake"

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
    career_stage = factor(.data[["career_stage"]],
                          levels = unique(.data[["career_stage"]][order(.data[["career_stage_value"]])]),
                          ordered = TRUE)                  
  )
# Ensure ordinal columns are numeric (they should already be, but be safe).
survey <- survey %>%
  dplyr::mutate(across(ends_with("_value"), as.numeric))

# ------------------------------------------------------------------
# Likert-vs-likert model, 
# ------------------------------------------------------------------

## (1) PRIMARY MODEL: cumulative probit with a monotonic predictor -----------
# Dirichlet prior on the simplex (spacing) parameter; weakly-informative
# Normal(0,1) prior on the (probit-scale) monotonic coefficient b.
# 
# reference paper is here: Burkner and Charpentier 2020
# https://doi.org/10.1111/bmsp.12195
# SCE needs to triple check the priors but I think ok?

# https://paulbuerkner.com/brms/reference/set_prior.html
#monotonic effects make use of a special parameter vector to estimate the
#'normalized distances' between consecutive predictor categories. This is
#'# realized in Stan using the simplex parameter type. This class is named
#'# "simo" (short for simplex monotonic) in brms.
#'

# used default brms priors
# but fliipped
priors <- c(
  # this would be the default
  #prior(normal(0, 1),         class = "b"),
  # matching scale somewhat to categorical below
  # because b ends up being between lowest and highest cat
  prior(normal(0, 0.33),         class = "b"),
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  # recommended default prior on the simplex from the manuscript
  # reasonably assume difference between all categories similar as a prior
  prior(dirichlet(1),         class = "simo", coef = "mocareer_stage1")
)


# fit monotonic
fit_mono <- brm(
  gen_attitude ~ mo(career_stage),
  data = survey %>% filter(!is.na(gen_attitude)&
                             !gen_attitude%in% c("Other","Indifferent")),
  family = cumulative("probit"),
  prior = priors, cores = 4, seed = 1,
  control = list(adapt_delta = 0.95)
)

#note due to ordering you cannot just
# sample prior = yes in the above to get the same answer
fit_prior_mono <- update(fit_mono,
                         sample_prior = 'only')


# y well within yrep
pp_check(fit_prior_mono, type = "bars", ndraws = 200) +
  ggtitle("Prior predictive: implied response distribution")

pp <- prior_post_draws(fit_mono, fit_prior_mono)
plot_prior_post(pp)

# Sce experimented with
# Kurz advice on the cutpoints for the intercepts
# https://solomonkurz.netlify.app/blog/2021-12-29-notes-on-the-bayesian-cumulative-probit/
# but gave illogical priors outside of range of posterior so default
# to burkner defaults

# cutpoints  <-tibble(rating = 1:5) %>%
#   mutate(proportion = 1/5) %>% 
#   mutate(cumulative_proportion = cumsum(proportion)) %>% 
#   mutate(right_hand_threshold = qnorm(cumulative_proportion))
# 
# priors_kurz <- c(
#   prior(normal(0, 1),class = "b"),
#   prior = c(prior(normal(-0.8416212, 1), class = Intercept, coef = 1),
#             prior(normal(-0.2533471, 1), class = Intercept, coef = 2),
#             prior(normal(0.2533471, 1), class = Intercept, coef = 3),
#             prior(normal(8416212, 1), class = Intercept, coef = 4)),
#   # recommended default prior on the simplex from the manuscript
#   # reasonably assume difference between all categories similar as a prior
#   prior(dirichlet(1),class = "simo", coef = "mocareer_stage1")
# )
# 
# 
# fit_mono_kurz <- brm(
#   gen_attitude ~ mo(career_stage),
#   data = survey %>% filter(!is.na(gen_attitude)&
#                              !gen_attitude%in% c("Other","Indifferent")),
#   family = cumulative("probit"),
#   prior = priors_kurz, cores = 4, seed = 1,
#   control = list(adapt_delta = 0.95)
# )
# 
# fit_prior_mono_kurz <- brm(
#   gen_attitude ~ mo(career_stage),
#   data = survey %>% filter(!is.na(gen_attitude)&
#                              !gen_attitude%in% c("Other","Indifferent")),
#   family = cumulative("probit"),
#   prior = priors_kurz, cores = 4, seed = 1,
#   control = list(adapt_delta = 0.95),
#   sample_prior = "only"       # ignores the likelihood entirely
# )
# 
# pp_check(fit_prior_mono_kurz, type = "bars", ndraws = 200) +
#   ggtitle("Prior predictive: implied response distribution")
# 

priors_fac <- c(
  prior(normal(0, 1),         class = "b"),
  prior(student_t(3, 0, 2.5), class = "Intercept")#,
)

fit_fac  <- brm(
  gen_attitude ~ career_stage,
  data = survey %>% filter(!is.na(gen_attitude)&
                             !gen_attitude%in% c("Other","Indifferent")),
  family = cumulative("probit"),
  prior = priors_fac, cores = 4, seed = 1,
  control = list(adapt_delta = 0.95) # career_stage unordered factor
)

fit_prior_fac <- update(fit_fac,
                         sample_prior = 'only')


pp_fac <- prior_post_draws(fit_fac, fit_prior_fac)
plot_prior_post(pp_fac)

pp_check(fit_prior_fac, type = "bars", ndraws = 200) +
  ggtitle("Prior predictive: implied response distribution")


# different variance model
# get divergent transitions on this one wi 0.95, trying
# with tighter adapt_delta
fit_ls <- brm(
  bf(gen_attitude ~ mo(career_stage), disc ~ mo(career_stage)),   
  data = survey %>% filter(!is.na(gen_attitude)&
                             !gen_attitude%in% c("Other","Indifferent")),
  family = cumulative("probit"),
  prior = priors, cores = 4, seed = 1,
  control = list(adapt_delta = 0.99)
)

fit_prior_ls <- update(fit_ls,
                        sample_prior = 'only')

pp_ls <- prior_post_draws(fit_ls, fit_prior_ls)
plot_prior_post(pp_ls)

pp_check(fit_prior_ls, type = "bars", ndraws = 200) +
  ggtitle("Prior predictive: implied response distribution")


#best one is highest
# ~2-4 differens is meaningful
# Model bakeoff, suggests more or less the fit_mono is the one we want
# ELPD differences + SE
loo_compare(add_criterion(fit_mono, "loo"), add_criterion(fit_fac, "loo"),
            add_criterion(fit_ls, "loo"))

# can add some other bits on why this is a good model
# but summary term is in the 
summary (fit_mono)

# interpret/viz
# predicted P(Y=k | x)
ps <-plot(conditional_effects(fit_mono, categorical = TRUE),
          plot = FALSE)
lapply(ps, \(p) p + theme_bw() +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)))

#model checking
# posteriors on priors

# bayes factor
bf <- bayestestR::bayesfactor_parameters(fit_mono)

# still to do, rhats, etc





########### want vs had
# Prep both 'training received' and 'training desired' dataframes
trainrec_df <- prep_select_all(df = survey %>%
                                 rename(ResponseId = response_id),
                               q = "training_received",
                               summarize = FALSE) %>% 
  dplyr::rename_with(.fn = ~ paste0("had_", .), .cols = -value) %>%
  mutate(response = "Yes", type = 'had')%>%
  rename(ResponseId = had_ResponseId)

traindes_df <- prep_select_all(df = survey %>%
                                 rename(ResponseId = response_id),
                               q = "training_desired", summarize = FALSE) %>% 
  dplyr::rename_with(.fn = ~ paste0("want_", .), .cols = -value)%>%
  mutate(response = "Yes", type = 'want')%>%
  rename(ResponseId = want_ResponseId)

# fill out the NO's by difference
train_binary <- bind_rows(trainrec_df, traindes_df)

not_response = expand_grid(ResponseId = unique(train_binary$ResponseId),
                           value  = unique(train_binary$value),
                           type = unique(train_binary$type)) %>%
  anti_join(., train_binary, by = c("ResponseId", "value", "type")) %>%
  mutate(response = "No")

# Join the selected and inferred not selected
train_binary <- bind_rows(train_binary, not_response) %>%
  rename(learning_mode = value) %>%
  mutate(response_numeric = ifelse(response == "Yes", 1, 0)) %>%
  mutate(learning_mode = factor(learning_mode))

library (lme4)
## Check structure
dplyr::glimpse(train_binary)

#relevel so the intercepts reflect what people WANT to learn
train_binary$type <- relevel(factor(train_binary$type), ref = "want")

# random intercepts as subjects not distinct
m_no_int_want_ref_baseline <- glmer(
  response_numeric ~ 0 + learning_mode + learning_mode:type + (1 | ResponseId),
  family = binomial,
  data = train_binary,
  #may not need this funkiness if working with real data where the ranefs should be more estimable
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# this one is probably more correct if it's not singular;
# would assume some people just overall WANT more training and/or HAD
# more training, but not that the same people want/have more training
m_no_int_want_ref_random <- glmer(
  response_numeric ~ 0 + learning_mode + learning_mode:type + (1 + type | ResponseId),
  family = binomial,
  data = train_binary,
  #may not need this funkiness if working with real data where the ranefs should be more estimable
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

#AIC lower is better
anova(m_no_int_want_ref_baseline, m_no_int_want_ref_random)

pred <- ggeffects::predict_response(m_no_int_want_ref_random,
                         terms = c("type", "learning_mode"),
                         type = "fixed",
                         bias_correction = TRUE)   # population-level, random effects at 0

plot(pred) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# because the ggeffects melts down on the no intercept model rerun
# even though the params are less interpretable this way
m_want_ref_random <- glmer(
  response_numeric ~ learning_mode*type + (1 + type | ResponseId),
  family = binomial,
  data = train_binary,
  #may not need this funkiness if working with real data where the ranefs should be more estimable
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

pred <- ggeffects::predict_response(m_want_ref_random,
                                    terms = c("type", "learning_mode"),
                                    type = "fixed",
                                    bias_correction = TRUE)   # population-level, random effects at 0
#still broken I don't think ggeffects is working correctly on this
plot(pred) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# type effect within each mode — what the 0+ coding gave you directly
emmeans(m_want_ref_random, ~ type | learning_mode) |> contrast("pairwise")

# whether modes differ in their type effect — the interaction proper
# this is less intepretable
emmeans(m_want_ref_random, ~ type | learning_mode) |> contrast("pairwise") |>
  contrast("pairwise", by = NULL)

