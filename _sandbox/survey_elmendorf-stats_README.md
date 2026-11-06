# survey_elmendorf-stats.r

Frequentist models of the survey's ordinal and select-all questions against
demographic predictors, with sensitivity checks, an exclusion table, and the
associations among the demographics. Everything here is **exploratory** (see
[Inference](#inference)).

Run from the repository root:

```r
source("_sandbox/survey_elmendorf-stats.r")
```

`rerun_mods <- FALSE` (top of the script) reloads the cached select-all fits from
`fits_ordinal/` and fits only the cells with no cached file, so it also resumes an
interrupted run. `TRUE` refits every cell and overwrites the cache. `FALSE` does not
check whether a cached fit is out of date, so after changing the data or the model
code use `TRUE`, or delete the affected `res_<tag>_subj.rds` and `res_<tag>_adj.rds`
files to refit only those cells.

A full refit took about 4.5 hours on a 2-core machine, mostly the
career-stage-adjusted and equal-spacing select-all fits (about 10-75 min per cell).
A run from the cache takes about a minute, including the ordinal models, which
refit on every run.

**Changing figures without a refit:** figures for ordered predictors are redrawn
from the cached results on every run. Figures for categorical predictors (gender,
work sector) are read from the cache, so only their font sizes update; other
styling changes to them need a refit. Font sizes are set per question in
`sa_specs`: `x_text_size` for the x-axis labels (11 pt) and `x_title_size` for the
x-axis title (14 pt).

---

## Methods

### Data and coding

- **Input:** `data/1_tidied-responses_no-free-text.csv`, restricted to the target
  audience upstream in `01_tidy.r`.
- **Frequency scales:** coded so higher means more use. AI-use frequency runs
  Never → Yearly → Monthly → Weekly → Daily. Data-science frequency runs
  "I do not use data science…" → Yearly → … → Daily.
- **Index of opportunity (IO):** the sum of four binary indicators, each scored 1
  for the more advantaged answer:
  - not a current caregiver (past caregivers count as 1)
  - not neurodivergent
  - not first-generation
  - White only (multiracial respondents count as 0)

  IO is missing if any one of the four is missing or "prefer not".
- **Work sector:** respondents whose role is Student are recoded to "Student",
  then excluded together with "Other". This analysis covers professionals only.
- **Gender:** "Prefer not", "Prefer to self-identify" and "Non-binary" are
  excluded. Non-binary has too few respondents in any one career stage to
  estimate.
- **General attitude:** analysed in two versions.
  - *Primary* (`gen_attitude`): "Indifferent" and "Other" excluded.
  - *Sensitivity* (`gen_attitude_mix`): "Indifferent" merged into "A mix of
    caution and enthusiasm"; "Other" excluded. In these figures, the
    Indifferent respondents show as a darker segment on top of the "A mix" bar.
- **Small groups:** levels with fewer than 20 respondents are handled by predictor
  type.
  - *Categorical predictors:* the level is dropped for that cell, because it
    would carry its own unstable estimate.
  - *Ordered predictors:* the level stays in the slope model and the figure,
    because one slope pools across levels. It is left out of the equal-spacing
    test only. Very small groups can still pull on a slope, for example the 4
    "Very restrictive/opposed" respondents on institutional policies.
- **Exclusion table:** every exclusion rule and its n is written to
  `exclusions.csv`.

### Ordinal outcomes: AI-use frequency and general attitude

These are cumulative link models with a probit link (`ordinal::clm`). On the
probit scale, coefficients are in units of the latent residual SD, so they can
be read as Cohen's *d*-like effects (Gambarota & Altoè, 2024; see also
Christensen's `ordinal` vignette). For ordered predictors this is a shift per
one-level step, not a two-group difference. Because the latent scale changes with
the predictors in the model, coefficients are not directly comparable between the
unadjusted and adjusted fits (Karlson et al., 2012).

- **Categorical predictors** (gender, work sector) are fitted as factors. The
  main effect is tested by likelihood-ratio test (LRT). Pairwise differences
  are reported as Tukey-adjusted contrasts on the latent scale.
- **Ordered predictors** (career stage, IO, data-science frequency, policies,
  AI-use frequency) are fitted as a single slope on level position (1..K), which
  assumes equal spacing between levels.
  - The assumption is tested by LRT of the slope model against a factor model,
    on the same respondents, with levels of n < 20 removed.
  - If equal spacing is rejected, the figure shows the factor model instead.
  - The test has **low power** (some groups hold only 20-40 people), so a
    non-significant result is weak support for equal spacing.
- **Assumption checks** for every model:
  - proportional odds: a partial proportional-odds model, compared by LRT
  - constant latent variance: a scale model, compared by LRT

  Both are fitted explicitly, so their convergence is recorded. A p-value from
  an invalid auxiliary fit is set to `NA` and flagged in `conv_flag`.
- **General attitude × AI-use frequency** is *not* modelled, because both are
  outcomes and the direction can't be identified. It is reported as a polychoric
  correlation in `demographic_associations.csv`.

### Select-all outcomes: opportunities, challenges, reasons for use

Opportunities and challenges are crossed with every demographic. Reasons for use
are crossed with AI-use frequency only.

- **Data layout:** one row per respondent × option.
  - "None of the above" is removed, along with respondents who ticked it plus
    another option.
  - Respondents who ticked nothing are treated as having skipped the question.
  - Within each cell, items endorsed by < 3% or > 97% are not modelled, nor are
    items separated along x.
- **Model:** a binomial GLMM (`lme4::glmer`) with `question * predictor` and a
  respondent random intercept. For reasons, positive and negative reasons get
  separate, correlated respondent random effects.
- **Tests:**
  - *Item × predictor interaction* by LRT: does the predictor's effect differ
    between items?
  - *Omnibus test* on each person's total endorsements: do people tick more or
    fewer boxes across the predictor? For opportunities and challenges this is a
    binomial GLM, switched to quasibinomial when overdispersed. For reasons it
    is a GLM with cluster-robust Wald tests, which also tests whether the
    positive/negative balance shifts.
- **Item-level detail:**
  - Categorical predictors: letter displays, multivariate-t adjusted *within
    each item*.
  - Ordered predictors: each item's slope as an odds ratio per one-level step,
    FDR-adjusted across the items of that figure. The figures show this as
    the line style; the odds ratios are in `item_or` in the summary table.
- **Equal spacing:** for ordered predictors, the slope model is compared with a
  factor model by LRT, jointly across all items. This is a **diagnostic only**; the
  slope model is still the one plotted and reported. A small p means the gaps
  between levels differ for at least some items, but not which items or which
  levels. In that cell, read an odds ratio as an average trend rather than a steady
  change per step, and use the grey bars to see the pattern. A dotted line can hide
  a real difference if an item rises and then falls.
- **Figures** show model estimates for a typical respondent (random effect = 0).
  By construction these sit further from 50% than the observed proportions, so
  gaps between groups look larger on the points than in the grey bars. State
  this in the paper's figure legends.

### Sensitivity to career stage

Every model, except those where the predictor is career stage, is refitted with
career stage added as a covariate, entered as a slope on `career_stage_value`
(1..4). In select-all models it is allowed to vary by item.

The summary tables report:
- the adjusted test (`*_adj`)
- the **unadjusted test on the same respondents** (`*_cc`)

Comparing the two separates the effect of the adjustment from the effect of
losing the respondents who have no career stage. The figures always show the
unadjusted models.

### Associations among demographics

- **Ordinal × ordinal:** two-step polychoric correlation (`polycor::polychor`),
  with Wald CIs and a test of bivariate normality.
- **Any pair involving a nominal variable** (gender, work sector):
  bias-corrected Cramér's V (`effectsize::cramers_v(adjust = TRUE)`; Bergsma
  2013), with simulated χ² p-values.

Both heatmaps leave out AI use frequency because it is an outcome here; its
associations with the demographics are in `demographic_associations.csv`.

Section 7 also prints cross-tabs of each demographic against career stage to
the console (not saved).

### References

- Christensen, R. H. B. Cumulative link models for ordinal regression with the R
  package ordinal. Vignette for the `ordinal` package.
  <https://CRAN.R-project.org/package=ordinal>
- Gambarota, F., & Altoè, G. (2024). Ordinal regression models made easy: A
  tutorial on parameter interpretation, data simulation and power analysis.
  *International Journal of Psychology, 59*(6), 1263-1292.
  <https://doi.org/10.1002/ijop.13243>
- Karlson, K. B., Holm, A., & Breen, R. (2012). Comparing regression coefficients
  between same-sample nested models using logit and probit: A new method.
  *Sociological Methodology, 42*(1), 286-313.
  <https://doi.org/10.1177/0081175012444861>

### Inference

There is **no correction for multiple testing across cells**. The grids hold
about 30 cells, each with several tests, so some p < .05 results are expected by
chance. The only adjustments are within a cell: letter displays within an item,
and FDR across the items of one figure. Report p-values as strength of evidence,
not as confirmatory tests.

---

## Files created

Created when the script runs; git-ignored, not committed.

```
graphs/
└── analysis_figs_ordinal/
    │   ── select-all figures (13) ──────────────────────────────────────────
    ├── opportunity_by_<demo>_subj.jpg     P(yes) per item by demographic; 6 demos*
    ├── challenges_by_<demo>_subj.jpg      same for challenges; 6 demos*
    ├── reasons_by_ai_use_freq_subj.jpg    reasons for use by AI-use frequency
    ├── select_all_summary.csv             one row per select-all cell (see below)
    │
    │   ── ordinal figures (18) ─────────────────────────────────────────────
    ├── ai_use_freq_by_<demo>.jpg          P(category) by demographic; 6 demos**
    ├── gen_attitude_by_<demo>.jpg         general attitude, Indifferent dropped; 6 demos***
    ├── gen_attitude_mix_by_<demo>.jpg     general attitude, Indifferent = "A mix"; 6 demos***
    ├── ordinal_summary.csv                one row per ordinal cell (see below)
    │
    │   ── exclusions and associations ─────────────────────────────────────
    ├── exclusions.csv                     every exclusion rule with its n
    ├── demographic_associations.csv       every demographic pair + attitude x AI use
    ├── demographic_assoc_polychoric.jpg   heatmap: ordinal x ordinal demographics, without AI use frequency
    └── demographic_assoc_cramersv.jpg     heatmap: nominal x every other demographic, without AI use frequency

fits_ordinal/                              cached select-all fits (read when rerun_mods = FALSE)
├── res_<sa>_by_<demo>_subj.rds            main fit_select_all() result, one per cell (13)
└── res_<sa>_by_<demo>_adj.rds             career-stage-adjusted fit; not for career stage (11)
```

`*` `<demo>` = `career_stage`, `gender`, `prof_work_sector`, `IO`, `ds_freq`,
`policies`.
`**` the same six demographics, minus `ai_use_freq` itself.
`***` the six demographics excluding `ai_use_freq`, which is in the association
table instead.

`<sa>` = `opportunity`, `challenges`, `reasons`. The `_subj` suffix marks the
typical-respondent (random effect = 0) predictions.

### Reading the figures

| Element | Meaning |
|---|---|
| grey bars | observed proportion |
| red points / line | model estimate (typical respondent, for select-all) |
| **Ordinal** figures | |
| panels | outcome categories |
| number above each bar | respondents in that group *who chose that category* |
| line style | solid if the slope's p < .05, dotted otherwise (p is in the summary table) |
| **Select-all** figures | |
| panels | items, most-endorsed first |
| number above each bar | respondents in that group |
| letters (categorical x) | groups sharing a letter don't differ within that item; no letters = none differ |
| line style (ordered x) | solid if the item's slope has p < .05, FDR-adjusted across the items of that figure; dotted otherwise. The odds ratios are in `item_or` in `select_all_summary.csv` |
| † on a title | "(e.g. …)" examples removed; full wording is in the fit's `$labels_full` |

Career-stage levels show as prep / early / mid / mature, and "I do not use data
science…" as "no use", in the figures only.

---

## Tables

p-values are unadjusted across cells throughout (see [Inference](#inference)).
`NA` means not applicable or not run; the note columns say why.

### `ordinal_summary.csv` — one row per ordinal outcome × demographic

| Column | Contents | How to interpret |
|---|---|---|
| `cell`, `tag` | `<outcome>__<demo>` and the figure's file stem | identifies the model |
| `error` | error message if the cell failed | `NA` = fitted; otherwise ignore the row |
| `n` | respondents in the main model | |
| `conv` | convergence code of the main fit | `0` clean; `1` imprecise but usable; `-3` **invalid, do not interpret** |
| `p_main` | LRT p for the predictor (slope for ordered x, factor for categorical) | the headline test |
| `dropped` | predictor levels dropped for n < 20 (categorical x) | those groups aren't in the model or figure |
| `p_po` | proportional-odds check | small: the predictor's effect differs across outcome thresholds, so one coefficient oversimplifies; `NA` if the check fit was invalid |
| `p_scale` | scale check | small: the predictor changes the spread of the latent outcome, not only its location |
| `n_adj` | respondents with a career stage | n for the `_cc` and `_adj` columns |
| `p_main_cc` | unadjusted p on those `n_adj` respondents | compare with `p_main` to see the effect of losing respondents with no career stage |
| `p_main_adj` | p with career stage added as a covariate | compare with `p_main_cc`: similar = no sign that career stage changes the result, though this does not rule out confounding; much larger = the effect may be career-stage composition |
| `b_main` | slope, all respondents (ordered x only) | latent SD change per one-level step; positive = higher outcome category |
| `b_cc`, `b_adj` | slope unadjusted / adjusted, on `n_adj` respondents | not directly comparable: adding a covariate rescales probit coefficients, tending to move them away from zero (Karlson et al., 2012). A shrinking slope suggests part of the association runs through career stage; a stable slope does not rule that out |
| `adj_note` | why the adjustment is missing | "not run (x is career stage)" or the failure message |
| `conv_flag` | every fit that was not clean: main, PO check, scale check, adjusted | empty = all clean; "INVALID" entries explain an `NA` p |
| `model_used` | `slope`, `factor` or `categorical` | what the figure shows; `factor` = equal spacing was rejected |
| `spacing_df` | df of the equal-spacing LRT (K − 2) | |
| `spacing_p` | equal-spacing LRT p (ordered x only) | small: levels are not evenly spaced in effect. Low power, so a large p is weak reassurance |
| `spacing_n` | respondents in the spacing test | smaller than `n` when sparse levels were dropped |
| `spacing_dropped` | levels dropped from the spacing test, with n | still in the slope model |
| `spacing_note` | why the spacing test wasn't run | empty = it was run |
| `p_factor` | factor-model p on the reduced data | only when `model_used = factor` |
| `p_factor_adj` | the same, adjusted for career stage | only when `model_used = factor` |

### `select_all_summary.csv` — one row per select-all question × demographic

| Column | Contents | How to interpret |
|---|---|---|
| `cell`, `tag` | `<sa>__<demo>` and the figure/fit file stem | identifies the model |
| `n_sep`, `n_empty` | separated / empty cells found before fitting | items or cells that could not be estimated |
| `dropped` | items not modelled (endorsement rate or separation) | report these as plain percentages instead |
| `error` | failure message | `NA` = fitted |
| `n_obs`, `n_id` | respondent × item rows; respondents | |
| `singular` | random-effect variance estimated at the boundary | `TRUE` is common and usually harmless here; note it |
| `conv_flag` | glmer warnings: singular, convergence messages | empty = clean |
| `p_total` | omnibus test: does the *number* of boxes ticked change with x? | quasibinomial F-test if overdispersed; Wald test for reasons |
| `p_valence` | reasons only: does the positive/negative balance shift with x? | small = AI-use frequency shifts people towards positive (or negative) reasons |
| `p_item_int` | LRT of item × x | small = the predictor matters more for some items than others; read the figure for which |
| `item_or` | each item's odds ratio per one-level step in x, as `item: OR 0.55 (95% CI 0.37-0.81, p = 0.038); ...` in figure order (ordered x only; `NA` for categorical x) | OR > 1 = endorsement rises with x, for a typical respondent. p is FDR-adjusted across the items of that cell and sets the figure's line style. The CI is unadjusted, so it can exclude 1 on a dotted line |
| `spacing_df`, `spacing_p` | equal-spacing LRT, slope vs factor, across all items (ordered x only) | **diagnostic only**; small p = the slope is a simplification for this predictor, so read `item_or` as an average trend and check the grey bars; low power, so a large p is weak reassurance |
| `spacing_n` | respondents in the spacing test | |
| `spacing_dropped` | levels with < 20 respondents dropped from the spacing test | |
| `spacing_note` | glmer flags of the factor fit, the reason the test wasn't run, or "n/a: categorical x" | |
| `n_id_adj` | respondents with a career stage | n for the `_cc` and `_adj` columns |
| `p_total_cc`, `p_total_adj` | omnibus p, unadjusted / adjusted, on `n_id_adj` respondents | compare the two, as for the ordinal table |
| `p_valence_cc`, `p_valence_adj` | valence p, unadjusted / adjusted (reasons only) | |
| `p_item_int_cc`, `p_item_int_adj` | item × x p, unadjusted / adjusted | |
| `item_or_adj` | `item_or` from the career-stage-adjusted fit, on `n_id_adj` respondents (`NA` for categorical x and when x is career stage) | does x affect this item among people at the same career stage? Compare p-values with `item_or`, not ORs: adding a covariate rescales them, and the smaller sample also raises p. Line styles follow `item_or` |
| `adj_note` | glmer flags of the adjusted fit, "not run (x is career stage)", or the failure message | |

### `exclusions.csv` — one row per exclusion rule

| Column | Contents | How to interpret |
|---|---|---|
| `scope` | `data`, `variable`, `select-all`, `ordinal cell`, `select-all cell` | where the rule applies: whenever the variable or question is used, or in one cell only |
| `variable` | the variable or question affected | |
| `cell` | the grid cell, for cell-level rules | `NA` for variable-level rules |
| `reason` | the rule | e.g. "level excluded on substantive grounds", "item endorsed by < 3%" |
| `level` | the level, item or IO component affected | |
| `n` | how many were excluded | `NA` for items |
| `unit` | `respondents`, `items` or `cell` | |

Rows **do not sum**, because one person can be missing several variables, and
the four IO-component rows overlap. The first row is the number of rows read
(after the upstream target-audience filter).

### `demographic_associations.csv` — one row per pair

| Column | Contents | How to interpret |
|---|---|---|
| `var1`, `var2` | the pair | |
| `measure` | `polychoric` (both ordinal) or `cramers_v_bc` (either nominal) | |
| `estimate` | ρ (−1..1, signed) or bias-corrected V (0..1, strength only) | rough guide: ~.1 weak, ~.3 moderate, ≥ .5 strong; **don't compare ρ and V directly** |
| `se` | standard error (polychoric only) | |
| `ci_low`, `ci_high` | 95% CI | Wald CI clipped to ±1 for ρ; two-sided CI for V |
| `p_value` | test of no association | Wald for ρ; simulated χ² for V |
| `n` | respondents with both variables (pairwise complete) | differs by pair |
| `normality_p` | test of bivariate normality behind ρ | small: ρ's latent-normal model fits poorly, so read ρ as "associated" rather than as an exact correlation |
| `error` | failure message | `NA` = computed |
| `set` | `demographics` or `outcome pair` | the outcome pair (gen_attitude × AI-use frequency, both versions) is in the table only, not the heatmaps |

Direction for ρ: ordered demographics run low → high (career stage prep →
mature; frequencies less → more use; IO less → more advantaged). A positive ρ
means the two increase together.
