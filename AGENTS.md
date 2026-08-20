# AI-in-a-day Perspective Survey — Agent Memory

## Project overview
- R-based survey analysis project for the *"AI in a Day" Working Group*.
- Development version (`v0.0.0.900`); conventions may still evolve.
- PIs: Nick J Lyon, Li Kui, Nate Emery, Sarah Elmendorf, Stevan Earl.

## Key files and folders

| Path | Purpose |
|------|---------|
| `-setup.r` | Source-able setup: clears env, creates `data/`, `graphs/`, `graphs_fake/`. Always source first in new top-level scripts. |
| `01_tidy.r` | Tidies raw Qualtrics exports into analysis-ready CSVs. |
| `02_viz-single-qs.r` | Produces single-question visualizations. |
| `data/` | Git-ignored output folder for raw-ish and processed data. Includes `01_tidied-responses.csv`, `01_question-lookup-table.csv`, and the de-identified `broken-row-survey-data.csv`. Raw Qualtrics exports are not committed; must be placed here manually. |
| `graphs/` | Git-ignored output folder for real-data figures. |
| `graphs_fake/` | Git-ignored output folder for figures from de-identified broken-row data. |
| `tools/` | Reusable `source`-able R function scripts. Function names match filenames with hyphens mapped to underscores (e.g., `prep_select-all.r` → `prep_select_all()`). Load with `purrr::walk(dir("tools", pattern = "*.r", full.names = TRUE), source)`. |
| `_sandbox/` | Informal/exploratory scripts. Not required to be FAIR; run at your own risk. Naming: `<descriptive>_<author>-explore.r`. |
| `_ancillary/` | FAIR-ish helper scripts not needed by all users (e.g., de-identification, fake-data download). |

## Naming conventions

- **Scripts:**
  - Top-level: `01_<verb>.r`, `02_<verb>.r`
  - Setup: `-setup.r`
  - Tools: `<verb>_<select-type>.r` (e.g., `graph_select-one.r`, `prep_select-all.r`)
  - Sandbox: `<descriptive>_<author>-explore.r`
- **Generated data files:** `01_<descriptive>.csv`
- **Graph outputs:** `survey-02_<category>_<question>.png` where category is one of `perspective_`, `job_`, `demographics_`.
- **Data columns:**
  - Qualtrics-style names: `AIUse_Freq`, `DS_Freq`, `Gen_Attitude`, `Policies`, `Career_Stage`, etc.
  - Numeric coded order columns: suffix `__value`.
  - Free-text columns: suffix `_TEXT` (dropped in de-identified data).
  - Select-all questions: semicolon-delimited strings.

## Coding conventions

- Load libraries with `librarian::shelf(...)`.
- Start main scripts with:
  1. `source(file.path("-setup.r"))`
  2. `rm(list = ls()); gc()`
  3. `purrr::walk(dir(path = "tools", pattern = "*.r", full.names = TRUE), source)`
- Use section banner comments ending in `----`.
- Visualizations use `ggplot2` + `supportR::theme_lyon()`.
- Common figure sizes: 7×7 in for single-select, 15×15 in for select-all bar charts.
- Use the `real_data` toggle in scripts:
  - `TRUE` → read `data/01_tidied-responses.csv`, save to `graphs/`
  - `FALSE` → read `data/broken-row-survey-data.csv`, save to `graphs_fake/`

## Important rules

- Never commit raw or processed data, graphs, or `.Rproj` files. They are git-ignored.
- Place new reusable functions in `tools/` with names matching their filenames.
- Put exploratory / non-FAIR scripts in `_sandbox/`.
- Put helper scripts not intended for general use in `_ancillary/`.
