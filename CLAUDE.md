# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Xing — Prognostic Scoring in HRS-AKI Patients Treated with Terlipressin**

Sub-project of the HARMONY Terlipressin Registry. Compares discriminative ability of MELD, MELD-Na, MELD 3.0, and CLIF-C scores for predicting 90-day mortality and HRS treatment response in terlipressin-treated patients.

Published reference: *Aliment Pharmacol Ther* 2025 — "MELD 3.0 Improves Risk Stratification and Mortality Prediction in Hepatorenal Syndrome" (PDF in project root).

## Analysis Architecture

The primary analysis file is `analysis.qmd` (converted from `old/code/Xing new.Rmd`). It sources the shared pipeline first, then adds project-specific variables.

### Key Analytical Steps

1. **Score computation at terlipressin day 0:** Day-0 MELD, MELD-Na, MELD 3.0, and CLIF-C scores recalculated from day-0 labs (not admission labs)
2. **Discharge MELD scores:** dc_meld, dc_meld_na, dc_meld_3 computed from discharge labs
3. **Survival recomputed from terlipressin initiation:** Time-to-event anchored at terlipressin start (day 0), not hospital admission — variables suffixed `_init`
4. **Cox proportional hazards:** One model per score → 90-day mortality
5. **Time-dependent ROC (timeROC):** AUC at days 7, 15, 30, 60, 89; pairwise comparisons via `timeROC::compare`
6. **Logistic regression ROC:** Each score predicting `hrs_responders_cat_2`; DeLong tests comparing each to MELD 3.0
7. **Cross-tabulation:** MELD 3.0 vs MELD-Na categories among decedents

### Median Imputation

This project median-imputes missing day-0 labs (`bili_terli_day0`, `scr_terli_day0`, `he_terli_day0`, `alb_terli_day0`, `inr_terli_day0`, `map_terli_day0_time0`, `norepi_terli_day0`, `fio2_terli_day0`, `spo2_terli_day0`, `wbc_terli_day0`, `age`) and discharge labs (`dc_scr`, `dc_tbili`, `dc_inr`, `dc_sna`, `dc_albumin`) before score calculation.

## Project-Specific Derived Variables

| Variable | Definition |
|:---------|:-----------|
| `map_terli_day0` | Median of 4 MAP readings on day 0 (zeros → NA first) |
| `clif_c_score_day0` | CLIF-C score using day-0 labs via local `calculate_clif_c_score()` |
| `death_hosp` | In-hospital death (disposition == 5) |
| `days_to_dc_init` | Days from terli initiation to discharge |
| `days_to_los_init` | Days from terli initiation to loss of follow-up |
| `days_to_lastencounter_init` | Days from terli initiation to last encounter |
| `days_to_after_dc_death_init` | Days from terli initiation to post-discharge death |
| `death_status_check` | Death flag (disposition 5 or dc_death_90days == 1) |
| `time_to_death_check` | Raw time-to-death from terli initiation |
| `death_status_90days_init` | 90-day mortality status (from terli initiation) |
| `time_to_death_90days_init` | 90-day censored survival time (from terli initiation) |
| `dc_meld` | Discharge MELD score |
| `dc_meld_na` | Discharge MELD-Na score (NA if MELD ≤ 11) |
| `dc_meld_3` | Discharge MELD 3.0 score |
| `day0_meld` | Day-0 MELD (recalculated from day-0 labs) |
| `day0_meld_na` | Day-0 MELD-Na (recalculated from day-0 labs) |
| `day0_meld_3` | Day-0 MELD 3.0 (recalculated from day-0 labs) |
| `female_factor` | Sex adjustment for MELD 3.0 (1.33 if female, 0 if male) |
| `meld_3_cat3` | MELD 3.0 categories: <25, 25-35, >35 (decedents only) |
| `meld_na_cat3` | MELD-Na categories: <25, 25-35, >35 (decedents only) |

## Code Visibility Convention

- **Setup chunk** (libraries + shared pipeline): `#| include: false` — never shown
- **All other code chunks**: `#| code-fold: true` — visible but collapsed by default
- Analysis `.qmd` sets `echo: true` in its YAML frontmatter to override the site-wide `echo: false`

## Additional R Dependencies (beyond shared)

timeROC, pROC, gmodels, DescTools, sqldf, MatchIt, splines
