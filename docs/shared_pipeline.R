## -----------------------------------------------------------------------------
#| label: libraries
#| include: false
library(tidyverse)
library(readxl)


## -----------------------------------------------------------------------------
#| label: load-data
#| include: false
master <- read_excel("/Users/to909/Desktop/Terlipressin projects/Terlipressin/data/final_master_01282026.xlsx")


## -----------------------------------------------------------------------------
#| label: fn-child-pugh
#| include: false
child_pugh_score <- function(tbili, albumin, inr, ascites, hegrade) {
  childpugh_bili <- ifelse(tbili < 2, 1, ifelse(tbili >= 2 & tbili <= 3, 2, 3))
  childpugh_alb <- ifelse(albumin > 3.5, 1, ifelse(albumin >= 2.8 & albumin <= 3.5, 2, 3))
  childpugh_inr <- ifelse(inr < 1.7, 1, ifelse(inr >= 1.7 & inr <= 2.3, 2, 3))
  childpugh_ascites <- ifelse(ascites == 1, 1, ifelse(ascites == 2, 2, 3))
  childpugh_he <- ifelse(hegrade == 0, 1, ifelse(hegrade %in% c(1, 2), 2, 3))
  childpugh_total <- childpugh_bili + childpugh_alb + childpugh_inr + childpugh_ascites + childpugh_he
  return(childpugh_total)
}


## -----------------------------------------------------------------------------
#| label: fn-estimate-creatinine
#| include: false
estimate_creatinine <- function(egfr, age, sex) {
  if (sex == 1) { # male
    A <- 0.9
    sex_factor <- 1
  } else { # female
    A <- 0.7
    sex_factor <- 1.012
  }
  estimated_creatinine <- function(Scr) {
    B <- ifelse(Scr <= A, ifelse(sex == 1, -0.302, -0.241), -1.2)
    calculated_egfr <- 142 * (Scr / A)^B * (0.9938^age) * sex_factor
    return((calculated_egfr - egfr)^2)
  }
  result <- optimize(estimated_creatinine, interval = c(0.1, 2.5))
  return(result$minimum)
}


## -----------------------------------------------------------------------------
#| label: fn-clif-c
#| include: false
calculate_clif_c_score <- function(tb_admit, peak_creatinine, he_grade_admission, inr_admit, map, pressor, fio2_admission, spo2_admission, age_admission, wbc_admit) {
  clif_score <- 0

  clif_score <- clif_score + case_when(
    is.na(tb_admit) ~ 0, tb_admit < 6 ~ 1, tb_admit >= 6 & tb_admit < 12 ~ 2, tb_admit >= 12 ~ 3, TRUE ~ 0)

  clif_score <- clif_score + case_when(
    is.na(peak_creatinine) ~ 0, peak_creatinine < 2 ~ 1, peak_creatinine >= 2 & peak_creatinine < 3.5 ~ 2, peak_creatinine >= 3.5 ~ 3, TRUE ~ 0)

  clif_score <- clif_score + case_when(
    is.na(he_grade_admission) ~ 0, he_grade_admission == 1 ~ 1, he_grade_admission %in% c(2, 3) ~ 2, he_grade_admission %in% c(4, 5) ~ 3, TRUE ~ 0)

  clif_score <- clif_score + case_when(
    is.na(inr_admit) ~ 0, inr_admit < 2 ~ 1, inr_admit >= 2 & inr_admit < 2.5 ~ 2, inr_admit >= 2.5 ~ 3, TRUE ~ 0)

  clif_score <- clif_score + case_when(
    is.na(map) ~ 0, map >= 70 & pressor != 1 ~ 1, map < 70 & pressor != 1 ~ 2, !is.na(map) & pressor == 1 ~ 3, TRUE ~ 0)

  if (!is.na(fio2_admission)) {
    ratio <- spo2_admission / fio2_admission
    clif_score <- clif_score + case_when(
      ratio > 357 ~ 1, ratio > 214 & ratio <= 357 ~ 2, ratio <= 214 ~ 3, TRUE ~ 0)
  }

  CLIF_C_Score <- 10 * (0.33 * clif_score + 0.04 * age_admission + 0.63 * log(wbc_admit) - 2)
  return(CLIF_C_Score)
}


## -----------------------------------------------------------------------------
#| label: fn-organ-failures
#| include: false
calculate_organ_failures <- function(bili_terli_day0, he_terli_day0, inr_terli_day0,
                                     map_terli_values, fio2_terli_day0,
                                     spo2_terli_day0, scr_terli_day0) {
  liver_f <- 0; brain_f <- 0; coagulation_f <- 0
  circulatory_f <- 0; respiratory_f <- 0; kidney_f <- 0

  if (!is.na(bili_terli_day0) && bili_terli_day0 >= 6) liver_f <- 1
  if (!is.na(he_terli_day0) && he_terli_day0 > 1) brain_f <- 1
  if (!is.na(inr_terli_day0) && inr_terli_day0 >= 2) coagulation_f <- 1
  if (!is.na(map_terli_values) && map_terli_values < 70) circulatory_f <- 1
  if (!is.na(fio2_terli_day0) && fio2_terli_day0 != 0 &&
      !is.na(spo2_terli_day0) && (spo2_terli_day0 / fio2_terli_day0) <= 357) respiratory_f <- 1
  if (!is.na(scr_terli_day0) && scr_terli_day0 >= 1.5) kidney_f <- 1

  return(sum(liver_f, brain_f, coagulation_f, circulatory_f, respiratory_f, kidney_f))
}


## -----------------------------------------------------------------------------
#| label: fn-ckd-epi
#| include: false
ckd_epi_gfr <- function(creat, male, age) {
  k <- ifelse(male == 1, 0.9, 0.7)
  a <- ifelse(male == 1, -0.302, -0.241)
  m <- ifelse(male == 1, 1, 1.012)
  ratio <- creat / k
  gfr <- 142 * (pmin(ratio, 1))^a * (pmax(ratio, 1))^-1.2 * 0.9938^age * m
  return(gfr)
}


## -----------------------------------------------------------------------------
#| label: fn-dosechange
#| include: false
create_dosechange_day <- function(df) {
  df$dosechange_day <- apply(df, 1, function(row) {
    for (i in 2:13) {
      varname <- paste0("dosechange_terli_day", i)
      if (varname %in% names(df) && !is.na(row[[varname]]) && row[[varname]] == 1) {
        return(i)
      }
    }
    return(NA)
  })
  return(df)
}


## -----------------------------------------------------------------------------
#| label: fn-create-table-one
#| include: false
# Custom render for continuous: Median [Q1, Q3]
render.continuous <- function(x, ...) {
  with(
    table1::stats.apply.rounding(table1::stats.default(x, ...), digits = 2),
    c("", `Median [Q1, Q3]` = sprintf("%s [%s, %s]", MEDIAN, Q1, Q3))
  )
}

# P-value function: tracks which test was used via global `test_used`
pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    n_groups <- nlevels(droplevels(g))
    if (n_groups == 2) {
      p <- tryCatch(wilcox.test(y ~ g)$p.value, error = function(e) NA)
      test_used <<- c(test_used, "wilcox")
    } else {
      p <- tryCatch(kruskal.test(y ~ g)$p.value, error = function(e) NA)
      test_used <<- c(test_used, "kruskal")
    }
  } else {
    tbl <- table(y, g)
    if (nrow(tbl) < 2 || ncol(tbl) < 2) {
      p <- NA
      test_used <<- c(test_used, "none")
    } else if (any(tbl < 5)) {
      p <- tryCatch(fisher.test(tbl)$p.value, error = function(e) NA)
      test_used <<- c(test_used, "fisher")
    } else {
      p <- tryCatch(chisq.test(tbl)$p.value, error = function(e) NA)
      test_used <<- c(test_used, "chisq")
    }
  }
  pval <- if (is.na(p)) "" else sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001))
  c("", pval)
}

# Initialize global test tracker
test_used <- character(0)

create_table_one <- function(all_vars, data, strata, output,
                             cat_vars = NULL, exact = NULL) {
  data[[strata]] <- factor(data[[strata]])

  if (!is.null(cat_vars)) {
    for (v in intersect(cat_vars, names(data))) {
      data[[v]] <- factor(data[[v]])
    }
  }

  all_vars <- intersect(all_vars, names(data))
  f <- as.formula(paste("~", paste(all_vars, collapse = " + "), "|", strata))

  test_used <<- character(0)

  tbl <- table1::table1(f, data = data, overall = "Overall",
                        render.continuous = render.continuous,
                        extra.col = list("P-value" = pvalue))

  numerical_vars <- if (!is.null(cat_vars)) setdiff(all_vars, cat_vars) else all_vars
  T1 <- tableone::CreateTableOne(
    vars = all_vars, strata = strata, data = data,
    factorVars = cat_vars, addOverall = TRUE, includeNA = FALSE
  )
  T1_print <- print(T1, showAllLevels = TRUE, exact = exact,
                    nonnormal = numerical_vars, missing = TRUE,
                    quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
  write.csv(T1_print, file = output, row.names = TRUE)

  tests <- setdiff(unique(test_used), "none")
  labels <- c(
    wilcox  = "Mann-Whitney U test (continuous, 2 groups)",
    kruskal = "Kruskal-Wallis test (continuous, &ge;3 groups)",
    fisher  = "Fisher&rsquo;s exact test (categorical, any cell &lt; 5)",
    chisq   = "Chi-squared test (categorical)"
  )
  footnote <- paste(na.omit(labels[tests]), collapse = "; ")

  htmltools::tagList(
    tbl,
    htmltools::tags$p(
      style = "font-size: 0.85em; color: #555; margin-top: 4px;",
      htmltools::HTML(paste0("Statistical tests: ", footnote))
    )
  )
}


## -----------------------------------------------------------------------------
#| label: mean-map
#| include: false
map_long <- master %>%
  pivot_longer(
    cols = starts_with("map_terli_day"),
    names_to = c("temp", "day", "time"),
    names_pattern = "^(map_terli)_day(\\d+)_time(\\d+)$",
    values_to = "map"
  ) %>%
  mutate(day = as.numeric(day), time = as.numeric(time)) %>%
  select(-temp)

map_day0 <- map_long %>%
  filter(day == 0) %>%
  group_by(subjectid) %>%
  summarise(mean_map_day0 = mean(map, na.rm = TRUE), .groups = "drop")


## -----------------------------------------------------------------------------
#| label: dosechange
#| include: false
master <- create_dosechange_day(master)

dosechange_cols <- c("dosechange_terli_day2", "dosechange_terli_day3",
                     "dosechange_terli_day4", "dosechange_terli_day5",
                     "dosechange_terli_day6", "dosechange_terli_day7",
                     "dosechange_terli_day8", "dosechange_terli_day9",
                     "dosechange_terli_day10", "dosechange_terli_day11",
                     "dosechange_terli_day12")

master <- master %>%
  rowwise() %>%
  mutate(
    dosechange_status = if_else(any(c_across(all_of(dosechange_cols)) == 1, na.rm = TRUE), 1, 0),
    time_to_dosechange = if_else(
      dosechange_status == 1,
      min(which(c_across(all_of(dosechange_cols)) == 1)) + 1,
      NA_integer_
    )
  ) %>%
  ungroup()


## -----------------------------------------------------------------------------
#| label: time-to-death
#| include: false
master <- master %>% mutate(
  dc_death_status = if_else(disposition == 5, 1, 0),
  after_discharge_90days_death_status = if_else(dc_death_90days == 1, 1, 0),
  dc_days_to_death = if_else(dc_death_90days == 2 | dc_death_90days == 0, 90, dc_days_to_death)
) %>%
  mutate(
    days_to_lost_to_followup = if_else(is.na(days_to_lost_to_followup), 0, days_to_lost_to_followup),
    time_to_death = case_when(
      dc_death_status == 1 ~ days_to_discharge,
      dc_death_status == 0 & dc_death_90days == 2 ~ days_to_discharge + days_to_lost_to_followup,
      dc_death_status == 0 & dc_death_90days != 2 ~ days_to_discharge + dc_days_to_death
    )
  ) %>%
  mutate(
    time_to_death_status = case_when(
      dc_death_status == 1 ~ 1,
      dc_death_status == 0 & after_discharge_90days_death_status == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  mutate(time_to_death = time_to_death - days_to_terli_initiation_day0)


## -----------------------------------------------------------------------------
#| label: derived-variables
#| include: false

# Filter to valid discontinuation days and merge MAP
master <- master %>%
  filter(!is.na(Discont_Day)) %>%
  rowwise() %>%
  mutate(
    last_scr = get(paste0("scr_terli_day", Discont_Day)),
    last_scr_2 = get(paste0("scr_terli_day", Discont_Day - 1)),
    discont_reason_terli = get(paste0("discont_reason_terli_day", Discont_Day)),
    terli_ae_abdpain = get(paste0("terli_ae_abdpain_day", Discont_Day)),
    terli_ae_nausea = get(paste0("terli_ae_nausea_day", Discont_Day)),
    terli_ae_diarrhea = get(paste0("terli_ae_diarrhea_day", Discont_Day)),
    terli_ae_volover = get(paste0("terli_ae_volover_day", Discont_Day)),
    terli_ae_brady = get(paste0("terli_ae_brady_day", Discont_Day)),
    terli_other = get(paste0("terli_other_day", Discont_Day))
  ) %>%
  ungroup() %>%
  merge(map_day0, by = "subjectid", all.x = TRUE) %>%

  # Lowest creatinine & HRS reversal
  mutate(lowest_creatinine = pmin(last_scr_2, last_scr, scr_day1_post_terli, na.rm = TRUE)) %>%
  mutate(HRS_reversal_new = case_when(
    lowest_creatinine <= 1.5 & (scr_terli_day0 - lowest_creatinine) >= 0.3 ~ 1,
    TRUE ~ 0
  )) %>%

  # Fix unit issues

  mutate(
    scr_terli_day0 = ifelse(scr_terli_day0 > 100, scr_terli_day0 / 100, scr_terli_day0),
    fio2_terli_day0 = ifelse(fio2_terli_day0 > 1, fio2_terli_day0 / 100, fio2_terli_day0)
  ) %>%

  # Transplant variables
  mutate(
    transplant_lt = case_when(
      lt_during_hospital == 1 | slkt_during_hospital == 1 ~ 1,
      dc_lt_90days == 1 | dc_lt_90days == 2 ~ 2,
      lt_during_hospital == 0 & lt_status_1yr == 1 & days_to_lt_1yr > 90 ~ 3,
      TRUE ~ 0
    ),
    time_to_transplant = case_when(
      transplant_lt == 1 & !is.na(days_to_lt) ~ days_to_lt,
      transplant_lt == 1 & is.na(days_to_lt) ~ days_to_discharge - days_to_lt_1yr,
      transplant_lt == 2 | transplant_lt == 3 ~ days_to_discharge + days_to_lt_1yr,
      TRUE ~ NA_real_
    ),
    time_to_transplant = time_to_transplant - days_to_terli_initiation_day0
  ) %>%
  mutate(transplant_lt_status = if_else(transplant_lt == 0, 0, 2)) %>%
  mutate(time_to_transplant_cmp = if_else(is.na(time_to_transplant), 999, time_to_transplant)) %>%
  mutate(transplant_slkt = case_when(
    slkt_during_hospital == 1 | dc_lt_90days == 2 | lt_type_1yr == 2 ~ 1, TRUE ~ 0
  )) %>%

  # Baseline creatinine
  mutate(
    egfr = 75,
    baseline_scr_calculated = mapply(estimate_creatinine, egfr, age, sex_male),
    baseline_scr_new = case_when(
      baseline_scr_days_to_admit != 0 & !is.na(baseline_scr) ~ baseline_scr,
      baseline_scr_days_to_admit == 0 & !is.na(baseline_scr_120to365_value) ~ baseline_scr_120to365_value,
      baseline_scr_days_to_admit == 0 & is.na(baseline_scr_120to365_value) ~ pmin(baseline_scr_calculated, scr_admit)
    ),
    baseline_scr_new_2 = case_when(
      baseline_scr_days_to_admit != 0 & !is.na(baseline_scr) ~ baseline_scr,
      baseline_scr_days_to_admit == 0 & !is.na(baseline_scr_120to365_value) ~ baseline_scr_120to365_value
    ),
    eGFR = mapply(ckd_epi_gfr, baseline_scr_new_2, sex_male, age)
  ) %>%

  # AKI staging
  mutate(
    ratio = round(scr_terli_day0 / baseline_scr_new, 4),
    delta = round(scr_terli_day0 - baseline_scr_new, 4),
    akin = case_when(
      (ratio >= 1.5 & ratio < 2) | (ratio < 1.5 & delta > 0.3) ~ 1,
      (ratio >= 2 & ratio < 3) ~ 2,
      ratio >= 3 ~ 3,
      TRUE ~ NA
    ),
    akin = ifelse(scr_terli_day0 >= 4, 3, akin)
  ) %>%

  # Bilirubin (coalesced)
  mutate(bili_cal = coalesce(bili_terli_day0, tbili_admit, mean(bili_terli_day0, na.rm = TRUE))) %>%

  # Clinical scores
  mutate(clif_c_score = mapply(calculate_clif_c_score, tbili_admit, scr_terli_day0, he_terli_day0,
                               inr_terli_day0, map_terli_day0_time0, norepi_terli_day0, fio2_terli_day0,
                               spo2_terli_day0, age, wbc_terli_day0)) %>%
  mutate(aclf_grade = mapply(calculate_organ_failures, bili_terli_day0, he_terli_day0, inr_terli_day0,
                             map_terli_day0_time0, fio2_terli_day0, spo2_terli_day0, scr_terli_day0)) %>%
  mutate(child_pugh_score = mapply(child_pugh_score, bili_terli_day0, alb_terli_day0, inr_terli_day0,
                                   ascites_aki, he_terli_day0)) %>%

  # Discontinuation AKI staging & responders
  mutate(
    discont_ratio = round(lowest_creatinine / baseline_scr_new, 4),
    discont_delta = round(lowest_creatinine - baseline_scr_new, 4),
    discont_aki_stage = case_when(
      is.na(discont_ratio) | is.na(discont_delta) ~ NA_real_,
      discont_ratio < 1.5 & discont_delta < 0.3 ~ 0,
      (discont_ratio >= 1.5 & discont_ratio < 2) | (discont_ratio < 1.5 & discont_delta >= 0.3) ~ 1,
      discont_ratio >= 2 & discont_ratio < 3 ~ 2,
      discont_ratio >= 3 ~ 3
    ),
    discont_aki_stage = ifelse(lowest_creatinine >= 4, 3, discont_aki_stage),
    difference = round(akin - discont_aki_stage, 0),
    hrs_responders = case_when(
      difference <= 0 ~ 0,
      difference > 0 & round(lowest_creatinine - baseline_scr_new, 2) > 0.3 ~ 1,
      round(lowest_creatinine - baseline_scr_new, 2) <= 0.3 ~ 2
    ),
    hrs_responders_cat_2 = ifelse(hrs_responders == 0, 0, 1)
  )


## -----------------------------------------------------------------------------
#| label: meld-scores
#| include: false
master <- master %>%
  mutate(
    # Day 0 MELD
    creatinine_adj_day0 = case_when(scr_terli_day0 < 1 ~ 1, scr_terli_day0 >= 4 ~ 4, TRUE ~ scr_terli_day0),
    day0_meld = round(9.57 * log(creatinine_adj_day0) + 3.78 * log(bili_terli_day0) + 11.20 * log(inr_terli_day0) + 6.43),
    day0_meld_na = round(day0_meld + 1.32 * (137 - pmin(pmax(na_terli_day0, 125), 137)) - (0.033 * day0_meld * (137 - pmin(pmax(na_terli_day0, 125), 137)))),
    day0_meld_na = ifelse(day0_meld > 11, day0_meld_na, NA),
    female_factor = if_else(sex_male == 0, 1.33, 0),
    day0_meld_3 = round(
      female_factor + 4.56 * log(bili_terli_day0) + 0.82 * (137 - na_terli_day0) -
      0.24 * (137 - na_terli_day0) * log(bili_terli_day0) + 9.09 * log(inr_terli_day0) +
      11.14 * log(creatinine_adj_day0) + 1.85 * (3.5 - alb_terli_day0) -
      1.83 * (3.5 - alb_terli_day0) * log(creatinine_adj_day0) + 6
    ),
    # Admission MELD
    admit_creatinine_adj = case_when(scr_admit < 1 ~ 1, scr_admit >= 4 ~ 4, TRUE ~ scr_admit),
    admit_meld = round(9.57 * log(admit_creatinine_adj) + 3.78 * log(tbili_admit) + 11.20 * log(inr_admit) + 6.43),
    admit_meld_na = round(admit_meld + 1.32 * (137 - pmin(pmax(sodium_admit, 125), 137)) - (0.033 * admit_meld * (137 - pmin(pmax(sodium_admit, 125), 137)))),
    admit_meld_na = ifelse(admit_meld > 11, admit_meld_na, NA),
    admit_meld_3 = round(
      female_factor + 4.56 * log(tbili_admit) + 0.82 * (137 - sodium_admit) -
      0.24 * (137 - sodium_admit) * log(tbili_admit) + 9.09 * log(inr_admit) +
      11.14 * log(admit_creatinine_adj) + 1.85 * (3.5 - albumin_admit) -
      1.83 * (3.5 - albumin_admit) * log(admit_creatinine_adj) + 6
    )
  ) %>%
  # Organ failure flags
  mutate(
    liver_f = ifelse(!is.na(bili_terli_day0) & bili_terli_day0 >= 6, 1, 0),
    brain_f = ifelse(!is.na(he_terli_day0) & he_terli_day0 > 1, 1, 0),
    coagulation_f = ifelse(!is.na(inr_terli_day0) & inr_terli_day0 >= 2, 1, 0),
    circulatory_f = ifelse(!is.na(map_terli_day0_time0) & map_terli_day0_time0 < 70, 1, 0),
    respiratory_f = ifelse(
      !is.na(fio2_terli_day0) & fio2_terli_day0 != 0 &
      !is.na(spo2_terli_day0) & (spo2_terli_day0 / fio2_terli_day0) <= 357, 1, 0
    ),
    kidney_f = ifelse(!is.na(scr_terli_day0) & scr_terli_day0 >= 1.5, 1, 0),
    organ_failure_sum = liver_f + brain_f + coagulation_f + circulatory_f + respiratory_f + kidney_f
  ) %>%
  mutate(rrt_2 = ifelse(rrt == 2, 0, rrt)) %>%
  mutate(aclf_grade = if_else(aclf_grade > 3, 3, aclf_grade))


## -----------------------------------------------------------------------------
#| label: criteria
#| include: false
master <- master %>%
  mutate(
    criteria_1 = ifelse((total_terli_day1 == 0 | is.na(total_terli_day1)) & discont_terli_day1 == 1, 1, 0),
    criteria_2 = ifelse(aclf_grade >= 3, 1, 0),
    criteria_3 = ifelse(scr_terli_day0 >= 5, 1, 0),
    criteria_4 = case_when(
      spo2_terli_day0 / fio2_terli_day0 < 357 ~ 1,
      spo2_terli_day0 / fio2_terli_day0 >= 357 ~ 0
    ),
    criteria_4 = ifelse(is.na(criteria_4), 0, criteria_4),
    criteria_5 = ifelse(admit_meld_3 > 35, 1, 0),
    HRS_criteria_meld = if_else((criteria_2 == 1 | criteria_3 == 1 | criteria_4 == 1 | criteria_5), 1, 0),
    HRS_criteria_3 = if_else((criteria_2 == 1 | criteria_3 == 1 | criteria_4 == 1), 1, 0),
    HRS_Criteria = case_when(
      criteria_1 == 0 & criteria_2 == 0 & criteria_3 == 0 & criteria_4 == 0 ~ 0,
      criteria_1 == 1 | criteria_2 == 1 | criteria_3 == 1 | criteria_4 == 1 ~ 1
    )
  )


## -----------------------------------------------------------------------------
#| label: ae-flags
#| include: false
master <- master %>%
  rowwise() %>%
  mutate(
    last_scr = get(paste0("scr_terli_day", Discont_Day)),
    discont_reason_terli = get(paste0("discont_reason_terli_day", Discont_Day)),
    terli_ae_abdpain = get(paste0("terli_ae_abdpain_day", Discont_Day)),
    terli_ae_nausea = get(paste0("terli_ae_nausea_day", Discont_Day)),
    terli_ae_diarrhea = get(paste0("terli_ae_diarrhea_day", Discont_Day)),
    terli_ae_volover = get(paste0("terli_ae_volover_day", Discont_Day)),
    terli_ae_brady = get(paste0("terli_ae_brady_day", Discont_Day))
  ) %>%
  ungroup() %>%
  mutate(
    terli_7 = if_else(discont_reason_terli == 7, 1, 0),
    terli_8 = if_else(discont_reason_terli == 8, 1, 0)
  ) %>%
  mutate(terli_ae_respfail = if_else(terli_7 == 1 | terli_ae_volover == 1, 1, 0)) %>%
  mutate(across(
    c(terli_ae_abdpain, terli_ae_nausea, terli_ae_diarrhea,
      terli_ae_volover, terli_ae_brady, terli_7, terli_8, terli_ae_respfail),
    ~ if_else(is.na(.x), 0, .x)
  )) %>%
  mutate(
    any_ae = if_else(
      terli_ae_abdpain == 1 | terli_ae_nausea == 1 | terli_ae_diarrhea == 1 |
      terli_ae_volover == 1 | terli_ae_brady == 1 | terli_7 == 1 |
      terli_8 == 1 | terli_ae_respfail == 1, 1, 0
    )
  ) %>%
  mutate(
    listed_transplant = if_else(lt_eval == 2 | lteval_listingstatus == 1, 1, 0),
    listed_transplant = ifelse(is.na(listed_transplant), 0, listed_transplant)
  )


## -----------------------------------------------------------------------------
#| label: competing-risk
#| include: false
master <- master %>%
  mutate(
    time_to_death_90days = case_when(
      time_to_death_status == 1 & time_to_death <= 90 ~ time_to_death,
      time_to_death_status == 1 & time_to_death > 90 ~ 90,
      time_to_death_status == 0 & time_to_death <= 90 ~ time_to_death,
      time_to_death_status == 0 & time_to_death > 90 ~ 90
    ),
    time_to_death_status_90days = case_when(
      time_to_death_status == 1 & time_to_death <= 90 ~ 1,
      TRUE ~ 0
    ),
    time_to_transplant_90days = case_when(
      transplant_lt_status == 2 & time_to_transplant_cmp <= 90 ~ time_to_transplant_cmp,
      transplant_lt_status == 2 & time_to_transplant_cmp > 90 ~ 90,
      transplant_lt_status == 0 & time_to_transplant_cmp <= 90 ~ time_to_transplant_cmp,
      transplant_lt_status == 0 & time_to_transplant_cmp > 90 ~ 90
    ),
    time_to_transplant_status_90days = case_when(
      transplant_lt_status == 2 & time_to_transplant_cmp <= 90 ~ 2,
      TRUE ~ 0
    ),
    time_to_death_90days_cmp = pmin(time_to_death_90days, time_to_transplant_90days),
    time_to_death_status_90days_cmp = case_when(
      time_to_death_90days <= time_to_transplant_90days ~ time_to_death_status_90days,
      time_to_death_90days > time_to_transplant_90days ~ time_to_transplant_status_90days
    )
  )

master <- master %>%
  mutate(
    fena = (100 * (scr_admit * ua_na_value_hrs) / (sodium_admit * ua_cr_value_hrs)),
    urine_pc = uaprotein_hrs / ua_cr_value_hrs
  ) %>%
  mutate(hrs_rebound = if_else(scr_day1_post_terli - last_scr >= 0.3, 1, 0)) %>%
  mutate(admit_meld_3_raw = admit_meld_3) %>%
  mutate(admit_meld_3 = if_else(is.na(admit_meld_3), median(admit_meld_3, na.rm = TRUE), admit_meld_3)) %>%
  mutate(
    race_2 = if_else(race == 1 | is.na(race), 1, 0),
    hispanic = if_else(is.na(hispanic), 0, hispanic)
  ) %>%
  mutate(transplant_outcome = if_else(time_to_transplant_status_90days == 2, 1, 0)) %>%
  mutate(ckd_baseline = if_else(eGFR < 60, 1, 0)) %>%
  mutate(total_albumin = rowSums(
    select(., albumintotal_terli_day0, albumintotal_terli_day1,
           albumintotal_terli_day2, albumintotal_terli_day3,
           albumintotal_terli_day4, albumintotal_terli_day5,
           albumintotal_terli_day6, albumintotal_terli_day7,
           albumintotal_terli_day8, albumintotal_terli_day9,
           albumintotal_terli_day10, albumintotal_terli_day11,
           albumintotal_terli_day12, albumintotal_terli_day13),
    na.rm = TRUE
  ))

master <- master %>%
  mutate(clif_c_score_new = if_else(is.na(clif_c_score), median(clif_c_score, na.rm = TRUE), clif_c_score))

