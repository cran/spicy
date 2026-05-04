## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

build_rich_tables <- identical(Sys.getenv("IN_PKGDOWN"), "true")

pkgdown_dark_gt <- function(tab) {
  tab |>
    gt::opt_css(
      css = paste(
        ".gt_table, .gt_heading, .gt_col_headings, .gt_col_heading,",
        ".gt_column_spanner_outer, .gt_column_spanner, .gt_title,",
        ".gt_subtitle, .gt_sourcenotes, .gt_sourcenote {",
        "  background-color: transparent !important;",
        "  color: currentColor !important;",
        "}",
        sep = "\n"
      )
    )
}

## ----setup--------------------------------------------------------------------
library(spicy)

## ----basic--------------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi, life_sat_health),
  by = sex
)

## ----robust-------------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  vcov = "HC3",
  statistic = TRUE
)

## ----cluster, eval = requireNamespace("clubSandwich", quietly = TRUE)---------
table_continuous_lm(
  sleep,
  select = extra,
  by = group,
  vcov = "CR2",
  cluster = ID,
  statistic = TRUE
)

## ----bootstrap, eval = FALSE--------------------------------------------------
# table_continuous_lm(
#   sochealth,
#   select = wellbeing_score,
#   by = sex,
#   vcov = "bootstrap",
#   boot_n = 1000  # default
# )

## ----weights------------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = education,
  weights = weight,
  show_weighted_n = TRUE
)

## ----numeric-by---------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = age,
  vcov = "HC3"
)

## ----es-d---------------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = smoking,
  effect_size = "d"
)

## ----es-g---------------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = smoking,
  effect_size = "g"
)

## ----es-omega2----------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = education,
  effect_size = "omega2"
)

## ----es-f2--------------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = age,
  effect_size = "f2"
)

## ----es-ci--------------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = smoking,
  effect_size = "g",
  effect_size_ci = TRUE
)

## ----es-ci-raw----------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = wellbeing_score,
  by = smoking,
  effect_size = "g",
  effect_size_ci = TRUE,
  output = "data.frame"
)

## ----es-ci-long---------------------------------------------------------------
out <- table_continuous_lm(
  sochealth,
  select = wellbeing_score,
  by = smoking,
  effect_size = "g",
  effect_size_ci = TRUE,
  output = "long"
)
out[, c("variable", "es_type", "es_value", "es_ci_lower", "es_ci_upper")]

## ----polish-------------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  labels = c(
    wellbeing_score = "WHO-5 wellbeing (0-100)",
    bmi = "Body-mass index (kg/m²)"
  ),
  effect_size = "g",
  effect_size_ci = TRUE,
  decimal_mark = ","
)

## ----gt-output, eval = build_rich_tables--------------------------------------
# pkgdown_dark_gt(
#   table_continuous_lm(
#     sochealth,
#     select = c(wellbeing_score, bmi, life_sat_health),
#     by = sex,
#     vcov = "HC3",
#     statistic = TRUE,
#     output = "gt"
#   )
# )

## ----tidy-glance--------------------------------------------------------------
out <- table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  effect_size = "g",
  effect_size_ci = TRUE
)

# One row per estimated parameter: emmean per level, contrast for
# binary predictors, slope for numeric predictors.
broom::tidy(out)

# One row per outcome with model-level statistics: r.squared,
# adj.r.squared, F / t, df, p.value, nobs, weighted_n, plus the
# effect-size summary.
broom::glance(out)

