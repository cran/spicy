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
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health)
)

## ----default-select-----------------------------------------------------------
table_continuous(sochealth)

## ----grouped------------------------------------------------------------------
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education
)

## ----lm-companion-------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  vcov = "HC3"
)

## ----pvalue-effect------------------------------------------------------------
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  statistic = TRUE,
  effect_size_ci = TRUE
)

## ----nonparametric------------------------------------------------------------
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  test = "nonparametric",
  statistic = TRUE,
  effect_size = TRUE
)

## ----effect-size-explicit-----------------------------------------------------
table_continuous(
  sochealth,
  select = wellbeing_score,
  by = sex,
  effect_size = "hedges_g",
  effect_size_ci = TRUE
)

## ----raw-output---------------------------------------------------------------
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  statistic = TRUE,
  effect_size = TRUE,
  output = "data.frame"
)

## ----tidyselect---------------------------------------------------------------
table_continuous(
  sochealth,
  select = starts_with("life_sat"),
  by = sex
)

## ----regex--------------------------------------------------------------------
table_continuous(
  sochealth,
  select = "^life_sat",
  regex = TRUE,
  by = education,
  output = "data.frame"
)

## ----exclude------------------------------------------------------------------
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health, life_sat_work),
  exclude = "life_sat_work",
  by = sex
)

## ----labels, eval = build_rich_tables-----------------------------------------
# pkgdown_dark_gt(
#   table_continuous(
#     sochealth,
#     select = c(bmi, wellbeing_score, life_sat_health),
#     by = education,
#     labels = c(
#       bmi = "Body mass index",
#       wellbeing_score = "Well-being score",
#       life_sat_health = "Satisfaction with health"
#     ),
#     output = "gt"
#   )
# )

## ----tinytable, eval = build_rich_tables--------------------------------------
# table_continuous(
#   sochealth,
#   select = c(bmi, wellbeing_score, life_sat_health),
#   by = education,
#   output = "tinytable"
# )

## ----export, eval = FALSE-----------------------------------------------------
# table_continuous(
#   sochealth,
#   select = c(bmi, wellbeing_score, life_sat_health),
#   by = education,
#   output = "excel",
#   excel_path = "table_continuous.xlsx"
# )
# 
# table_continuous(
#   sochealth,
#   select = c(bmi, wellbeing_score, life_sat_health),
#   by = education,
#   output = "word",
#   word_path = "table_continuous.docx"
# )

## ----display-opts-------------------------------------------------------------
table_continuous(
  sochealth,
  select = wellbeing_score,
  by = sex,
  ci = FALSE,
  show_n = FALSE,
  p_digits = 4
)

## ----tidy-glance--------------------------------------------------------------
out <- table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = sex
)

# Long descriptive rows: one per (variable x group) with broom-style
# columns (outcome, label, group, estimate = mean, std.error,
# conf.low / conf.high, n, min, max, sd).
broom::tidy(out)

# One row per outcome with the omnibus test + effect-size summary
# (test_type, statistic, df, df.residual, p.value, es_type, es_value,
# es_ci_lower / es_ci_upper, n_total).
broom::glance(out)

# Or just unbox to a plain data.frame (long-format underlying data)
head(as.data.frame(out))

