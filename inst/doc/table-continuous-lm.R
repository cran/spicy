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

## ----weights------------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = education,
  weights = weight
)

## ----numeric-by---------------------------------------------------------------
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = age,
  vcov = "HC3",
  output = "long"
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

