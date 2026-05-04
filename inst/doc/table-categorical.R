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
table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education
)

## ----oneway-------------------------------------------------------------------
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  output = "default"
)

## ----gt, eval = build_rich_tables---------------------------------------------
# pkgdown_dark_gt(
#   table_categorical(
#     sochealth,
#     select = c(smoking, physical_activity, dentist_12m),
#     by = education,
#     output = "gt"
#   )
# )

## ----tinytable, eval = build_rich_tables--------------------------------------
# table_categorical(
#   sochealth,
#   select = c(smoking, physical_activity),
#   by = sex,
#   output = "tinytable"
# )

## ----data-frame---------------------------------------------------------------
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  output = "data.frame"
)

## ----labels, eval = build_rich_tables-----------------------------------------
# pkgdown_dark_gt(
#   table_categorical(
#     sochealth,
#     select = c(smoking, physical_activity),
#     by = education,
#     labels = c(
#       smoking           = "Smoking status",
#       physical_activity = "Regular physical activity"
#     ),
#     output = "gt"
#   )
# )

## ----assoc-measure, eval = build_rich_tables----------------------------------
# # Uniform: same measure for every row variable
# table_categorical(
#   sochealth,
#   select = smoking,
#   by = education,
#   assoc_measure = "lambda",
#   output = "tinytable"
# )

## ----assoc-measure-named, eval = build_rich_tables----------------------------
# # Per-row: pick the right measure for each variable.
# # `smoking` x `education` is 2x3 (binary x ordered) -> Cramer's V;
# # `self_rated_health` x `education` is ordered x ordered -> Tau-b.
# # The mixed result collapses the header to "Effect size" and adds an
# # APA `Note.` line documenting the per-row measure.
# table_categorical(
#   sochealth,
#   select = c(smoking, self_rated_health),
#   by = education,
#   assoc_measure = c(
#     smoking           = "cramer_v",
#     self_rated_health = "tau_b"
#   ),
#   output = "tinytable"
# )

## ----ci-rendered, eval = build_rich_tables------------------------------------
# pkgdown_dark_gt(
#   table_categorical(
#     sochealth,
#     select = c(smoking, physical_activity),
#     by = education,
#     assoc_ci = TRUE,
#     output = "gt"
#   )
# )

## ----ci-data------------------------------------------------------------------
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  assoc_ci = TRUE,
  output = "data.frame"
)

## ----weighted, eval = build_rich_tables---------------------------------------
# pkgdown_dark_gt(
#   table_categorical(
#     sochealth,
#     select = c(smoking, physical_activity),
#     by = education,
#     weights = "weight",
#     rescale = TRUE,
#     output = "gt"
#   )
# )

## ----missing, eval = build_rich_tables----------------------------------------
# pkgdown_dark_gt(
#   table_categorical(
#     sochealth,
#     select = income_group,
#     by = education,
#     drop_na = FALSE,
#     output = "gt"
#   )
# )

## ----levels-keep, eval = build_rich_tables------------------------------------
# pkgdown_dark_gt(
#   table_categorical(
#     sochealth,
#     select = income_group,
#     by = education,
#     drop_na = FALSE,
#     levels_keep = c("(Missing)", "Low", "High"),
#     output = "gt"
#   )
# )

## ----formatting, eval = build_rich_tables-------------------------------------
# pkgdown_dark_gt(
#   table_categorical(
#     sochealth,
#     select = smoking,
#     by = education,
#     percent_digits = 2,
#     p_digits = 4,
#     v_digits = 3,
#     output = "gt"
#   )
# )

## ----align--------------------------------------------------------------------
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  align = "auto"
)

## ----tidy-glance--------------------------------------------------------------
out <- table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex
)

# One row per (variable x level x group) with broom-style columns
# (outcome, level, group, n, proportion). The synthetic Total
# margin is excluded so each observation is counted once.
broom::tidy(out)

# One row per outcome with the omnibus chi-squared test and the
# chosen association measure (test_type, statistic, df, p.value,
# assoc_type, assoc_value, assoc_ci_lower / assoc_ci_upper, n_total).
broom::glance(out)

