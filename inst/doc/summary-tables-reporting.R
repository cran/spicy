## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

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

## ----grammar-categorical------------------------------------------------------
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Smoking status", "Regular physical activity"),
  output = "tinytable"
)

## ----grammar-continuous-------------------------------------------------------
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  labels = c(
    bmi = "Body mass index",
    wellbeing_score = "Well-being score",
    life_sat_health = "Satisfaction with health"
  ),
  output = "tinytable"
)

## ----report-categorical-------------------------------------------------------
pkgdown_dark_gt(
  table_categorical(
    sochealth,
    select = c(smoking, physical_activity, dentist_12m),
    by = education,
    labels = c(
      "Smoking status",
      "Regular physical activity",
      "Visited a dentist in the last 12 months"
    ),
    output = "gt"
  )
)

## ----report-continuous--------------------------------------------------------
pkgdown_dark_gt(
  table_continuous(
    sochealth,
    select = c(bmi, wellbeing_score, life_sat_health),
    by = education,
    labels = c(
      bmi = "Body mass index",
      wellbeing_score = "Well-being score",
      life_sat_health = "Satisfaction with health"
    ),
    p_value = TRUE,
    effect_size = TRUE,
    output = "gt"
  )
)

## ----output-flextable, eval = FALSE-------------------------------------------
# if (requireNamespace("flextable", quietly = TRUE)) {
#   table_continuous(
#     sochealth,
#     select = c(bmi, wellbeing_score, life_sat_health),
#     by = education,
#     output = "flextable"
#   )
# }

## ----postprocess-gt-----------------------------------------------------------
tab <- pkgdown_dark_gt(table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Smoking status", "Regular physical activity"),
  output = "gt"
))

tab |>
  gt::tab_header(
    title = "Health behaviors by education",
    subtitle = "Categorical summary table"
  ) |>
  gt::tab_source_note(
    gt::md("*Percentages are computed within each education group.*")
  )

## ----postprocess-tinytable----------------------------------------------------
tab <- table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Smoking status", "Regular physical activity"),
  output = "tinytable"
)

tab |>
  tinytable::style_tt(
    i = 2:3,
    j = 2:5,
    background = "red",
    color = "white",
    bold = TRUE
  )

## ----postprocess-flextable, eval = FALSE--------------------------------------
# if (requireNamespace("flextable", quietly = TRUE)) {
#   tab <- table_continuous(
#     sochealth,
#     select = c(bmi, wellbeing_score),
#     by = education,
#     output = "flextable"
#   )
# 
#   tab |>
#     flextable::theme_booktabs() |>
#     flextable::autofit() |>
#     flextable::fontsize(size = 10, part = "all")
# }

