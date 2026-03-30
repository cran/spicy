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

## ----gt-----------------------------------------------------------------------
pkgdown_dark_gt(
  table_categorical(
    sochealth,
    select = c(smoking, physical_activity, dentist_12m),
    by = education,
    output = "gt"
  )
)

## ----tinytable----------------------------------------------------------------
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  output = "tinytable"
)

## ----data-frame---------------------------------------------------------------
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  output = "data.frame"
)

## ----labels-------------------------------------------------------------------
pkgdown_dark_gt(
  table_categorical(
    sochealth,
    select = c(smoking, physical_activity),
    by = education,
    labels = c("Smoking status", "Regular physical activity"),
    output = "gt"
  )
)

## ----assoc-measure------------------------------------------------------------
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  assoc_measure = "lambda",
  output = "tinytable"
)

## ----ci-rendered--------------------------------------------------------------
pkgdown_dark_gt(
  table_categorical(
    sochealth,
    select = c(smoking, physical_activity),
    by = education,
    assoc_ci = TRUE,
    output = "gt"
  )
)

## ----ci-data------------------------------------------------------------------
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  assoc_ci = TRUE,
  output = "data.frame"
)

## ----weighted-----------------------------------------------------------------
pkgdown_dark_gt(
  table_categorical(
    sochealth,
    select = c(smoking, physical_activity),
    by = education,
    weights = "weight",
    rescale = TRUE,
    output = "gt"
  )
)

## ----missing------------------------------------------------------------------
pkgdown_dark_gt(
  table_categorical(
    sochealth,
    select = income_group,
    by = education,
    drop_na = FALSE,
    output = "gt"
  )
)

## ----levels-keep--------------------------------------------------------------
pkgdown_dark_gt(
  table_categorical(
    sochealth,
    select = income_group,
    by = education,
    drop_na = FALSE,
    levels_keep = c("(Missing)", "Low", "High"),
    output = "gt"
  )
)

## ----formatting---------------------------------------------------------------
pkgdown_dark_gt(
  table_categorical(
    sochealth,
    select = smoking,
    by = education,
    percent_digits = 2,
    p_digits = 4,
    v_digits = 3,
    output = "gt"
  )
)

