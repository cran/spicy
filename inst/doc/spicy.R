## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(spicy)

## ----varlist------------------------------------------------------------------
varlist(sochealth, tbl = TRUE)

## ----varlist-select-----------------------------------------------------------
varlist(sochealth, starts_with("bmi"), income, weight, tbl = TRUE)

## ----freq---------------------------------------------------------------------
freq(sochealth, education)

## ----freq-weighted------------------------------------------------------------
freq(sochealth, education, weights = weight, rescale = TRUE)

## ----crosstab-----------------------------------------------------------------
cross_tab(sochealth, smoking, education)

## ----crosstab-pct-------------------------------------------------------------
cross_tab(sochealth, smoking, education, percent = "col")

## ----crosstab-by--------------------------------------------------------------
cross_tab(sochealth, smoking, education, by = sex)

## ----crosstab-ordinal---------------------------------------------------------
cross_tab(sochealth, self_rated_health, education)

## ----assoc-measures-----------------------------------------------------------
tbl <- xtabs(~ smoking + education, data = sochealth)
assoc_measures(tbl)

## ----cramer-detail------------------------------------------------------------
cramer_v(tbl, detail = TRUE)

## ----table-categorical-tt-----------------------------------------------------
table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education,
  output = "tinytable"
)

## ----table-continuous---------------------------------------------------------
table_continuous(
  sochealth,
  select = c(bmi, life_sat_health),
  by = education
)

## ----mean-n-------------------------------------------------------------------
sochealth |>
  dplyr::mutate(
    mean_sat  = mean_n(select = starts_with("life_sat")),
    sum_sat   = sum_n(select = starts_with("life_sat"), min_valid = 2),
    n_missing = count_n(select = starts_with("life_sat"), special = "NA")
  ) |>
  dplyr::select(starts_with("life_sat"), mean_sat, sum_sat, n_missing) |>
  head() |>
  as.data.frame()

