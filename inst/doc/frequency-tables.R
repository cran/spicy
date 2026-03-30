## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(spicy)

## ----freq-basic---------------------------------------------------------------
freq(sochealth, education)

## ----freq-sort----------------------------------------------------------------
freq(sochealth, education, sort = "-")

## ----freq-sort-name-----------------------------------------------------------
freq(sochealth, education, sort = "name+")

## ----freq-cum-----------------------------------------------------------------
freq(sochealth, smoking, cum = TRUE)

## ----freq-weighted------------------------------------------------------------
freq(sochealth, education, weights = weight)

## ----freq-weighted-raw--------------------------------------------------------
freq(sochealth, education, weights = weight, rescale = FALSE)

## ----freq-labelled------------------------------------------------------------
# Create a labelled version of the smoking variable
sh <- sochealth
sh$smoking_lbl <- labelled::labelled(
  ifelse(sh$smoking == "Yes", 1L, 0L),
  labels = c("Non-smoker" = 0L, "Current smoker" = 1L)
)

# Default: [code] label
freq(sh, smoking_lbl)

# Labels only (no codes)
freq(sh, smoking_lbl, labelled_levels = "labels")

# Codes only (no labels)
freq(sh, smoking_lbl, labelled_levels = "values")

## ----freq-naval---------------------------------------------------------------
freq(sochealth, income_group, na_val = "High")

## ----cross-basic--------------------------------------------------------------
cross_tab(sochealth, smoking, education)

## ----cross-col----------------------------------------------------------------
cross_tab(sochealth, smoking, education, percent = "col")

## ----cross-row----------------------------------------------------------------
cross_tab(sochealth, smoking, education, percent = "row")

## ----cross-by-----------------------------------------------------------------
cross_tab(sochealth, smoking, education, by = sex)

## ----cross-by-interaction-----------------------------------------------------
cross_tab(sochealth, smoking, education,
          by = interaction(sex, age_group))

## ----cross-ordinal------------------------------------------------------------
cross_tab(sochealth, self_rated_health, education)

## ----cross-assoc--------------------------------------------------------------
cross_tab(sochealth, self_rated_health, education, assoc_measure = "gamma")

## ----cross-ci-----------------------------------------------------------------
cross_tab(sochealth, smoking, education, assoc_ci = TRUE)

## ----cross-weighted-raw-------------------------------------------------------
cross_tab(sochealth, smoking, education, weights = weight)

## ----cross-weighted-----------------------------------------------------------
cross_tab(sochealth, smoking, education, weights = weight, rescale = TRUE)

## ----cross-simulate-----------------------------------------------------------
cross_tab(sochealth, smoking, education,
          simulate_p = TRUE, simulate_B = 5000)

## ----cross-df-----------------------------------------------------------------
cross_tab(sochealth, smoking, education,
          percent = "col", styled = FALSE)

## ----options, eval = FALSE----------------------------------------------------
# options(
#   spicy.percent   = "column",
#   spicy.simulate_p = TRUE,
#   spicy.rescale   = TRUE
# )

