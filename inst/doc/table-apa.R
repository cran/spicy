## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(spicy)

## ----basic--------------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = c("smoking", "physical_activity", "dentist_12m"),
  group_var = "education"
)

## ----gt-----------------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = c("smoking", "physical_activity", "dentist_12m"),
  group_var = "education",
  output = "gt"
)

## ----tinytable----------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = c("smoking", "physical_activity"),
  group_var = "education",
  output = "tinytable"
)

## ----wide-report--------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = "smoking",
  group_var = "education",
  output = "wide",
  style = "report"
)

## ----labels-------------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = c("smoking", "physical_activity"),
  group_var = "education",
  labels = c("Smoking status", "Regular physical activity"),
  output = "gt"
)

## ----assoc-measure------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = "smoking",
  group_var = "education",
  assoc_measure = "phi",
  output = "tinytable"
)

## ----ci-rendered--------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = c("smoking", "physical_activity"),
  group_var = "education",
  assoc_ci = TRUE,
  output = "gt"
)

## ----ci-data------------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = "smoking",
  group_var = "education",
  assoc_ci = TRUE,
  output = "wide",
  style = "report"
)

## ----weighted-----------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = c("smoking", "physical_activity"),
  group_var = "education",
  weights = "weight",
  rescale = TRUE,
  output = "gt"
)

## ----missing------------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = "income_group",
  group_var = "education",
  drop_na = FALSE,
  output = "gt"
)

## ----levels-keep--------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = "income_group",
  group_var = "education",
  drop_na = FALSE,
  levels_keep = c("Low", "High", "(Missing)"),
  output = "gt"
)

## ----formatting---------------------------------------------------------------
table_apa(
  sochealth,
  row_vars = "smoking",
  group_var = "education",
  percent_digits = 2,
  p_digits = 4,
  v_digits = 3,
  output = "gt"
)

