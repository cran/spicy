## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

build_rich_tables <- identical(Sys.getenv("IN_PKGDOWN"), "true")

## ----setup--------------------------------------------------------------------
library(spicy)

## ----label-from-names---------------------------------------------------------
df <- tibble::tibble(
  "age. Age of respondent" = c(25, 30, 41),
  "edu. Highest education level" = c("Lower", "Upper", "Tertiary"),
  "smoke. Current smoker" = c("No", "Yes", "No")
)

out <- label_from_names(df)
labelled::var_label(out)

## ----varlist-interactive, eval = FALSE----------------------------------------
# varlist(sochealth)

## ----vl-interactive, eval = FALSE---------------------------------------------
# vl(sochealth)

## ----varlist-all--------------------------------------------------------------
varlist(sochealth, tbl = TRUE)

## ----varlist-include-na-------------------------------------------------------
head(subset(varlist(sochealth, include_na = TRUE, tbl = TRUE), NAs > 0))

## ----varlist-values-----------------------------------------------------------
head(subset(varlist(sochealth, values = TRUE, tbl = TRUE), N_distinct <= 5))

## ----varlist-selected---------------------------------------------------------
varlist(sochealth, smoking, education, income_group, tbl = TRUE)

## ----varlist-tidyselect-------------------------------------------------------
varlist(sochealth, starts_with("life_sat"), tbl = TRUE)

## ----varlist-numeric----------------------------------------------------------
varlist(sochealth, where(is.numeric), tbl = TRUE)

## ----vl-example---------------------------------------------------------------
vl(sochealth, starts_with("bmi"), tbl = TRUE)

## ----code-book-basic, eval = build_rich_tables--------------------------------
# if (requireNamespace("DT", quietly = TRUE)) {
#   code_book(sochealth)
# }

## ----code-book-select, eval = build_rich_tables-------------------------------
# if (requireNamespace("DT", quietly = TRUE)) {
#   code_book(
#     sochealth,
#     starts_with("bmi"),
#     values = TRUE,
#     title = "BMI codebook",
#     filename = "bmi_codebook"
#   )
# }

## ----code-book-values, eval = build_rich_tables-------------------------------
# if (requireNamespace("DT", quietly = TRUE)) {
#   code_book(sochealth, values = TRUE, include_na = TRUE)
# }

