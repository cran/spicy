## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

build_rich_tables <- identical(Sys.getenv("IN_PKGDOWN"), "true")

source("_pkgdown-helpers.R")

## ----setup--------------------------------------------------------------------
library(spicy)

## ----basic--------------------------------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(fit)

## ----beta---------------------------------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(fit, standardized = "refit")

## ----partial------------------------------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking + education,
          data = sochealth)
table_regression(
  fit,
  show_columns = c("b", "p", "all_eta2", "all_omega2")
)

## ----ame-lm-------------------------------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(
  fit,
  show_columns = c("b", "ame", "ame_ci", "ame_p")
)

## ----multi--------------------------------------------------------------------
m_wellbeing <- lm(wellbeing_score ~ age + sex + smoking,
                   data = sochealth)
m_bmi       <- lm(bmi             ~ age + sex + smoking,
                   data = sochealth)
table_regression(list(m_wellbeing, m_bmi))

## ----sochealth-cc-------------------------------------------------------------
sochealth_cc <- sochealth |>
  dplyr::select(wellbeing_score, age, sex, smoking, bmi, region) |>
  na.omit()

## ----nested-------------------------------------------------------------------
m1 <- lm(wellbeing_score ~ age + sex,                 data = sochealth_cc)
m2 <- lm(wellbeing_score ~ age + sex + smoking,       data = sochealth_cc)
m3 <- lm(wellbeing_score ~ age + sex + smoking + bmi, data = sochealth_cc)
table_regression(list(m1, m2, m3), nested = TRUE)

## ----hc-----------------------------------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(fit, vcov = "HC3")

## ----cr-----------------------------------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth_cc)
table_regression(
  fit,
  vcov = "CR2",
  cluster = ~region,
  show_columns = c("b", "se", "ci", "p", "ame", "ame_p")
)

## ----p-adjust-----------------------------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking + education,
          data = sochealth)
table_regression(fit, p_adjust = "bonferroni")

## ----keep---------------------------------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking + bmi + education,
          data = sochealth)
table_regression(fit, keep = c("^smoking", "^bmi$"))

## ----drop---------------------------------------------------------------------
table_regression(fit, drop = "^education")

## ----glm-basic----------------------------------------------------------------
sh <- sochealth |>
  dplyr::mutate(education = factor(education, ordered = FALSE))

fit <- glm(smoking ~ sex + age + education, data = sh,
           family = binomial)
table_regression(fit)

## ----glm-or-------------------------------------------------------------------
table_regression(fit, exponentiate = TRUE)

## ----glm-partial-chi2---------------------------------------------------------
fit2 <- glm(smoking ~ age + education, data = sh, family = binomial)
table_regression(fit2, show_columns = c("b", "partial_chi2", "p"))

## ----glm-pseudo---------------------------------------------------------------
table_regression(fit, standardized = "pseudo")

## ----glm-ame------------------------------------------------------------------
table_regression(fit, show_columns = c("b", "p", "ame", "ame_ci", "ame_p"))

## ----glm-profile--------------------------------------------------------------
table_regression(fit, ci_method = "profile")

## ----glm-nested---------------------------------------------------------------
m1 <- glm(smoking ~ sex,                   data = sh, family = binomial)
m2 <- glm(smoking ~ sex + age,             data = sh, family = binomial)
m3 <- glm(smoking ~ sex + age + education, data = sh, family = binomial)
table_regression(list(m1, m2, m3), nested = TRUE)

## ----stars--------------------------------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(fit, stars = TRUE)

## ----polish-------------------------------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + education, data = sochealth)
table_regression(
  fit,
  labels = c(
    age            = "Age (years)",
    sex            = "Sex",
    education      = "Education"
  ),
  reference_style = "annotation",
  intercept_position = "last",
  decimal_mark = ","
)

## ----data-frame---------------------------------------------------------------
out <- table_regression(
  lm(wellbeing_score ~ age + sex + smoking, data = sochealth),
  output = "data.frame"
)
str(out)

## ----long---------------------------------------------------------------------
out <- table_regression(
  lm(wellbeing_score ~ age + sex + smoking, data = sochealth),
  output = "long"
)
out[, c("model_id", "term", "estimate_type", "estimate",
        "std.error", "p.value")]

## ----gt-output, eval = build_rich_tables--------------------------------------
# pkgdown_dark_gt(
#   table_regression(
#     lm(wellbeing_score ~ age + sex + smoking, data = sochealth),
#     output = "gt"
#   )
# )

## ----broom--------------------------------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
out <- table_regression(fit, standardized = "refit")

broom::tidy(out)
broom::glance(out)

