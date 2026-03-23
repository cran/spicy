## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(spicy)

## ----assoc-all----------------------------------------------------------------
tbl <- xtabs(~ smoking + education, data = sochealth)
assoc_measures(tbl)

## ----cramer-------------------------------------------------------------------
tbl <- xtabs(~ smoking + education, data = sochealth)
cramer_v(tbl)

## ----cramer-detail------------------------------------------------------------
cramer_v(tbl, detail = TRUE)

## ----phi----------------------------------------------------------------------
tbl_22 <- xtabs(~ smoking + physical_activity, data = sochealth)
phi(tbl_22, detail = TRUE)

## ----contingency--------------------------------------------------------------
contingency_coef(tbl, detail = TRUE)

## ----gamma--------------------------------------------------------------------
tbl_ord <- xtabs(~ self_rated_health + education, data = sochealth)
gamma_gk(tbl_ord, detail = TRUE)

## ----tau-b--------------------------------------------------------------------
kendall_tau_b(tbl_ord, detail = TRUE)

## ----tau-c--------------------------------------------------------------------
kendall_tau_c(tbl_ord, detail = TRUE)

## ----somers-------------------------------------------------------------------
somers_d(tbl_ord, detail = TRUE)

## ----lambda-------------------------------------------------------------------
tbl <- xtabs(~ self_rated_health + education, data = sochealth)
lambda_gk(tbl, detail = TRUE)

## ----gk-tau-------------------------------------------------------------------
goodman_kruskal_tau(tbl, detail = TRUE)

## ----uncertainty--------------------------------------------------------------
uncertainty_coef(tbl, detail = TRUE)

## ----yule---------------------------------------------------------------------
tbl_22 <- xtabs(~ smoking + physical_activity, data = sochealth)
yule_q(tbl_22, detail = TRUE)

## ----cross-tab-auto-----------------------------------------------------------
# Nominal: Cramer's V
cross_tab(sochealth, smoking, education)

# Ordinal: Kendall's Tau-b (automatic)
cross_tab(sochealth, self_rated_health, education)

## ----cross-tab-override-------------------------------------------------------
cross_tab(sochealth, self_rated_health, education, assoc_measure = "gamma")

## ----ci-level-----------------------------------------------------------------
cramer_v(tbl, detail = TRUE, conf_level = 0.99)

## ----ci-null------------------------------------------------------------------
cramer_v(tbl, detail = TRUE, conf_level = NULL)

## ----digits-------------------------------------------------------------------
cramer_v(tbl, detail = TRUE, digits = 4)

## ----digits-table-------------------------------------------------------------
assoc_measures(tbl, digits = 2)

## ----digits-print-------------------------------------------------------------
res <- cramer_v(tbl, detail = TRUE)
print(res, digits = 5)

