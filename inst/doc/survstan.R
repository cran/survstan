## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(survstan)
library(dplyr)

ovarian <- ovarian %>%
  mutate(
    across(c("rx", "resid.ds"), as.factor)
  )

survreg <- survreg(
  Surv(futime, fustat) ~ ecog.ps + rx, 
  dist = "weibull", data = ovarian
)

survstan <- aftreg(
  Surv(futime, fustat) ~ ecog.ps + rx, 
  dist = "weibull", data = ovarian
)


## -----------------------------------------------------------------------------
summary(survreg)
summary(survstan)

## -----------------------------------------------------------------------------
survstan <- phreg(Surv(futime, fustat) ~ ecog.ps + rx, data = ovarian, dist = "weibull")
cox <- coxph(
  Surv(futime, fustat) ~ ecog.ps + rx, data = ovarian
)
coef(survstan)
coef(cox)

