## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(survstan)
library(ggplot2)
library(dplyr)
library(GGally)

## -----------------------------------------------------------------------------
data(ipass)
glimpse(ipass)

ipass <- ipass %>%
  mutate(
    arm = as.factor(ipass$arm), 
    arm = ifelse(arm == 1, "gefitinib", "carboplatin/paclitaxel")
  )

km <- survfit(Surv(time, status) ~ arm, data = ipass)
p <- ggsurv(km) 
p

## -----------------------------------------------------------------------------
ph <- phreg(Surv(time, status)~arm, data=ipass, baseline = "weibull")
po <- poreg(Surv(time, status)~arm, data=ipass, baseline = "weibull")
yp <- ypreg(Surv(time, status)~arm, data=ipass, baseline = "weibull")


anova(ph, yp)
anova(po, yp)

AIC(ph, po, yp)

## -----------------------------------------------------------------------------
glimpse(veteran)

veteran <- veteran %>%
  mutate(across(c(trt, celltype), as.factor))

dist <- "loglogistic"
formula <- Surv(time, status) ~ celltype + trt + karno

cox <- coxph(
  formula = formula,
  data=veteran
)

cox.zph(cox)

yp <- ypreg(
  formula = formula,
  data = veteran, dist = dist
)

ph <- phreg(
  formula = formula,
  data = veteran, dist = dist
)

po <- poreg(
  formula = formula,
  data = veteran, dist = dist
)

anova(ph, yp)
anova(po, yp)


logLik(ph, po, yp)
AIC(ph, po, yp)


