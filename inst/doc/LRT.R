## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
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
ggsurv(km) 

## -----------------------------------------------------------------------------
aft <- aftreg(Surv(time, status)~arm, data=ipass, dist = "weibull")
ah <- ahreg(Surv(time, status)~arm, data=ipass, dist = "weibull")
ph <- phreg(Surv(time, status)~arm, data=ipass, dist = "weibull")
po <- poreg(Surv(time, status)~arm, data=ipass, dist = "weibull")
yp <- ypreg(Surv(time, status)~arm, data=ipass, dist = "weibull")
eh <- ehreg(Surv(time, status)~arm, data=ipass, dist = "weibull")

anova(ph, yp)
anova(po, yp)

anova(aft, eh)
anova(ah, eh)
anova(ph, eh)


