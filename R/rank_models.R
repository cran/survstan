
#' Rank a collection of survstan models
#' @aliases rank_models
#' @export
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which function is called.
#' @param survreg survival regression models to be fitted (AFT, AH, PH, PO, YP and EH).
#' @param baseline baseline distributions to be fitted; options currently available are: exponential, weibull, lognormal, loglogistic and Birnbaum-Saunders (fatigue) distributions.
#' @param dist alternative way to specify the baseline distributions (for compability with the \code{\link[survival]{survreg}} function); default is NULL.
#' @param ... further arguments passed to other methods.
#' @return a tibble containing the fitted models ranked according to their AICs.
#' @examples
#' \donttest{
#' library(survstan)
#' library(dplyr)
#'
#' veteran <- veteran %>%
#'   mutate(across(c(trt, prior, celltype), as.factor))
#' fits <- rank_models(
#'   formula = Surv(time, status) ~ celltype+karno,
#'   data = veteran,
#'   survreg = c("aftreg", "ahreg", "phreg", "poreg", "ypreg", "ehreg"),
#'   baseline = c("exponential", "weibull", "lognormal", "loglogistic", "fatigue", "gamma", "rayleigh")
#' )
#' }
#'
#'

rank_models <- function(formula, data, survreg, baseline, dist = NULL, ...){

  opt <- options(pillar.sigfig = 6)

  if(!is.null(dist)){
    baseline <- dist
  }

  # fit <- function(survreg, baseline, formula, data) {
  #   do.call(survreg,
  #           list(
  #             formula = formula,
  #             baseline = baseline,
  #             data = quote(data)
  #           )
  #   )
  # }

  fit <- function(survreg, baseline, formula, data) {
    tryCatch(
      expr = {
        do.call(survreg,
                list(
                  formula = formula,
                  baseline = baseline,
                  data = quote(data)
                )
        )
      }, error = function(e) NULL
    )
  }

  models <- tidyr::expand_grid(
    survreg = survreg,
    baseline = baseline
  ) %>%
    dplyr::mutate(
      fit = purrr::pmap(
        list(survreg, baseline),
        purrr::possibly(fit, otherwise = NA, quiet = TRUE),
        formula = formula, data = data
      )
    ) %>%
    dplyr::mutate(
      loglik = purrr::map_dbl(fit, purrr::possibly( ~ logLik(.x), otherwise = NA, quiet = TRUE)),
      npars = purrr::map_dbl(fit, purrr::possibly( ~ length(estimates((.x), otherwise = NA, quiet = TRUE)))),
      aic = purrr::map_dbl(fit, purrr::possibly( ~ AIC(.x), otherwise = NA, quiet = TRUE))
    ) %>%
    dplyr::arrange(.data$aic)
}

