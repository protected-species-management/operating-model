#' @title Summarise distribution class object
#' @description Returns summary statistics for a given distribution.
#' @param object input distribution class object
#' @param ... (ignored)
#' @include distribution-class.R
#' @exportS3Method base::summary
summary.distribution <- function(object, ...) {
    
    if (grepl("^unspecified", object@density))  return(.show_unspecified_moments(object@.Data))
    if (grepl("^uniform", object@density))      return(.show_uniform_moments(object@pars))
    if (grepl("^beta", object@density))         return(.show_beta_moments(object@pars))
    if (grepl("^normal", object@density))       return(.show_normal_moments(object@pars))
    if (grepl("^zt?.normal", object@density))   return(.show_ztnormal_moments(object@pars))
    if (grepl("^log?normal", object@density))   return(.show_lognormal_moments(object@pars))
    if (grepl("^gamma", object@density))        return(.show_gamma_moments(object@pars))
    if (grepl("^logit?normal", object@density)) return(.show_logitnormal_moments(object@pars))
}
#' @export
#' @rdname summary.distribution
expectation <- function(object, ...) UseMethod("expectation")
#' @exportS3Method om::expectation
expectation.distribution <- function(object, ...) {
    summary(object)['E[x]']
}