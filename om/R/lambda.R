#' @title Extract lambda
#' 
#' @description Extracts lambda from om object.
#' @param object \code{om} class object
#' @export
#' @include om-class.R distribution-class.R sample.distribution.R
#' @import cli
#{{{ shape()
# wrapper for execution of population
# dynamics function
setGeneric("lambda", function(object, ...) standardGeneric("lambda"))
setMethod("lambda", signature = c(object = "om"), function(object) {
  
    if (is(object@pars$r, "distribution")) {
    
        x <- sample(object@pars$r, n = 1e5)
        x <- distribution(values = exp(x), density = "lognormal", name = "lambda")
        
        return(x)
        
    } else {
        
        cli_alert_danger("'@pars' does not contain 'r'")
    }
})
