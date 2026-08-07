#' @title Extract growth rate
#' @description Extracts \eqn{lambda} from an \code{om} object.
#' @param object \code{om} class object
#' @param log logical value indication whether \eqn{r = \log(\lambda)} should be returned.
#' @param ... (not used)
#' @export
#' @include om-class.R distribution-class.R sample.distribution.R
#' @importFrom methods is
#' @importFrom cli cli_alert_danger
#{{{ shape()
# wrapper for execution of population
# dynamics function
setGeneric("lambda", function(object, ...) standardGeneric("lambda"))
#' @rdname lambda
setMethod("lambda", signature = c(object = "om"), function(object, log = FALSE) {
  
    if (is(object@pars$r, "distribution")) {
    
        x <- sample(object@pars$r, size = 1e5)
        x <- distribution(values = x,      density = "normal",    name = "r")
        y <- distribution(values = exp(x), density = "lognormal", name = "lambda")
        
        if (log) {
          return(x)
        } else {
          return(y) 
        }
        
    } else {
        
        cli_alert_danger("'@pars' does not contain 'r'")
    }
})
