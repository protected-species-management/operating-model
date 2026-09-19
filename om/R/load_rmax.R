#' @title Load maximum intrinsic growth parameter
#' 
#' @description Load \eqn{r_{max}} into the \code{pars} slot of an \code{\link{om}} class object.
#' @details The intrinsic growth rate is assumed to have normal distribution. It is converted to a zero-truncated normal on assignment because the maximum growth rate is always assumed to be greater than zero (i.e., \eqn{r_{max} > 0}). 
#' @param object \code{om} class object.
#' @param value \code{distribution} class object. If no value is provided, the distribution is obtained directly from the life-history parameters stored in the object.
#' @param ... arguments for the generic function definition.
#' @include om-class.R distribution-class.R sample.distribution.R
#' @importFrom cli cli_alert_danger
#' @export
#{{{ load rmax into om object
setGeneric("load_rmax", function(object, value, ...) standardGeneric("load_rmax"))
#{{ distribution object
#' @rdname load_rmax
setMethod("load_rmax", signature = c("om", "distribution"), function(object, value) {

    # checks
    if (value@density != "unspecified") {
        if (value@density != "normal" & value@density != "zt-normal") {
            cli_alert_danger("input distribution is not 'normal' or 'zt-normal'")
        } else {
            value@density <- "zt-normal"
        }
    }
    
    # assign zt-density distribution
    object@pars$rmax      <- value
    object@pars$rmax@name <- "max. intrinsic growth rate"
    
    # return    
    return(object)
})
#' @rdname load_rmax
setMethod("load_rmax", signature = c("om", "missing"), function(object, value) {
    
    # use 'r' by default
    value <- object@pars$r 

    # return    
    return(load_rmax(object, value))
})
