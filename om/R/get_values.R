#' @title Get values from object
#' @aliases get_value
#' @description
#' Extract values from \code{\link{om}} object for use within a function call.
#' @param object \code{om} class object
#' @param iter index value
#' @param env environment into which values should be returned using \code{\link{assign}}
#' @param ... (not used)
#' @seealso \code{\link{get_settings}}, \code{\link{get_dim}}, \code{\link{get_seeds}}, \code{\link{get_shape}}
#' @importFrom methods slot
#' @include om-class.R
#' @export
get_values <- function(object, ...) UseMethod("get_values")
#' @rdname get_values
#' @exportS3Method om::get_values
get_values.om <- function(object, iter = 1, env = environment(), ...) {
    
    ll <- object@pars
    
    rm(object)
    
    if (is.environment(env)) {
        lapply(names(ll), function(x) assign(x, ll[[x]][, iter], envir = env))
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}
