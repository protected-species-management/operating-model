#' @title Get dimensions and values stored in \code{\link{om-class}} object.
#' @aliases get_values
#' @description
#' Extract dimensions and/or values from object for use within a function call.
#' @include om-class.R
#' @export
get_dim <- function(object, ...) UseMethod("get_dim")
#' @rdname get_dim
#' @export
get_dim.om <- function(object, env) {
    
    ages   <- object@ages
    time   <- object@time
    niter  <- object@iter
    
    nages  <- length(ages)
    ntime  <- length(time)
    
    rm(object)
    
    if (is.environment(env)) {
        lapply(ls(), function(x) assign(x, get(x), envir = env))
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}
#' @export
get_values <- function(object, ...) UseMethod("get_values")
#' @rdname get_dim
#' @export
get_values.om <- function(object, iter = 1, env) {
    
    l1 <- object@pars
    l2 <- object@life_history
    l3 <- object@fishery_inputs
    ll <- c(l1, l2,l3)
    
    rm(object)
    
    if (is.environment(env)) {
        lapply(names(ll), function(x) assign(x, ll[[x]][, iter], envir = env))
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}
