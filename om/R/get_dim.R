#' @title get_dim
#' @description
#' Extract dimensions from object for use within a function call.
#' @include om-class.R
#' @export
get_dim <- function(object, ...) UseMethod("get_dim")
#' @rdname get_dim
#' @export
get_dim.om <- function(object, env = globalenv()) {
    
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
#' @rdname get_values
#' @export
get_values.om <- function(object, iter = 1, env = globalenv()) {
    
    l1 <- object@productivity
    l2 <- object@life_history
    l3 <- object@fishing
    ll <- c(l1, l2,l3)
    
    rm(object)
    
    if (is.environment(env)) {
        lapply(names(ll), function(x) assign(x, ll[[x]][, iter], envir = env))
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}
