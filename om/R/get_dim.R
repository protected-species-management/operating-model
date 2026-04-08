#' @title Get dimensions and values stored in \code{\link{om-class}} object.
#' @aliases get_values
#' @description
#' Extract dimensions and/or values from object for use within a function call.
#' @include om-class.R
#' @export
get_dim <- function(object, ...) UseMethod("get_dim")
#' @rdname get_dim
#' @export
get_dim.om <- function(object, env = environment()) {
    
    ages   <- object@ages
    time   <- object@time
    niter  <- object@iter[1]
    siter  <- object@iter[2]
    
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
get_values.om <- function(object, iter = 1, env = environment()) {
    
    ll <- object@pars
    
    rm(object)
    
    if (is.environment(env)) {
        lapply(names(ll), function(x) assign(x, ll[[x]][, iter], envir = env))
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}
#' @export
get_fixed <- function(object, ...) UseMethod("get_fixed")
#' @rdname get_dim
#' @export
get_fixed.om <- function(object, env = environment()) {
    
    if (is.environment(env)) {
        lapply(names(object@fixed), function(x) assign(x, slot(get("object"), "fixed")[[x]], envir = env))
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}
#' @export
get_seeds <- function(object, ...) UseMethod("get_seeds")
#' @rdname get_dim
#' @export
get_seeds.om <- function(object, env = environment()) {
    
    if (is.environment(env)) {
        assign("rng_seed", slot(get("object"), "seeds"), envir = env)
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}

#' @export
get_shape <- function(object, ...) UseMethod("get_shape")
#' @rdname get_dim
#' @export
get_shape.om <- function(object, env = environment()) {
    
    if (is.environment(env)) {
        assign("shape", slot(get("object"), "shape"), envir = env)
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}

