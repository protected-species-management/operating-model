#' @title Get values from object
#' @aliases get_value get_settings get_seeds get_shape
#' @description
#' Extract values from \code{\link{om-class}} object for use within a function call.
#' @param object \code{om} class object
#' @param iter index value
#' @param env environment into which values should be returned using \code{\link{assign}}
#' @param ... (not used)
#' @importFrom methods slot
#' @include om-class.R
#' @export
get_values <- function(object, ...) UseMethod("get_values")
#' @export
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
#' @export
get_settings <- function(object, ...) UseMethod("get_settings")
#' @rdname get_values
#' @export
get_settings.om <- function(object, env = environment(), ...) {
    
    if (is.environment(env)) {
        lapply(names(object@settings), function(x) assign(x, slot(get("object"), "settings")[[x]], envir = env))
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}
#' @export
get_seeds <- function(object, ...) UseMethod("get_seeds")
#' @rdname get_values
#' @export
get_seeds.om <- function(object, env = environment(), ...) {
    
    if (is.environment(env)) {
        assign("rng_seed", slot(get("object"), "seeds"), envir = env)
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}
#' @export
get_shape <- function(object, ...) UseMethod("get_shape")
#' @rdname get_values
#' @export
get_shape.om <- function(object, env = environment(), ...) {
    
    if (is.environment(env)) {
        assign("shape", slot(get("object"), "shape"), envir = env)
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}

