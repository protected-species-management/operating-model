#' @title Get dimensions from object
#' @aliases get_dims
#' @description
#' Extract dimensions from \code{object@settings} slot in \code{\link{om}} object for use within a function call. Which dimensions are extracted depends on whether reference points are being estimated or a projection is being performed. 
#' @param object \code{om} class object
#' @param projection logical value
#' @param ref_points logical value
#' @param env environment into which values should be returned using \code{\link{assign}}
#' @param ... (not used)
#' @importFrom methods slot
#' @include om-class.R
get_dim <- function(object, ...) UseMethod("get_dim")
#' @rdname get_dim
get_dim.om <- function(object, projection = TRUE, ref_points = !projection, env = environment(), ...) {
    
    ages   <- object@ages
    time   <- object@time
    
    NITER  <- object@samples
    SITER  <- ifelse(is.na(object@settings$projection$iterations), 1L, object@settings$projection$iterations)
    
    NAGES  <- length(ages)
    NTIME  <- length(time)
    
    STOCHASTIC <- object@settings$projection$stochastic
    
    if (ref_points) {
        
        NTIME <- object@settings$ref_points$time
        SITER <- ifelse(is.na(object@settings$ref_points$iterations), 1L, object@settings$ref_points$iterations)
        
        STOCHASTIC <- object@settings$ref_points$stochastic
    }
    
    if (is.environment(env)) {
        lapply(c("ages", "time", "NITER", "SITER", "NAGES", "NTIME", "STOCHASTIC"), function(x) assign(x, get(x), envir = env))
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}

