#' @title Get random seeds from object
#' @aliases get_seed
#' @description
#' Extract vector of random number seeds from \code{\link{om}} object.
#' @returns An integer vector of length equal to \code{object@samples} is assigned to \code{'rng_seed'} within the specified environment. This vector contains random number seeds generated automatically during construction of the \code{om} object. These seeds are used to ensure that each function call during conditioning of an \code{om} object execute the same sequence of random samples from the life-history input distributions.
#' @param object \code{om} class object
#' @param env environment into which values should be returned using \code{\link{assign}}
#' @param ... (not used)
#' @seealso \code{\link{get_values}}, \code{\link{get_dim}}, \code{\link{get_settings}}, \code{\link{get_shape}}
#' @examples
#' om_object <- om(ages = 0:1, time = 1, samples = 3)
#' 
#' get_seeds(om_object, env = globalenv())
#' rng_seed
#' 
#' ff <- function() { 
#'     get_seeds(om_object, env = environment()); return(rng_seed) 
#' }
#' ff()
#' 
#' ff <- function() { 
#'     get_seeds(om_object, env = environment())
#'     unlist(lapply(rng_seed, function(x) { set.seed(x); rnorm(1) }))
#' }
#' ff()
#' 
#' @importFrom methods slot
#' @include om-class.R
#' @export
get_seeds <- function(object, ...) UseMethod("get_seeds")
#' @rdname get_seeds
#' @exportS3Method om::get_seeds
get_seeds.om <- function(object, env = environment(), ...) {
    
    if (is.environment(env)) {
        assign("rng_seed", slot(get("object"), "seeds"), envir = env)
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}
