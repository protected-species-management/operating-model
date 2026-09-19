#' @noRd
#' @title Get shape values from object
#' @description
#' Extract vector of shape values from \code{\link{om}} object.
#' @returns A vector of length equal to \code{object@samples} is assigned to \code{'shape'} within the specified environment. This will rarely be useful in a standard workflow. Use \code{\link{shape}} instead.
#' @param object \code{om} class object
#' @param env environment into which values should be returned using \code{\link{assign}}
#' @param ... (not used)
#' @seealso \code{\link{.get_values}}, \code{\link{.get_dim}}, \code{\link{.get_seeds}}, \code{\link{.get_settings}}, \code{\link{shape}}
#' @examples
#' \dontrun{
#' om_object <- om(ages = 0:1, time = 1, samples = 3)
#' 
#' # assign and access using 'shape' function
#' shape(om_object) <- 1
#' shape(om_object)
#' 
#' # access using 'get_shape'
#' .get_shape(om_object, env = globalenv())
#' shape
#' 
#' ff <- function() { .get_shape(om_object, env = environment()); return(shape) }
#' ff()
#' }
#' @importFrom methods slot
#' @include om-class.R
.get_shape <- function(object, ...) UseMethod(".get_shape")
.get_shape.om <- function(object, env = environment(), ...) {
    
    if (is.environment(env)) {
        assign("shape", slot(get("object"), "shape"), envir = env)
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}

