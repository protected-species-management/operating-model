#' @title Get settings from object
#' @description
#' Extract settings from \code{\link{om}} object.
#' @returns Lists containing values in \code{object@settings} are assigned to \code{'cv'}, \code{'bias'}, \code{'qn'}, \code{'projection'} and \code{'ref_points'} within the specified environment.
#' @param object \code{om} class object
#' @param env environment into which values should be returned using \code{\link{assign}}
#' @param ... (not used)
#' @seealso \code{\link{get_values}}, \code{\link{get_dim}}, \code{\link{get_seeds}}, \code{\link{get_shape}}
#' @examples
#' om_object <- om(ages = 0:1, time = 11, samples = 3)
#' 
#' # examine object contents
#' # directly
#' om_object@settings
#' 
#' # return to local environment
#' get_settings(om_object, env = globalenv())
#' ls()
#' 
#' # coefficients of variation for
#' # stochastic projection
#' unlist(cv)
#' 
#' # bias for
#' # projection
#' unlist(bias)
#' 
#' # numbers quantile for
#' # projection
#' unlist(qn)
#'
#' # default settings
#' # for projection 
#' unlist(projection)
#' 
#' # default settings
#' # for projection 
#' unlist(ref_points)
#' 
#' @importFrom methods slot
#' @include om-class.R
#' @export
get_settings <- function(object, ...) UseMethod("get_settings")
#' @rdname get_settings
#' @exportS3Method om::get_settings
get_settings.om <- function(object, env = environment(), ...) {
    
    if (is.environment(env)) {
        lapply(names(object@settings), function(x) assign(x, slot(get("object"), "settings")[[x]], envir = env))
    } else {
        warning("not a valid environment!")    
    }
    
    invisible()
}
