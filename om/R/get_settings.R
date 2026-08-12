#' @title Get settings from object
#' @description
#' Extract values from \code{\link{om}} object for use within a function call.
#' @param object \code{om} class object
#' @param iter index value
#' @param env environment into which values should be returned using \code{\link{assign}}
#' @param ... (not used)
#' @seealso \code{\link{get_values}}, \code{\link{get_dim}}, \code{\link{get_seeds}}, \code{\link{get_shape}}
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
