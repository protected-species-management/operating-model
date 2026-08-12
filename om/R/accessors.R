#' @title Access slots within an \code{om} object. 
#' @aliases targets diagnostics pst objectives pars numbers settings
#' @description Access information stored in \code{\link{om}} object. The same information can be accessed in raw format using \code{object@<function>}.
#' @param object \code{\link{om}} class object. 
#' @param ... arguments for the generic function definition
#' @importFrom cli cli_alert_danger cli_alert_warning
#' @importFrom dplyr bind_rows
#' @include om-class.R get_dim.R array2dfr.R
#' @examples
#' 
#' # error messages are
#' # generated when applied
#' # to an empty object
#' om_object <- om(ages = 0:1, time = 1, samples = 3)
#' 
#' targets(om_object)
#' objectives(om_object)
#' diagnostics(om_object)
#' pst(om_object)
#' 
#' # an empty object has
#' # the following settings
#' # by default
#' settings(om_object)
#' 
#' # when slots have values
#' # they can be retrieved
#' data(om_hdo)
#' 
#' targets(om_hdo)
#' pars(om_hdo)
#' 
#' # some slots are empty
#' # until a projection 
#' # has been performed
#' pst(om_hdo)
#' pst(pdyn(om_hdo))
#' diagnostics(om_hdo)
#' diagnostics(pdyn(om_hdo))
#' 
#{{{
#' @export
setGeneric("targets", function(object, ...) standardGeneric("targets"))
# accessor function
#' @rdname targets
setMethod("targets", signature = c("om"), function(object) {
    
    get_dim(object, env = environment())
    NITER <- get("NITER")
    
    if (all(is.na(unlist(object@targets)))) {
        cli_alert_danger("'<object>@targets' is empty - run 'shape()' and 'rp()'")
    } else {
        if (all(is.na(object@targets$harvest_rate))) {
            cli_alert_danger("'<object>@targets' is not valid - re-run 'shape()' and 'rp()'")
        } else {
            if (all(is.na(object@targets$captures)) | all(is.na(object@targets$depletion))) {
                cli_alert_warning("some '<object>@targets' are empty - run 'rp()'")
            }
            lapply(object@targets, function(x) { y <- data.frame(sample = 1:NITER, value = x);  as_tibble(y) })  
        }
    }
})
#}}}
#{{{
#' @export
setGeneric("diagnostics", function(object, ...) standardGeneric("diagnostics"))
# accessor function
#' @rdname targets
setMethod("diagnostics", signature = c("om"), function(object) {
    tryCatch(lapply(object@diagnostics, function(x) array2dfr(x, dim.names = list(sample = 1:dim(x)[1], iteration = 1:dim(x)[2], time = object@time))), error = function(e) cli_alert_danger("'<object>@diagnostics' is empty - run 'pdyn()'"))
})
#}}}
#{{{
#' @export
setGeneric("pst", function(object, ...) standardGeneric("pst"))
# accessor function
#' @rdname targets
setMethod("pst", signature = c("om"), function(object) {
    get_dim(object, env = environment())
	NITER <- get("NITER")
    SITER <- get("SITER")
    tryCatch(array2dfr(object@pst$value, dim.names = list(sample = 1:NITER, iteration = 1:SITER, time = object@time)), error = function(e) cli_alert_danger("'<object>@pst' is empty - run 'pdyn()'"))
})
#}}}

#{{{
#' @export
setGeneric("objectives", function(object, ...) standardGeneric("objectives"))
# accessor function
#' @rdname targets
setMethod("objectives", signature = c("om"), function(object) {
    get_dim(object, env = environment())
	NITER <- get("NITER")
    tryCatch(lapply(object@objectives,  function(x) array2dfr(x, dim.names = list(sample = 1:NITER, time = object@time))), error = function(e) cli_alert_danger("'<object>@objectives' is empty - run 'pdyn()'"))
})
#}}}

#{{{
#' @export
setGeneric("pars", function(object, ...) standardGeneric("pars"))
# accessor function
#' @rdname targets
setMethod("pars", signature = c("om"), function(object) {
    loc <- unlist(lapply(object@pars, function(x) is(x, 'distribution')))
    if (sum(loc) > 0) object@pars[loc] else cli_alert_danger("'<object>@pars' is empty")
})
#}}}
#{{{
#' @export
setGeneric("settings", function(object, ...) standardGeneric("settings"))
# accessor function
#' @rdname targets
setMethod("settings", signature = c("om"), function(object) {
    lapply(lapply(object@settings, bind_rows), data.frame)
})
#}}}
#{{{
#' @export
setGeneric("numbers", function(object, ...) standardGeneric("numbers"))
# accessor function
#' @rdname targets
setMethod("numbers", signature = c("om"), function(object) {
    
	get_dim(object, env = environment())
	
	NITER <- get("NITER")
    SITER <- get("SITER")
	
    tryCatch(array2dfr(object@.Data, dim.names = list(sample = 1:NITER, iteration = 1:SITER, time = object@time)), error = function(e) cli_alert_danger("'<object>@.Data' is empty - run 'pdyn()'"))
})
#}}}

