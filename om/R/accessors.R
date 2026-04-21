
#' @title Access outputs from \code{\link{om-class}} object. 
#' @aliases targets diagnostics pst objectives
#' @description Access outputs stored in \code{\link{om-class}} object following call to [pdyn()].
#' @param object \code{\link{om-class}} object. 
#' @importFrom crayon blue
#' @include om-class.R get_dim.R array2dfr.R
#{{{
#' @export
setGeneric("targets", function(object, ...) standardGeneric("targets"))
# accessor function
#' @rdname targets
setMethod("targets", signature = c("om"), function(object) {
    get_dim(object, env = environment())
    #message(blue("::: management target :::"))
    lapply(object@targets, function(x) { y <- data.frame(sample = 1:NITER, value = x);  as_tibble(y) })  
})
#}}}
#{{{
#' @export
setGeneric("diagnostics", function(object, ...) standardGeneric("diagnostics"))
# accessor function
#' @rdname targets
setMethod("diagnostics", signature = c("om"), function(object) {
    get_dim(object, env = environment())
    #message(blue("::: operating model output :::"))
    lapply(object@diagnostics, function(x) array2dfr(x, dim.names = list(sample = 1:dim(x)[1], iteration = 1:dim(x)[2], time = object@time)))
})
#}}}
#{{{
#' @export
setGeneric("pst", function(object, ...) standardGeneric("pst"))
# accessor function
#' @rdname targets
setMethod("pst", signature = c("om"), function(object) {
    get_dim(object, env = environment())
    #message(blue("::: operating model output :::"))
    array2dfr(object@pst$value, dim.names = list(sample = 1:NITER, iteration = 1:SITER, time = object@time))
})
#}}}

#{{{
#' @export
setGeneric("objectives", function(object, ...) standardGeneric("objectives"))
# accessor function
#' @rdname targets
setMethod("objectives", signature = c("om"), function(object) {
    get_dim(object, env = environment())
    #message(blue("::: probability of reaching management target :::"))
    lapply(object@objectives,  function(x) array2dfr(x, dim.names = list(sample = 1:NITER, time = object@time)))
})
#}}}

#{{{
#' @export
setGeneric("pars", function(object, ...) standardGeneric("pars"))
# accessor function
#' @rdname targets
setMethod("pars", signature = c("om"), function(object) {
    object@pars
})
#}}}


