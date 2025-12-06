
#' @title Access outputs from \code{\link{om-class}} object. 
#' @aliases targets diagnostics pst objectives
#' @description Access outputs stored in \code{\link{om-class}} object following call to [pdyn()].
#' @param object \code{\link{om-class}} object. 
#' @importFrom tibble as_tibble
#' @importFrom crayon blue
#' @include om-class.R get_dim.R
#{{{
#' @export
setGeneric("targets", function(object, ...) standardGeneric("targets"))
# accessor function
#' @rdname targets
setMethod("targets", signature = c("om"), function(object) {
    get_dim(object, env = environment())
    #message(blue("::: management target :::"))
    lapply(object@targets, function(x) { y <- data.frame(iter = 1:object@iter, value = x[1,]);  as_tibble(y) })  
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
    lapply(object@diagnostics, function(x) { dimnames(x) <- list(time = time, iter = 1:niter);  y <- array2DF(x, responseName = "value"); y$iter <- as.integer(y$iter); y$time <- as.integer(y$time); as_tibble(y)})
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
    x <- object@pst$value
    dimnames(x) <- list(time = time, iter = 1:niter);  y <- array2DF(x, responseName = "value"); y$iter <- as.integer(y$iter); y$time <- as.integer(y$time); as_tibble(y)
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
    lapply(object@objectives,  function(x) { y <- data.frame(time = time, value = x);  as_tibble(y) })
})
#}}}

