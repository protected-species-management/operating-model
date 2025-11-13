#' @title Operating model class
#' 
#' @description 
#' Operating model class definition.
#'
#{{{
# class definition
setClass("om", contains = "array", slots = list(ages = 'integer', iter = 'integer', time = 'numeric', pars = 'list', fishery_inputs = 'list', life_history = 'list', population_dynamics = 'function', pst = 'list'))
#}}}
#{{{
# initialisation function
setMethod("initialize", "om", function(.Object, pdyn_function, iter, time, ages, ...) {
    
    if(missing(pdyn_function)) {
        .Object@population_dynamics <- function() NA_real_
    } else {
        .Object@population_dynamics <- pdyn_function
    }
    
    if(missing(iter)) {
        .Object@iter <- NA_integer_
    } else {
        .Object@iter <- iter
    }
    
    if(missing(time)) {
        .Object@time <- NA_integer_
    } else {
        .Object@time <- time
    }
    
    if(missing(ages)) {
        .Object@ages <- NA_integer_
    } else {
        .Object@ages <- ages
    }
    
    # default reference point
    # tuning parameter
    .Object@pst$phi  <- 1
    .Object@pst$rmax <- NA_real_
    
    return(.Object)
})
#}}}

#{{{
# class definition
#setClass("omIter", contains = "matrix",
#         slots=list(
#             ages                = 'numeric',
#             time                = 'numeric',
#             productivity        = 'list',
#             fishing             = 'list',
#             life_history        = 'list',
#             population_dynamics = 'function',
#             pst                 = 'list'
#         )
#)
#}}}
