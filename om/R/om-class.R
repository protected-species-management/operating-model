#' @title Operating model class
#' 
#' @description 
#' Operating model class definition.
#'
#' @importFrom crayon blue red
#{{{
# class definition
setClass("om", contains = "array", slots = list(ages = 'integer', iter = 'integer', time = 'numeric', pars = 'list', fishery_inputs = 'list', life_history = 'list', population_dynamics = 'function', pst = 'list', diagnostics = 'list'))
#}}}
#{{{
# initialisation function
setMethod("initialize", "om", function(.Object, ages, pdyn_function, iter, time, ...) {
    
    if(missing(pdyn_function) | missing(ages)) {
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
    .Object@pst$phi     <- 1
    .Object@pst$rmax    <- NA_real_
    .Object@pst$numbers <- NA_real_
    .Object@pst$value   <- NA_real_
    
    # set up diagnostics
    .Object@diagnostics$catch_pst <- NA_integer_
    .Object@diagnostics$h_hmnpl   <- NA_integer_
    
    # return
    return(.Object)
})
#}}}
# {{{
setMethod("show", "om",
          function(object) {
              message(blue("om-class object"))
              message("time: ", if (length(object@time) > 8) paste0(c(object@time[1:6], "...", object@time[length(object@time)]), collapse = ", ") else paste0(object@time, collapse = ", "))
              message("ages: ", if (length(object@ages) > 14) paste0(c(object@ages[1:12], "...", object@ages[length(object@ages)]), collapse = ", ") else paste0(object@ages, collapse = ", "))
              message("\t")
              message("ntime: ", if (all(is.na(object@time))) NA_character_ else length(object@time))
              message("nages: ", if (all(is.na(object@ages))) NA_character_ else length(object@ages))
              message("niter: ", object@iter)
              message("\t")
              message("fishery_inputs: ", if (length(object@fishery_inputs) > 0)  paste0(names(object@fishery_inputs), collapse = ", ") else red("NULL"))
              message("life_history: ", if (length(object@life_history) > 0)  paste0(names(object@life_history), collapse = ", ") else red("NULL"))
              message("pars: ", if (length(object@pars) > 0) paste0(names(object@pars), collapse = ", ") else red("NULL"))
              message("\n")
              print(object@population_dynamics)
          })
# }}}

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
