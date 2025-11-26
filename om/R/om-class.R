#' @title Operating model class
#' 
#' @description 
#' Operating model class definition.
#' @slot ages integer vector of ages assumed by operating model. Set to \code{NA} when a cohort aggregated model is assumed.
#' @slot time integer vector times used for operating model projection.
#' @slot iter integer value indicating number of stochastic iterations.
#' @slot life_history names list of life history inputs. See \code{\link{load_life_history}}.
#' @slot fishery_inputs named list of fishery inputs. See \code{\link{load_fishery_inputs}}.
#' @slot pars list of values used by the operating model. See \code{\link{load_pars}}.
#' @slot population_dynamics function containing the operating model. Can take any value stored in the \code{life_history}, \code{fishery_inputs} and \code{pars} slots.
#' @slot pst list containing \code{phi}, \code{rmax}, \code{numbers} and \code{value} elements related to the PST threshold reference point.
#' @slot targets list containing \code{catch}, \code{depletion} and \code{harvest_rate} target reference points. These should be set at the appropriate level for the operating model being assumed. See \code{load_targets}.
#' @slot objectives list containing probability values indicating whether management target has been reached (i.e., the realised objective values) for comparison with the probabilistic management objective. 
#' 
#' @details Each list entry in \code{life_history}, \code{fishery_inputs} and \code{pars} slots should be an array with \code{dim(x)[length(dim(x))] == iter} (i.e., length of the last dimension should be equal to the number of iterations).
#' 
#' @importFrom crayon blue red
#{{{
# class definition
setClass("om", contains = "array", slots = list(ages = 'integer', iter = 'integer', time = 'numeric', pars = 'list', fishery_inputs = 'list', life_history = 'list', population_dynamics = 'function', pst = 'list', targets = 'list', diagnostics = 'list', objectives = 'list'))
#}}}
#{{{
# initialisation function
setMethod("initialize", "om", function(.Object, ages, pdyn_function, iter, time, phi = 1, ...) {
    
    if(missing(pdyn_function) | missing(ages)) {
        .Object@population_dynamics <- function() NA_real_
    } else {
        .Object@population_dynamics <- pdyn_function
    }
    
    if (grepl("\\(i\\ ", deparse1(pdyn_function))) stop("'pdyn_function' cannot contain 'i' index")
    
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
    
    if(missing(ages) | is.null(ages)) {
        .Object@ages <- NA_integer_
    } else {
        .Object@ages <- ages
    }
    
    # setup PST limit
    # reference point
    .Object@pst$phi     <- phi
    .Object@pst$rmax    <- NA_real_
    .Object@pst$value   <- NA_real_
    
    # setup management
    # target reference points
    # (MNPL values)
    .Object@targets$catch        <- NA_real_
    .Object@targets$harvest_rate <- NA_real_
    .Object@targets$depletion    <- 0.5
    
    # set up diagnostics
    .Object@diagnostics$catch        <- NA_real_
    .Object@diagnostics$depletion    <- NA_real_
    .Object@diagnostics$harvest_rate <- NA_real_
    
    # set up objectives
    .Object@objectives$catch        <- NA_real_
    .Object@objectives$depletion    <- NA_real_
    .Object@objectives$harvest_rate <- NA_real_
    
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
              message("fishery_inputs: ", if (length(object@fishery_inputs) > 0)  paste0(names(object@fishery_inputs), collapse = ", ") else red("EMPTY"))
              message("life_history: ", if (length(object@life_history) > 0)  paste0(names(object@life_history), collapse = ", ") else red("EMPTY"))
              message("pars: ", if (length(object@pars) > 0) paste0(names(object@pars), collapse = ", ") else red("EMPTY"))
              message("\npopulation dynamics function:")
              message(writeLines(deparse(object@population_dynamics)))
              message("population dynamics:")
              print(object@.Data)
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
