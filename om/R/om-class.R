#' @title Operating model class
#' 
#' @description 
#' Operating model class definition.
#' @slot ages integer vector of ages assumed by operating model. Set to \code{NA} when a cohort aggregated model is assumed.
#' @slot time integer vector of times used for operating model projection or single value given the number of time steps.
#' @slot iter integer value indicating number of stochastic iterations.
#' @slot life_history names list of life history inputs. See \code{\link{load_life_history}}.
#' @slot fishery_inputs named list of fishery inputs. See \code{\link{load_fishery_inputs}}.
#' @slot pars list of estimated values used by the operating model. See \code{\link{load_pars}}.
#' @slot pars list of data values used by the operating model. See \code{\link{load_data}}.
#' @slot harvest_rate function containing the harvest rate function.
#' @slot pst list containing \code{phi}, \code{rmax}, \code{numbers} and \code{value} elements related to the PST threshold reference point.
#' @slot targets list containing \code{catch}, \code{depletion} and \code{harvest_rate} target reference points. These should be set at the appropriate level for the operating model being assumed. See \code{load_targets}.
#' @slot objectives list containing probability values indicating whether management target has been reached (i.e., the realised objective values) for comparison with the probabilistic management objective. 
#' 
#' @details Each list entry in \code{life_history}, \code{fishery_inputs} and \code{pars} slots should be an array with \code{dim(x)[length(dim(x))] == iter} (i.e., length of the last dimension should be equal to the number of iterations).
#' 
#' @importFrom crayon blue red
#{{{
# class definition
setClass("om", contains = "array", slots = list(ages = 'integer', iter = 'integer', time = 'numeric', pars = 'list', data = 'list', fishery_inputs = 'list', life_history = 'list', harvest_rate = 'function', pst = 'list', targets = 'list', diagnostics = 'list', objectives = 'list'))
#}}}
#{{{
# initialisation function
setMethod("initialize", "om", function(.Object, ages, harvest_function, iter, time, phi = 1, ...) {
    
    if(missing(harvest_function) | missing(ages)) {
        .Object@harvest_rate <- function() NA_real_
    } else {
        .Object@harvest_rate <- harvest_function
    }
    
    if (!grepl("object", deparse1(harvest_function))) stop("'harvest_function' must contain 'object' as its first argument")
    
    if(missing(iter)) {
        stop("'iter' is a required input")
    } else {
        .Object@iter <- iter
    }
    
    if(missing(time)) {
        stop("'time' is a required input")
    } else {
        if (length(time) > 1) {
            .Object@time <- time
        } else {
            .Object@time <- 1:time
        }
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
    .Object@targets$depletion    <- NA_real_
    
    # set up diagnostics
    .Object@diagnostics$catch        <- NA_real_
    .Object@diagnostics$depletion    <- NA_real_
    .Object@diagnostics$harvest_rate <- NA_real_
    
    # set up objectives
    .Object@objectives$catch        <- NA_real_
    .Object@objectives$depletion    <- NA_real_
    .Object@objectives$harvest_rate <- NA_real_
    
    # add dimensions to 
    # reference point
    #.Object@pst$numbers <- matrix(NA_real_, nrow = length(.Object@time), ncol = .Object@iter)
    #.Object@pst$value   <- matrix(NA_real_, nrow = length(.Object@time), ncol = .Object@iter)
    
    # add dimensions to 
    # diagnostics
    #.Object@diagnostics <- lapply(.Object@diagnostics, function(x) matrix(NA_real_, nrow = length(.Object@time), ncol = .Object@iter))
    
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
              message("\nharvest rate function:\n")
              message(writeLines(deparse(object@harvest_rate)))
              message("\nrmax:")
              show(distribution(list(value = object@pst$rmax, distribution = "lognormal")))
              message("\npopulation dynamics:\n\n")
              print(object@.Data)
          })
# }}}

