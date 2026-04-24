#' @title Operating model class
#' 
#' @description 
#' Operating model class definition.
#' @slot ages integer vector of ages assumed by operating model. Set to \code{NA} when a cohort aggregated model is assumed.
#' @slot time integer vector of times used for operating model projection or single value given the number of time steps.
#' @slot iter integer value indicating number of stochastic iterations.
#' @slot stochastic logical indicating whether stochastic dynamics are being assumed. 
#' @slot pars list of estimated values used by the operating model. See \code{\link{load_pars}}.
#' @slot fixed list of fixed input values used by the operating model. See \code{\link{load_data}}.
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
setClass("om", contains = "array", slots = list(ages = 'integer', samples = 'integer', time = 'numeric', shape = 'numeric', settings = 'list', pars = 'list', harvest_rate = 'function', pst = 'list', targets = 'list', diagnostics = 'list', objectives = 'list', seeds = 'integer'))
#}}}
#{{{
# initialisation function
setMethod("initialize", "om", function(.Object, ages, harvest_function, samples = 1, time, shape = 1, phi = 1, ...) {
    
    if(missing(harvest_function) | missing(ages)) {
        .Object@harvest_rate <- function() NA_real_
    } else {
        .Object@harvest_rate <- harvest_function
    }
    
    if (!grepl("object", deparse1(harvest_function))) stop("'harvest_function' must contain 'object' as its first argument")
    
    if(missing(samples)) {
        stop("'samples' is a required input")
    } else {
        .Object@samples <- samples
    }
    
    if(missing(time)) {
        stop("'time' is a required input")
    } else {
        if (length(time) > 1) {
            .Object@time <- time
        } else {
            .Object@time <- 0:(time - 1)
        }
    }
    
    if(missing(ages) | is.null(ages)) {
        .Object@ages <- NA_integer_
    } else {
        .Object@ages <- ages
    }
    
    # setup settings required
    # for reference point
    # estimation and projection
    .Object@settings$ref_points <- list(stochastic = NA, iterations = NA_integer_, time = NA_integer_)
    .Object@settings$projection <- list(stochastic = NA, iterations = NA_integer_, time = length(.Object@time))
    .Object@settings$cv         <- list(survivorship = 0.0, birth = 0.0, observation = 0.0, mortality = 0.0)
    .Object@settings$qn         <- list(observation = c(0.0, NA_real_))
    .Object@settings$bias       <- list(observation = 1.0, mortality = 1.0)
    
    # setup PST limit
    # reference point
    .Object@pst$phi      <- phi
    .Object@pst$rmax     <- NA_real_
    .Object@pst$ogive    <- NA_real_
    .Object@pst$value    <- NA_real_
    
    # setup pars
    # (intrinsic growth)
    .Object@pars$r <- NA_real_ 
    # (adult female natural mortality)
    .Object@pars$M <- NA_real_
    # (females born per adult female)
    .Object@pars$f <- NA_real_
    # (age at female maturity)
    .Object@pars$a <- NA_real_
    # (age at observation)
    .Object@pars$o <- NA_real_
    # (selectivity)
    .Object@pars$v <- NA_real_
    # (carrying capacity)
    .Object@pars$K <- NA_real_
    
    # setup management
    # target reference points
    # (estimated or assumed MNPL values)
    .Object@targets$captures     <- NA_real_
    .Object@targets$harvest_rate <- NA_real_
    .Object@targets$depletion    <- NA_real_
    
    # set up diagnostics
    .Object@diagnostics$captures     <- NA_real_
    .Object@diagnostics$depletion    <- NA_real_
    .Object@diagnostics$harvest_rate <- NA_real_
    
    # set up objectives
    .Object@objectives$captures     <- NA_real_
    .Object@objectives$depletion    <- NA_real_
    .Object@objectives$harvest_rate <- NA_real_

    # record rng seeds
    seeds <- floor(runif(samples, 1, 1e6))
    while (length(seeds[!duplicated(seeds)]) < length(seeds)) seeds <- floor(runif(samples, 1, 1e6))
    .Object@seeds <- as.integer(seeds)
    
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
              message("niter: ", object@samples)
              message("siter: ", object@settings$ref_points$iterations, " (ref. points)")
			  message("siter: ", object@settings$projections$iterations, " (projections)")
              message("pars: ", if (length(object@pars) > 0) paste0(names(object@pars), collapse = ", ") else red("EMPTY"))
              message("shape: ", if (length(object@shape) > 0) round(object@shape, 2) else red("EMPTY"))
              message("\nharvest rate function:")
              message(writeLines(deparse(object@harvest_rate)))
              message("rmax:")
              show(object@pst$rmax)
              #message("\npopulation dynamics:\t")
              #print(object@.Data)
          })
# }}}

