
.check_rp <- function(object, stochastic, equilibrium_time, iterations) {
 
    #get("stochastic", envir = env)
    
    if (missing(stochastic)) {
        if (is.na(object@stochastic$ref_points)) {
            stop("'stochastic' argument unspecified (ref. points)")    
        } else {
            cli_alert_info(paste0("'stochastic' = ", ifelse(object@stochastic$ref_points, "TRUE", "FALSE"), " (ref. points)"))
        }
    } else {
        if (is.logical(stochastic)) {
            if (!is.na(object@stochastic$ref_points) & object@stochastic$ref_points != stochastic) {
                cli_alert_info("'stochastic' argument updates value in 'object@stochastic$ref_points'")
            }
            object@stochastic$ref_points <- stochastic
        } else {
            stop("'stochastic' is not logical")    
        }
    }
    if (missing(equilibrium_time)) {
        if (is.na(object@settings$equilibrium_time)) {
            stop("'equilibrium_time' argument unspecified")    
        } else {
            cli_alert_info(paste0("'equilibrium_time' = ", object@settings$equilibrium_time))
        }
    } else {
        if (equilibrium_time %% 1 == 0) {
            equilibrium_time <- as.integer(equilibrium_time)
            if (!is.na(object@settings$equilibrium_time) & object@settings$equilibrium_time != equilibrium_time) {
                cli_alert_info("'equilibrium_time' argument updates value in 'object@settings$equilibrium_time'")
            }
            object@settings$equilibrium_time <- equilibrium_time
        } else {
            stop("'equilibrium_time' is not an integer")    
        }
    }
    if (missing(iterations)) {
        if (is.na(object@settings$stochastic_iterations) & object@stochastic$ref_points) {
            stop("'iterations' argument unspecified for stochastic model")    
        } else {
            if (object@stochastic$ref_points) {
                cli_alert_info(paste0("'iterations' = ", object@settings$stochastic_iterations))
            }
        }
    } else {
        if (iterations %% 1 == 0) {
            iterations <- as.integer(iterations)
            if (!is.na(object@settings$stochastic_iterations) & !object@stochastic$ref_points) {
                stop("'iterations' argument specified but model is not stochastic")
            }
            if (!is.na(object@settings$stochastic_iterations) & object@settings$stochastic_iterations != iterations) {
                cli_alert_info("'iterations' argument updates value in 'object@settings$stochastic_iterations'")
            }
            object@settings$stochastic_iterations <- iterations
        } else {
            stop("'iterations' is not an integer")    
        }
    }
    
    return(object)   
}

.check_pdyn <- function(object, stochastic, time, iterations) {
    
    if (missing(stochastic)) {
        if (is.na(object@stochastic$projection)) {
            stop("'stochastic' argument unspecified (projection)")    
        } else {
            cli_alert_info(paste0("'stochastic' = ", ifelse(object@stochastic$projection, "TRUE", "FALSE"), " (projection)"))
        }
    } else {
        if (is.logical(stochastic)) {
            if (!is.na(object@stochastic$projection) & object@stochastic$projection != stochastic) {
                cli_alert_info("'stochastic' argument updates value in 'object@stochastic$projection'")
            }
            object@stochastic$projection <- stochastic
        } else {
            stop("'stochastic' is not logical")    
        }
    }
    if (missing(time)) {
        if (any(is.na(object@time))) {
            stop("'time' argument unspecified")    
        }
    } else {
        if (length(time) == 1) {
            if (time > 0) {
                time <- as.integer(0:time)
            } else {
                stop("'time' must be >0")
            }
        } else {
            time <- as.integer(time)    
        }
        if (!all(object@time %in% time) | !all(time %in% object@time)) {
            cli_alert_info("'time' argument updates values in 'object@time'")
        }
        object@time <- time
        
    }
    if (missing(iterations)) {
        if (is.na(object@iter[2]) & object@stochastic$projection) {
            stop("'iterations' argument unspecified for stochastic model")    
        } else {
            if (object@stochastic$projection) {
                cli_alert_info(paste0("'iterations' = ", object@iter[2]))
            }
        }
    } else {
        if (iterations %% 1 == 0) {
            iterations <- as.integer(iterations)
            if (!is.na(object@iter[2]) & !object@stochastic$projection) {
                stop("'iterations' argument specified but model is not stochastic")
            }
            if (!is.na(object@iter[2]) & object@iter[2] != iterations) {
                cli_alert_info("'iterations' argument updates value in 'object@iter[2]'")
            }
            object@iter[2] <- iterations
        } else {
            stop("'iterations' is not an integer")    
        }
    }
    
    return(object)   
}



