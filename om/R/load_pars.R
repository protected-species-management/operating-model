#' @title Load or update parameters
#' @aliases update_pars 
#' @description Load or update parameters in \code{\link{om-class}} object. Each parameter should be provided as a \code{\link{distribution-class}}.
#' @param value named list object containing parameter distributions. 
#' @include om-class.R distribution-class.R
#' @export
#{{{
setGeneric("load_pars", function(object, value, ...) standardGeneric("load_pars"))
setMethod("load_pars", signature = c("om", "list"), function(object, value, ...) {
    
    # assign
    object <- update_pars(object, value)
    
    # return
    return(object)
})
#}}}
#' @export
#' @rdname load_pars
#{{{
setGeneric("update_pars", function(object, value, ...) standardGeneric("update_pars"))
setMethod("update_pars", signature = c("om", "list"), function(object, value, ...) {
    
    # check names
    lapply(names(value), function(a) stopifnot(a %in% names(object@pars)))
    
    # assign
    for (i in 1:length(value)) {
		if (is(value[[i]], 'distribution')) {
			if (names(value)[i] %in% names(object@pars)) {
			    object@pars[[which(names(object@pars) %in% names(value)[i])]] <- value[[i]]
			} else {
			    stop(paste0("'", names(value)[i], "' not assigned"))
			}
		} else {
			stop("value must be of class 'distribution'")
		}
    }
    
    # calculate r
    if (is(object@pars[["m"]], "distribution") & is(object@pars[["s"]], "distribution") & is(object@pars[["b"]], "distribution") & is(object@pars[["c"]], "distribution")) {
        
        lambda_values <- numeric(1e5)
        
        for (i in 1:length(lambda_values)) {
            
            m_sample <- sample(object@pars[["m"]])
            s_sample <- sample(object@pars[["s"]])
            b_sample <- sample(object@pars[["b"]])
            c_sample <- sample(object@pars[["c"]])
            
            lambda_values[i] <- .solve_lambda(m = m_sample, s = s_sample, s0 = s_sample * c_sample, b = b_sample)
        }
        
        r_values <- log(lambda_values[lambda_values > 1])
        
        if (length(r_values) > 1) {
            
            # calculate log-normal pars
            # from log(r) ~ N(mu, sigma)
            object@pars[["r"]] <- distribution(pars = c(mean(log(r_values)), sd(log(r_values))), density = "lognormal", name = "intrinsic growth rate")
            
        } else {
          
            warning("intrinsic growth not log-normal (lambda <= 1)")  
        }
    }
    
    # return
    return(object)
})
#}}}




