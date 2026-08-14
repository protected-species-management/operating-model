#' @title Class containing a probability distribution
#' @description 
#' This is an S4 object class that includes both a numeric vector for storage of values generated using Monte Carlo methods, and a list of parameters describing the associated parameteric distribution. Only a selection of two-parameter distributions are currently supported.
#' @details
#' The inputs determine how the \code{distribution} object is initialised. If a vector of values are provided these are stored. If a distribution is also provided via the \code{density} argument then parameters for this distribution are estimated. Parameters can be provided without values via the combined \code{pars} and \code{density} arguments. If \code{pars}, \code{density} and \code{iter} are all provided then values are generated internally during initialisation of the object. 
#' 
#' Monte Carlo simulation from the \code{distribution} object will depend on whether values are present. If present, they are sampled at random (i.e., non-parametrically) regardless of whether the distribution is specified. If values are missing, then parametric sampling is performed. Switching between parametric and non-parametric sampling is possible by adding or removing values. 
#' @seealso \code{\link{sample}}, \code{\link{summary}}, \code{\link{plot}}
#' @slot .Data numeric vector of values
#' @slot pars  distribution parameter values
#' @slot density  probability density (or mass) function
#' @slot name  optional label
#' @importFrom logitnorm rlogitnorm momentsLogitnorm logit
#' @importFrom crayon blue
#' @importFrom stats sd var runif rbeta rgamma
#' @importFrom methods show
#' @importFrom cli cli_alert_danger
#' @export
setClass("distribution", contains = "numeric", slots = list(name = "character", pars = "numeric", density = "character"))
# initialisation function
setMethod("initialize", "distribution", function(.Object, ...) {
    
    .Object@.Data   <- numeric()
    .Object@density <- "unspecified"
    .Object@pars    <- c(NA_real_, NA_real_)
    .Object@name    <- character()
    
    iter <- 0
    
    x <- list(...)
    
    if (!missing(x)) {
        
        f1 <- function(x) if(isTRUE(sum(x) > 0))  x else NA
        f2 <- function(x, y) if(!is.null(x)) x else y
        
        # assignments
        .Object@.Data   <- f2(x[[f1(which(unlist(lapply(names(x), function(y) grepl("^value?", y) & is.numeric(x[[y]])))))]], .Object@.Data)
        .Object@density <- f2(x[[f1(which(unlist(lapply(names(x), function(y) grepl("^dens*", y)  & is.character(x[[y]])))))]], .Object@density)
        .Object@pars    <- f2(x[[f1(which(unlist(lapply(names(x), function(y) grepl("^par?", y)   & is.numeric(x[[y]]) & length(x[[y]]) == 2))))]], .Object@pars)
        .Object@name    <- f2(x[[f1(which(unlist(lapply(names(x), function(y) grepl("^name?", y)  & is.character(x[[y]])))))]], .Object@name)
        
        # get iterations
        if (any(grepl("^iter*", names(x)))) {
            iter <- x[[which(grepl("^iter*", names(x)))]] |> as.integer()
        }
        
        # strip NA values if present
        if (any(is.na(.Object@.Data))) {
            .Object@.Data <- .Object@.Data[!is.na(.Object@.Data)]
        }
    }
    
    # if pars but no density generate error
    if (.Object@density == "unspecified" & !any(is.na(.Object@pars))) {
        cli_alert_danger("'pars' provided with no 'density'")
    }
    
    # if data and density then estimate pars
    EST_PARS <- length(.Object@.Data) > 0 & .Object@density != "unspecified" & all(is.na(.Object@pars))
    if (EST_PARS) {
        
        if (grepl("^uniform", .Object@density)) {
            .Object@pars <- .calc_uniform_pars(.Object@.Data)    
        }
        
        if (grepl("^int?.uniform", .Object@density)) {
            .Object@pars <- .calc_intuniform_pars(.Object@.Data)    
        }
        
        if (grepl("^beta", .Object@density)) {
            .Object@pars <- .calc_beta_pars(.Object@.Data)    
        }
        
        if (grepl("^normal", .Object@density)) {
            .Object@pars <- .calc_normal_pars(.Object@.Data)    
        }
        
        if (grepl("^zt?.normal", .Object@density)) {
            .Object@pars <- .calc_ztnormal_pars(.Object@.Data)    
        }
        
        if (grepl("^log?.normal", .Object@density)) {
            .Object@pars <- .calc_lognormal_pars(.Object@.Data)    
        }
        
        if (grepl("^gamma", .Object@density)) {
            .Object@pars <- .calc_gamma_pars(.Object@.Data)    
        }
        
        if (grepl("^logit?.normal", .Object@density)) {
            .Object@pars <- .calc_logitnormal_pars(.Object@.Data)    
        }
    }
    
    # if distribution and pars then simulate values
    SIM_VALUES <- length(.Object@.Data) == 0 & .Object@density != "unspecified" & !any(is.na(.Object@pars))
    if (SIM_VALUES) {
        if (iter > 0) {
            
            if (grepl("^uniform", .Object@density)) {
                .Object@.Data <- runif(iter, min = .Object@pars[1], max = .Object@pars[2])    
            }
            
            if (grepl("^int?.uniform", .Object@density)) {
                .Object@.Data <- (.Object@pars[1]:.Object@pars[2])[sample.int(length(.Object@pars[1]:.Object@pars[2]), iter, replace = TRUE)]
            }
            
            if (grepl("^beta", .Object@density)) {
                .Object@.Data <- rbeta(iter, shape1 = .Object@pars[1], shape2 = .Object@pars[2])    
            }
            
            if (grepl("^normal", .Object@density)) {
                .Object@.Data <- rnorm(iter, mean = .Object@pars[1], sd = .Object@pars[2])    
            }
            
            if (grepl("^zt?.normal", .Object@density)) {
                .Object@.Data <- .Object@pars[1] + .Object@pars[2] * qnorm(runif(iter, pnorm((0 - .Object@pars[1]) / .Object@pars[2]), pnorm(Inf))) 
            }
            
            if (grepl("^log?.normal", .Object@density)) {
                .Object@.Data <- rlnorm(iter, meanlog = .Object@pars[1], sdlog = .Object@pars[2])    
            }
            
            if (grepl("^gamma", .Object@density)) {
                .Object@.Data <- rgamma(iter, shape = .Object@pars[1], scale = .Object@pars[2])    
            }
            
            if (grepl("^logit?.normal", .Object@density)) {
                .Object@.Data <- rlogitnorm(iter, mu = .Object@pars[1], sigma = .Object@pars[2])    
            }
        } else {
            #cli_alert_warning("'iter' argument must be >0 for simulation of values")
        }
    }
    
    return(.Object)
})

# {{{
setMethod("show", "distribution",
          function(object) {
              message(blue("distribution object class"))
              message("density: ", object@density)
              message("pars: ", paste(round(object@pars, 3), collapse = ", "))
              message("values: ", if (length(object@.Data) == 0 | all(is.na(object@.Data))) red("EMPTY") else if (length(object@.Data) > 14) paste0(c(round(object@.Data[1:12], 3), "...", round(object@.Data[length(object@.Data)], 3)), collapse = ", ") else paste0(round(object@.Data, 3), collapse = ", "))
              message("name: ", if (length(object@name) == 0) "--" else object@name)
              message("\t")
        })
# }}}
# {{{
#' @rdname distribution
setMethod("[<-",
          signature(x = "distribution", i = "ANY", j = "missing", value = "numeric"),
          function(x, i, value) {
              
              if (missing(i)) {
                  x@.Data    <- value
              } else {
                  x@.Data[i] <- value
              }
              
              return(x)
          }
)
# }}}

# distribution-specific functions
# {{{
.calc_uniform_pars <- function(x) {
    
    # 
    a <- min(x)
    b <- max(x)
    
    # return
    return(c(a, b))
}

.show_uniform_moments <- function(x) {
    
    a <- x[1]
    b <- x[2]
    
    # return
    c('E[x]' = round((a + b) / 2, 5), 'VAR[x]' = round(((b - a)^2) / 12, 5), 'CV[x]' = round(sqrt(((b - a)^2) / 12) / ((a + b) / 2), 5))
}

.calc_intuniform_pars <- function(x) {
    
    # 
    a <- min(x)
    b <- max(x)
    
    if (!(a %% 1 == 0 & b %% 1 == 0)) {
        cli_abort("'int-uniform' distribution requires integer 'a' and 'b' parameters")
    }
    
    # return
    return(c(a, b))
}

.show_intuniform_moments <- function(x) {
    
    a <- x[1]
    b <- x[2]
    
    # return
    c('E[x]' = round((a + b) / 2, 5), 'VAR[x]' = round(((b - a)^2) / 12, 5), 'CV[x]' = round(sqrt(((b - a)^2) / 12) / ((a + b) / 2), 5))
}

.calc_beta_pars <- function(x) {
    
    # 
    xbar <- mean(x)
    xvar <- var(x)
    
    a <- xbar * (xbar * (1 - xbar) / xvar - 1) 
    b <- (1 - xbar) * (xbar * (1 - xbar) / xvar - 1)
    
    # 
    if (xvar > xbar * (1 - xbar)) {
        stop("variance of input values is too high for estimation of beta distribution parameters using the method-of-moments")    
    }
    
    # return
    return(c(a, b))
}

.show_beta_moments <- function(x) {
    
    a <- x[1]
    b <- x[2]
    
    # return
    c('E[x]' = round(a / (a + b), 5), 'VAR[x]' = round(a * b / ((a + b)^2 * (a + b + 1)), 5), 'CV[x]' = round(sqrt(a * b / ((a + b)^2 * (a + b + 1))) / (a / (a + b)), 5))
}

.calc_normal_pars <- function(x) {
    
    # estimate parameters of
    # normal distribution
    mu     <- mean(x)
    sigma  <- sd(x)
    
    # return
    return(c(mu, sigma))
}

.show_normal_moments <- function(x) {
    
    mu     <- x[1]
    sigma  <- x[2]
    sigma2 <- sigma^2
    
    # return
    c('E[x]' = round(mu, 5), 'SD[x]' = round(sigma, 5), 'VAR[x]' = round(sigma2, 5), 'CV[x]' = round(sigma / mu, 5))
}

.calc_ztnormal_pars <- function(x) {
    
    mu     <- mean(x)
    sigma  <- sd(x)
    
    # return
    return(c(mu, sigma))
}

.show_ztnormal_moments <- function(x) {
    
    mu     <- x[1]
    sigma  <- x[2]
    
    # return
    c('E[x]' = round(mu / (1 - pnorm(0, mu, sigma)), 5), 'VAR[x]' = round(NA_real_, 5), 'CV[x]' = round(NA_real_, 5))
}

.calc_lognormal_pars <- function(x) {
    
    # transform to normal
    y <- log(x)
    
    # estimate parameters of
    # normal distribution log(x)
    mu     <- mean(y)
    sigma  <- sd(y)
    
    # return
    return(c(mu, sigma))
}

.show_lognormal_moments <- function(x) {
    
    mu     <- x[1]
    sigma  <- x[2]
    sigma2 <- sigma^2
    
    theta <- exp(mu + sigma2/2)
    nu    <- exp(2*mu + sigma2)*(exp(sigma2) - 1)
    cv    <- sqrt(exp(sigma2) - 1)
    
    # return
    c('E[log(x)]' = round(mu, 5), 'SD[log(x)]' = round(sigma, 5), 'E[x]' = round(theta, 5), 'VAR[x]' = round(nu, 5), 'CV[x]' = round(cv, 5))
}

.calc_gamma_pars <- function(x) {
    
    # 
    ln.x   <- log(x)
    x.ln.x <- x * log(x) 
    
    theta <- mean(x.ln.x) - mean(x) * mean(ln.x)
    alpha <- mean(x) / theta
    
    # return
    return(c(alpha, theta))
}

.show_gamma_moments <- function(x) {
    
    alpha  <- x[1]
    theta  <- x[2]
    
    # return
    c('E[x]' = round(alpha * theta, 5), 'VAR[x]' = round(alpha * theta^2, 5), 'CV[x]' = round(sqrt(alpha * theta^2) / alpha * theta, 5))
}

.show_unspecified_moments <- function(x) {
    
    # return
    c('E[x]' = round(mean(x), 5), 'MIN[x]' = round(min(x), 5), 'MAX[x]' = round(max(x), 5))
}

.calc_logitnormal_pars <- function(x) {
    
    # transform form 
    # to (0, 1) to (-Inf,Inf)
    y <- logit(x)
    
    # estimate parameters of
    # normal distribution logit(x)
    mu     <- mean(y)
    sigma  <- sd(y)
    
    # return
    return(c(mu, sigma))
}

.show_logitnormal_moments <- function(x) {
    
    mu     <- x[1]
    sigma  <- x[2]
    
    theta <- as.numeric(momentsLogitnorm(mu, sigma)[1])
    nu    <- as.numeric(momentsLogitnorm(mu, sigma)[2])
    cv    <- sqrt(nu) / theta
    
    # return
    c('E[logit(x)]' = round(mu, 5), 'SD[logit(x)]' = round(sigma, 5), 'E[x]' = round(theta, 5), 'VAR[x]' = round(nu, 5), 'CV[x]' = round(cv, 5))
}

# }}}
