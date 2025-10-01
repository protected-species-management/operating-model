#' @title index
#' 
#' @description Generate simualated abundance index values
#' 
#' @export
#' 
#' @include dsm-class.R
#' 
#{{{ index()
# generate simulated index observations
setGeneric("index", function(.Object, ...) standardGeneric("index"))
setMethod("index", signature = "dsm", function(.Object, stochastic = FALSE, ...) {
    
    if(!(length(.Object@q)>0))
        .Object <- catchability(.Object)
    
    index  <- .Object@empirical.data$index
    sigmao <- .Object@empirical.data$sigmao
 
    bexp <- biomass(.Object, type = 'exploitable')
    
    q     <- .Object@q
    
    time  <- .Object@empirical.data$time
    tmax  <- length(.Object@empirical.data$time)
    nidx  <- dim(.Object@empirical.data$index)[2]
    niter <- .Object@iter
    
    predicted.index <- array(dim = c(tmax,nidx,niter), dimnames = list(time=time, index=1:nidx, iter=1:niter))
    
    # scale exploitable biomass by catchability
    for(i in 1:nidx) {
        predicted.index[,i,] <- sweep(bexp, 2, q[i,], '*')
    }
    
    # apply stochastic observation error
    if (stochastic) {
        for (i in 1:nidx) {
            if (niter > 1) {
                residual.error  <- sweep(predicted.index[,i,], 1, index[,i], function(x,y) log(x/y))
                simulated.error <- apply(residual.error, 2, function(x) .simulate.residual.error(x, sigmao[i]))
            } else {
                residual.error  <- log(predicted.index[,i,]/index[,i])
                simulated.error <- .simulate.residual.error(residual.error, sigmao[i])
            }
            predicted.index[,i,] <- predicted.index[,i,] * exp(-simulated.error)
        }
    }
    
    # check missing data is cleaned out
    # (should not be necessary when stochastic = TRUE)
    for (i in 1:nidx) {
        missing.data <- .Object@empirical.data$index[,i]
        missing.data[!is.na(missing.data)] <- 1
        if (niter > 1) {
            predicted.index[,i,] <- sweep(predicted.index[,i,], 1, missing.data,'*')
        } else {
            predicted.index[,i,] <- predicted.index[,i,] * missing.data
        }
    }
    
    .Object@.Data <- predicted.index
    
    .Object
    
})
#}}}
#{
# simulation function for log-residual error
.simulate.residual.error <- function(x, sigma) {
    
    ##########################################
    # FIT AR1 MODEL TO OBERVATION ERROR      #
    # RESIDUALS AND SIMULATE NEW RESIDUALS   # 
    # WITH BIAS AND AUTO-CORRELATION IN THE  #
    # RESIDUAL PREDICTION ERROR              #
    ##########################################
    #
    ## fit auto-regressive model
    #x.fit <- ar(x, order.max = 1, na.action = na.exclude)
    #
    ## extract first-order autoregression
    ## coefficient if significant
    #if(x.fit$order>0) {
    #    x.alpha <- x.fit$ar[1]
    #} else x.alpha <- 0
    #
    ## residual prediction error
    #x.sd <- sqrt(x.fit$var.pred)
    #
    ## location of non-NA values
    #loc <- which(!is.na(x))
    #
    ## non-NA time series
    #x.loc <- x[loc] 
    #
    ## simulate forward using AR0 or AR1 process
    #x.loc[1] <- x.loc[1] + rnorm(1, x.alpha * x.loc[1], x.sd)
    #for(i in 2:length(x.loc))
    #    x.loc[i] <- x.loc[i] + rnorm(1, x.alpha * x.loc[i-1], x.sd)
    #
    ## re-assign to input vector
    #x[loc] <- x.loc
    
    #######################################
    # GENERATE UNBIASED RESIDUALS WITH NO #
    # OBSERVATION ERROR STRUCTURE         #
    #######################################
    
    # residual prediction error
    x.sd <- sigma
    
    # location of non-NA values
    loc <- which(!is.na(x))
    
    # non-NA time series
    x.loc <- x[loc] 
    
    # simulate observation error residuals
    x.loc <- rnorm(length(x.loc), -(x.sd^2)/2, x.sd)
    
    # re-assign to input vector
    x[loc] <- x.loc
    
    #########
    return(x)
    
}
#}



