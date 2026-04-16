#' @title Plot dynamics from \code{om} object
#' @description
#' Plots the dynamics over time of the projected captures, depletion or harvest rate.
#' 
#' @param object \code{om} class object.
#' @param pars character vector of model parameters to be plotted. Must be one or more of \code{'depletion'}, \code{'captures'} or \code{'harvest_rate'}.
#' @param labels character vector of labels per model run
#' @param ... additional \code{om} class objects
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package. The plotted dynamics are summarised as the mean and the 75th and 95th percentiles. 
#' @include array2dfr.R
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' 
#' @export
dynplot <- function(object, ...) UseMethod("dynplot")
#'
#' @rdname dynplot
#' @export
dynplot.om <- function(object, pars = 'depletion') {
    
    stopifnot(all(pars %in% c("depletion", "harvest_rate", "captures")))
    
    get_dim(object, env = environment())
    
    y <- object
    
    lst <- list()
    
    dm <- dimnames(object@.Data)[c(1,2,4)]
    
    for (par in pars) {
        
        if (par %in% c("harvest_rate", "captures")) dm$time <- dm$time[-length(dm$time)] 
        
        #dfr <- array2dfr(slot(y, 'diagnostics')[[par]], dim.names = dm)
    
        dfr <- slot(y, 'diagnostics')[[par]]
        dimnames(dfr) <- dm
        dfr <- array2DF(dfr, responseName = "value")
        
        dfr$time      <- as.numeric(dfr$time)
        dfr$sample    <- as.numeric(dfr$sample)
        dfr$iteration <- as.numeric(dfr$iteration)
        
        lst[[par]] <- na.omit(dfr)
    }
    
    dfr <- bind_rows(lst, .id = 'par')    
    
    dfr <- left_join(dfr, data.frame(par = c("depletion", "harvest_rate", "captures"), par2 = c("Depletion", "Harvest rate", "Captures")), by = 'par')
    
    gg <- ggplot(dfr, aes(.data$time, .data$value))

    gg <- gg + 
        stat_summary(fun.min = function(x) quantile(x, 0.025), fun.max = function(x) quantile(x, 0.975), geom = 'ribbon', alpha = 0.3) +
        stat_summary(fun.min = function(x) quantile(x, 0.125), fun.max = function(x) quantile(x, 0.875), geom = 'ribbon', alpha = 0.3) +
        stat_summary(fun = function(x) mean(x), geom = 'line', lwd = 1)
        #stat_summary(fun = function(x) median(x), geom = 'line', lwd = 0.5, linetype = "dashed")
    
    if (length(pars) > 1) {
        gg <- gg + facet_grid(.data$par2~., scales  =  'free_y')
    }
    
    return(gg)
}
#'
#' @rdname dynplot
#' @export
dynplot.list <- function(object, pars = 'depletion', labels = character()) {
    
    stop("not currently working for list input...")
    
    stopifnot(all(pars %in% c("depletion", "harvest_rate", "captures")))
    
    get_dim(object[[1]], env = environment())
    
    y <- object #c(object, list(...))
    
    dm <- dimnames(object@.Data)[c(1,2,4)]
    
    is.labelled <- ifelse(length(labels) > 0, TRUE, FALSE)
    
    if (is.labelled & length(y) != length(labels)) {
        stop("'labels' vector length does not match number of models")  
    }
    
    if (is.labelled) {
        
        names(y) <- labels
        
    } else {
        
        labels <- 1:length(y)
        labels <- ifelse(labels < 10, paste0("0", labels), labels)
        
        names(y) <- labels   
    }
    
    lst <- list()
    
    for (par in pars) {
        
        if (par == "depletion") dm <- list(iter = 1:niter, time = time) else dm <- list(iter = 1:niter, time = time[-ntime]) 
        
        dfr <- bind_rows(lapply(y, function(x) array2dfr(slot(x, 'diagnostics')[[par]], dim.names = dm)), .id = 'label')
        
        dfr$label <- factor(dfr$label, levels = labels)
        
        dfr$time <- as.numeric(dfr$time)
        dfr$iter <- as.numeric(dfr$iter)
        
        lst[[par]] <- na.omit(dfr)
    }
    
    dfr <- bind_rows(lst, .id = 'par')    
    
    dfr <- left_join(dfr, data.frame(par = c("depletion", "harvest_rate", "captures"), par2 = c("Depletion", "Harvest rate", "Captures")), by = "par")
    
    if (length(y) > 1) {
        gg <- ggplot(dfr, aes(.data$time, .data$value, col = .data$label, fill = .data$label))
    } else {
        gg <- ggplot(dfr, aes(.data$time, .data$value))
    }
    
    gg <- gg + 
        stat_summary(fun.min = function(x) quantile(x, 0.025), fun.max = function(x) quantile(x, 0.975), geom = 'ribbon', alpha = 0.3) +
        stat_summary(fun.min = function(x) quantile(x, 0.125), fun.max = function(x) quantile(x, 0.875), geom = 'ribbon', alpha = 0.3) +
        stat_summary(fun = function(x) median(x), geom = 'line', lwd = 1) +
        stat_summary(fun = function(x) mean(x), geom = 'line', lwd = 0.5, linetype = "dashed")
    
    if (length(pars) > 1) {
        gg <- gg + facet_grid(.data$par2~., scales  =  'free_y')
    }
    
    return(gg)
}
#}}}

#}}}
