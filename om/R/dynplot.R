#' @title Plot dynamics from \code{om} object
#' @description
#' Plots the dynamics over time of the estimated biomass, depletion, harvest rate or surplus production.
#' @details
#' Depletion is measured as the biomass over the carrying capacity, harvest rate is the catch over the estimated biomass, and surplus production is the production function multiplied by the process error residual. Multiple model runs can be provided, in which case the are superimposed.
#' 
#' @param object \code{om} class object.
#' @param pars character vector of model parameters to be plotted. Must be one or more of \code{'depletion'} or \code{'harvest_rate'}.
#' @param labels character vector of labels per model run
#' @param ... additional \code{om} class objects
#' 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manuipulated using further arguments from the \pkg{ggplot2} package. The plotted dynamics are summarised as the median and the 75th and 95th percentiles. The posterior mean is shown as a dashed line. 
#' @include array2dfr.R
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#' 
#' @export
dynplot <- function(object, ...) UseMethod("dynplot")
#'
#' @rdname dynplot
#' @export
dynplot.om <- function(object, pars = 'depletion') {
    
    y <- object
    
    lst <- list()
    
    for (par in pars) {
        
        dfr <- array2dfr(slot(y, 'diagnostics')[[par]], dim.names = list(iter = 1:object@iter, time = object@time))
    
        dfr$time <- as.numeric(dfr$time)
        dfr$iter <- as.numeric(dfr$iter)
        
        lst[[par]] <- dfr
    }
    
    dfr <- bind_rows(lst, .id = 'par')    
    
    dfr <- left_join(dfr, data.frame(par = c("depletion", "harvest_rate", "catch"), par2 = c("Depletion", "Harvest rate", "Catch")), by = 'par')
    
    gg <- ggplot(dfr, aes(.data$time, .data$value))# + labs(x = 'Time', y = 'Predicted Value')

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
#'
#' @rdname dynplot
#' @export
dynplot.list <- function(object, ..., par = 'depletion', labels = character()) {
    
    y <- c(object, list(...))
    
    is.labelled <- ifelse(length(labels) > 0, TRUE, FALSE)
    
    if (is.labelled & length(y) != length(labels)) {
        stop("'labels' vector length does not match number of models")  
    }
    
    if (is.labelled) {
        names(y) <- labels
    } else {
        names(y) <- 1:length(y)    
    }
    
    lst <- list()
    
    for (par in pars) {
        
        dfr <- bind_rows(lapply(y, function(x) array2dfr(slot(x, 'diagnostics')[[par]], dim.names = list(iter = 1:object@iter, time = object@time))), .id = 'label')
        
        if (is.labelled) {
            dfr$label <- factor(dfr$label, levels = labels)
        }
        
        dfr$time <- as.numeric(dfr$time)
        dfr$iter <- as.numeric(dfr$iter)
        
        lst[[par]] <- dfr
    }
    
    dfr <- bind_rows(lst, .id = 'par')    
    
    dfr <- left_join(dfr, data.frame(par = c("depletion", "harvest_rate"), par2 = c("Depletion", "Harvest rate")))
    
    if (length(y) > 1) {
        gg <- ggplot(dfr, aes(.data$time, .data$value, col = .data$label, fill = .data$label))# + labs(x = 'Time', y = 'Predicted Value', col = 'Model\nrun', fill = 'Model\nrun')
    } else {
        gg <- ggplot(dfr, aes(.data$time, .data$value))# + labs(x = 'Time', y = 'Predicted Value')
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
