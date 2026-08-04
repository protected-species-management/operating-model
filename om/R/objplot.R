#' @title Plot performance diagnostics over time
#' @description
#' Plots the dynamics over time of the projected captures, depletion or harvest rate, each summarised as a probability relative to the MNPL reference points.
#' 
#' @param object \code{om} class object.
#' @param pars character vector of model parameters to be plotted. Must be one or more of \code{'depletion'}, \code{'captures'} or \code{'harvest_rate'}.
#' @param labels character vector of labels per model run
#' @param ... additional \code{om} class objects
#' @note Multiple model objects can be supplied, in which case they are over-plotted, using the \code{labels} argument in the legend if supplied. 
#' @return Returns a \code{ggplot} object that can be displayed or assigned and manipulated using further arguments from the \pkg{ggplot2} package. The plotted dynamics are summarised as the mean and the 75th and 95th percentiles across samples from the input life-history distributions. 
#' @importFrom ggplot2 ggplot stat_summary facet_grid aes
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows left_join
#' @importFrom stats na.omit
#' @seealso \code{\link{dynplot}}
#' @export
objplot <- function(object, ...) UseMethod("objplot")
#'
#' @rdname objplot
#' @export
objplot.om <- function(object, ..., pars = 'depletion', labels) {
    
    stopifnot(all(pars %in% c("depletion", "harvest_rate", "captures")))
    
    y <- list(object, ...)
    
    lst1 <- list()
    lst2 <- list()
    
    for (mdl in 1:length(y)) {
        
        get_dim(y[[mdl]], env = environment())
        
        dm <- list(sample = 1:NITER, time = time)
        
        for (par in pars) {
            
            dm2 <- dm
            if (par %in% c("harvest_rate", "captures")) {
                dm2$time <- dm$time[-length(dm$time)] 
            } else {
                dm2$time <- dm$time
            }
        
            dfr <- slot(y[[mdl]], 'objectives')[[par]]
            dimnames(dfr) <- dm2
            dfr <- array2DF(dfr, responseName = "value")
            
            dfr$time      <- as.numeric(dfr$time)
            dfr$sample    <- as.numeric(dfr$sample)
            
            lst1[[par]] <- na.omit(dfr)
        }
    
        lst2[[mdl]] <- bind_rows(lst1, .id = 'par')    
    
        lst2[[mdl]] <- left_join(lst2[[mdl]], data.frame(par = c("depletion", "harvest_rate", "captures"), par2 = c("P(D > D[MNPL])", "P(H < H[MNPL])", "P(C < C[MNPL])")), by = 'par')
        
    }
    
    if (missing(labels)) {
        names(lst2) <- as.character(unlist(as.list(match.call())[-1]))[1:length(y)] #LETTERS[1:length(y)]
    } else {
        names(lst2) <- labels
    }
        
    dfr <- bind_rows(lst2, .id = 'model')
    
    if (length(y) > 1) {
        gg <- ggplot(dfr, aes(x = .data$time, y = .data$value, fill = .data$model, col = .data$model))
    } else {
        gg <- ggplot(dfr, aes(x = .data$time, y = .data$value))
    }

    gg <- gg + 
        stat_summary(fun.min = function(x) quantile(x, 0.025), fun.max = function(x) quantile(x, 0.975), geom = 'ribbon', alpha = 0.3) +
        stat_summary(fun.min = function(x) quantile(x, 0.125), fun.max = function(x) quantile(x, 0.875), geom = 'ribbon', alpha = 0.3) +
        stat_summary(fun = function(x) mean(x), geom = 'line', lwd = 1)
        #stat_summary(fun = function(x) median(x), geom = 'line', lwd = 0.5, linetype = "dashed")
    
    if (length(pars) > 1) {
        gg <- gg + facet_grid(.data$par2~., scales  =  'free_y', labeller = label_parsed)
    }
    
    return(gg)
}
