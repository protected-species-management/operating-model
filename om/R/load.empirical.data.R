#' @title load.empirical.data
#' 
#' @description Load empirical catch and abundance data into dsm object
#' 
#' @export
#' 
#' @include dsm-class.R
#' 
#{{{ load empirical data into dsm object
setGeneric("load.empirical.data", function(.Object,x, ...) standardGeneric("load.empirical.data"))
#{{ list object
setMethod("load.empirical.data",signature=c("dsm","list"),function(.Object,x, ...) {
  
    if(is.list(x)) {
        loc <- match('index',names(x))
        if(!is.na(loc)) {
            .Object@empirical.data$index <- as.matrix(x[[loc]])
            .Object@empirical.data$index[.Object@empirical.data$index <= 0] <- NA
        }
        loc <- match('sigmao',names(x))
        if(!is.na(loc)) {
            .Object@empirical.data$sigmao <- as.numeric(x[[loc]])
        }
        loc <- match('harvest',names(x))
        if(!is.na(loc)) {
            .Object@empirical.data$harvest <- as.numeric(x[[loc]])
        }
        loc <- match('time',names(x))
        if(!is.na(loc)) {
            .Object@empirical.data$time <- as.integer(x[[loc]])
        } else .Object@empirical.data$time <- 1:length(.Object@empirical.data$harvest)
    } else  
        stop('empirical data should be a list of harvest and index values\n')
    
    .Object
    
})
    