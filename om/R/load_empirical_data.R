#' @title load.empirical_data
#' 
#' @description Load empirical catch and abundance data into om object
#' 
#' @export
#' 
#' @include om-class.R
#' 
#{{{ load empirical data into om object
setGeneric("load_empirical_data", function(object,x, ...) standardGeneric("load_empirical_data"))
#{{ list object
setMethod("load_empirical_data",signature=c("om","list"),function(object,x, ...) {
  
    if(is.list(x)) {
        loc <- match('index',names(x))
        if(!is.na(loc)) {
            object@empirical_data$index <- as.matrix(x[[loc]])
            object@empirical_data$index[object@empirical_data$index <= 0] <- NA
        }
        loc <- match('sigmao',names(x))
        if(!is.na(loc)) {
            object@empirical_data$sigmao <- as.matrix(x[[loc]])
			object@empirical_data$sigmao[object@empirical_data$sigmao <= 0] <- NA
        }
        loc <- match('harvest',names(x))
        if(!is.na(loc)) {
            object@empirical_data$harvest <- as.numeric(x[[loc]])
        }
        loc <- match('time',names(x))
        if(!is.na(loc)) {
            object@empirical_data$time <- as.integer(x[[loc]])
        } else object@empirical_data$time <- 1:length(object@empirical_data$harvest)
    } else  
        stop('empirical data should be a list of harvest and index values\n')
    
    return(object)
})
    