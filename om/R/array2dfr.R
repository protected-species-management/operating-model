#' @title Cast array to data frame
#' @param object \code{array} class object
#' @param value_to column header for array value
#' @param dim.names list of dimension names
#' @export
array2dfr <- function(object, value_to = "value", dim.names = list()) {
    
    dimnames(object) <- dim.names
    
    # melt to data frame
    object <- array2DF(object, responseName = value_to)
    
    # coerce iterations to integer values
    class(object[,which(grepl("iter", colnames(object)))]) <- "integer"
    
    # return
    return(object)
}
