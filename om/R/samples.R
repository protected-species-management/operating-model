#' @title Specify number of samples
#' 
#' @description Specify the number of samples with which to represent uncertainty in the input parameters.
#' 
#' @export
setGeneric("samples", function(object, ...) standardGeneric("samples"))
#' @rdname samples
setMethod("samples", signature = c(object = "om"), function(object, ...) {
    object@samples
})
#' @rdname samples
#' @export
setGeneric("samples<-", function(object, ..., value) standardGeneric("samples<-"))
#' @rdname samples
setMethod("samples<-",
          signature(object = "om", value = "numeric"),
          function(object, value) {
              
              if (value %% 1 == 0) {
                  
                  value <- as.integer(value)
                  
                  if (!all(is.na(unlist(object@targets)))) {
                          
                      LESS_THAN <- value < object@samples
                      GRTR_THAN <- value > object@samples
                      
                      if (LESS_THAN) {
                        i              <- sample.int(object@samples, size = value, replace = FALSE)
                        object@targets <- lapply(object@targets, function(x) x[i])
                        object@shape   <- object@shape[i]
                      }
                      if (GRTR_THAN) {
                          i              <- sample.int(object@samples, size = value, replace = TRUE)
                          object@targets <- lapply(object@targets, function(x) x[i])
                          object@shape   <- object@shape[i]
                      }
                  }
                  
                  object@samples <- value
                  
              } else {
                  stop("'value' is not an integer")    
              }
              
              return(object)
          }
)
#}}

