#' @title dsm-class definition
#' 
#' @description Data simulation module (dsm) class definition
#'
#{{{
# class definition
setClass("dsm",contains="array",
         slots=list(
                    ainf    = 'numeric',
                    iter    = 'numeric',
                    empirical.data = 'list',
                    lh.data = 'list',
                    pdyn    = 'function',
                    sr      = 'character',
                    B0      = 'numeric',
                    selectivity = 'matrix',
                    q       = 'matrix',
                    n       = 'array'
                    )
)
#}}}