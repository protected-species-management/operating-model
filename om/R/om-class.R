#' @title om-class definition
#' 
#' @description Data simulation module (om) class definition
#'
#{{{
# class definition
setClass("om",contains="array",
         slots=list(
                    ainf           = 'numeric',
                    iter           = 'numeric',
                    empirical_data = 'list',
                    lh_data        = 'list',
                    pdyn           = 'function',
                    sr             = 'character',
                    B0             = 'numeric',
                    selectivity    = 'matrix',
                    q              = 'matrix',
                    n              = 'array'
                    )
)
#}}}
