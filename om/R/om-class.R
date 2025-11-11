#' @title Operating model class
#' 
#' @description 
#' Operating model class definition.
#'
#{{{
# class definition
setClass("om", contains = "array", slots = list(ages = 'numeric',
                    iter                = 'numeric',
                    time                = 'numeric',
                    productivity        = 'list',
                    fishing             = 'list',
                    life_history        = 'list',
                    population_dynamics = 'function',
                    pst                 = 'list'
                    )
)
#}}}

#{{{
# class definition
setClass("omIter", contains = "matrix",
         slots=list(
             ages                = 'numeric',
             time                = 'numeric',
             productivity        = 'list',
             fishing             = 'list',
             life_history        = 'list',
             population_dynamics = 'function',
             pst                 = 'list'
         )
)
#}}}
