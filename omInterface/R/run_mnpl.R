#' @title Test
#' @description Test shinyApp functionality within R-package environment.
#' @param object object of class \code{\link{om-class}}.
#' @details Generates interface for interactive use within workflow. 
#' @import shiny
#' @import bslib
#' @importFrom dplyr %>% bind_rows mutate filter select summarise
#' @include pdyn.R update_pars.R accessors.R
#' @export
"run_mnpl" <- function(object, ...) UseMethod("run_mnpl")
#' @rdname run_mnpl
#' @export
"run_mnpl.om" <- function(object) {

  # Define UI for app that draws a histogram ----
  ui <- page_sidebar(
    # App title ----
    title = "Depletion function",
    # Sidebar panel for inputs ----
    sidebar = sidebar(
      # Input: Slider for theta ----
      sliderInput(
        inputId = "theta",
        label = "Shape parameter:",
        min = 0.5,
        max = 5,
        value = 1
      ),
      sliderInput(
        inputId = "cvx",
        label = "Process error:",
        min = 0.0,
        max = 0.1,
        value = 0.01
      ),
      actionButton("run", "Compute")
    ),
    # Output: Production function ----
    plotOutput(outputId = "curvePlot")
  )
  
  harvest_rates <- seq(0, 0.05, length.out = 21)
  
  # Define server logic required to draw a histogram ----
  server <- function(input, output) {
    
    # Histogram of the Old Faithful Geyser Data ----
    # with requested number of bins
    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    
    output$curvePlot <- renderPlot({
      
      out_depletion <- list()
      
      withProgress( 
        message = 'Calculation in progress', 
        detail = '...', 
        value = 0, 
        {
          for (i in 1:length(harvest_rates)) {
                
            object <- object %>% update_pars(list(
              'harvest_rate' = harvest_rates[i],
              'shape'        = input$theta,
              'cv_dynamics'  = input$cvx
            ))
            
            object <- pdyn(object)
            
            dfr <- diagnostics(object)$depletion
            dfr <- dfr %>% filter(time == max(object@time)) %>% select(-time) %>% summarise(depletion = mean(value), depletion_upp = quantile(value, 0.975), depletion_low = quantile(value, 0.025))
            dfr <- dfr %>% mutate(harvest_rate = harvest_rates[i])
            
            out_depletion[[i]] <- dfr
            
            incProgress(1/21) 
          }
        }
      )
      
      out_depletion <- bind_rows(out_depletion)
      
      plot(depletion ~ harvest_rate, data = out_depletion, xlab = "Harvest rate", ylab = "Depletion", cex.lab = 2, type = 'l')
      lines(depletion_upp ~ harvest_rate, data = out_depletion, lty = 2)
      lines(depletion_low ~ harvest_rate, data = out_depletion, lty = 2)
      
    }) |> bindEvent(input$run)
  }
  
  shinyApp(ui = ui, server = server)

}

