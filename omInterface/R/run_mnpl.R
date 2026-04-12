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
      actionButton("run", "Compute")
    ),
    # Output: Production function ----
    plotOutput(outputId = "curvePlot")
  )
  
  # Define server logic required to draw a histogram ----
  server <- function(input, output) {
    
    output$curvePlot <- renderPlot({
      
      shape(object) <- input$theta
      
      dfr <- sp(object, harvest_rate = seq(0.00, 0.05, length = 101))
      
      #withProgress( 
      #  message = 'Calculation in progress', 
      #  detail = '...', 
      #  value = 0, 
      #  {
      #      # estimate reference points
      #      
      #      incProgress(1/21) 
      #  }
      #)
      
      plot(captures ~ depletion, data = dfr, xlab = "Harvest rate", ylab = "Depletion", cex.lab = 2, type = 'l')
      
    }) |> bindEvent(input$run)
  }
  
  shinyApp(ui = ui, server = server)

}

