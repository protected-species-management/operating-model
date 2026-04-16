#' @title Test
#' @description Test shinyApp functionality within R-package environment.
#' @param object object of class \code{\link{om-class}}.
#' @details Generates interface for interactive use within workflow. 
#' @import shiny
#' @import bslib
#' @import om
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
        inputId = "depletion",
        label = "Depletion at MNPL:",
        min = 0.4,
        max = 1,
        value = 0.5
      ),
      sliderInput(
        inputId = "time",
        label = "Equilibrium time:",
        min = 100,
        max = 1000,
        value = 200
      ),
      checkboxInput(
        inputId = "stochastic",
        label = "Stochastic",
        value = FALSE
      ),
      actionButton("run", "Compute"),
      
      # Button
      downloadButton("downloadData", "Download")
    ),
    textOutput("theta"),
    # Output: Production function ----
    plotOutput(outputId = "curvePlot")
  )
  
  # Define server logic required to draw curve ----
  server <- function(input, output) {
    
    output$theta <- renderText({
      object <- shape(object, depletion = input$depletion, stochastic = input$stochastic, equilibrium_time = input$time, iterations = 300L, verbose = FALSE)
      paste0("Shape parameter = ", round(mean(object@shape), 2)) 
    }) |> bindEvent(input$run)
    
    output$curvePlot <- renderPlot({
      
      object <- shape(object, depletion = input$depletion, stochastic = input$stochastic, equilibrium_time = input$time, iterations = 300L, verbose = FALSE)
      object <- rp(object, verbose = FALSE)
      
      dfr <- sp(object, harvest_rate = seq(0.00, 0.05, length = 101))
      
      plot(captures ~ depletion, data = dfr, xlab = "Harvest rate", ylab = "Depletion", cex.lab = 2, type = 'l')
      plot(captures ~ depletion, dfr, type = 'l'); abline(v = object@targets$depletion, lty = 2); points(x = object@targets$depletion, y = object@targets$captures, pch = 19)
      
    }) |> bindEvent(input$run)
  }
  
  shinyApp(ui = ui, server = server)

}

