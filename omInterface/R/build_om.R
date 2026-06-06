
#https://stackoverflow.com/questions/23036739/downloading-rdata-files-with-shiny

build_om <- function() {
    
    ui <- fluidPage(
        
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
        conditionalPanel(
            condition = "input.stochastic",
            sliderInput(
                inputId = "iterations",
                label = "Stochastic iterations:",
                min = 1,
                max = 1000,
                value = 200
            )
        ),
        actionButton("run", "Compute"),
        
        # Button
        downloadButton("downloadData", "Download"),
        
        textOutput("theta")
    )
    
    server <- function(input, output) {
        
        object <- reactiveValues()
        observe({
                isolate(
                    object <<- om(ages = 0:12, samples = 1L, time = input$time)
                )
        }) |> bindEvent(input$run)
        
        # The requested dataset
        #object <- bindEvent(reactive({
        #    x <- om(ages = 0:12, samples = 1L, time = input$time)# |> shape(depletion = input$depletion, stochastic = input$stochastic, time = input$time) |> rp()
        #    shape(x) <- 1
        #    return(x)
        #}), input$run)
        
        #output$theta <- bindEvent(renderText({
        #    paste0("Shape parameter = ", round(slot(object(), "shape"), 2)) 
        #}), input$run)
        
        output$downloadData <- downloadHandler(
            filename <- function(){
                paste("OModel.RData")
            },
            
            content = function(file) {
                save(object, file = file)
            }
        )
    }
    
    shinyApp(ui, server)
    
}