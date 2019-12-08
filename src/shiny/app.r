library(shiny)

ui <- fluidPage("select the level of data",
                
                # inputs
                radioButtons(
                    inputId = "level", 
                    label = h3("Sample level"),
                    choices = list("By tube" = 1, "By treatment" = 2),
                    selected = 1),
                
                checkboxGroupInput(
                    "accumulation", 
                    label = h3("Checkbox group"), 
                    choices = list("Daily" = 1, "Cumulative by phase" = 2, "Cumulative overall" = 3),
                    selected = 1),
                
                sliderInput(
                    "days", 
                    label = h3("Slider Range"), 
                    min = 0, max = 319, value = c(0, 319)),
                
                plotOutput("co2")
                
                # outputs
                # hr(),
                # fluidRow(column(2, verbatimTextOutput("value")))
)

server <- function(input, output, session){
    output$co2 <- renderPlot({
        hist(rnorm(input$days))
    }
    
    )
}

shinyApp(ui = ui, server = server)
