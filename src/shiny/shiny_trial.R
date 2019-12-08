library(shiny)
library(ggplot2)

# source("src/chooser.R")
irga_data_clean <- read.csv(here::here('results/tubes_to_plot.csv'))
# irga_data_clean <- list(to_plot, to_plot)

ui <- fluidPage(
    "select the level of data",
    
    # inputs
    
    # MCs
    # chooserInput("mychooser", "Available MCs", "Selected MCs",
    #              row.names(irga_data_clean$MC), c(), size=10, multiple=TRUE),
    # verbatimTextOutput("selection"),

    column(
        5,
        DT::dataTableOutput('rawdata')
    ),
    column(
        7,
        
        plotOutput('parcoord'),
        hr(),
        selectizeInput('state', label = NULL, choices = NULL, options = list(
            placeholder = 'Type a state name, e.g. Iowa', maxOptions = 5)
        )
    ),
    # 
    # checkboxGroupInput("dynamic", "Dynamic",
    #                    choices = c("BU" = "option1",
    #                                "BG" = "option2",
    #                                "GU" = "option3",
    #                                "GG" = "option4"),
    #                    selected = "option2"
    # ),
    # 
    # selectizeInput(
    #     'e2',
    #     'Microbial community',
    #     choices = irga_data_clean$MC,
    #     multiple = TRUE),
    
    # dropdown
    # selectInput(
    #     "select_data", 
    #     label = h3("Select box"), 
    #     choices = list("Tube gross, daily" = 1, 
    #                    "Tube diff, daily" = 2, 
    #                    "Cumulative gross, daily" = 3,
    #                    "Cumulative diff, daily" = 4), 
    #     selected = 1),
    # 
    # radioButtons(
    #     inputId = "resolution", 
    #     label = h3("Sample level"),
    #     choices = list("By tube" = 1, "By treatment" = 2),
    #     selected = 1),
    # 
    # checkboxGroupInput(
    #     "accumulation", 
    #     label = h3("Checkbox group"), 
    #     choices = list("Daily" = 1, "Cumulative by phase" = 2, "Cumulative overall" = 3),
    #     selected = 1),
    # 
    # sliderInput(
    #     "days", 
    #     label = h3("Slider Range"), 
    #     min = 0, max = 319, value = c(0, 319)),
    
    # plotOutput("myPlot")
    
    # outputs
    # hr(),
    # fluidRow(column(2, verbatimTextOutput("value")))
)

server <- function(input, output, session){
    level_reactive <- reactive({
        # which_data <- input$resolution
        # days_range <- input$days
        # data_resolution <- irga_data_clean
        
        # input$select_data
        
        data_subset <- irga_data_clean %>% 
            filter()
        
    })
    
    
    output$myPlot <- renderPlot({
        plot_by <- input$treatment
        
        # plot_nav(level_reactive(), group_by = plot_by)
    })
}

shinyApp(ui = ui, server = server)
