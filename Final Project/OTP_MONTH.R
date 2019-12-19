library(dplyr); library(plotly); library(shiny)

df<- read.csv("https://raw.githubusercontent.com/ChristinaValore/CUNY_DATA_608/master/Final%20Project/MTA_CLEANED.csv", header=TRUE, check.names = FALSE)

# Define UI ----
ui <- fluidPage(
    titlePanel("Subway on-time performance: December"),
    
    headerPanel(""),
    
    sidebarLayout(
        sidebarPanel(
            selectInput('nme', 'Subway line', unique(df$INDICATOR_NAME), selected='one'),
            actionButton('action', label = "Sort"),
            
            hr(),
            fluidRow(column(2, verbatimTextOutput("value")))),
        
        mainPanel(
            plotlyOutput('plot1')
        )
        
    )
)

# Define server logic ----
server <- function(input, output) {
    
    selected <- reactive({
        sub_df <- df %>% 
            filter(PERIOD_MONTH == 'Dec', INDICATOR_NAME == input$nme) %>% 
            mutate(PERIOD_YEAR = factor(PERIOD_YEAR, levels = unique(PERIOD_YEAR)[order(DAYS_ON_TIME, decreasing = ifelse((input$action %% 2) == 0,TRUE,FALSE))]))
        
    })
    
    output$plot1 <- renderPlotly({
        
        sub_df <- df %>% 
            filter(PERIOD_MONTH == 'Dec', INDICATOR_NAME == input$nme) %>% 
            mutate(PERIOD_YEAR = factor(PERIOD_YEAR, levels = unique(PERIOD_YEAR)[order(DAYS_ON_TIME, decreasing = ifelse((input$action %% 2) == 0,TRUE,FALSE))]))
        
        plot_ly(selected(), x = ~PERIOD_YEAR, y = ~DAYS_ON_TIME, color = ~PERIOD_YEAR, type='bar',
                mode = 'lines')
    })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)