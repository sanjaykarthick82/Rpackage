install.packages(c("shiny", "quantmod", "plotly"))
library(shiny)
library(quantmod)
library(plotly)

ui <- fluidPage(
  titlePanel("Stock Price Graph"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stockSymbol", "Select Stock Symbol:", choices = c("AAPL", "GOOGL", "MSFT", "AMZN", "FB", "TSLA", "NFLX", "NVDA", "INTC", "AMD", "PYPL", "CRM", "ADBE", "CSCO", "PEP", "CMCSA", "ABNB", "SBUX", "DIS", "WMT")),
      actionButton("updateGraph", "Update Graph")
    ),
    mainPanel(
      plotlyOutput("stockPricePlot")
    )
  )
)

server <- function(input, output) {
  stockData <- reactive({
    data <- getSymbols(input$stockSymbol, src = "yahoo", auto.assign = FALSE)
    data <- data.frame(date = index(data), coredata(data))
    return(data)
  })
  
  observeEvent(input$updateGraph, {
    data <- stockData()
    p <- plot_ly(data = data, x = ~date, y = ~get(paste(input$stockSymbol, ".Adjusted", sep = "")), type = 'scatter', mode = 'lines', name = input$stockSymbol)
    p <- p %>% layout(
      title = paste("Live Stock Price of", input$stockSymbol),
      xaxis = list(title = "Time", range = c(Sys.Date() - 30, Sys.Date())), # Adjusted this line
      yaxis = list(title = "Price")
    )
    
    output$stockPricePlot <- renderPlotly({p})
  })
}

shinyApp(ui = ui, server = server)
