library(shiny)
library(DT)
library(plotly)
library(data.table)



function(input, output, session) {
  source('my_functions.R')
  
  my_list <- reactive({
    szoveg<-as.character(input$stock_list)
    tickers <- strsplit(gsub(' ','',szoveg),",")[[1]]
    return(tickers)
  })
  
  my_data <- reactive({
    adatom_teljes <- prices(my_list())
    adatom_teljes$Date <- as.Date(adatom_teljes$Date)
    return(adatom_teljes)
  })
  
  
  my_plot <- reactive({
    tozsde_plot(number_of_days = input$integer, my_adatom = my_data(), list_of_markets =my_list() )
  })
  
  output$summary_plot <- renderPlotly({
    my_plot()
  })
  
}
