library(shiny)
library(plotly)
library(DT)
library(data.table)


navbarPage(
  title="Tözsde plot",
  tabPanel("Elemzés",
           sidebarLayout(
             sidebarPanel(
               textInput("stock_list", "Write tickers ", value = "TSLA, NVDA, AMD, AAPL, MU"),
               sliderInput("integer", "Number of days before:", min=0, max=100, value=50)
               
             ),
             mainPanel(
               plotlyOutput('summary_plot')
             )
           )
           
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  )# http://bootswatch.com/#Grafikon_tab
  
  
  
)#nav