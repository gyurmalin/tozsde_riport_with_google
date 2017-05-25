library(data.table)
prices <- function(tickers) {
  adat <- data.table()
  
  for (i in tickers){
    tmp<- read.csv(paste('https://www.google.com/finance/historical?output=csv&q=', i, sep = ''))
    tmp$ticker <- i
    adat <- rbind(adat,tmp)

  }
  
  
  adat$Date <- as.character(adat$Date)
  adat$Date <- ifelse(as.numeric(sapply(strsplit(adat$Date, "-"), '[', 1))<10 & as.numeric(sapply(strsplit(adat$Date, "-"), '[', 1))>1 , paste('0', adat$Date,sep = ''), adat$Date)
  adat$Date <- ifelse(as.numeric(sapply(strsplit(adat$Date, "-"), '[', 1))==1  , paste('0', adat$Date,sep = ''), adat$Date)
  adat$Date <-gsub( 'Jan', '01',adat$Date)
  adat$Date <-gsub( 'Feb', '02',adat$Date)
  adat$Date <-gsub( 'Mar', '03',adat$Date)
  adat$Date <-gsub( 'Apr', '04',adat$Date)
  adat$Date <-gsub( 'May', '05',adat$Date)
  adat$Date <-gsub( 'Jun', '06',adat$Date)
  adat$Date <-gsub( 'Jul', '07',adat$Date)
  adat$Date <-gsub( 'Aug', '08',adat$Date)
  adat$Date <-gsub( 'Sep', '09',adat$Date)
  adat$Date <-gsub( 'Oct', '10',adat$Date)
  adat$Date <-gsub( 'Nov', '11',adat$Date)
  adat$Date <-gsub( 'Dec', '12',adat$Date)
  adat$Date <- as.Date(adat$Date, '%d-%m-%y')
  

  return(adat[,c(1,5,7)])
}

my_list <- c('TSLA', 'GE', 'AAPL')
adat <- prices(tickers = my_list)


tozsde_plot <- function(number_of_days, my_adatom, list_of_markets){
  
  my_days <- sort(unique(my_adatom$Date), decreasing = T)[c(1:number_of_days)]
  adatom <- data.table(my_adatom[my_adatom$Date %in% my_days,])
  setorder(adatom, ticker, Date)
  
  
  for (i in list_of_markets) {
    baseline <- adatom[ticker == i, Close][1]
    adatom[ticker == i, change := (Close/baseline-1)*100]
  }
  
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "Date",
    titlefont = f
  )
  y <- list(
    title = "Change (%)",
    titlefont = f
  )
  
  m <- list(
    l = 100,
    r = 100,
    b = 10,
    t = 150,
    pad = 4
  )
  p<-plot_ly(adatom, x = ~Date, y = ~change, color =~ticker, text= ~Close)%>%
    add_lines()%>%layout(title = paste(number_of_days, 'Days'), xaxis = x, yaxis = y, height = 900, width = 1200)%>%
    subplot(nrows=100, shareX = T )
  
  return(p)
  
}
tozsde_plot(10,adat,my_list)
