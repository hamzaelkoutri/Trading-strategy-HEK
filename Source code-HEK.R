# In this file, you find the code for the trading strategy for DB stocks : Work by Hamza ELKOUTRI
# Please note that this code was made block by block, it is recommended to compile it by blocks to see the conclusions

# Please install the packages before loading, they are used in this code
# Please pay attention to quantmod, it may contain bugs, a fix is included in the markdown file
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(xts)
library(shiny)
library(shinythemes)
library(data.table)


# Extracting the Bitcoin stock data from yfinance starting from 2017, the year where bitcoin started skyrocketing

Get_DB<-function(Ticker,DateFrom,DateTo){
  DBT<-getSymbols(Ticker, src = 'yahoo', from = DateFrom, to = DateTo,auto.assign = FALSE)
  DBT$Simple_Ret<- 100*diff(Cl(DBT))/lag(Cl(DBT), 1)
  DBT$Log_Ret<- 100*diff(log(Cl(DBT)))
  DBT$Simple_Ret[is.na(DBT$Simple_Ret)] <- 0
  DBT$Log_Ret[is.na(DBT$Log_Ret)] <- 0
  colnames(DBT)<-c("DB.Open","DB.High","DB.Low","DB.Close","DB.Volume","DB.Adjusted","DB.Simple_Ret","DB.Log_Ret")
  return(DBT)
}

# Now we will use built in packages for trading indicators

# Exponential Moving Average Indicator of 20 and 50 days
# These ones would help us have two indicator : 
# Fast Diff(20-50 SMA that gives in and out market)
Trigger_EMA<-function(DB,period){
  ema_period_db <- EMA(Cl(DB), n = period)
  return(ema_period_db)
}

Crossover_EMA<-function(DB){
  ema_period_A_db <- EMA(Cl(DB), n = 20)
  ema_period_B_db <- EMA(Cl(DB), n = 50)
  ema_db_trigger <- Lag(
    ifelse(Lag(ema_period_A_db) < Lag(ema_period_B_db) & ema_period_A_db > ema_period_B_db,1,
           ifelse(Lag(ema_period_A_db) > Lag(ema_period_B_db) & ema_period_A_db < ema_period_B_db,-1,0)))
  ema_db_trigger[is.na(ema_db_trigger)] <- 0
  chartSeries(DB,TA=c(addVo(),addEMA(20,col="red"),addEMA(50,col = "pink"),addTDI(),addVolatility()))
   legend('left', col = c('green','red','pink'),
          legend = c(paste('Ticker'),'EMA_20','EMA_50'), lty = 1, bty = 'n',
          text.col = 'white', cex = 0.8)
  return(ema_db_trigger)
}

Trigger_SMA<-function(DB,period){
  sma_period_db <- SMA(Cl(DB), n = period)
  return(sma_period_db)
}

Crossover_SMA<-function(DB){
  sma_period_A_db <- Trigger_SMA(DB,20)
  sma_period_B_db <- Trigger_SMA(DB,50)
  sma_db_trigger <- Lag(
    ifelse(Lag(sma_period_A_db) < Lag(sma_period_B_db) & sma_period_A_db > sma_period_B_db,1,
           ifelse(Lag(sma_period_A_db) > Lag(sma_period_B_db) & sma_period_A_db < sma_period_B_db,-1,0)))
  sma_db_trigger[is.na(sma_db_trigger)] <- 0
  chartSeries(DB,TA=c(addVo(),addSMA(20,col="red"),addSMA(50,col = "pink"),addTDI(),addVolatility()))
  legend('left', col = c('green','red','pink'),
         legend = c(paste('Ticker'),'SMA_20','SMA_50'), lty = 1, bty = 'n',
         text.col = 'white', cex = 0.8)
  
  return(sma_db_trigger)
}

Trigger_SMI<-function(DB){
  smi_db <- SMI(HLC(DB),
                n = 13, nFast = 2, nSlow = 25, nSig = 9)
  return(smi_db)
}

Crossover_SMI<-function(DB){
  smi_db <- Trigger_SMI(DB)
  smi_db_trigger <- Lag(
    ifelse(Lag(smi_db[,1]) < Lag(smi_db[,2]) & smi_db[,1] > smi_db[,2],1, 
           ifelse(Lag(smi_db[,1]) > Lag(smi_db[,2]) & smi_db[,1] < smi_db[,2],-1,0)))
  smi_db_trigger[is.na(smi_db_trigger)] <- 0
  chartSeries(DB,TA=c(addVo(),addSMI(),addTDI(),addVolatility()))
  legend('left', col = c('green'),
         legend = c(paste('Ticker')), lty = 1, bty = 'n',
         text.col = 'white', cex = 0.8)
  return(smi_db_trigger)
}

# Creating Trading Strategies using Triggers

Strategy_EMA<-function(DB){
  ema_A_db <- Trigger_EMA(DB,20)
  ema_B_db <- Trigger_EMA(DB,50)
  ema_db_trigger <- Crossover_EMA(DB)
  ema_db_strat <- ifelse(ema_db_trigger > 1,0,1)
  for (i in 1 : length(Cl(DB))) {
    ema_db_strat[i] <- ifelse(ema_db_trigger[i] == 1,1,ifelse(ema_db_trigger[i] == -1,0,ema_db_strat[i-1]))
  }
  ema_db_strat[is.na(ema_db_strat)] <- 1
  ema_db_strat_final <- cbind(ema_A_db, ema_B_db, ema_db_trigger, ema_db_strat)
  colnames(ema_db_strat_final) <- c('EMA(20)','EMA(50)','EMA SIGNAL','EMA POSITION')
  return(ema_db_strat_final)
}

Strategy_SMA<-function(DB){
  sma_A_db <- Trigger_SMA(DB,20)
  sma_B_db <- Trigger_SMA(DB,50)
  sma_db_trigger <- Crossover_SMA(DB)
  sma_db_strat <- ifelse(sma_db_trigger > 1,0,1)
  for (i in 1 : length(Cl(DB))) {
    sma_db_strat[i] <- ifelse(sma_db_trigger[i] == 1,1,ifelse(sma_db_trigger[i] == -1,0,sma_db_strat[i-1]))
  }
  sma_db_strat[is.na(sma_db_strat)] <- 1
  sma_db_strat_final <- cbind(sma_A_db, sma_B_db, sma_db_trigger, sma_db_strat)
  colnames(sma_db_strat_final) <- c('SMA(20)','SMA(50)','SMA SIGNAL','SMA POSITION')
  return(sma_db_strat_final)
}

Strategy_SMI<-function(DB){
  smi_db <- Trigger_SMI(DB)
  smi_db_trigger <- Crossover_SMI(DB)
  smi_db_strat <- ifelse(smi_db_trigger > 1,0,1)
  for (i in 1 : length(Cl(DB))) {
    smi_db_strat[i] <- ifelse(smi_db_trigger[i] == 1,1,ifelse(smi_db_trigger[i] == -1,0,smi_db_strat[i-1]))
  }
  smi_db_strat[is.na(smi_db_strat)] <- 1
  smi_db_strat_final <- cbind(smi_db[,1],smi_db[,2],smi_db_trigger,smi_db_strat)
  colnames(smi_db_strat_final) <- c('SMI','SMI(S)','SMI SIGNAL','SMI POSITION')
  return(smi_db_strat_final)
}

Strategy_All_UI<-function(DB){
  ema_A_db <- Trigger_EMA(DB,20)
  ema_B_db <- Trigger_EMA(DB,50)
  ema_db_trigger <- Crossover_EMA(DB)
  
  ema_db_strat_UI <- ifelse(ema_db_trigger > 1,"Not holding","Holding")
  for (i in 1 : length(Cl(DB))) {
    ema_db_strat_UI[i] <- ifelse(ema_db_trigger[i] == 1,"Holding",ifelse(ema_db_trigger[i] == -1,"Not holding",ema_db_strat_UI[i-1]))
  }
  ema_db_strat_UI[is.na(ema_db_strat_UI)] <- "Holding"
  
  ema_db_trigger_UI <- Lag(
    ifelse(Lag(ema_A_db) < Lag(ema_B_db) & ema_A_db > ema_B_db,"Buy",
           ifelse(Lag(ema_A_db) > Lag(ema_B_db) & ema_A_db < ema_B_db,"Sell","Neutral")))
  ema_db_trigger_UI[is.na(ema_db_trigger_UI)] <- "Neutral"
  
  sma_A_db <- Trigger_SMA(DB,20)
  sma_B_db <- Trigger_SMA(DB,50)
  sma_db_trigger <- Crossover_SMA(DB)
  
  sma_db_strat_UI <- ifelse(sma_db_trigger > 1,"Not holding","Holding")
  for (j in 1 : length(Cl(DB))) {
    sma_db_strat_UI[j] <- ifelse(sma_db_trigger[j] == 1,"Holding",ifelse(sma_db_trigger[j] == -1,"Not holding",sma_db_strat_UI[j-1]))
  }
  sma_db_strat_UI[is.na(sma_db_strat_UI)] <- "Holding"
  
  sma_db_trigger_UI <- Lag(
    ifelse(Lag(sma_A_db) < Lag(sma_B_db) & sma_A_db > sma_B_db,"Buy",
           ifelse(Lag(sma_A_db) > Lag(sma_B_db) & sma_A_db < sma_B_db,"Sell","Neutral")))
  sma_db_trigger_UI[is.na(sma_db_trigger_UI)] <- "Neutral"
  
  db_strat_final <- cbind(data.frame(Dates=index(DB),coredata(DB)),ema_db_trigger_UI, ema_db_strat_UI,sma_db_trigger_UI,sma_db_strat_UI)
  colnames(db_strat_final) <- c("Dates","DB.Open","DB.High","DB.Low","DB.Close","DB.Volume","DB.Adjusted","DB.Simple_Ret","DB.Log_Ret",'EMA TRIGGER','EMA POSITION','SMA TRIGGER','SMA POSITION')
  return(db_strat_final)
}


#Results and benchmarks

Ticker_Return<-function(DB){
  ret_db <- diff(log(Cl(DB)))
  benchmark_db <- ret_db
  return(benchmark_db)
}

Results_EMA<-function(DB){
  benchmark_db<-Ticker_Return(DB)
  ema_db_strat_final<-Strategy_EMA(DB)
  ema_db_trigger<-Crossover_EMA(DB)
  ema_db_strat<-ema_db_strat_final[,4]
  ema_db_ret <- benchmark_db*ema_db_strat
  ema_db_ret_commission_adj <- ifelse((ema_db_trigger == 1|ema_db_trigger == -1) & ema_db_strat != Lag(ema_db_trigger), (benchmark_db-0.05)*ema_db_strat, benchmark_db*ema_db_strat)
  ema_db_comp <- cbind(ema_db_ret, ema_db_ret_commission_adj, benchmark_db)
  colnames(ema_db_comp) <- c('EMA','EMA Commission Adj','Ticker Benchmark')
  #charts.PerformanceSummary(ema_db_comp, main =paste('EMA Performance'))
  return(ema_db_comp)
}

Results_SMA<-function(DB){
  benchmark_db<-Ticker_Return(DB)
  sma_db_strat_final<-Strategy_SMA(DB)
  sma_db_trigger<-Crossover_SMA(DB)
  sma_db_strat<-sma_db_strat_final[,4]
  sma_db_ret <- benchmark_db*sma_db_strat
  sma_db_ret_commission_adj <- ifelse((sma_db_trigger == 1|sma_db_trigger == -1) & sma_db_strat != Lag(sma_db_trigger), (benchmark_db-0.05)*sma_db_strat, benchmark_db*sma_db_strat)
  sma_db_comp <- cbind(sma_db_ret, sma_db_ret_commission_adj, benchmark_db)
  colnames(sma_db_comp) <- c('SMA','SMA Commission Adj','Ticker Benchmark')
  #charts.PerformanceSummary(sma_db_comp, main =paste('SMA Performance'))
  return(sma_db_comp)
}

Results_SMI<-function(DB){
  benchmark_db<-Ticker_Return(DB)
  smi_db_strat_final<-Strategy_SMI(DB)
  smi_db_trigger<-Crossover_SMI(DB)
  smi_db_strat<-smi_db_strat_final[,4]
  smi_db_ret <- benchmark_db*smi_db_strat
  smi_db_ret_commission_adj <- ifelse((smi_db_trigger == 1|smi_db_trigger == -1) & smi_db_strat != Lag(smi_db_trigger), (benchmark_db-0.05)*smi_db_strat, benchmark_db*smi_db_strat)
  smi_db_comp <- cbind(smi_db_ret, smi_db_ret_commission_adj, benchmark_db)
  colnames(smi_db_comp) <- c('SMI','SMI Commission Adj','Ticker Benchmark')
  #charts.PerformanceSummary(smi_db_comp, main =paste('SMI Performance'))
  return(smi_db_comp)
}

#Profit

Profit_EMA<-function(DB,invest){
  i<-0
  Buy<-invest
  Growth <-0
  ema_db_strat_final<-Strategy_EMA(DB)
  while(i<=length((ema_db_strat_final$`EMA SIGNAL`))){
    if(isTRUE((ema_db_strat_final$`EMA POSITION`)[i]==1))
    {if(isTRUE((ema_db_strat_final$`EMA SIGNAL`)[i]==1)){
      
      Buy<-Buy-as.numeric(Cl(DB)[i])
    }
      Growth<-Growth+as.numeric(DB$DB.Simple_Ret[i])
    }
    if(isTRUE((ema_db_strat_final$`EMA SIGNAL`)[i]==-1)){
      Buy<-Buy+as.numeric(Cl(DB)[i])}
    i<-i+1}
  return(paste('With ',invest,'$(USD) investment, profit of this Strategy EMA on this stock: ',invest*Growth/100,'$(USD)'))
}

Profit_SMA<-function(DB,invest){
  i<-0
  Buy<-invest
  Growth <-0
  sma_db_strat_final<-Strategy_SMA(DB)
  while(i<=length((sma_db_strat_final$`SMA SIGNAL`))){
    if(isTRUE((sma_db_strat_final$`SMA POSITION`)[i]==1))
    {if(isTRUE((sma_db_strat_final$`SMA SIGNAL`)[i]==1)){
      Buy<-Buy-as.numeric(Cl(DB)[i])
    }
      Growth<-Growth+as.numeric(DB$DB.Simple_Ret[i])
    }
    if(isTRUE((sma_db_strat_final$`SMA SIGNAL`)[i]==-1)){
      Buy<-Buy+as.numeric(Cl(DB)[i])}
    i<-i+1}
  return(paste('With ',invest,'$(USD) investment, profit of this Strategy SMA on this stock: ',invest*Growth/100,'$(USD)'))
}

Profit_SMI<-function(DB,invest){
  i<-0
  Buy<-invest
  Growth <-0
  smi_db_strat_final<-Strategy_SMI(DB)
  while(i<=length((smi_db_strat_final$`SMI SIGNAL`))){
    if(isTRUE((smi_db_strat_final$`SMI POSITION`)[i]==1))
    {if(isTRUE((smi_db_strat_final$`SMI SIGNAL`)[i]==1)){
      Buy<-Buy-as.numeric(Cl(DB)[i])
    }
      Growth<-Growth+as.numeric(DB$DB.Simple_Ret[i])
    }
    if(isTRUE((smi_db_strat_final$`SMI SIGNAL`)[i]==-1)){
      Buy<-Buy+as.numeric(Cl(DB)[i])}
    i<-i+1}
  return(paste('With ',invest,'$(USD) investment, profit of this Strategy SMI on this stock: ',invest*Growth/100,'$(USD)'))
}


####################
# Application code #
####################


ui <- fluidPage(
  tags$img(src = "https://emlyon.brightspace.com/d2l/lp/navbars/6606/theme/viewimage/495707/view?v=20.22.4.36719",height = 50, width = 50),
  headerPanel("Trading calculator (by HEK)"),
  theme = shinytheme("yeti"),
  sidebarLayout(
  sidebarPanel(
     h4("Choose your parameters:"),
     textInput(inputId = "ticker", label="Enter ticker symbol:",placeholder = "AAPL, TSLA, TWTR...",value="TSLA"),
     dateInput(inputId = "Start_date",label="Choose starting date:",max=Sys.Date(),value=Sys.Date()-365),
     dateInput(inputId = "End_date",label="Choose starting date:",max=Sys.Date(),value=Sys.Date()),
     numericInput(inputId = "Capital",label="Enter your inital investement in USD($):",min=0,value=100)),
      mainPanel(
        tabsetPanel(
          tabPanel("Overall View",dataTableOutput(outputId = 'raw_extract')),
          tabPanel("EMA 20-50 Analysis", h4("Graphical representation:"),plotOutput(output="Plot_EMA"),h4("Returns annualized:"),tableOutput(outputId = 'Result_EMA')),
          tabPanel("SMA 20-50 Analysis", h4("Graphical representation:"),plotOutput(output="Plot_SMA"),h4("Returns annualized:"),tableOutput(outputId = 'Result_SMA')),
          tabPanel("SMI Analysis", h4("Graphical representation:"),plotOutput(output="Plot_SMI"),h4("Returns annualized:"),tableOutput(outputId = 'Result_SMI')),
          tabPanel("Performance Analysis",plotOutput(output="Perf_EMA"),plotOutput(output="Perf_SMA"),plotOutput(output="Perf_SMI")),
          tabPanel("Profit forecast",h4("Profit following EMA:"),textOutput(outputId="Profit_EMA"),h4("Profit following SMA:"),textOutput(outputId="Profit_SMA"),h4("Profit following SMI:"),textOutput(outputId="Profit_SMI"))
          )
      )
  )
)

server <- function(input, output) {
  output$raw_extract <-renderDataTable(Strategy_All_UI(Get_DB(input$ticker,input$Start_date,input$End_date)))
  output$Result_EMA <- renderTable(table.AnnualizedReturns(Results_EMA(Get_DB(input$ticker,input$Start_date,input$End_date))))
  output$Plot_EMA<-renderPlot(Crossover_EMA(Get_DB(input$ticker,input$Start_date,input$End_date)))
  output$Result_SMA <- renderTable(table.AnnualizedReturns(Results_SMA(Get_DB(input$ticker,input$Start_date,input$End_date))))
  output$Plot_SMA<-renderPlot(Crossover_SMA(Get_DB(input$ticker,input$Start_date,input$End_date)))
  output$Result_SMI <- renderTable(table.AnnualizedReturns(Results_SMI(Get_DB(input$ticker,input$Start_date,input$End_date))))
  output$Plot_SMI<-renderPlot(Crossover_SMI(Get_DB(input$ticker,input$Start_date,input$End_date)))
  output$Perf_EMA<-renderPlot(charts.PerformanceSummary(Results_EMA(Get_DB(input$ticker,input$Start_date,input$End_date)), main =paste('EMA Performance')))
  output$Perf_SMA<-renderPlot(charts.PerformanceSummary(Results_SMA(Get_DB(input$ticker,input$Start_date,input$End_date)), main =paste('SMA Performance')))
  output$Perf_SMI<-renderPlot(charts.PerformanceSummary(Results_SMI(Get_DB(input$ticker,input$Start_date,input$End_date)), main =paste('SMI Performance')))
  output$Profit_EMA<-renderText(Profit_EMA(Get_DB(input$ticker,input$Start_date,input$End_date),input$Capital))
  output$Profit_SMA<-renderText(Profit_SMA(Get_DB(input$ticker,input$Start_date,input$End_date),input$Capital))
  output$Profit_SMI<-renderText(Profit_SMI(Get_DB(input$ticker,input$Start_date,input$End_date),input$Capital))
  
  }

shinyApp(ui = ui, server = server)