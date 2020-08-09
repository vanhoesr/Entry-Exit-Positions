## Install these packages, algo will not work w/out

################################################################################

install.packages("quantmod"); install.packages("alphavantager")
install.packages("tidyquant"); install.packages("readxl")
install.packages("data.table",dependencies = TRUE); install.packages("graphics")

################################################################################

## StartUp 
## Retrieves ticker as user input and converts to compatible format
## Assigns API key & ticker input to Global Env

################################################################################

startUp = function(startUp)
{
  library(quantmod); library(alphavantager); library(tidyquant); 
  library(readxl); library(graphics); library(data.table); library(xts)
  api_key <- "A**************L"; assign("api_key", api_key, envir = globalenv())
  ticker = toupper(readline(prompt = "Input ticker symbol: ")) ;assign("ticker", 
            ticker, envir = globalenv())
  placeholder <- ""; assign("placeholder", placeholder, envir = globalenv())

  
  retrieve_ticker()
}

################################################################################

## Retrieve's historic price data for ticker symbol 'ticker'
## In fread(xts.....), nrows reads '250' which for some reason returns data for
# the last ~365+ days, not sure why
## tz variable is unknown, 'US/Eastern' is acting as a stand-in/approximation

################################################################################

retrieve_ticker = function(retrieve_ticker)
{
  url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_",
                placeholder,"DAILY&symbol=",ticker,"&outputsize=full&apikey=",
                api_key,"&datatype=csv")
  data_raw <- fread(url, header = TRUE, sep = ",") 
  data_xts <- xts(data_raw[,c("open","high","close","low","volume")],
             order.by = as.POSIXct(data_raw$timestamp, format = "%Y-%m-%d",
                                   tz = "US/Eastern"))
  assign("TICKER_DATA_XTS", data_xts, envir = globalenv())
  
  chart_ticker()
}

################################################################################

## Charts all data for last 6 months including MA & vol TA's for visualization 
## 'Sys.Sleep' will wait for x seconds then recur to retrieve_ticker()
## Identify Buy/Sell points, in this case: Point of EMA50 >= EMA200 & plot
## MACD tolerances work best between 0.075-0.070 (as of testing in Ticker:MCD)

################################################################################

chart_ticker = function(chart_ticker)
{
  EMA50 <- as.xts(EMA(TICKER_DATA_XTS$close, n = 50)); EMA50[is.na(EMA50) <- 0]
  EMA200 <- as.xts(EMA(TICKER_DATA_XTS$close, n=200));EMA200[is.na(EMA200) <- 0]
  assign("EMA50", EMA50, envir = globalenv()); assign("EMA200", EMA200, envir = 
              globalenv())
  
  BS_Signal <- as.xts(EMA50 >= EMA200)
              BS_Signal$EMA <- as.integer(as.logical(BS_Signal$EMA))
              BS_Signal <- na.locf(BS_Signal, fromLast = TRUE)
              BS_Signal[BS_Signal$EMA == 1] <- max(TICKER_DATA_XTS$close)
  assign("BS_Signal",BS_Signal,env = globalenv())
  
  Visual_Declutter <- TICKER_DATA_XTS
              Visual_Declutter <- cbind(Visual_Declutter$close)
              Visual_Declutter[Visual_Declutter$close<(max
                (TICKER_DATA_XTS$close) + 1)] <- max(TICKER_DATA_XTS$close)
  assign("Visual_Declutter", Visual_Declutter, env = globalenv())
              
  chartSeries(TICKER_DATA_XTS, name = ticker, theme = chartTheme("black"), 
              up.col = "green", dn.col = "red", type = "bars", color.vol =FALSE, 
              TA = "addVo(log.scale=FALSE);addTA(EMA50,on=1,col=4);addTA(EMA200, 
              on=1, col = 18); addTA(BS_Signal, on=1,col=7); addMACD(fast = 50, 
              slow = 200); addTA(Visual_Declutter,on = -1,col=8);
              addTA(TICKER_DATA_XTS$close, on = -1, col = 8)")
  reChart(color.vol = FALSE, subset = "last 24 months")
  
  detect()
}

################################################################################

## Informs whether to BUY/SELL, adjusts Broker Signal (functionality not 
# implemented yet)
## Remove "#" for Sys.Sleep and retrieve_ticker() to enable looping, "60" 
# indicates 60 seconds wait until looping back to retrieve_ticker() function-
# change this to "86400" to loop once every 24 hours 

################################################################################

detect <- function(detect)
{
  if (BS_Signal >= max(TICKER_DATA_XTS$close)){
    Broker_Signal <- "BUY/HOLD"
  } else{
    Broker_Signal <- "SELL/STAY"
  }
 
  #Sys.sleep(60)
  #retrieve_ticker()
}

################################################################################

startUp()

################################################################################









