library(jsonlite)
library(xts)
library(tidyverse)


# Function to get data via NBU API
get_FX <- function(start, end, currency) {
  start <- as.Date(start, format = "%Y%m%d")
  end <- as.Date(end, format = "%Y%m%d")
  
  url <- paste("https://bank.gov.ua/NBU_Exchange/exchange_site?", 
               "start=", start, 
               "&end=", end, 
               "&valcode=", currency, 
               "&sort=exchangedate&order=desc&json", 
               sep="")
  
  df <- fromJSON(url, simplifyDataFrame = TRUE)
  return(df)
}

# Function to aggregate data
agg_FX <- function(series) {
  ave <- apply.yearly(series, mean, na.rm=TRUE)
  eop <- apply.yearly(series, last)
  fx_tbl <- merge(ave, eop)
  colnames(fx_tbl) <- c("ave", "eop")
  return(fx_tbl)
}

# Get data and convert to xts
fx <- get_FX('20220101','20230831','EUR') 
fx_xts <- xts(fx$rate, order.by = as.Date(fx$exchangedate, format = "%d.%m.%Y"))

# Get series for previous year fractional period
last_index <- index(last(fx_xts)) - years(1)
fx_xts_last <- fx_xts[paste0(year(last_index), "/", last_index)]

# Calculate annual average and end-of-year values, add previous year fractional period
fx_agg <- agg_FX(fx_xts) |> fortify()
fx_last_agg <- agg_FX(fx_xts_last) |> fortify()
fx_agg <- rbind(fx_agg,fx_last_agg)



