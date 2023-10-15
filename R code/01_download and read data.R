library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(purrr)
library(plotly)
library(readxl)

# Function to construct api path
api_construct <- function(budgetCode, 
                          budgetItem, # "INCOMES","EXPENSES","FINANCING_DEBTS","FINANCING_CREDITOR","CREDITS"),
                          classificationType, # "PROGRAM","FUNCTIONAL","ECONOMIC","CREDIT"
                          period = "MONTH",
                          year) {
  
  api_base <- "https://api.openbudget.gov.ua/api/public/localBudgetData?"
  
  if (budgetItem %in% c("EXPENSES", "CREDITS")) {  
    api_path <- 
      paste(api_base,
            "budgetCode=", budgetCode,
            "&budgetItem=", budgetItem,
            "&classificationType=", classificationType,  # classificationType parameter is mandatory for EXPENSES and CREDITS items
            "&period=", period,
            "&year=", year,
            sep = "")
  } else {
    api_path <- 
      paste(api_base,
            "budgetCode=", budgetCode,
            "&budgetItem=", budgetItem,
            "&period=", period,
            "&year=", year,
            sep = "")
  }
  
  return(api_path)
}


# Function to call api, read in and parse data
call_api <- function(api_path, col_types) {
  data_call <- GET(api_path) |> 
    pluck("content") |> 
    rawToChar()
  
  if (missing(col_types)) {
    data_read <- data_call |> 
    read_delim(delim = ";")
  } else {
    data_read <- data_call |> 
    read_delim(delim = ";", col_types = col_types) |> 
    mutate(REP_PERIOD = readr::parse_date(REP_PERIOD, "%m.%Y"))
  }
  
  return(data_read)
}


# Read in data across multiple periods for a single category
df_1 <- api_construct(budgetCode = "26000000000", year = c(2022,2023), budgetItem = "INCOMES") |> 
  map_dfr(call_api)

# Read in data across multiple periods and categories into a nested data frame
codes <- read_excel("./data/Open Budget variable types.xlsx")

df_m <- codes |> 
  distinct(pick(budgetItem, classificationType)) |> 
  mutate(budgetItem = str_trim(budgetItem),
         classificationType = str_trim(classificationType)) |> # trim white space in category names
  mutate(budgetCode = "26000000000", 
         period = "MONTH") |> 
  mutate(year = list(c(2022,2023))) |> 
  rowwise() |> 
  mutate(api_path = list(api_construct(budgetCode, budgetItem, classificationType, period, year))) |> 
  mutate(data = list(map_dfr(api_path, call_api)))


# Compare across periods
(g1 <- df_1 |> 
  mutate(REP_PERIOD = readr::parse_date(REP_PERIOD, "%m.%Y")) |> 
  filter(month(REP_PERIOD) == month(max(REP_PERIOD)),  # take latest monthly data, note it's cumulative
         FUND_TYP == "T") |>   # data for different fund types seems to overlap
  mutate(category = NAME_INC |> 
           fct_lump_n(n=7, w = abs(FAKT_AMT)) |> # some FAKT_AMT values are negative, which is probably a typo
           fct_reorder(abs(FAKT_AMT))
         ) |> 
  ggplot(aes(x = format(REP_PERIOD, "%b %Y") |> as.factor(), 
             y = FAKT_AMT,
             fill = category)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = scales::label_comma(0.1, scale = 1e-9)) +
  labs(x = "", y = "UAH billion",
       title = "Kyiv budget income comparison 2023 vs 2022, ",
       caption = "Data: Open budget") +
  theme(legend.position = "none")
)

ggplotly(g1)  




