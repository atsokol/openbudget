library(httr)
library(jsonlite)
library(tidyverse)
library(plotly)
library(readxl)
library(writexl)

# Inputs
BUDGETCODE <- "26000000000" # Lviv code: "1356300000"
YEAR <- c(2022,2023)
PERIOD <- "MONTH"


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
      read_delim(delim = ";") |> 
      mutate(REP_PERIOD = readr::parse_date(REP_PERIOD, "%m.%Y") |> 
               ceiling_date(unit="month") - days(1))
  } else {
      data_read <- data_call |> 
      read_delim(delim = ";", col_types = col_types) |> 
      mutate(REP_PERIOD = readr::parse_date(REP_PERIOD, "%m.%Y") |> 
             ceiling_date(unit="month") - days(1))
  }
  
  return(data_read)
}


# Read in data across multiple periods for a single category
df_1 <- api_construct(budgetCode = BUDGETCODE, budgetItem = "INCOMES", year = YEAR) |> 
  map_dfr(call_api, col_types = "cfcicddd")

# Read in data across multiple periods and categories into a nested data frame
codes <- read_excel("./data/Open Budget variable types.xlsx")

df_m <- codes |> 
  group_by(budgetItem, classificationType) |> 
  summarise(col_type = paste(colType, collapse = "")) |> 
  mutate(budgetItem = str_trim(budgetItem),
         classificationType = str_trim(classificationType)) |> # trim white space in category names
  mutate(budgetCode = BUDGETCODE, 
         period = PERIOD) |> 
  mutate(year = list(YEAR)) |> 
  rowwise() |> 
  mutate(api_path = list(api_construct(budgetCode, budgetItem, classificationType, period, year))) |> 
  mutate(data = list(map_dfr(api_path, call_api, col_type)))

# Write data to Excel file
data_l <- df_m$data
names(data_l) <- if_else(!is.na(df_m$classificationType),
                         paste(df_m$budgetItem, df_m$classificationType, sep=", "),
                         df_m$budgetItem)

last_date <- max(data_l$INCOMES$REP_PERIOD)

write_xlsx(data_l, "./data/data_output.xlsx")

# Aggregate data by category
inc <- data_l$INCOMES |>
  mutate(TYPE = cut(COD_INCO, 
                    breaks = c(0,19999999,29999999,39999999,60000000),
                    labels = c("Tax","Non-tax","Cap_rev","Transfers")
  )
  ) |> 
  filter(month(REP_PERIOD) %in% c(month(max(REP_PERIOD)),12),
         FUND_TYP == "T") |>
  group_by(TYPE,REP_PERIOD) |> 
  summarise(FAKT_AMT = sum(FAKT_AMT),
            ZAT_AMT = sum(ZAT_AMT))

exp <- data_l$`EXPENSES, ECONOMIC` |> 
  mutate(TYPE = cut(COD_CONS_EK, 
                    breaks = c(0,2280,2281,2399,2421,2999,8999,9001),
                    labels = c("Opex","Capex","Opex","Interest","Opex","Capex","Opex"))
         ) |> 
  filter(month(REP_PERIOD) %in% c(month(max(REP_PERIOD)),12),
         FUND_TYP == "T") |> 
  group_by(TYPE,REP_PERIOD) |> 
  summarise(FAKT_AMT = sum(FAKT_AMT),
            ZAT_AMT = sum(ZAT_AMT))


fin <- data_l$FINANCING_DEBTS |> 
  mutate(TYPE=if_else(COD_FINA==401000, {"New Borrowing"},
                      if_else(COD_FINA==402000,"Debt Repayments",
                              if_else(COD_FINA==602300,"Interbudget loans","NA")))
         ) |> 
  filter(month(REP_PERIOD) %in% c(month(max(REP_PERIOD)),12),
         FUND_TYP == "T")|>
  group_by(TYPE,REP_PERIOD)%>% 
  summarise(FAKT_AMT = sum(FAKT_AMT),
            ZAT_AMT = sum(ZAT_AMT))

# Function to reshape aggregate data table for export
reshape_table <- function(table, last_date) {
  table |> 
    select(-ZAT_AMT) |> 
    pivot_wider(names_from = "REP_PERIOD", values_from = "FAKT_AMT") |> 
    left_join(
      pivot_wider(table |> 
                    select(-FAKT_AMT) |> 
                    filter(REP_PERIOD == last_date) |> 
                    mutate(REP_PERIOD = paste0(year(last_date), "_B")), 
                  names_from = "REP_PERIOD", 
                  values_from = "ZAT_AMT"),
      by = c("TYPE" = "TYPE")
    ) |> 
    relocate(as.character(floor_date(last_date, unit="year") - days(1)), .after="TYPE") |> 
    ungroup()
}

reshape_table(inc, last_date)
reshape_table(exp, last_date)
reshape_table(fin, last_date)


 




