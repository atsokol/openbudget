library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)

# Inputs
BUDGETCODE <- c("1356300000", "13563000000") # different codes for different years
YEAR <- c(2022, 2023)
PERIOD <- "MONTH"


# Function to construct API path
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


# Function to call API, read in and parse data
call_api <- function(api_path, col_types) {
  data_call <- GET(api_path) |> 
    pluck("content") |> 
    rawToChar() |> 
    read_delim(delim = ";", col_types = col_types) |> 
    mutate(REP_PERIOD = readr::parse_date(REP_PERIOD, "%m.%Y") |> 
             ceiling_date(unit="month") - days(1)) 
  
  return(data_call)
}


# Read in API codes
codes <- read_excel("./data/Open Budget variable types.xlsx")

# Construct API calls
df_api <- codes |> 
  group_by(budgetItem, classificationType) |> 
  summarise(col_type = paste(colType, collapse = ""), .groups = "drop") |> 
  mutate(across(everything(), str_trim)) |>  # trim white space in category names
  expand_grid(budgetCode = BUDGETCODE, period = PERIOD, year = YEAR) |> 
  rowwise() |> 
  mutate(api_path = api_construct(budgetCode, budgetItem, classificationType, period, year)) 

# Read in data across multiple periods and categories into a nested data frame
df_n <- df_api |> 
  mutate(data = list(call_api(api_path, col_type))) |> 
  select(budgetItem, classificationType, data) |> 
  group_by(budgetItem, classificationType) |> 
  summarise(data = list(map_dfr(data, rbind) |> arrange(REP_PERIOD)))


# Extract nested data column as a list
data_l <- df_n$data
names(data_l) <- if_else(!is.na(df_n$classificationType),
                         paste(df_n$budgetItem, df_n$classificationType, sep=", "),
                         df_n$budgetItem)

# Function to aggregate and reshape data in the SUMMARY table
reshape_table <- function(df, last_date, group_var) {
  df_agg <- df |> 
    filter(month(REP_PERIOD) %in% c(month(last_date),12),
           FUND_TYP == "T")|>
    group_by({{group_var}}, REP_PERIOD) |>  
    summarise(FAKT_AMT = sum(FAKT_AMT),
              ZAT_AMT = sum(ZAT_AMT), .groups = "drop")
  
  budget <- df_agg |>  
    select(-FAKT_AMT) |>
    filter(REP_PERIOD == last_date) |> 
    mutate(REP_PERIOD = paste0(year(last_date), "_B")) |>  # period label for budget amounts
    pivot_wider(names_from = "REP_PERIOD", values_from = "ZAT_AMT")
  
  df_table <- df_agg |> 
    select(-ZAT_AMT) |> 
    mutate(REP_PERIOD = ifelse(month(REP_PERIOD) == 12, 
                               year(REP_PERIOD), 
                               paste0(month(REP_PERIOD), "m ", year(REP_PERIOD)))) |> # period labels for actual amounts
    pivot_wider(names_from = "REP_PERIOD", values_from = "FAKT_AMT") |> 
    left_join(budget, by = join_by({{group_var}} == {{group_var}})) |> 
    mutate(across(where(is.double), ~ round(.x / 10^6, 0))) # convert units to millions UAH
    
  return(df_table)
}

# Reporting date
last_date <- max(data_l$INCOMES$REP_PERIOD) # most recent date by default
# last_date <- ymd('2023-06-30') # manual entry as an alternative

# Aggregate data by category
inc <- data_l$INCOMES |>
  mutate(TYPE = cut(COD_INCO, 
                    breaks = c(0,19999999,29999999,39999999,60000000),
                    labels = c("Tax","Non-tax","Capital revenues","Transfers"))
         ) |> 
  reshape_table(last_date, TYPE) |> 
  mutate(CAT = "Income", .before=1)


exp <- data_l$`EXPENSES, ECONOMIC` |> 
  mutate(TYPE = cut(COD_CONS_EK, 
                    breaks = c(0,2280,2281,2399,2421,2999,8999,9001),
                    labels = c("Opex","Capex","Opex","Interest","Opex","Capex","Opex"))
         ) |> 
  reshape_table(last_date, TYPE) |> 
  mutate(CAT = "Expense", .before=1)|>
  mutate(across(where(is.numeric),~.x*-1)) #change the sign of the inputs

fin <- data_l$FINANCING_DEBTS |> 
  mutate(TYPE = case_when(COD_FINA == 401000 ~ "New Borrowing",
                          COD_FINA == 402000 ~ "Debt Repayments",
                          COD_FINA == 602300 ~ "Interbudget loans",
                          TRUE ~ "NA")
         ) |> 
  reshape_table(last_date, TYPE) |> 
  mutate(CAT = "Financing", .before=1)

credit <- data_l$`CREDITS, CREDIT` |>
  mutate(TYPE = "Budget loans balance") |>
  reshape_table(last_date, TYPE) |> 
  mutate(CAT = "Loans", .before=1)|>
  mutate(across(where(is.numeric),~.x*-1)) #change the sign of the inputs

#Arrange the table according to the template

for_template <- function (df,
                          category,
                          codes) {
  
  category_df <- df|>
    filter(TYPE %in% c(codes))|>
    summarise_if(is.numeric,sum)|>
    mutate(CAT="Total",TYPE=category,.before = 1)
  
  df_temp<-rbind.data.frame(df,category_df)
  
  return(df_temp)
}

template <- rbind(inc, exp, fin, credit)|>
  for_template(category="Current revenues",codes=c("Tax","Non-tax","Transfers"))|>
  for_template("Operating surplus", codes=c("Current revenues","Opex"))|>
  for_template("Current_surplus", codes=c("Operating surplus","Interest"))|>
  for_template("Capital surplus",codes=c("Capital revenues","Capex"))|>
  for_template("Net surplus before financing",codes=c("Capital surplus","Current surplus","Budget loans balance"))|>
  for_template("Net debt",codes=c("New Borrowing","Debt Repayments","Interbudget loans"))|>
  for_template("Net surplus",codes = c("Net surplus before financing","Net debt"))|>
  slice(1,2,4,13,5,14,7,15,3,6,16,12,17,11,8,9,18,19)

# Write data to Excel file
if('SUMMARY' %in% names(data_l)) {
  data_l$SUMMARY <- template
} else {
  data_l <- append(list(SUMMARY = template), data_l)
}

write_xlsx(data_l, "./data/output.xlsx")




