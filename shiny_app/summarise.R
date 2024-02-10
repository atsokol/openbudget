# Functions to summarise data downloaded from Open Budget portal

# Function to aggregate and reshape data in the SUMMARY table
reshape_table <- function(df, date, group_var) {
  # Latest date for which data is available
  last_date <- max(df$REP_PERIOD)
  
  # Latest available budget (plan) figures
  budget <- df |>
    filter(REP_PERIOD == last_date,
           FUND_TYP == "T") |>
    group_by({{group_var}}, REP_PERIOD) |>
    summarise(ZAT_AMT = sum(ZAT_AMT), .groups = "drop") |>
    mutate(REP_PERIOD = paste0(year(REP_PERIOD), "_B")) |>  # period label for budget amounts
    pivot_wider(names_from = "REP_PERIOD", values_from = "ZAT_AMT")
  
  # Actual figures
  df_table <- df |> 
    filter(month(REP_PERIOD) %in% c(month(date), 12),
           FUND_TYP == "T")|>
    group_by({{group_var}}, REP_PERIOD) |>  
    summarise(FAKT_AMT = sum(FAKT_AMT), .groups = "drop") |> 
    mutate(REP_PERIOD = ifelse(month(REP_PERIOD) == 12, 
                               year(REP_PERIOD), 
                               paste0(month(REP_PERIOD), "m ", year(REP_PERIOD)))) |> # period labels for actual amounts
    pivot_wider(names_from = "REP_PERIOD", values_from = "FAKT_AMT") |> 
    full_join(budget, by = join_by({{group_var}} == {{group_var}})) |> 
    mutate(across(where(is.double), ~ round(.x / 10^6, 2))) # convert units to millions UAH
  
  return(df_table)
}

summarise_data <- function(data_l, period, adj_cat = NULL) {
  # Latest date for which data is available
  last_date <- max(data_l$INCOMES$REP_PERIOD)
  
  # Reporting date based on chooser in the Shiny app
  date <- case_when(
    period == 1 ~ last_date,
    period == 2 ~ ymd(paste(year(last_date), 03, 31)),
    period == 3 ~ ymd(paste(year(last_date), 06, 30)),
    period == 4 ~ ymd(paste(year(last_date), 09, 30)),
    period == 5 ~ ymd(paste(year(last_date), 12, 31))
  )
    
  inc_exp_categ_upd<-read_excel(here("data/Open Budget category for update.xlsx"))
  inc_categ_upd<-inc_exp_categ_upd|>
    filter(CATEG=="INC")
  exp_categ_upd<-inc_exp_categ_upd|>
    filter(CATEG=="EXP")
  
  inc <- data_l$INCOMES |>
    mutate(TYPE = cut(COD_INCO, 
                      breaks = c(0,inc_categ_upd$BREAK_END),
                      labels = c(inc_categ_upd$NAME_TYPE))
    )
  
  #Check the transfers and re-categorize capital grants
  transfers <- inc |>
    filter(TYPE == "Transfers",
           FUND_TYP == "T")
  
  transfers_names <- unique(transfers$NAME_INC) |>
    cbind(unique(transfers$COD_INCO)) 
  
  #Aggregate final income data for the update including adjustment for capital grants
  inc <- inc |>
    mutate(TYPE = replace(TYPE, COD_INCO %in% adj_cat, "Capital revenues")) |> #the budget code was from the transfers table
    reshape_table(date, TYPE) |> 
    mutate(CAT = "Income", .before=1)
  
  exp <- data_l$`EXPENSES, ECONOMIC` |> 
    mutate(TYPE = cut(COD_CONS_EK, 
                      breaks = c(0,exp_categ_upd$BREAK_END),
                      labels = c(exp_categ_upd$NAME_TYPE)
    )
    ) |> 
    reshape_table(date, TYPE) |> 
    mutate(CAT = "Expense", .before=1)|>
    mutate(across(where(is.numeric), ~.x*-1)) #change the sign of the inputs
  
  fin <- data_l$FINANCING_DEBTS |> 
    mutate(TYPE = case_when(COD_FINA == 401000 ~ "New Borrowing",
                            COD_FINA == 402000 ~ "Debt Repayments",
                            COD_FINA == 602300 ~ "Interbudget loans",
                            TRUE ~ "NA")
    ) |> 
    reshape_table(date, TYPE) |> 
    mutate(CAT = "Financing", .before=1)
  
  credit <- data_l$`CREDITS, CREDIT` |>
    mutate(TYPE = "Budget loans balance") |>
    reshape_table(date, TYPE) |> 
    mutate(CAT = "Loans", .before=1)|>
    mutate(across(where(is.numeric), ~.x*-1)) #change the sign of the inputs
  
  cash <- data_l$FINANCING_DEBTS |>
    mutate(across(where(is.numeric),~ abs(.x)))|>
    mutate(TYPE = case_when(COD_FINA == 602100  ~ "Cash, bop",
                            COD_FINA == 602200 ~ "Cash, eop",
                            COD_FINA == 601000 ~ "Cash, eop",
                            TRUE ~ "NA")
    )|>
    reshape_table(date, TYPE)|>
    mutate(CAT = "Cash balance", .before=1)
  
  #Function to arrange the table according to the template
  for_template <- function (df, category, codes) {
    
    category_df <- df|>
      filter(TYPE %in% c(codes))|>
      summarise_if(is.numeric, sum)|>
      mutate(across(where(is.double), ~ round(.x, 6))) |> 
      mutate(CAT = "Total", TYPE = category, .before = 1)
    
    df_temp <- rbind.data.frame(df, category_df)
    
    return(df_temp)
  }
  
  #Summarize the template for monitoring
  template <- rbind(inc, exp, fin, credit, cash)|>
    for_template(category = "Current revenues", codes = c("Tax","Non-tax","Transfers"))|>
    for_template("Operating surplus", codes = c("Current revenues","Opex"))|>
    for_template("Current surplus", codes = c("Operating surplus","Interest"))|>
    for_template("Capital surplus", codes = c("Capital revenues","Capex"))|>
    for_template("Net surplus before financing", codes = c("Capital surplus","Current surplus"))|>
    for_template("Net debt", codes = c("New Borrowing","Debt Repayments"))|>
    for_template("Net surplus",codes = c("Net surplus before financing","Net debt","Budget loans balance"))|>
    arrange(factor(TYPE, levels = c("Tax","Non-tax","Transfers","Current revenues","Opex","Operating surplus","Interest",
                                    "Current surplus","Capital revenues","Capex","Capital surplus",
                                    "Net surplus before financing","New Borrowing","Debt Repayments",
                                    "Net debt","Budget loans balance","Net surplus","Interbudget loans", 
                                    "Cash, bop","Cash, eop"))) |> 
    filter(TYPE != "NA")
  

  # Write final output
  output <- data_l
  output <- append(output, list(SUMMARY_UPDATE = template), after=0)

  
  #Aggregate income data for the model
  inc_exp_categ <- read_excel(here("data/Open Budget category for model.xlsx"))
  
  inc_categ <- inc_exp_categ |>
    filter(CATEG == "INC")
  
  exp_categ <- inc_exp_categ |>
    filter(CATEG == "EXP")
  
  inc_m <- data_l$INCOMES |>
    mutate(TYPE = cut(COD_INCO, 
                      breaks = c(0, inc_categ$BREAK_END),
                      labels = c(inc_categ$NAME_TYPE))
    )
  
  
  #Aggregate final income data for the model including adjustment for capital grants
  inc_m <- inc_m |>
    mutate(TYPE = replace(TYPE, COD_INCO %in% adj_cat, "Capital grants")) |> #the budget code was from the transfers table
    reshape_table(date, TYPE) |> 
    mutate(CAT = "Income", .before=1)
  
  cap_grants <- inc_m|>
    filter(TYPE=="Capital grants")
  
  #Aggregate expense data for the model
  exp_m <- data_l$`EXPENSES, ECONOMIC` |> 
    mutate(TYPE = cut(COD_CONS_EK, 
                      breaks = c(0, exp_categ$BREAK_END),
                      labels = c(exp_categ$NAME_TYPE))
    ) |> 
    reshape_table(date, TYPE) |> 
    mutate(CAT = "Expense", .before=1)|>
    mutate(across(where(is.numeric), ~.x*-1)) #change the sign of the inputs
  
  model_template <- rbind(inc_m, exp_m, fin, credit, cash) |>
    filter(!TYPE=="NA")

  
  #Add separate tab to output excel
  output <- append(output, list(SUMMARY_MODEL = model_template), after=0)

  
  return(output)
  
  
}
