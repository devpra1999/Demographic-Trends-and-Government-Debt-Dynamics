library("mFilter")
### Helper Functions----------------------------------------
get_country_code <- function(country) {
  if (country == "Germany") { return("DEU") }
  else if (country == "France") { return("FRA") }
  else if (country == "Italy") { return("ITA") }
  else if (country == "Netherlands") { return("NLD") }
  else if (country == "Spain") { return("ESP") }
  else if (country %in% c("United States", "United States of America")) { return("USA") }
  else {
    print("Enter a Valid Country")
    return(NULL)
  }
}

### Functions to make and save datasets------------------------------------------------------------------
make_macro_dataset <- function(country) {
  c <- get_country_code(country)
  if (is.null(c)) return()
  if (c=="USA"){
    api.key <- "6e430d537f672f3c96b4ba404c42949d"
    fred <- FredR(api.key)
    # Total GDP - nominal
    y_nom <- fred$series.observations(series_id = 'GDPA') %>% select(date,value)
    colnames(y_nom) <- c("Year","GDP")
    y_nom$Year <- year(y_nom$Year)
    y_nom$Country <- country
    y_nom$GDP <- as.numeric(y_nom$GDP)
    # Total GDP - real
    y_r <- fred$series.observations(series_id = 'GDPC1') %>% 
      select(date,value) %>%
      mutate(
        Country = country,
        Year = year(date),
        quarter = quarter(date)
      ) %>%
      filter(quarter == 4) %>%  # Keep only Q4
      select(Country,Year, value)
    colnames(y_r) <- c("Country","Year","GDP_r")
    y_r$GDP_r <- as.numeric(y_r$GDP_r)
    # Potential Output - real
    y_pot <- fred$series.observations(series_id = 'GDPPOT') %>% 
      select(date,value) %>%
      mutate(
        Country = country,
        Year = year(date),
        quarter = quarter(date)
      ) %>%
      filter(quarter == 4) %>%  # Keep only Q4
      select(Country,Year, value)
    colnames(y_pot) <- c("Country","Year","GDPPOT")
    y_pot$GDPPOT <- as.numeric(y_pot$GDPPOT)
  } else{
    # Total GDP - nominal - UVGD
    y_nom <- rdb(ids = paste0("AMECO/UVGD/", c, ".1.0.0.0.UVGD")) %>% select(Country, period, value)
    colnames(y_nom) <- c("Country", "Year", "GDP")
    # Total GDP - real - OVGD
    y_r <- rdb(ids = paste0("AMECO/OVGD/", c, ".1.1.0.0.OVGD")) %>% select(Country, period, value)
    colnames(y_r) <- c("Country", "Year", "GDP_r")
    # Potential Output - real - OVGDP
    y_pot <- rdb(ids = paste0("AMECO/OVGDP/", c, ".1.0.0.0.OVGDP")) %>% select(Country, period, value)
    colnames(y_pot) <- c("Country", "Year", "GDPPOT")
  }
  # Merge
  df <- merge(merge(y_nom, y_r), y_pot)
  if (c!="USA"){
    df$Year <- year(df$Year) 
  }
  # Inflation
  df$Deflator <- 100 * df$GDP / df$GDP_r
  df$pi <- log(df$Deflator) - log(lag(df$Deflator))
  # change sofia: Rebase US deflator to 2015 = 100
  if (c == "USA") {
    deflator_2015 <- df %>% filter(Year == 2015) %>% pull(Deflator)
    df <- df %>%
      mutate(
        Deflator = Deflator / deflator_2015 * 100,
        GDP_r = GDP / Deflator * 100
      )
  }
  # #Alternate GDP - From ECB
  # y_n <- read_csv(paste0("https://data-api.ecb.europa.eu/service/data/MNA/A.N.",substr(c,1,2),".W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.V.N?format=csvdata"))
  # y_n <- y_n %>% select(TIME_PERIOD,OBS_VALUE) %>% mutate(Country = country)
  # colnames(y_n)[1:2] <- c("Year","GDP_alt")
  # y_n$GDP_alt <- y_n$GDP_alt/1000
  # df <- merge(df,y_n, by = c("Country","Year"), all = TRUE)
  #Alternate GDP - From FRED
  if (c!="USA"){
    api.key <- "6e430d537f672f3c96b4ba404c42949d"
    fred <- FredR(api.key)
    tmp <- fred$series.observations(series_id = paste0(c,'GDPNADSMEI')) %>% select(date,value)
    colnames(tmp) <- c("Year","GDP_alt")
    tmp$Year <- year(tmp$Year)
    tmp$Country <- country
    tmp$GDP_alt <- as.numeric(tmp$GDP_alt)/(1000000000)
    df <- merge(df,tmp, by = c("Country","Year"), all = TRUE)
    #Alternate Deflator - From FRED
    tmp <- fred$series.observations(series_id = paste0(c,'GDPDEFAISMEI')) %>% select(date,value)
    colnames(tmp) <- c("Year","Deflator_alt")
    tmp$Year <- year(tmp$Year)
    tmp$Country <- country
    tmp$Deflator_alt <- as.numeric(tmp$Deflator_alt)
    df <- merge(df,tmp, by = c("Country","Year"), all = TRUE)
  }
  df$Country[df$Country %in% c("United States", "United States of America", "USA")] <- "US"
  filename <- paste0("data/main/", country, "/macros.csv")
  write_csv(df, filename)
  return(df)
}

make_revenue_dataset <- function(country) {
  c <- get_country_code(country)
  if (is.null(c)) return()
  # Fetch revenue components
  revenue_total <- rdb(ids = paste0("AMECO/URTG/", c, ".1.0.0.0.URTG")) %>% select(Country, period, value)
  colnames(revenue_total) <- c("Country", "Year", "Revenue_Total")
  tax_indirect <- rdb(ids = paste0("AMECO/UTVG/", c, ".1.0.0.0.UTVG")) %>% select(Country, period, value)
  colnames(tax_indirect) <- c("Country", "Year", "Revenue_Taxes_Indirect")
  tax_direct <- rdb(ids = paste0("AMECO/UTYG/", c, ".1.0.0.0.UTYG")) %>% select(Country, period, value)
  colnames(tax_direct) <- c("Country", "Year", "Revenue_Taxes_Direct")
  social_contribution <- rdb(ids = paste0("AMECO/UTSG/", c, ".1.0.0.0.UTSG")) %>% select(Country, period, value)
  colnames(social_contribution) <- c("Country", "Year", "Revenue_Social_Contribution")
  capital_transfers <- rdb(ids = paste0("AMECO/UKTTG/", c, ".1.0.0.0.UKTTG")) %>% select(Country, period, value)
  colnames(capital_transfers) <- c("Country", "Year", "Revenue_Capital_Transfers")
  others_current <- rdb(ids = paste0("AMECO/UTOG/", c, ".1.0.0.0.UTOG")) %>% select(Country, period, value)
  colnames(others_current) <- c("Country", "Year", "Revenue_Others")
  #Merge revenue components
  df <- purrr::reduce(list(revenue_total, tax_indirect, tax_direct, social_contribution, capital_transfers, others_current), 
               left_join, by = c("Country", "Year"))
  filename <- paste0("data/main/", country, "/Revenue.csv")
  write_csv(df, filename)
  return(df)
}

make_expenditure_dataset <- function(country) {
  c <- get_country_code(country)
  if (is.null(c)) return()
  # Fetch expenditure components
  exp_total <- rdb(ids = paste0("AMECO/UUTG/", c, ".1.0.0.0.UUTG")) %>% select(Country, period, value)
  colnames(exp_total) <- c("Country", "Year", "Expenditure_Total")
  interest <- rdb(ids = paste0("AMECO/UYIG/", c, ".1.0.0.0.UYIG")) %>% select(Country, period, value)
  colnames(interest) <- c("Country", "Year", "Expenditure_Interest")
  subsidies <- rdb(ids = paste0("AMECO/UYVG/", c, ".1.0.0.0.UYVG")) %>% select(Country, period, value)
  colnames(subsidies) <- c("Country", "Year", "Expenditure_Subsidies")
  social_households <- rdb(ids = paste0("AMECO/UYTGH/", c, ".1.0.0.0.UYTGH")) %>% select(Country, period, value)
  colnames(social_households) <- c("Country", "Year", "Expenditure_Social_Households")
  social_markets <- rdb(ids = paste0("AMECO/UYTGM/", c, ".1.0.0.0.UYTGM")) %>% select(Country, period, value)
  colnames(social_markets) <- c("Country", "Year", "Expenditure_Social_Markets")
  consumption <- rdb(ids = paste0("AMECO/UCTGI/", c, ".1.0.0.0.UCTGI")) %>% select(Country, period, value)
  colnames(consumption) <- c("Country", "Year", "Expenditure_Consumption_Intmd")
  compensation <- rdb(ids = paste0("AMECO/UWCG/", c, ".1.0.0.0.UWCG")) %>% select(Country, period, value)
  colnames(compensation) <- c("Country", "Year", "Expenditure_Compensation_Employees")
  others_current <- rdb(ids = paste0("AMECO/UUOG/", c, ".1.0.0.0.UUOG")) %>% select(Country, period, value)
  colnames(others_current) <- c("Country", "Year", "Expenditure_Other_Curr")
  cap_formation <- rdb(ids = paste0("AMECO/UIGG0/", c, ".1.0.0.0.UIGG0")) %>% select(Country, period, value)
  colnames(cap_formation) <- c("Country", "Year", "Expenditure_Capital_Formation")
  cap_others <- rdb(ids = paste0("AMECO/UKOG/", c, ".1.0.0.0.UKOG")) %>% select(Country, period, value)
  colnames(cap_others) <- c("Country", "Year", "Expenditure_Capital_Exp_Others")
  #Merge expenditure components
  df <- purrr::reduce(list(exp_total, interest, subsidies, social_households, social_markets, consumption, compensation, others_current, cap_formation, cap_others), 
               left_join, by = c("Country", "Year"))
  filename <- paste0("data/main/", country, "/Expenditure.csv")
  write_csv(df, filename)
  return(df)
}

make_deficit_dataset <- function(country) {
  c <- get_country_code(country)
  if (is.null(c)) return()
  # Load existing revenue and expenditure data
  revenue <- read_csv(paste0("data/main/", country, "/Revenue.csv"), show_col_types = FALSE)
  expenditure <- read_csv(paste0("data/main/", country, "/Expenditure.csv"), show_col_types = FALSE)
  #Add other variables
  total_debt <- rdb(ids = paste0("AMECO/UDGG/", c, ".1.0.0.0.UDGG")) %>% select(Country, period, value)
  colnames(total_debt) <- c("Country", "Year", "Total_Debt")
  sfa <- rdb(ids = paste0("AMECO/UDGGS/", c, ".1.0.0.0.UDGGS")) %>% select(Country, period, value)
  colnames(sfa) <- c("Country", "Year", "SFA")
  ir <- rdb(ids = paste0("AMECO/AYIGD/", c, ".1.0.0.0.AYIGD")) %>% select(Country, period, value)
  colnames(ir) <- c("Country", "Year", "IR")
  # Merge datasets
  df <- purrr::reduce(list(revenue[, 1:3], expenditure[, 1:4], total_debt, sfa, ir), 
               left_join, by = c("Country", "Year")) %>%
    mutate(
      Surplus = Revenue_Total - Expenditure_Total,
      Primary_Surplus = Revenue_Total - Expenditure_Total + Expenditure_Interest
    )
  filename <- paste0("data/main/", country, "/Debt.csv")
  write_csv(df, filename)
  return(df)
}

### Function to Create Historical Datasets (Revenue and Expenditure) ------------
create_historical_data <- function(country) {
  c <- get_country_code(country)
  if (is.null(c)) return()
  #Get OECD Economic Outlook data
  url <- paste0("https://sdmx.oecd.org/public/rest/data/OECD.ECO.MAD,DSD_EO@DF_EO,1.2/",c,".YRGT+YPGX+YPG+YRGX+YRG+CFKG+CGAA+IGAA+NLG+GGINTR+GGFL+GGFLM+CAPOG+SAVG+TOCR+YPOTG+YPEPG+YPERG+SSPG+SSRG+TAXQ+TIND+TY+YPGT+TYB+TKPG+TKTRG+NLGX+GNINTP+GGINTP+GDPV_ANNPCT.A?startPeriod=1961&endPeriod=2024&format=csvfilewithlabels")
  #change sofia: there were leading spaces that caused issues reading the data. in general sometimes the url goes and other times it's problematic
  df_hist <- read.csv(url) %>%
    select(Reference.area, MEASURE, TIME_PERIOD, OBS_VALUE) #sofia change: reference.area did not exist, I put REF_AREA
  df_hist$OBS_VALUE <- df_hist$OBS_VALUE/1e9
  #Map revenue, expenditure and deficit measures between the OECD and AMECO datasets
  #sofia change - was reading local, changed read.csv to read_csv and now works for me
  revenue_measures <- c(
    "YRGT" = "Revenue_Total",
    "TIND" = "Revenue_Taxes_Indirect",
    "TY" = "Revenue_Taxes_Direct",
    "SSRG" = "Revenue_Social_Contribution",
    "TKTRG" = "Revenue_Capital_Transfers",
    "YPERG" = "YPERG",  # To be combined with TOCR
    "TOCR" = "TOCR"      # To calculate Revenue_Others
  )
  expenditure_measures <- c(
    "YPGT" = "Expenditure_Total",
    "GGINTP" = "Expenditure_Interest",
    "SSPG" = "Expenditure_Social_Households"
  )
  deficit_measures <- c(
    "YRGT" = "Revenue_Total",
    "YPGT" = "Expenditure_Total",
    "GGINTP" = "Expenditure_Interest",
    "GGFL" = "Total_Debt"
    )
  #Create historical revenue dataset
  df_rev <- df_hist %>%
    filter(MEASURE %in% names(revenue_measures)) %>%
    mutate(MEASURE = dplyr::recode(MEASURE, !!!revenue_measures)) %>%
    pivot_wider(
      names_from = MEASURE,
      values_from = OBS_VALUE,
      id_cols = c(Reference.area, TIME_PERIOD)
    )
  if ("YPERG" %in% colnames(df_rev) && "TOCR" %in% colnames(df_rev)) {
    df_rev <- df_rev %>%
      mutate(Revenue_Others = YPERG + TOCR) %>%
      select(-YPERG, -TOCR)
  }
  df_rev_final <- df_rev %>%
    rename(Country = Reference.area, Year = TIME_PERIOD) %>%
    arrange(Country, Year)
  df_rev_final$Country[df_rev_final$Country %in% c("United States", "United States of America", "USA")] <- "US"
  write_csv(df_rev_final, paste0("data/main/", country, "/Historical_Revenue.csv"))
  #Create historical expenditure dataset
  df_exp <- df_hist %>%
    filter(MEASURE %in% names(expenditure_measures)) %>%
    mutate(MEASURE = dplyr::recode(MEASURE, !!!expenditure_measures)) %>%
    pivot_wider(
      names_from = MEASURE,
      values_from = OBS_VALUE,
      id_cols = c(Reference.area, TIME_PERIOD)
    ) %>%
    rename(Country = Reference.area, Year = TIME_PERIOD) %>%
    arrange(Country, Year)
  df_exp$Country[df_exp$Country %in% c("United States", "United States of America", "USA")] <- "US"
  write_csv(df_exp, paste0("data/main/", country, "/Historical_Expenditure.csv"))
  #Create historical deficit dataset
  df_deficit <- df_hist %>%
    filter(MEASURE %in% names(deficit_measures)) %>%
    mutate(MEASURE = dplyr::recode(MEASURE, !!!deficit_measures)) %>%
    pivot_wider(
      names_from = MEASURE,
      values_from = OBS_VALUE,
      id_cols = c(Reference.area, TIME_PERIOD)
    ) %>%
    rename(Country = Reference.area, Year = TIME_PERIOD) %>%
    arrange(Country, Year)
  df_deficit <- df_deficit  %>%
    mutate(
      SFA = Total_Debt - lag(Total_Debt) + Revenue_Total - Expenditure_Total,
      IR = (Expenditure_Interest/lag(Total_Debt)) * 100,
      Surplus = Revenue_Total - Expenditure_Total,
      Primary_Surplus = Revenue_Total - Expenditure_Total + Expenditure_Interest
    )
  df_deficit$Country[df_deficit$Country %in% c("United States", "United States of America", "USA")] <- "US"
  write_csv(df_deficit, paste0("data/main/", country, "/Historical_Debt.csv"))
  return(list(df_rev_final,df_exp,df_deficit))
}


###Validation Functions ------------------------------------------------------

#Validate sum of revenue components is equal to total revenue
validate_revenue <- function(country, diff_threshold = 1e-3) {
  filename <- paste0("data/main/", country, "/Revenue.csv")
  if (!file.exists(filename)) {
    stop(paste("Revenue dataset for", country, "not found."))
  }
  revenue <- read_csv(filename, show_col_types = FALSE)
  validation <- revenue %>%
    mutate(Sum_Components = Revenue_Taxes_Indirect + Revenue_Taxes_Direct + Revenue_Social_Contribution + Revenue_Capital_Transfers + Revenue_Others) %>%
    select(Country, Year, Revenue_Total, Sum_Components) %>%
    filter(abs(Revenue_Total - Sum_Components) > diff_threshold) # Allow for minor floating-point differences
  if (nrow(validation) == 0) {
    print(paste("Revenue validation for", country, "passed: All rows match. Sum of revenue components is equal to total revenue"))
  } else {
    print(paste("Revenue validation for", country, "failed: Mismatch in the following rows:"))
    print(validation)
  }
}

#Validate sum of expenditure components is equal to total expenditure
validate_expenditure <- function(country, diff_threshold = 1e-3) {
  filename <- paste0("data/main/", country, "/Expenditure.csv")
  if (!file.exists(filename)) {
    stop(paste("Expenditure dataset for", country, "not found."))
  }
  expenditure <- read_csv(filename, show_col_types = FALSE)
  validation <- expenditure %>%
    mutate(Sum_Components = Expenditure_Interest + Expenditure_Subsidies + Expenditure_Social_Households + Expenditure_Social_Markets + Expenditure_Consumption_Intmd + Expenditure_Compensation_Employees + Expenditure_Other_Curr + Expenditure_Capital_Formation + Expenditure_Capital_Exp_Others) %>%
    select(Country, Year, Expenditure_Total, Sum_Components) %>%
    filter(abs(Expenditure_Total - Sum_Components) > diff_threshold) # Allow for minor floating-point differences
  if (nrow(validation) == 0) {
    print(paste("Expenditure validation for", country, "passed: All rows match. Sum of expenditure components is equal to total expenditure"))
  } else {
    print(paste("Expenditure validation for", country, "failed: Mismatch in the following rows:"))
    print(validation)
  }
}

#Validate deficits using debt simulations
validate_deficit <- function(country, diff_threshold = 0.1) {
  filename <- paste0("data/main/", country, "/Debt.csv")
  if (!file.exists(filename)) {
    stop(paste("Deficit dataset for", country, "not found."))
  }
  deficit <- read_csv(filename, show_col_types = FALSE)
  validation <- deficit %>%
    mutate(Implied_Debt = lag(Total_Debt) - Surplus + SFA) %>%
    select(Country, Year, Total_Debt, Implied_Debt) %>%
    filter(abs(Total_Debt - Implied_Debt) > diff_threshold) # Allow for minor floating-point differences
  if (nrow(validation) == 0) {
    print(paste("Deficit validation for", country, "passed: All rows match. Total Debt is equal to simulation implied debt"))
  } else {
    print(paste("Deficit validation for", country, "failed: Mismatch in the following rows:"))
    print(validation)
  }
}

### Validation Function for Historical vs. Main Datasets ----------------------
validate_historical_vs_main <- function(country) {
  main_rev <- read_csv(paste0("data/main/", country, "/Revenue.csv"), show_col_types = FALSE) %>% filter(year(Year) >= 1995, year(Year) <= 2000)
  main_exp <- read_csv(paste0("data/main/", country, "/Expenditure.csv"), show_col_types = FALSE) %>% filter(year(Year) >= 1995, year(Year) <= 2000)
  main_deficit <- read_csv(paste0("data/main/", country, "/Debt.csv"), show_col_types = FALSE) %>% filter(year(Year) >= 1995, year(Year) <= 2000)
  hist_rev <- read_csv(paste0("data/main/", country, "/Historical_Revenue.csv"), show_col_types = FALSE) %>% filter(Year >= 1995, Year <= 2000)
  hist_exp <- read_csv(paste0("data/main/", country, "/Historical_Expenditure.csv"), show_col_types = FALSE) %>% filter(Year >= 1995, Year <= 2000)
  hist_deficit <- read_csv(paste0("data/main/", country, "/Historical_Debt.csv"), show_col_types = FALSE) %>% filter(Year >= 1995, Year <= 2000)
  
  rev_cols <- intersect(names(main_rev), names(hist_rev))
  rev_cols <- rev_cols[-1:-2]
  rev_diff <- main_rev[,rev_cols]  - hist_rev[,rev_cols]
  print("The Revenue difference between AMECO and historical dataset is:")
  print(cbind(hist_rev[,1:2],rev_diff))
  
  exp_cols <- intersect(names(main_exp), names(hist_exp))
  exp_cols <- exp_cols[-1:-2]
  exp_diff <- main_exp[,exp_cols] - hist_exp[,exp_cols]
  print("The Expenditure difference between AMECO and historical dataset is:")
  print(cbind(hist_exp[,1:2],exp_diff))
  
  deficit_cols <- intersect(names(main_deficit), names(hist_deficit))
  deficit_cols <- deficit_cols[-1:-2]
  deficit_diff <- main_deficit[,deficit_cols] - hist_deficit[,deficit_cols]
  print("The Debt Dynamics difference between AMECO and historical dataset is:")
  print(cbind(hist_deficit[,1:2],deficit_diff))
}



make_potential_gdp <- function(real_gdp) {
  if (all(is.na(real_gdp))) {
    return(real_gdp)
  }
  # Store original NA positions (leading and trailing)
  non_na <- which(!is.na(real_gdp))
  first_non_na <- min(non_na)
  last_non_na <- max(non_na)
  # Extract the non-NA portion for HP filter
  gdp_vals <- real_gdp[first_non_na:last_non_na]
  # Apply HP filter (using default lambda for quarterly data)
  hp_result <- mFilter::hpfilter(gdp_vals, freq = 1600, type = "lambda")
  # Create output vector with same length as input
  output <- rep(NA, length(real_gdp))
  output[first_non_na:last_non_na] <- hp_result$trend
  return(output)
}


