rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
### Packages-------------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(purrr)
#install.packages("rdbnomics")
library(rdbnomics)
#vignette(rdbnomics)

### Functions------------------------------------------------------------------
source("code/aux_funs_make_db.R")

### Generate Datasets for All Countries ----------------------------------------
countries <- c("Germany", "France", "Italy", "Spain", "Netherlands")
# Generate macro datasets
L_macro <- lapply(countries, make_macro_dataset)
# Generate revenue datasets
L_rev <- lapply(countries, make_revenue_dataset)
# Generate expenditure datasets
L_exp <- lapply(countries, make_expenditure_dataset)
# Generate deficit datasets
L_deficit <- lapply(countries, make_deficit_dataset)

### Validation ---------------------------------------------------------------------
#Check sum
validate_revenue("Italy", diff_threshold = 0.1)
validate_expenditure("Germany", diff_threshold = 0.1)
validate_deficit("France", diff_threshold = 0.1)

### Make historical datasets from OECD----------------------------------------------------------------------
L_historical <- lapply(countries,create_historical_data)

###  Validation for historical (OECD) vs ameco -------------------------------------------------------------
validate_historical_vs_main("Italy")
validate_historical_vs_main("France")
validate_historical_vs_main("Germany")
validate_historical_vs_main("Spain")
validate_historical_vs_main("Netherlands")

### Merge dataframes and save--------------------------------------------------------
df_macro <- bind_rows(L_macro)
write_csv(df_macro,"data/main/Macros_big5.csv")
df_rev <- bind_rows(L_rev)
write_csv(df_rev,"data/main/Revenue_big5.csv")
df_exp <- bind_rows(L_exp)
write_csv(df_exp,"data/main/Expenditure_big5.csv")
df_deficit <- bind_rows(L_deficit)
write_csv(df_deficit,"data/main/Debt_big5.csv")

df_rev_hist <- rbind(L_historical[[1]][[1]],L_historical[[2]][[1]],L_historical[[3]][[1]],L_historical[[4]][[1]],L_historical[[5]][[1]])
write_csv(df_rev_hist,"data/main/Historical_Revenue_big5.csv")
df_exp_hist <- rbind(L_historical[[1]][[2]],L_historical[[2]][[2]],L_historical[[3]][[2]],L_historical[[4]][[2]],L_historical[[5]][[2]])
write_csv(df_exp_hist,"data/main/Historical_Expenditure_big5.csv")
df_deficit_hist <- rbind(L_historical[[1]][[3]],L_historical[[2]][[3]],L_historical[[3]][[3]],L_historical[[4]][[3]],L_historical[[5]][[3]])
write_csv(df_deficit_hist,"data/main/Historical_Debt_big5.csv")
