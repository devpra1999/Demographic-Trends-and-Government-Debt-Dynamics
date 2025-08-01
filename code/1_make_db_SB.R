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
library(FredR)
#vignette(rdbnomics)

# %>%
#   mutate(Country = ifelse(Country == "United States of America", "United States", Country))

### Set up demorgraphic databases-----------------------------------------------
country_list <- c("Germany","France","Italy","Spain","Netherlands","United States of America")
### Demographics Estimates-----------------------------------------------------
filename = "data/pop_estimates.xlsx"
df = read_xlsx(filename,sheet = "Estimates", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% 
  filter(Country %in% country_list)
write_csv(df,"data/raw/pop_estimates_big5.csv")

### Demographics Projections----------------------------------------------------
filename = "data/pop_estimates.xlsx"
df = read_xlsx(filename,sheet = "Medium variant", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% 
  filter(Country %in% country_list)
write_csv(df,"data/raw/pop_projections_big5.csv")

### Dependency Estimates------------------------------------------------------
filename = "data/dependency_ratio.xlsx"
df = read_xlsx(filename,sheet = "Estimates", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% 
  filter(Country %in% country_list)
write_csv(df,"data/raw/dependency_ratio_estimates_big5.csv")

### Dependency Projections------------------------------------------------------
filename = "data/dependency_ratio.xlsx"
df = read_xlsx(filename,sheet = "Medium variant", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% 
  filter(Country %in% country_list)
write_csv(df,"data/raw/dependency_ratio_projections_big5.csv")

### Mortality (by age) Estimates---------------------------------------------------
filename = "data/mortality_by_age.xlsx"
df = read_xlsx(filename,sheet = "Estimates", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% 
  filter(Country %in% country_list)
write_csv(df,"data/raw/mortality_by_age_estimates_big5.csv")

### Mortality (by age) Projections--------------------------------------------------
filename = "data/mortality_by_age.xlsx"
df = read_xlsx(filename,sheet = "Medium variant", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% 
  filter(Country %in% country_list)
write_csv(df,"data/raw/mortality_by_age_projections_big5.csv")



### Functions for macro dbs------------------------------------------------------------------
source("code/1_aux_funs_make_db_SB.R")

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
countries <- c("Germany", "France", "Italy", "Spain", "Netherlands", "United States")
L_historical <- lapply(countries,create_historical_data)
L_macro <- lapply(countries, make_macro_dataset)

### change sofia: Check value of real and nominal GDP in FRED to find base year----------------------------------------------------------------------
# df_US <- make_macro_dataset("United States")
# #view(df_US) #deflator never 100, but closest to 100 in 2017, which lines up with FRED documentation
# df_US %>% filter(Year == 2017) %>% select(Country, Year, GDP, GDP_r, Deflator)
# 
# df_fr <- read_csv("data/main/France/macros.csv")
# #view(df_fr) #base year is 2015
# 
# deflator_2015 <- df_US %>%
#   filter(Year == 2015) %>%
#   pull(Deflator)
# df_US <- df_US %>%
#   mutate(
#     Deflator_rebased = Deflator / deflator_2015 * 100,
#     GDP_r_rebased = GDP / Deflator_rebased * 100  # real GDP with 2015 base year
#   )

###  Validation for historical (OECD) vs ameco -------------------------------------------------------------
validate_historical_vs_main("Italy")
validate_historical_vs_main("France")
validate_historical_vs_main("Germany")
validate_historical_vs_main("Spain")
validate_historical_vs_main("Netherlands")

### Merge dataframes and save--------------------------------------------------------
df_macro <- bind_rows(L_macro)

#Construct potential output from GDP_alt
df_macro$GDP_r_alt <- df_macro$GDP_alt*100/df_macro$Deflator_alt
df_macro <- df_macro %>%
  group_by(Country) %>%
  mutate(ygap = log(GDP_r) - log(GDPPOT)) %>%
  mutate(GDPPOT_alt = make_potential_gdp(GDP_r_alt),
         ygap_alt = log(GDP_r_alt) - log(GDPPOT_alt)) %>%
  ungroup()
# Create comparison plots
ggplot(df_macro %>% filter(Country == "Germany"), aes(x = Year)) +
  geom_line(aes(y = ygap, color = "OECD Output Gap"), linewidth = 1) +
  geom_line(aes(y = ygap_alt, color = "FRED (HP-filter) Output Gap"), linewidth = 1, linetype = "dashed") +
  labs(
    title = "Comparison of Output Gaps - Germany",
    x = "Year",
    y = "Output Gap",
    color = "Variable"  # Legend title
  ) +
  scale_color_manual(
    values = c("OECD Output Gap" = "blue", "FRED (HP-filter) Output Gap" = "red")  # Explicit color mapping
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
ggplot(df_macro %>% filter(Country == "Germany"), aes(x = Year)) +
  geom_line(aes(y = log(GDP_r), color = "OECD Real GDP"), linewidth = 1) +
  geom_line(aes(y = log(GDP_r_alt), color = "FRED Real GDP"), linewidth = 1, linetype = "dashed") +
  labs(
    title = "Comparison of Real GDP - Germany",
    x = "Year",
    y = "Log Real GDP",
    color = "Variable"  # Legend title
  ) +
  scale_color_manual(
    values = c("OECD Real GDP" = "blue", "FRED Real GDP" = "red")  # Explicit color mapping
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
#Merge alternate and main gdp for Germany
df_macro$GDP[df_macro$Country == "Germany" & is.na(df_macro$GDP)] <- df_macro$GDP_alt[df_macro$Country == "Germany" & is.na(df_macro$GDP)]
df_macro$GDPPOT[df_macro$Country == "Germany" & is.na(df_macro$GDPPOT)] <- df_macro$GDPPOT_alt[df_macro$Country == "Germany" & is.na(df_macro$GDPPOT)]
df_macro$Deflator[df_macro$Country == "Germany" & is.na(df_macro$Deflator)] <- df_macro$Deflator_alt[df_macro$Country == "Germany" & is.na(df_macro$Deflator)]
df_macro$GDP_r <- df_macro$GDP*100/df_macro$Deflator
df_macro$pi <- log(df_macro$Deflator) - log(dplyr::lag(df_macro$Deflator))
write_csv(df_macro,"data/main/Macros_big5.csv")


df_rev <- bind_rows(L_rev)
write_csv(df_rev,"data/main/Revenue_big5.csv")
df_exp <- bind_rows(L_exp)
write_csv(df_exp,"data/main/Expenditure_big5.csv")
df_deficit <- bind_rows(L_deficit)
write_csv(df_deficit,"data/main/Debt_big5.csv")

df_rev_hist <- rbind(L_historical[[1]][[1]],L_historical[[2]][[1]],L_historical[[3]][[1]],L_historical[[4]][[1]],L_historical[[5]][[1]],L_historical[[6]][[1]])
write_csv(df_rev_hist,"data/main/Historical_Revenue_big5.csv")
df_exp_hist <- rbind(L_historical[[1]][[2]],L_historical[[2]][[2]],L_historical[[3]][[2]],L_historical[[4]][[2]],L_historical[[5]][[2]],L_historical[[6]][[2]])
write_csv(df_exp_hist,"data/main/Historical_Expenditure_big5.csv")
df_deficit_hist <- rbind(L_historical[[1]][[3]],L_historical[[2]][[3]],L_historical[[3]][[3]],L_historical[[4]][[3]],L_historical[[5]][[3]],L_historical[[6]][[3]])
write_csv(df_deficit_hist,"data/main/Historical_Debt_big5.csv")


### Make trade weights--------------------------------------------------------------
process_tradeflows <- function(code) {
  df <- read_xls("data/raw/tradeflows.xls",sheet = code)
  colnames(df)[1:2] <- c("Year","Code")
  df <- df %>% select(Year,Code,'132','134','136','138','184')
  df_clean <- df %>%
    mutate(across(where(is.character), ~as.numeric(.))) %>%
    replace(is.na(.), 0)
  df_normalized <- df_clean %>%
    rowwise() %>%
    mutate(across(-c(Year, Code), ~ . / sum(c_across(-c(Year, Code)), na.rm = TRUE))) %>%
    ungroup()
  
  return(df_normalized)
}

fr <- process_tradeflows("132")
de <- process_tradeflows("134")
it <- process_tradeflows("136")
nl <- process_tradeflows("138")
es <- process_tradeflows("184")

W <- bind_rows(fr,de,it,nl,es) %>% arrange(Year) %>% select(-Code)
W_avg <- W %>%
  mutate(row = rep(1:5, times = length(unique(Year)))) %>%
  group_by(row) %>%
  summarise(across(`132`:`184`, ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  select(`132`, `134`, `136`, `138`, `184`) %>%
  as.matrix()
rownames(W_avg) <- c("FR", "DE", "IT", "NL", "ES")
colnames(W_avg) <- c("FR", "DE", "IT", "NL", "ES")
save(W, W_avg, file = "data/main/trade_weights.rda")
