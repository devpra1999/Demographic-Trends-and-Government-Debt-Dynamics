library(dplyr)
library(readr)
library(pracma)
library(panelr)

prepare_demographics_data <- function(country_name){
  ### Read files------------------------------------------------------------------
  # Read main data file
  df <- read_csv("data/all_data.csv") %>% 
    filter(Location == country_name, Time <= 2100) %>%
    select(Location, Time, CBR, CDR, NetMigrations, CNMR, TPopulation1July,TFR)
  colnames(df)[1:2] <- c("Country", "Year")
  
  # Smooth net migrations
  df$NetMigrations_smooth <- pracma::hampel(df$NetMigrations, k = 3, t0 = 3)$y
  
  # Add population dependency ratio data to df
  df_dr <- read_csv("data/raw/dependency_ratio_estimates_big5.csv")
  df_dr_proj <- read_csv("data/raw/dependency_ratio_projections_big5.csv")
  df_dr <- rbind(df_dr,df_dr_proj)
  rm(df_dr_proj)
  df_dr <- df_dr %>% 
    filter(Country == country_name) %>%
    select(Country,Year,`Annual total dep. ratio [(0-19 & 65+) / 20-64] (%)`,`Annual child dep. ratio [0-19 / 20-64] (%)`,`Annual old-age dep. ratio [65+ / 20-64] (%)`,
           `Annual total dep. ratio [(0-19 & 70+) / 20-69] (%)`,`Annual child dep. ratio [0-19 / 20-69] (%)`,`Annual old-age dep. ratio [70+ / 20-69] (%)`)
  colnames(df_dr) <- c("Country","Year","D_UN","D_Y_UN","D_R_UN","D_UN_alt","D_Y_UN_alt","D_R_UN_alt")
  df <- merge(df,df_dr,by = c("Country","Year"))
  
  # Read population data
  df_pop <- read_csv("data/raw/pop_estimates_big5.csv")
  df_pop_proj <- read_csv("data/raw/pop_projections_big5.csv")
  df_pop <- rbind(df_pop, df_pop_proj)
  
  # Filter for selected country (age-wise population)
  df_pop <- df_pop %>% filter(Country == country_name)
  
  # Read mortality data
  df_mort <- read_csv("data/raw/mortality_by_age_estimates_big5.csv")
  df_mort_proj <- read_csv("data/raw/mortality_by_age_projections_big5.csv")
  df_mort <- rbind(df_mort, df_mort_proj)
  
  # Filter for selected country (age-wise mortality)
  df_mort <- df_mort %>% 
    filter(Country == country_name) %>% 
    select(Country, Year, '0-19', '20+', '40+', '50+', '65+')
  
  # Calculate additional age groups
  df_mort$'45+' <- (df_mort$`40+` + df_mort$`50+`) / 2
  df_mort$'20-44' <- df_mort$`20+` - df_mort$`45+`
  df_mort$'45-64' <- df_mort$`45+` - df_mort$`65+`
  
  ### Set up variables-----------------------------------------------------------
  # Population by age groups
  df$N_Y <- apply(df_pop[3:22], 1, sum)       # Young
  df$N_M1 <- apply(df_pop[23:47], 1, sum)     # Middle-aged 1
  df$N_M2 <- apply(df_pop[48:67], 1, sum)     # Middle-aged 2
  df$N_R <- apply(df_pop[68:ncol(df_pop)], 1, sum)  # Retired
  
  # Mortality by age groups
  df$FAT_Y <- df_mort$`0-19`
  df$FAT_M1 <- df_mort$`20-44`
  df$FAT_M2 <- df_mort$`45-64`
  df$FAT_R <- df_mort$`65+`
  
  # Mortality rates
  df$mu_Y <- df$FAT_Y / lag(df$N_Y)
  df$mu_M1 <- df$FAT_M1 / lag(df$N_M1)
  df$mu_M2 <- df$FAT_M2 / lag(df$N_M2)
  df$mu_R <- df$FAT_R / lag(df$N_R)
  df$MY <- df$N_M2/df$N_M1
  return(df)
}

get_calibrated_data <- function(df) {
  ### Calibration----------------------------------------------------------------
  df <- df %>%
    mutate(
      nu_t = CBR / 1000,
      N = N_Y + N_M1 + N_M2 + N_R,
      p_y = (lag(N_Y) + nu_t * lag(N) - FAT_Y - N_Y) / lag(N_Y),
      p_m1 = (lag(N_M1) + p_y * lag(N_Y) + NetMigrations - FAT_M1 - N_M1) / lag(N_M1),
      p_m2 = (lag(N_M2) + p_m1 * lag(N_M1) - FAT_M2 - N_M2) / lag(N_M2)
    )
  return(df)
}

validate_calibrated_data <- function(df){
  df %>%
    mutate(
      u_t = N_R - (lag(N_R) + p_m2 * lag(N_M2) - FAT_R)
    )
}


###Extra --------------------------------------------------------------------
# df_IT <- df_IT %>%
#   mutate(
#     nu_t = CBR/1000,
#     N = N_Y + N_M1 + N_M2 + N_R,
#     p_y = (lag(N_Y) + nu_t*lag(N) - mu_Y*lag(N_Y) - N_Y) / lag(N_Y),
#     p_m1 = (lag(N_M1) + p_y*lag(N_Y) + NetMigrations - mu_M*lag(N_M1) - N_M1) / lag(N_M1),
#     p_m2 = (lag(N_M2) + p_m1*lag(N_M1) - mu_M*lag(N_M2) - N_M2)/ lag(N_M2)
#   )

# validation_results <- df_IT %>%
#   mutate(
#     mu_t_observed = mu_R,
#     mu_t_implied = (lag(N_R) + p_m2 * lag(N_M2) - N_R) / lag(N_R)
#   ) %>%
#   select(Year, mu_t_observed, mu_t_implied) %>%
#   filter(!is.na(mu_t_observed) & !is.na(mu_t_implied))

# plot(df_IT$Year,df_IT$mu_Y, col = "black", type = "l", lwd = 2, ylim = c(0,0.1), ylab = "mu", xlab = "Year", main = "Mortality Rates by Age Group")
# lines(df_IT$Year, df_IT$mu_M, col = "blue", lwd = 2)
# lines(df_IT$Year, df_IT$mu_R, col = "red", lwd = 2)
# lines(validation_results$Year,validation_results$mu_t_implied, col = "red", lty = "dotted", lwd = 2)

# mu_y_t <- df_IT$mu_Y[curr_idx:end_idx]
# mu_m1_t <- df_IT$mu_M[curr_idx:end_idx]
# mu_m2_t <- df_IT$mu_M[curr_idx:end_idx]
# mu_r_t <- df_IT$mu_R[curr_idx:end_idx]