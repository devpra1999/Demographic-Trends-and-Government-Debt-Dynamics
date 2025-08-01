rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
### Packages-------------------------------------------------------------------
library(dplyr)
library(readr)
library(pracma)
library(panelr)
library(ggplot2)
library(purrr)
library(lubridate)
library(plm)
library(xts)
library(systemfit)
library(RColorBrewer)
library(tidyr)
library(stringr)
library(restriktor)
library(tseries)
#Aux funs----------------------------------------------------------------------
source("code/4_aux_funs_estimation.R")
country_map <- c("US" = "US", "Germany" = "DE", "Netherlands" = "NL", "France" = "FR",
                 "Spain" = "ES", "Italy" = "IT")
### Make dataset--------------------------------------------------------------
df_macro <- read_csv("data/main/Macros_big5.csv")
FRED_data <- read.delim("data/raw/HISTDATA.TXT", sep = ",")
FRED_data$DATE <- seq(as.Date("1968-04-01"),length=nrow(FRED_data), by="quarters")
PTR_US <- FRED_data %>% select(DATE, PTR)
rm(FRED_data)
PTR_US$Year <- format(PTR_US$DATE, "%Y")
# Calculate annual averages
annual_PTR <- aggregate(PTR ~ Year, data = PTR_US, FUN = mean)
annual_PTR$Country <- "United States"
df_macro <- merge(df_macro,annual_PTR,all.x = TRUE)

df_demographics <- read_csv("data/main/Demographics_big5.csv")
#df_debt <- read_csv("data/main/Debt_big5.csv")
df_debt_historical <- read_csv("data/main/Historical_Debt_big5.csv")
df <- merge(df_macro,df_demographics, by = c("Country","Year"), all = TRUE)
#Transform dataframe to make required variables
df <- df %>%
  mutate(Country = str_replace(Country, "United States", "US")) %>%
  group_by(Country) %>% 
  mutate(
    MY = N_M2/N_M1,
    YM = N_M1/N_M2,
    D = (N_Y+N_R)/(N_M1+N_M2),
    D_lag = dplyr::lag(D),
    dD = (D - D_lag)/D_lag,
    D_Y = (N_Y)/(N_M1+N_M2),
    D_Y_lag = dplyr::lag(D_Y),
    dD_Y = (D_Y - D_Y_lag)/D_Y_lag,
    D_R = (N_R)/(N_M1+N_M2),
    D_R_lag = dplyr::lag(D_R),
    dD_R = (D_R - D_R_lag)/D_R_lag,
    GDP_lag = dplyr::lag(GDP_r),
    y = log(GDP_r),
    y_lag = log(GDP_lag),
    dy = y - y_lag,
    N_lag = dplyr::lag(N),
    n = log(N),
    n_lag = log(N_lag),
    dn = n - n_lag,
    GDPPOT_lag = dplyr::lag(GDPPOT),
    ypot = log(GDPPOT),
    ypot_lag = log(GDPPOT_lag),
    ygap = y - ypot,
    ygap_lag= y_lag-ypot_lag,
    dypot = ypot - ypot_lag,
    dypot_lag = dplyr::lag(dypot),
    pi_lag = dplyr::lag(pi)) %>%
  ungroup()

### Output & Growth Modelling---------------------------------------------------------
df_output <- df %>%
  select(Country,Year,y,ypot,dy,dypot,n,dn, pi, YM,MY, D,D_Y,D_R,ygap,ygap_lag) %>%
  mutate(g = dy + pi) %>%
  pivot_wider(names_from = Country, values_from = -c(Country,Year))

#Potential GDP Growth-----------------------------
eqns <- list()
for (ctry in names(country_map)) {
  eqns[[ctry]] <- as.formula(
    paste0("dypot_", ctry, " ~ ",
           "dn_", ctry, " + ",
           "YM_", ctry, " + ",
           "D_R_", ctry, " + ",
           "-1"
    )
  )
}
mod_dypot_unrestricted <- systemfit(eqns, method = "OLS",data = df_output %>% filter(Year >= 1960))
summary(mod_dypot_unrestricted)
#Restricted model
regressors <- c("dn","YM", "D_R") #
vars_to_restrict <- c("dn")
restr <- build_restriction_matrix(vars_to_restrict, names(country_map), regressors)
mod_dypot_restricted_OLS <- systemfit(eqns, data = df_output %>% filter(Year >= 1960), method = "OLS",
                                      restrict.matrix = restr$R, restrict.rhs = restr$q)
summary(mod_dypot_restricted_OLS)

results <- list()
for (i in 1:6) {
  c <- names(country_map)[i]
  results[[c]] <- process_single_model(mod_dypot_restricted_OLS$eq[[i]],
                                       df_output %>% filter(Year >= 1960), 
                                       sysfit = T)
}

latex_table <- generate_summary_regression_table(results)
cat(latex_table)


#Real Growth-------------------------------------
eqns <- list()
for (ctry in names(country_map)) {
  eqns[[ctry]] <- as.formula(
    paste0("dy_", ctry, " ~ ",
           "dypot_", ctry, " + ",
           "ygap_lag_", ctry, " + ",
           "D_", ctry, " + ",
           "-1"
    )
  )
}
mod_y_unrestricted <- systemfit(eqns, method = "OLS",data = df_output %>% filter(Year >= 1960))
summary(mod_y_unrestricted)
#Restricted model
regressors <- c("dypot", "ygap_lag", "D")
vars_to_restrict <- c("dypot","D")
restr <- build_restriction_matrix(vars_to_restrict, names(country_map), regressors)
mod_y_restricted_OLS <- systemfit(eqns, data = df_output %>% filter(Year >= 1960), method = "OLS",
                                  restrict.matrix = restr$R, restrict.rhs = restr$q)
summary(mod_y_restricted_OLS)
results <- list()
for (i in 1:6) {
  c <- names(country_map)[i]
  results[[c]] <- process_single_model(mod_y_restricted_OLS$eq[[i]],
                                       df_output %>% filter(Year >= 1960), 
                                       sysfit = T)
}

latex_table <- generate_summary_regression_table(results)
cat(latex_table)

### Inflation Modelling--------------------------------------------------------------
df_inf <- df %>%
  select(Country, Year, pi, pi_lag, PTR) %>% 
  mutate(PTR = PTR/100) %>%
  pivot_wider(names_from = Country, values_from = -c(Country, Year))
#Model US
mod_pi_US <- restriktor(
  lm(pi_US ~ pi_lag_US + PTR_US-1, data = df_inf),
  constraints = "pi_lag_US + PTR_US == 1"
)
summary(mod_pi_US)
results <- list()
results[["US"]] <- process_single_model(mod_pi_US, df_inf, sysfit = F)
#Model other countries
eqns <- list()
for (ctry in names(country_map)[2:6]) {
  eqns[[ctry]] <- as.formula(
    paste0("pi_", ctry, " ~ ",
           "pi_lag_", ctry, " + ",
           "pi_US - 1"
    )
  )
}
R <- matrix(0, nrow = 5, ncol = 10)
for (i in 1:5) {
  R[i, (2*i - 1):(2*i)] <- c(1, 1) 
}
q <- rep(1, 5)
mod_pi_restricted <- systemfit(eqns, method = "OLS", data = df_inf %>% filter(Year > 1970),
                               restrict.matrix = R, restrict.rhs = q)
summary(mod_pi_restricted)
for (i in 2:6) {
  c <- names(country_map)[i]
  results[[c]] <- process_single_model(mod_pi_restricted$eq[[i-1]],
                                       df_inf %>% filter(Year > 1970), 
                                       sysfit = T)
}

latex_table <- generate_summary_regression_table(results)
cat(latex_table)

### Avg. cost of debt Modelling-------------------------------------------------------
#Set-up dataframe
df_debt <- read_csv("data/main/Historical_Debt_big5.csv")
df_debt <- df_debt %>%
  mutate(IR = IR/100,
         Country = str_replace(Country, "United States", "US"))
df <- merge(df, df_debt, by = c("Country","Year"), all.x = T)
df_debt <- df %>%
  select(Country,Year,y,dy,ypot,dypot,ygap,pi,MY,D,D_Y,D_R,IR,PTR,Total_Debt,Expenditure_Interest,GDP,Surplus,Primary_Surplus,SFA) %>%
  mutate(PTR = PTR/100,
         #IR_r = IR - PTR,
         pi_target = 0.02,
         b = Total_Debt/GDP,
         b_lag = dplyr::lag(b),
         s_star = (IR-dy)/(1+dy) * lag(b),
         s_star_lag = dplyr::lag(s_star),
         s = Primary_Surplus/GDP,
         s_ex = s - s_star,
         sfa = SFA/GDP,
         dy_lag = lag(dy),
         log_IR = log(IR),
         lag_IR = dplyr::lag(IR)) %>%
  pivot_wider(names_from = Country, values_from = -c(Country,Year))

#Model US inflation
#mod_r_US <- lm(IR_US ~ dypot_US + PTR_US + D_R_US, data = df_debt)
mod_r_US <- lm(IR_US ~ dypot_US + PTR_US + MY_US, data = df_debt)
summary(mod_r_US)
results <- list()
results[["US"]] <- process_single_model(mod_r_US, df_debt, sysfit = F)

mod_r_DE <- lm(IR_Germany ~ IR_US + lag_IR_Germany, data = df_debt)
summary(mod_r_DE)
mod_r_DE <- restriktor(mod_r_DE, constraints = "IR_US + lag_IR_Germany == 1")
summary(mod_r_DE)
results[["DE"]] <- process_single_model(mod_r_DE, df_debt, sysfit = F)

eqns <- list()
for (ctry in names(country_map)[3:6]) {
  df_debt[,paste0("Spread_",ctry)] <- df_debt[,paste0("IR_",ctry)] - df_debt[,"IR_Germany"]
  df_debt[,paste0("b_diff_",ctry)] <- log(df_debt[,paste0("b_",ctry)]) - log(df_debt[,"b_Germany"])
  df_debt[,paste0("b_diff_lag_",ctry)] <- dplyr::lag(df_debt[,paste0("b_diff_",ctry)])
  eqns[[ctry]] <- as.formula(
    paste0("Spread_", ctry, " ~ ",
           "b_diff_lag_", ctry#, " + ",
           #"-1"
    )
  )
}
mod_r_unrestricted <- systemfit(eqns, method = "OLS",data = df_debt %>% filter(Year>1999))
summary(mod_r_unrestricted)
#Restricted Model
regressors <- c("(Intercept)","b_diff")
vars_to_restrict <- c("b_diff")
restr <- build_restriction_matrix(vars_to_restrict, names(country_map)[3:6], regressors)
mod_r_restricted_OLS <- systemfit(eqns, data = df_debt %>% filter(Year>1999), method = "OLS",
                                  restrict.matrix = restr$R, restrict.rhs = restr$q)
summary(mod_r_restricted_OLS)
for (i in 3:6) {
  c <- names(country_map)[i]
  results[[c]] <- process_single_model(mod_r_restricted_OLS$eq[[i-2]],
                                       df_debt %>% filter(Year>1999), 
                                       sysfit = T)
}

latex_table <- generate_summary_regression_table(results)
cat(latex_table)

### Deficit/Surplus Modelling--------------------------------------------------------
# Create new variables for each country
for (ctry in names(country_map)) {
  ygap_var <- paste0("ygap_", ctry)
  df_debt[[paste0("ygap_plus_", ctry)]] <- ifelse(df_debt[[ygap_var]] > 0, df_debt[[ygap_var]], 0)
  df_debt[[paste0("ygap_minus_", ctry)]] <- ifelse(df_debt[[ygap_var]] < 0, df_debt[[ygap_var]], 0)
}

eqns <- list()
for (ctry in names(country_map)) {
  eqns[[ctry]] <- as.formula(
    paste0("s_", ctry, " ~ ",
           "ygap_plus_", ctry, " + ",   # Coefficient for positive ygap
           "ygap_minus_", ctry, " + ",  # Coefficient for negative ygap
           "b_lag_", ctry, " + ",
           "D_R_", ctry
    )
  )
}

# Unrestricted model
mod_s_unrestricted <- systemfit(eqns, method = "OLS", data = df_debt %>% filter(Year > 1990))
summary(mod_s_unrestricted)
# For restricted model
regressors <- c("(Intercept)", "ygap_plus", "ygap_minus", "b_lag", "D_R")
vars_to_restrict <- c("b_lag")  # Restrict only b_lag coefficients
restr <- build_restriction_matrix(vars_to_restrict, names(country_map), regressors)
mod_s_restricted <- systemfit(eqns, method = "OLS", data = df_debt %>% filter(Year > 1990),
                              restrict.matrix = restr$R, restrict.rhs = restr$q)
summary(mod_s_restricted)
results <- list()
for (i in 1:6) {
  c <- names(country_map)[i]
  results[[c]] <- process_single_model(mod_s_restricted$eq[[i]],
                                       df_debt %>% filter(Year > 1990), 
                                       sysfit = T)
}

latex_table <- generate_summary_regression_table(results)
cat(latex_table)


eqns <- list()
for (ctry in names(country_map)) {
  eqns[[ctry]] <- as.formula(
    paste0("s_", ctry, " ~ ",
           "ygap_", ctry, " + ",
           #"dy_", ctry, " + ",
           "b_lag_", ctry, " + ",
           #"D_Y_", ctry, " + ",
           "D_R_", ctry
    )
  )
}

mod_s_unrestricted <- systemfit(eqns, method = "OLS",data = df_debt %>% filter(Year>1990))
summary(mod_s_unrestricted)
results <- list()
for (i in 1:6) {
  c <- names(country_map)[i]
  results[[c]] <- process_single_model(mod_s_unrestricted$eq[[i]],
                                       df_debt, 
                                       sysfit = T)
}

latex_table <- generate_summary_regression_table(results)
cat(latex_table)
#Custom Restricted Model
Rmat <- matrix(0, nrow = 9, ncol = 24)
r <- 1
# 1. b_lag = 0 for US (3), FR (15), IT (23)
Rmat[r, 3]  <- 1; r <- r + 1
Rmat[r, 15] <- 1; r <- r + 1
Rmat[r, 23] <- 1; r <- r + 1

# 2. b_lag equal for DE (7), NL (11), ES (19)
Rmat[r, 7] <- 1; Rmat[r, 11] <- -1; r <- r + 1
Rmat[r, 7] <- 1; Rmat[r, 19] <- -1; r <- r + 1

# 3. D_R equal for US (4), ES (20), IT (24)
Rmat[r, 4] <- 1; Rmat[r, 20] <- -1; r <- r + 1
Rmat[r, 4] <- 1; Rmat[r, 24] <- -1; r <- r + 1

# 4. D_R equal for DE (8), NL (12), FR (16)
Rmat[r, 8]  <- 1; Rmat[r, 12] <- -1; r <- r + 1
Rmat[r, 8]  <- 1; Rmat[r, 16] <- -1

# Right-hand side vector
qvec <- rep(0, nrow(Rmat))
mod_s_restricted <- systemfit(eqns, data = df_debt %>% filter(Year>1990), method = "OLS",
                              , restrict.matrix = Rmat, restrict.rhs = qvec)
summary(mod_s_restricted)
results <- list()
for (i in 1:6) {
  c <- names(country_map)[i]
  results[[c]] <- process_single_model(mod_s_restricted$eq[[i]],
                                       df_debt %>% filter(Year > 1990), 
                                       sysfit = T)
}

latex_table <- generate_summary_regression_table(results)
cat(latex_table)

#Restricted Model
# regressors <- c("(Intercept)","dy_lag","b_lag","D_R")
# vars_to_restrict <- c("dy_lag")
# restr <- build_restriction_matrix(vars_to_restrict, names(country_map), regressors)
# mod_s_restricted <- systemfit(eqns, data = df_debt %>% filter(Year>1990), method = "OLS",
#                                   restrict.matrix = restr$R, restrict.rhs = restr$q)
# summary(mod_s_restricted)
# results <- list()
# for (i in 1:6) {
#   c <- names(country_map)[i]
#   results[[c]] <- process_single_model(mod_s_unrestricted$eq[[i]],
#                                        df_debt %>% filter(Year > 1990), 
#                                        sysfit = T)
# }
# 
# latex_table <- generate_summary_regression_table(results)
# cat(latex_table)



###FIT PLOTS-----------------------------------------------------------------------
coefs_dypot <- coef(mod_dypot_restricted_OLS)
pot_data <- data.frame()
for (ctry in names(country_map)) {
  df_plot <- filter(df_output, Year >= 1960, Year <= 2024)
  temp_df <- data.frame(
    Country = ctry,
    Year = df_plot$Year,
    Actual = as.numeric(unlist(df_plot[, paste0("dypot_", ctry)])),
    Fitted = as.numeric(unlist(df_plot[, paste0("dn_", ctry)])) * coefs_dypot[paste0(ctry, "_dn_", ctry)] +
      as.numeric(unlist(df_plot[, paste0("YM_", ctry)])) * coefs_dypot[paste0(ctry, "_YM_", ctry)] +
      as.numeric(unlist(df_plot[, paste0("D_R_", ctry)])) * coefs_dypot[paste0(ctry, "_D_R_", ctry)]
  )
  temp_df$Residuals <- temp_df$Actual - temp_df$Fitted
  pot_data <- rbind(pot_data, temp_df)
}

plot_data <- pot_data %>%
  tidyr::pivot_longer(cols = c(Actual, Fitted, Residuals), 
                      names_to = "Type", 
                      values_to = "Value")

ggplot(plot_data, aes(x = Year, y = Value, color = Type, linetype = Type)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ Country, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Actual" = "black", "Fitted" = "blue", "Residuals" = "red")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Fitted" = "dashed", "Residuals" = "dashed")) +
  labs(title = "Potential Growth: Actual, Fitted, and Residuals",
       x = "Year",
       y = "Growth") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("plots/dypot_model_fit.pdf", width = 12, height = 8)


coefs_dy <- coef(mod_y_restricted_OLS)
growth_data <- data.frame()
for (ctry in names(country_map)) {
  df_plot <- filter(df_output, Year >= 1960, Year <= 2024)
  temp_df <- data.frame(
    Country = ctry,
    Year = df_plot$Year,
    Actual = as.numeric(unlist(df_plot[, paste0("dy_", ctry)])),
    Fitted = as.numeric(unlist(df_plot[, paste0("dypot_", ctry)])) * coefs_dy[paste0(ctry, "_dypot_", ctry)] +
      as.numeric(unlist(df_plot[, paste0("ygap_lag_", ctry)])) * coefs_dy[paste0(ctry, "_ygap_lag_", ctry)] +
      as.numeric(unlist(df_plot[, paste0("D_", ctry)])) * coefs_dy[paste0(ctry, "_D_", ctry)]
  )
  temp_df$Residuals <- temp_df$Actual - temp_df$Fitted
  growth_data <- rbind(growth_data, temp_df)
}

plot_data <- growth_data %>%
  tidyr::pivot_longer(cols = c(Actual, Fitted, Residuals), 
                      names_to = "Type", 
                      values_to = "Value")

ggplot(plot_data, aes(x = Year, y = Value, color = Type, linetype = Type)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ Country, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Actual" = "black", "Fitted" = "blue", "Residuals" = "red")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Fitted" = "dashed", "Residuals" = "dashed")) +
  labs(title = "Growth: Actual, Fitted, and Residuals",
       x = "Year",
       y = "Growth") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("plots/growth_model_fit.pdf", width = 12, height = 8)

coefs_r <- coef(mod_r_restricted_OLS)
plot_data <- data.frame(
  Year = df_debt$Year[as.numeric(names(fitted(mod_r_US)))],
  Actual = fitted(mod_r_US) + residuals(mod_r_US),
  Fitted = fitted(mod_r_US),
  Residuals = residuals(mod_r_US)
)
plot_data <- plot_data %>%
  tidyr::pivot_longer(cols = c(Actual, Fitted, Residuals), 
                      names_to = "Type", 
                      values_to = "Value")
ggplot(plot_data, aes(x = Year, y = Value, color = Type, linetype = Type)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_color_manual(values = c("Actual" = "black", "Fitted" = "blue", "Residuals" = "red")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Fitted" = "dashed", "Residuals" = "dashed")) +
  labs(
    x = "Year",
    y = "Nominal Interest Rates") +
  theme_minimal() +
  theme(legend.position = "bottom", text=element_text(size=15))
ggsave("plots/US_r_fit.pdf", width = 12, height = 8)

#Fit plot
plot_data <- data.frame(
  Year = df_debt$Year[as.numeric(rownames(fitted(mod_r_DE)))],
  Actual = fitted(mod_r_DE) + residuals(mod_r_DE),
  Fitted = fitted(mod_r_DE),
  Residuals = residuals(mod_r_DE)
)
plot_data <- plot_data %>%
  tidyr::pivot_longer(cols = c(Actual, Fitted, Residuals), 
                      names_to = "Type", 
                      values_to = "Value")
ggplot(plot_data, aes(x = Year, y = Value, color = Type, linetype = Type)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_color_manual(values = c("Actual" = "black", "Fitted" = "blue", "Residuals" = "red")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Fitted" = "dashed", "Residuals" = "dashed")) +
  labs(
    x = "Year",
    y = "Nominal Interest Rates") +
  theme_minimal() +
  theme(legend.position = "bottom", text=element_text(size=15))
ggsave("plots/Germany_r_fit.pdf", width = 12, height = 8)

spread_data <- data.frame()
for (ctry in names(country_map)[3:6]) {
  df_plot <- filter(df_debt, Year >= 2000, Year <= 2024)
  temp_df <- data.frame(
    Country = ctry,
    Year = df_plot$Year,
    Actual = as.numeric(unlist(df_plot[, paste0("Spread_", ctry)])),
    Fitted = coefs_r[paste0(ctry,"_(Intercept)")] + 
      as.numeric(unlist(df_plot[,paste0("b_diff_",ctry)])) * coefs_r[paste0(ctry,"_b_diff_lag_",ctry)]
  )
  temp_df$Residuals <- temp_df$Actual - temp_df$Fitted
  spread_data <- rbind(spread_data, temp_df)
}

plot_data <- spread_data %>%
  tidyr::pivot_longer(cols = c(Actual, Fitted, Residuals), 
                      names_to = "Type", 
                      values_to = "Value")

ggplot(plot_data, aes(x = Year, y = Value, color = Type, linetype = Type)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ Country, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Actual" = "black", "Fitted" = "blue", "Residuals" = "red")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Fitted" = "dashed", "Residuals" = "dashed")) +
  labs(
    x = "Year",
    y = "Spread") +
  theme_minimal() +
  theme(legend.position = "bottom", text=element_text(size=15))
ggsave("plots/spread_model_fit.pdf", width = 12, height = 8)

coefs_s <- coef(mod_s_restricted)
surplus_data <- data.frame()
for (ctry in names(country_map)) {
  df_plot <- filter(df_debt, Year > 1960, Year <= 2024)
  temp_df <- data.frame(
    Country = ctry,
    Year = df_plot$Year,
    Actual = as.numeric(unlist(df_plot[, paste0("s_", ctry)])),
    Fitted = coefs_s[paste0(ctry,"_(Intercept)")] + 
      as.numeric(unlist(df_plot[,paste0("ygap_",ctry)])) * coefs_s[paste0(ctry,"_ygap_",ctry)] +
      as.numeric(unlist(df_plot[,paste0("b_lag_",ctry)])) * coefs_s[paste0(ctry,"_b_lag_",ctry)] +
      #as.numeric(unlist(df_plot[,paste0("D_Y_",ctry)])) * coefs_s[paste0(ctry,"_D_Y_",ctry)] +
      as.numeric(unlist(df_plot[,paste0("D_R_",ctry)])) * coefs_s[paste0(ctry,"_D_R_",ctry)]
  )
  temp_df$Residuals <- temp_df$Actual - temp_df$Fitted
  surplus_data <- rbind(surplus_data, temp_df)
}

plot_data <- surplus_data %>%
  tidyr::pivot_longer(cols = c(Actual, Fitted, Residuals), 
                      names_to = "Type", 
                      values_to = "Value")

ggplot(plot_data, aes(x = Year, y = Value, color = Type, linetype = Type)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ Country, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Actual" = "black", "Fitted" = "blue", "Residuals" = "red")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Fitted" = "dashed", "Residuals" = "dashed")) +
  labs(title = "Primary Surplus/GDP: Actual, Fitted, and Residuals",
       x = "Year",
       y = "s") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save the plot
ggsave("plots/surplus_model_fit.pdf", width = 12, height = 8)


# #Make y*
# load("data/main/trade_weights.rda")
# country_map <- c("France" = "FR", "Germany" = "DE", "Italy" = "IT", 
#                  "Netherlands" = "NL", "Spain" = "ES")
# df <- df %>%
#   mutate(Code = country_map[Country])
# gdp_wide <- df %>%
#   select(Year, Code, y) %>%
#   pivot_wider(names_from = Code, values_from = y)
# gdp_matrix <- as.matrix(gdp_wide[, colnames(W_avg)])  # Ensure column order
# GDP_star_matrix <- gdp_matrix %*% t(W_avg)
# GDP_star_df <- as.data.frame(GDP_star_matrix)
# GDP_star_df$Year <- gdp_wide$Year
# GDP_star_long <- pivot_longer(GDP_star_df, cols = -Year, names_to = "Code", values_to = "y_star")
# df <- df %>%
#   left_join(GDP_star_long, by = c("Year", "Code"))
# df$y_star_diff = df$y - df$y_star
# df$lag_y_star_diff <- dplyr::lag(df$y_star_diff)
# 
# df_panel <- pdata.frame(df, index = c("Country", "Year"))
# 
# df$Year <- as.numeric(df$Year)
# df_long <- df %>%
#   pivot_longer(
#     cols = c(y, y_star),
#     names_to = "variable",
#     values_to = "value"
#   ) %>%
#   mutate(
#     data_type = case_when(
#       variable == "y" ~ "GDP",
#       variable == "y_star" ~ "GDP*"
#     )
#   )
# grey_palette <- brewer.pal(n = 6, name = "Greys")[6:2]
# names(grey_palette) <- names(country_map)
# 
# y_star_plot <- ggplot(filter(df_long, Year < 2025), aes(x = Year, y = value, color = Country, linetype = data_type)) +
#   geom_line(size = 1) +
#   scale_color_manual(values = grey_palette) +
#   scale_linetype_manual(
#     values = c("GDP" = "solid", "GDP*" = "dashed"),
#     name = "Data Type"
#   ) +
#   theme_minimal() +
#   guides(color = guide_legend(title = "Country"))
# y_star_plot
# ggsave("plots/est_y_star_big5.pdf",y_star_plot)
# eqns <- list()
# for (ctry in names(country_map)) {
#   eqns[[ctry]] <- as.formula(
#     paste0("IR_r_", ctry, " ~ ",
#            "dypot_", ctry, " + ",
#            #"pi_", ctry, " + ",
#            #"n_m_", ctry, " + ",
#            #"D_", ctry, " + ",
#            "D_R_", ctry, " + ",
#            "D_Y_", ctry, " + ",
#            #"lag_y_star_diff_", ctry,
#            "-1"
#            )
#   )
# }
# mod_r_unrestricted <- systemfit(eqns, method = "OLS",data = df_debt %>% filter(Year>1991))
# summary(mod_r_unrestricted)
# mod_summary <- summary(mod_r_US)$coefficients
# coefs <- t(matrix(mod_summary[,1],nrow=3,ncol=1))
# se <- t(matrix(mod_summary[,2],nrow=3,ncol=1))
# p <- t(matrix(mod_summary[,4],nrow=3,ncol=1))
# generate_regression_table(round(coefs,3), round(se,3), p, "US", c("dypot","PTR","MY"), type = "latex")
# 
