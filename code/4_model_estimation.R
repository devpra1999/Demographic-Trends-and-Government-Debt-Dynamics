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
library(systemfit)
library(RColorBrewer)
library(tidyr)
#Aux funs----------------------------------------------------------------------
source("code/4_aux_funs_estimation.R")
country_map <- c("France" = "FR", "Germany" = "DE", "Italy" = "IT", 
                                 "Netherlands" = "NL", "Spain" = "ES")
grey_palette <- brewer.pal(n = 6, name = "Greys")[6:2]
names(grey_palette) <- names(country_map)
### Make dataset--------------------------------------------------------------
df_macro <- read_csv("data/main/Macros_big5.csv")
FRED_data <- read.delim("data/raw/HISTDATA.TXT", sep = ",")
FRED_data$DATE <- seq(as.Date("1968-04-01"),length=nrow(FRED_data), by="quarters")
PTR_US <- FRED_data %>% select(DATE, PTR)
rm(FRED_data)
PTR_US$Year <- format(PTR_US$DATE, "%Y")
# Calculate annual averages
annual_PTR <- aggregate(PTR ~ Year, data = PTR_US, FUN = mean)
df_macro <- merge(df_macro,annual_PTR,all.x = TRUE)
df_macro <- df_macro %>%
  group_by(Country) %>%
  mutate(pi = log(Deflator) - dplyr::lag(log(Deflator))) %>%
  ungroup()

df_demographics <- read_csv("data/main/Demographics_big5.csv")
df_debt <- read_csv("data/main/Debt_big5.csv")
df_debt_historical <- read_csv("data/main/Historical_Debt_big5.csv")
#df_macro$Year <- year(df_macro$Year)
df <- merge(df_macro,df_demographics, by = c("Country","Year"), all = TRUE)
# df$Deflator_alt <- as.numeric(df$Deflator_alt)
# df$GDP[df$Country == "Germany" & is.na(df$GDP)] <- df$GDP_alt[df$Country == "Germany" & is.na(df$GDP)]
# df$GDPPOT[df$Country == "Germany" & is.na(df$GDPPOT)] <- df$GDPPOT_alt[df$Country == "Germany" & is.na(df$GDPPOT)]
# df$Deflator[df$Country == "Germany" & is.na(df$Deflator)] <- df$Deflator_alt[df$Country == "Germany" & is.na(df$Deflator)]
# df$GDP_r <- df$GDP*100/df$Deflator
# df_dr <- read_csv("data/raw/dependency_ratio_estimates_big5.csv")
# df_dr_proj <- read_csv("data/raw/dependency_ratio_projections_big5.csv")
# df_dr <- rbind(df_dr,df_dr_proj)
# rm(df_dr_proj)
# df_dr <- df_dr %>%
#   select(Country,Year,`Annual total dep. ratio [(0-19 & 65+) / 20-64] (%)`,`Annual child dep. ratio [0-19 / 20-64] (%)`,`Annual old-age dep. ratio [65+ / 20-64] (%)`,
#          `Annual total dep. ratio [(0-19 & 70+) / 20-69] (%)`,`Annual child dep. ratio [0-19 / 20-69] (%)`,`Annual old-age dep. ratio [70+ / 20-69] (%)`)
# colnames(df_dr) <- c("Country","Year","D_UN","D_Y_UN","D_R_UN","D_UN_alt","D_Y_UN_alt","D_R_UN_alt")
# df <- merge(df,df_dr,by = c("Country","Year"))
#Transform dataframe to make required variables
df <- df %>%
  group_by(Country) %>% 
  mutate(
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
    dypot_lag = dplyr::lag(dypot)) %>%
  ungroup()
# df$D_UN_alt <- df$D_UN_alt/100
# df$D_Y_UN_alt <- df$D_Y_UN_alt/100
# df$D_R_UN_alt <- df$D_R_UN_alt/100


### Output & Growth Modelling---------------------------------------------------------
df_output <- df %>%
  select(Country,Year,y,ypot,dy,dypot,n,D,D_Y,D_R,ygap,ygap_lag) %>% 
  pivot_wider(names_from = Country, values_from = -c(Country,Year))
eqns <- list()
#c(y,y_lag,dy,ypot,ypot_lag,dypot,n,dn,D,D_Y,D_R,ygap,ygap_lag,lag_y_star_diff)

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
mod_y_unrestricted <- systemfit(eqns, method = "OLS",data = df_output)
summary(mod_y_unrestricted)
#Restricted model
regressors <- c("dypot", "ygap_lag", "D") #
vars_to_restrict <- c("dypot","D")
restr <- build_restriction_matrix(vars_to_restrict, names(country_map), regressors)
mod_y_restricted_SUR <- systemfit(eqns, data = df_output, method = "SUR",
                            restrict.matrix = restr$R, restrict.rhs = restr$q)
mod_y_restricted_OLS <- systemfit(eqns, data = df_output, method = "OLS",
                                  restrict.matrix = restr$R, restrict.rhs = restr$q)
summary(mod_y_restricted_SUR)
summary(mod_y_restricted_OLS)
mod_summary <- summary(mod_y_restricted_OLS)$coefficients
coefs <- t(matrix(mod_summary[,1],nrow=3,ncol=5))
se <- t(matrix(mod_summary[,2],nrow=3,ncol=5))
p <- t(matrix(mod_summary[,4],nrow=3,ncol=5))
generate_regression_table(round(coefs,3), round(se,3), p, names(country_map), regressors, type = "latex")


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

# Save the plot
ggsave("plots/growth_model_fit.pdf", width = 12, height = 8)

### Avg. cost of debt Modelling-------------------------------------------------------
df_debt <- read_csv("data/main/Historical_Debt_big5.csv")
df_debt$IR <- df_debt$IR/100
df <- merge(df, df_debt, by = c("Country","Year"), all.x = T)

ir_plot <- ggplot(df_debt , aes(x = Year, y = IR, color = Country)) +
  geom_line(size = 1) +
  labs(title = "Interest (1950-2100)",
       y = "Interest ",
       x = "Year") +
  scale_color_manual(values = grey_palette) +
  theme_minimal()
ir_plot
#N_M1,N_M2,N,D_UN_alt,D_Y_UN_alt,D_R_UN_alt,dD,ygap,ygap_lag
df_debt <- df %>%
  select(Country,Year,y,dy,ypot,dypot,ygap,pi,D,D_Y,D_R,IR,PTR,Total_Debt,GDP,Surplus,Primary_Surplus) %>%
  mutate(PTR = PTR/100,
         IR_r = IR - PTR,
         pi_target = 0.02,
         b = Total_Debt/GDP,
         b_lag = dplyr::lag(b),
         s_star = (IR-dy)/(1+dy) * lag(b),
         s_star_lag = dplyr::lag(s_star),
         s = Primary_Surplus/GDP,
         s_ex = s - s_star,
         log_IR = log(IR)) %>%
  pivot_wider(names_from = Country, values_from = -c(Country,Year))

mod_r_DE <- lm(log_IR_Germany ~ dypot_Germany + D_Y_Germany + D_R_Germany, data = df_debt %>% filter(Year>1990))
summary(mod_r_DE)
mod_summary <- summary(mod_r_DE)$coefficients
coefs <- t(matrix(mod_summary[,1],nrow=4,ncol=1))
se <- t(matrix(mod_summary[,2],nrow=4,ncol=1))
p <- t(matrix(mod_summary[,4],nrow=4,ncol=1))
generate_regression_table(round(coefs,3), round(se,3), p, "Germany", c("(Intercept)","dypot","D_Y","D_R"), type = "latex")

# eqns <- list()
# for (ctry in names(country_map)) {
#   eqns[[ctry]] <- as.formula(
#     paste0("IR_r_", ctry, " ~ ",
#            "dypot_", ctry, " + ",
#            "D_Y_", ctry, " + ",
#            "D_R_", ctry, " + ",
#            "-1"
#     )
#   )
# }
# mod_r_unrestricted <- systemfit(eqns, method = "OLS",data = df_debt)
# summary(mod_r_unrestricted)
# #Restricted model
# regressors <- c("dypot", "D_Y", "D_R") #
# vars_to_restrict <- c("dypot")
# restr <- build_restriction_matrix(vars_to_restrict, names(country_map), regressors)
# mod_r_restricted_OLS <- systemfit(eqns, data = df_debt, method = "OLS",
#                                   restrict.matrix = restr$R, restrict.rhs = restr$q)
# summary(mod_r_restricted_OLS)
# mod_summary <- summary(mod_r_restricted_OLS)$coefficients
# coefs <- t(matrix(mod_summary[,1],nrow=3,ncol=5))
# se <- t(matrix(mod_summary[,2],nrow=3,ncol=5))
# p <- t(matrix(mod_summary[,4],nrow=3,ncol=5))
# generate_regression_table(round(coefs,3), round(se,3), p, names(country_map), regressors, type = "latex")
# 
# 
# mod_r_NL <- lm(IR_r_Netherlands ~ dypot_Netherlands + D_Y_Netherlands + D_R_Netherlands-1, data = df_debt %>% filter(Year>1960))
# summary(mod_r_NL)
# mod_r_ES <- lm(IR_r_Spain ~ dypot_Spain + D_Y_Spain + D_R_Spain-1, data = df_debt %>% filter(Year>1960))
# summary(mod_r_ES)
# mod_r_IT <- lm(IR_r_Italy ~ dypot_Italy + D_Y_Italy + D_R_Italy-1, data = df_debt %>% filter(Year>1960))
# summary(mod_r_IT)
# mod_r_FR <- lm(IR_r_France ~ dypot_France + D_Y_France + D_R_France-1, data = df_debt %>% filter(Year>1960))
# summary(mod_r_FR)
# 
# mod_r_NL <- lm(IR_Netherlands ~ dypot_Netherlands + PTR_France + D_Y_Netherlands + D_R_Netherlands-1, data = df_debt %>% filter(Year>1960))
# summary(mod_r_NL)
# mod_r_ES <- lm(IR_Spain ~ dypot_Spain + PTR_France + D_Y_Spain + D_R_Spain-1, data = df_debt %>% filter(Year>1960))
# summary(mod_r_ES)
# mod_r_IT <- lm(IR_Italy ~ dypot_Italy + PTR_France + D_Y_Italy + D_R_Italy-1, data = df_debt %>% filter(Year>1960))
# summary(mod_r_IT)
# mod_r_FR <- lm(IR_France ~ dypot_France + PTR_France + D_Y_France + D_R_France-1, data = df_debt %>% filter(Year>1960))
# summary(mod_r_FR)
# eqns <- list()
# for (ctry in names(country_map)) {
#   eqns[[ctry]] <- as.formula(
#     paste0("IR_", ctry, " ~ ",
#            "dypot_", ctry, " + ",
#            "PTR_", ctry, " + ",
#            "D_Y_", ctry, " + ",
#            "D_R_", ctry, " + ",
#            "-1"
#     )
#   )
# }
# mod_r_unrestricted <- systemfit(eqns, method = "OLS",data = df_debt)
# summary(mod_r_unrestricted)
# #Restricted model
# regressors <- c("dypot", "PTR", "D_Y", "D_R") #
# vars_to_restrict <- c("dypot")
# restr <- build_restriction_matrix(vars_to_restrict, names(country_map), regressors)
# mod_r_restricted_OLS <- systemfit(eqns, data = df_debt, method = "OLS",
#                                   restrict.matrix = restr$R, restrict.rhs = restr$q)
# summary(mod_r_restricted_OLS)
# mod_summary <- summary(mod_r_restricted_OLS)$coefficients
# coefs <- t(matrix(mod_summary[,1],nrow=4,ncol=5))
# se <- t(matrix(mod_summary[,2],nrow=4,ncol=5))
# p <- t(matrix(mod_summary[,4],nrow=4,ncol=5))
# generate_regression_table(round(coefs,3), round(se,3), p, names(country_map), regressors, type = "latex")
# 
# 
# 
# mod_summary <- summary(mod_r_FR)$coefficients
# coefs <- t(matrix(mod_summary[,1],nrow=4,ncol=1))
# df_plot <- df_debt %>%
#   filter(Year >= 1971 & Year <= 2024) %>%
#   select(Year,IR_France, IR_r_France, dypot_France,D_Y_France,D_R_France)
# fitted_FR <- coefs[1,1] + 
#   coefs[1,2]*df_plot$dypot_France + 
#   coefs[1,3]*df_plot$D_Y_France + 
#   coefs[1,4]*df_plot$D_R_France
# residuals_FR <- df_plot$IR_France - fitted_FR
# df_plot <- df_plot %>%
#   mutate(IR_fitted_France = fitted_FR,
#          IR_residuals_France = residuals_FR)
# pdf("plots/France_IR_fit.pdf")
# plot(df_plot$Year, df_plot$IR_France, type = "l", lwd = 2, 
#      xlab = "Year", ylab = "Interest Rate", ylim = c(-0.01,0.1),
#      main = "France Interest Rates: Actual, Fitted, and Residuals")
# abline(h = 0, lty = "dotted")
# lines(df_plot$Year, df_plot$IR_fitted_France, col = "blue", lty = "dashed", lwd = 2)
# lines(df_plot$Year, df_plot$IR_residuals_France, col = "red", lty = "dashed", lwd = 2)
# legend("topright", 
#        legend = c("Actual", "Fitted", "Residuals"), 
#        col = c("black", "blue", "red"), 
#        lty = c("solid", "dashed", "dashed"), 
#        lwd = 2)
# dev.off()
# 
# 
# mod_r_DE <- lm(IR_r_Germany ~ dypot_Germany + D_Y_Germany + D_R_Germany, data = df_debt %>% filter(Year>1990))
# mod_summary <- summary(mod_r_DE)$coefficients
# coefs <- t(matrix(mod_summary[,1],nrow=4,ncol=1))
# df_plot <- df_debt %>%
#   filter(Year >= 1971 & Year <= 2024) %>%
#   select(Year,IR_Germany, dypot_Germany,D_Y_Germany,D_R_Germany)
# fitted_DE <- coefs[1,1] + 
#                    coefs[1,2]*df_plot$dypot_Germany + 
#                    coefs[1,3]*df_plot$D_Y_Germany + 
#                    coefs[1,4]*df_plot$D_R_Germany + 0.02
# residuals_DE <- df_plot$IR_Germany - fitted_DE
# df_plot <- df_plot %>%
#   mutate(IR_fitted_Germany = fitted_DE,
#          IR_residuals_Germany = residuals_DE)
# pdf("plots/Germany_IR_fit.pdf")
# plot(df_plot$Year, df_plot$IR_Germany, type = "l", lwd = 2, 
#      xlab = "Year", ylab = "Interest Rate", ylim = c(-0.01,0.3),
#      main = "Germany Interest Rates: Actual, Fitted, and Residuals")
# abline(h = 0, lty = "dotted")
# lines(df_plot$Year, df_plot$IR_fitted_Germany, col = "blue", lty = "dashed", lwd = 2)
# lines(df_plot$Year, df_plot$IR_residuals_Germany, col = "red", lty = "dashed", lwd = 2)
# legend("topright", 
#        legend = c("Actual", "Fitted", "Residuals"), 
#        col = c("black", "blue", "red"), 
#        lty = c("solid", "dashed", "dashed"), 
#        lwd = 2)
# dev.off()

eqns <- list()
country_list <- c("France","Italy","Netherlands","Spain")
for (ctry in country_list) {
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
restr <- build_restriction_matrix(vars_to_restrict, country_list, regressors)
mod_r_restricted_SUR <- systemfit(eqns, data = df_debt %>% filter(Year>1999), method = "SUR",
                                  restrict.matrix = restr$R, restrict.rhs = restr$q)
mod_r_restricted_OLS <- systemfit(eqns, data = df_debt %>% filter(Year>1999), method = "OLS",
                                  restrict.matrix = restr$R, restrict.rhs = restr$q)
summary(mod_r_restricted_SUR)
summary(mod_r_restricted_OLS)
coefs_r <- coef(mod_r_restricted_OLS)
mod_summary <- summary(mod_r_restricted_OLS)$coefficients
coefs <- t(matrix(mod_summary[,1],nrow=2,ncol=4))
se <- t(matrix(mod_summary[,2],nrow=2,ncol=4))
p <- t(matrix(mod_summary[,4],nrow=2,ncol=4))
generate_regression_table(round(coefs,3), round(se,3), p, country_list, regressors, type = "latex")
spread_data <- data.frame()
for (ctry in country_list) {
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
  labs(title = "Spread: Actual, Fitted, and Residuals",
       x = "Year",
       y = "Spread") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save the plot
ggsave("plots/spread_model_fit.pdf", width = 12, height = 8)

### Deficit/Surplus Modelling--------------------------------------------------------
eqns <- list()
for (ctry in names(country_map)) {
  eqns[[ctry]] <- as.formula(
    paste0("s_", ctry, " ~ ",
           "ygap_", ctry, " + ",
           "b_lag_", ctry, " + ",
           "D_R_", ctry, " + ",
           "D_Y_", ctry#, " + ",
           #"-1"
           )
  )
}

mod_s_unrestricted <- systemfit(eqns, method = "OLS",data = df_debt)
summary(mod_s_unrestricted)
#Restricted Model
regressors <- c("(Intercept)","ygap","b_lag","D_R","D_Y")
vars_to_restrict <- c("(Intercept)","ygap","b_lag")
restr <- build_restriction_matrix(vars_to_restrict, names(country_map), regressors)
mod_s_restricted_OLS <- systemfit(eqns, data = df_debt %>% filter(Year>1960), method = "OLS",
                                  restrict.matrix = restr$R, restrict.rhs = restr$q)
summary(mod_s_restricted_OLS)
coefs_s <- coef(mod_s_restricted_OLS)
mod_summary <- summary(mod_s_restricted_OLS)$coefficients
coefs <- t(matrix(mod_summary[,1],nrow=5,ncol=5))
se <- t(matrix(mod_summary[,2],nrow=5,ncol=5))
p <- t(matrix(mod_summary[,4],nrow=5,ncol=5))
generate_regression_table(round(coefs,3), round(se,3), p, names(country_map), regressors, type = "latex")

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
      as.numeric(unlist(df_plot[,paste0("D_R_",ctry)])) * coefs_s[paste0(ctry,"_D_R_",ctry)] +
      as.numeric(unlist(df_plot[,paste0("D_Y_",ctry)])) * coefs_s[paste0(ctry,"_D_Y_",ctry)]
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

