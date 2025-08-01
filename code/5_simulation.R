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

### Run model estimations---------------------------------------------------------------------
source("code/4_model_estimation.R")
df_proj <- merge(df_output,df_debt, all.x = T)
rm(list = setdiff(ls(), c("df", "df_proj","mod_y_restricted_OLS","mod_r_DE","mod_r_restricted_OLS","mod_s_restricted_OLS")))

### Set up parameters---------------------------------------------------------------------
curr_year <- 2024
curr_idx = which(df_proj$Year==curr_year)
country_map <- c("France" = "FR", "Germany" = "DE", "Italy" = "IT", 
                 "Netherlands" = "NL", "Spain" = "ES", "United States" = "US")
blue_palette <- brewer.pal(n = 6, name = "Blues")[6:2]
blue_palette <- c(blue_palette,"red")
names(blue_palette) <- names(country_map)


### Output Simuation---------------------------------------------------------------------
#Format Coefficients
coefs_y <- coef(mod_y_restricted_OLS)
df_proj$dypot_France[df_proj$Year>curr_year] <- mean(df_proj$dypot_France[(curr_idx-20):curr_idx+2])
df_proj$dypot_Germany[df_proj$Year>curr_year] <- mean(df_proj$dypot_Germany[(curr_idx-20):curr_idx+2])
df_proj$dypot_Italy[df_proj$Year>curr_year] <- mean(df_proj$dypot_Italy[(curr_idx-20):curr_idx+2])
df_proj$dypot_Spain[df_proj$Year>curr_year] <- mean(df_proj$dypot_Spain[(curr_idx-20):curr_idx+2])
df_proj$dypot_Netherlands[df_proj$Year>curr_year] <- mean(df_proj$dypot_Netherlands[(curr_idx-20):curr_idx+2])
# df_proj$dypot_France[df_proj$Year>2026] <- 0
# df_proj$dypot_Germany[df_proj$Year>2026] <- 0
# df_proj$dypot_Italy[df_proj$Year>2026] <- 0
# df_proj$dypot_Spain[df_proj$Year>2026] <- 0
# df_proj$dypot_Netherlands[df_proj$Year>2026] <- 0
for (c in names(country_map)){
  df_proj[,paste0("dy_std_",c)] <- df_proj[,paste0("dy_",c)]
  df_proj[,paste0("y_std_",c)] <- df_proj[,paste0("y_",c)]
  df_proj[,paste0("ygap_std_",c)] <- df_proj[,paste0("ygap_",c)]
  df_proj[,paste0("dy_const_",c)] <- df_proj[,paste0("dy_",c)]
  df_proj[,paste0("y_const_",c)] <- df_proj[,paste0("y_",c)]
  df_proj[,paste0("ygap_const_",c)] <- df_proj[,paste0("ygap_",c)]
  df_proj[,paste0("D_const_",c)] <- df_proj[,paste0("D_",c)]
  for (idx in (curr_idx+1):nrow(df_proj)){
    df_proj[idx,paste0("D_const_",c)] <- df_proj[idx-1,paste0("D_const_",c)] 
    
    #Projections with dependency in model
    df_proj[idx,paste0("dy_",c)] <- coefs_y[paste0(c,"_dypot_",c)]*df_proj[idx,paste0("dypot_",c)] +
      coefs_y[paste0(c,"_ygap_lag_",c)]*df_proj[idx-1,paste0("ygap_",c)] +
      coefs_y[paste0(c,"_D_",c)]*df_proj[idx,paste0("D_",c)]
    #Level of output, potential output and output gap
    df_proj[idx,paste0("y_",c)] <- df_proj[idx-1,paste0("y_",c)] + df_proj[idx,paste0("dy_",c)]
    df_proj[idx,paste0("ypot_",c)] <- df_proj[idx-1,paste0("ypot_",c)] + df_proj[idx,paste0("dypot_",c)]
    df_proj[idx,paste0("ygap_",c)] <- df_proj[idx,paste0("y_",c)] - df_proj[idx,paste0("ypot_",c)]
    
    #Projections with constant dependency
    df_proj[idx,paste0("dy_const_",c)] <- coefs_y[paste0(c,"_dypot_",c)]*df_proj[idx,paste0("dypot_",c)] +
      coefs_y[paste0(c,"_ygap_lag_",c)]*df_proj[idx-1,paste0("ygap_const_",c)] +
      coefs_y[paste0(c,"_D_",c)]*df_proj[idx,paste0("D_const_",c)]
    #Level of output and output gap
    df_proj[idx,paste0("y_const_",c)] <- df_proj[idx-1,paste0("y_const_",c)] + df_proj[idx,paste0("dy_const_",c)]
    df_proj[idx,paste0("ygap_const_",c)] <- df_proj[idx,paste0("y_const_",c)] - df_proj[idx,paste0("ypot_",c)]
    
    #Projections without dependency
    df_proj[idx,paste0("dy_std_",c)] <- coefs_y[paste0(c,"_dypot_",c)]*df_proj[idx,paste0("dypot_",c)] +
      coefs_y[paste0(c,"_ygap_lag_",c)]*df_proj[idx-1,paste0("ygap_std_",c)]
    #Level of output and output gap
    df_proj[idx,paste0("y_std_",c)] <- df_proj[idx-1,paste0("y_std_",c)] + df_proj[idx,paste0("dy_std_",c)]
    df_proj[idx,paste0("ygap_std_",c)] <- df_proj[idx,paste0("y_std_",c)] - df_proj[idx,paste0("ypot_",c)]
  }
}

### Cost of debt (IR) Simulation----------------------------------------------------------
coef_r_DE <- coef(mod_r_DE)
country_list <- c("France","Italy","Netherlands","Spain")
coefs_r <- coef(mod_r_restricted_OLS)
coefs_s <- coef(mod_s_restricted_OLS)
#Projections of r for Germany
df_proj$IR_Germany[(curr_idx+1):nrow(df_proj)] <- exp(coef_r_DE["(Intercept)"] +
  coef_r_DE["dypot_Germany"] * df_proj$dypot_Germany[(curr_idx+1):nrow(df_proj)] + 
  coef_r_DE["D_Y_Germany"] * df_proj$D_Y_Germany[(curr_idx+1):nrow(df_proj)] +
  coef_r_DE["D_R_Germany"] * df_proj$D_R_Germany[(curr_idx+1):nrow(df_proj)])# + 0.02
c <- "Germany"
#Projections of b for Germany
for (idx in (curr_idx+1):nrow(df_proj)){
  df_proj[idx,paste0("s_",c)] <- coefs_s[paste0(c,"_(Intercept)")] +
    coefs_s[paste0(c,"_ygap_",c)]*df_proj[idx,paste0("ygap_",c)] +
    coefs_s[paste0(c,"_b_lag_",c)]*df_proj[idx-1,paste0("b_",c)] +
    coefs_s[paste0(c,"_D_Y_",c)]*df_proj[idx,paste0("D_Y_",c)] + 
    coefs_s[paste0(c,"_D_R_",c)]*df_proj[idx,paste0("D_R_",c)]
  df_proj$b_Germany[idx] <- -df_proj$s_Germany[idx] +
    df_proj$b_Germany[idx-1]*(1+df_proj$IR_Germany[idx])/(1+df_proj$dy_Germany[idx] + 0.02)
}


#Projections of spreads for rest
for (c in country_list){
  for (idx in (curr_idx+1):nrow(df_proj)){
    #s_t
    df_proj[idx,paste0("s_",c)] <- coefs_s[paste0(c,"_(Intercept)")] +
      coefs_s[paste0(c,"_ygap_",c)]*df_proj[idx,paste0("ygap_",c)] +
      coefs_s[paste0(c,"_b_lag_",c)]*df_proj[idx-1,paste0("b_",c)] +
      coefs_s[paste0(c,"_D_Y_",c)]*df_proj[idx,paste0("D_Y_",c)] + 
      coefs_s[paste0(c,"_D_R_",c)]*df_proj[idx,paste0("D_R_",c)]
    #r_t
    df_proj[idx,paste0("Spread_",c)] <- coefs_r[paste0(c,"_(Intercept)")] + 
      coefs_r[paste0(c,"_b_diff_lag_",c)]*df_proj[idx-1,paste0("b_diff_",c)]
    df_proj[idx,paste0("IR_",c)] <- df_proj[idx,paste0("Spread_",c)] + df_proj[idx,"IR_Germany"]
    #b_t
    df_proj[idx,paste0("b_",c)] <- -df_proj[idx,paste0("s_",c)] + 
      (1 + df_proj[idx,paste0("IR_",c)])/(1 + df_proj[idx,paste0("dy_",c)]+0.02) * df_proj[idx-1,paste0("b_",c)]
    #Make b_diff_t
    df_proj[idx,paste0("b_diff_",c)] <- log(df_proj[idx,paste0("b_",c)]) - log(df_proj[idx,"b_Germany"])
  }
}



### Plots----------------------------------------------------------------------------------

country_names <- paste(names(country_map), collapse = "|")

df_proj_long<- df_proj %>%
  pivot_longer(
    cols = -Year,
    names_to = c(".value", "Country"),
    names_pattern = paste0("^(.*)_(", country_names, ")$")
  )
countries <- c("Italy","France","Germany","Spain","Netherlands")
blue_palette <- brewer.pal(n = 6, name = "Blues")[6:2]   # light to dark blue
red_palette <- brewer.pal(n = 6, name = "Reds")[6:2]
grey_palette <- brewer.pal(n = 6, name = "Greys")[6:2]
orange_palette <- brewer.pal(n = 6, name = "Oranges")[6:2]
green_palette <- brewer.pal(n = 6, name = "Greens")[6:2]
names(blue_palette) <- countries
names(red_palette) <- countries
names(green_palette) <- countries
names(orange_palette) <- countries
names(grey_palette) <- countries

df_proj_long$Country <- factor(df_proj_long$Country, 
                          levels = names(grey_palette))
projections_baseline <- df_proj_long %>% select(Country, Year, b, s, dy, IR, ygap)
save(projections_baseline, file = "data/main/baseline_projections.rda")


b_plot <- ggplot(df_proj_long %>% filter(Year>=2000) %>% filter(Year<=2050), aes(x = Year, y = b, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Debt Ratio (2000-2100)",
       y = "Debt/GDP",
       x = "Year") +
  scale_color_manual(values = red_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_proj_long$b), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_proj_long$b), 
           label = "Projected", color = "black")
b_plot

IR_plot <- ggplot(df_proj_long %>% filter(Year>=2000) %>% filter(Year<=2050), aes(x = Year, y = IR, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Cost of Debt (2000-2100)",
       y = "R_t^av",
       x = "Year") +
  scale_color_manual(values = red_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_proj_long$IR), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_proj_long$IR), 
           label = "Projected", color = "black")
IR_plot

dy_plot <- ggplot(df_proj_long %>% filter(Year>=2000) %>% filter(Year<=2050), aes(x = Year, y = dy, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Growth(2000-2100)",
       y = "g",
       x = "Year") +
  scale_color_manual(values = red_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_proj_long$dy), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_proj_long$dy), 
           label = "Projected", color = "black")
dy_plot

surplus_plot <- ggplot(df_proj_long %>% filter(Year>=2000), aes(x = Year, y = s, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Primary Surplus / GDP (2000-2100)",
       y = "s",
       x = "Year") +
  scale_color_manual(values = red_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_proj_long$s), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_proj_long$s), 
           label = "Projected", color = "black")
surplus_plot

ggsave("plots/sim_debt_ratio_big5.pdf",b_plot)
ggsave("plots/sim_growth_big5.pdf",dy_plot)
ggsave("plots/sim_cost_of_debt_big5.pdf",IR_plot)
ggsave("plots/sim_surplus_big5.pdf",surplus_plot)

# #Plot output
# df_y_long <- df_proj_long %>%
#   filter(Year > curr_year - 2) %>%
#   filter(Year < 2060) %>%
#   select(Country,Year,y,y_const,y_std) %>%
#   pivot_longer(
#     cols = c(y, y_const,y_std),
#     names_to = "variable",
#     values_to = "value"
#   ) %>%
#   mutate(
#     data_type = case_when(
#       variable == "y" ~ "Output with projected dependency",
#       variable == "y_const" ~ "Output with constant dependency",
#       variable == "y_std" ~ "Output w/o dependency")
#   )
# y_plot <- ggplot(df_y_long, aes(x = Year, y = value, color = Country, linetype = data_type)) +
#   geom_line(size = 1) +
#   geom_vline(xintercept = curr_year, linetype = "dashed", color = "red") +
#   labs(title = "Total Output Projections",
#        y = "Output",
#        x = "Year") +
#   scale_color_manual(values = grey_palette) +
#   scale_linetype_manual(
#     values = c("Output with projected dependency" = "solid",
#                "Output with constant dependency" = "dashed",
#                "Output w/o dependency" = "dotted"),
#     name = "Data Type"
#   ) +
#   theme_minimal() +
#   #scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
#   guides(color = guide_legend(title = "Country"))
# y_plot
# ggsave("plots/output_projections.pdf",y_plot)
# 
# df_ygap_long <- df_proj_long %>%
#   filter(Year > curr_year - 2) %>%
#   filter(Year < 2060) %>%
#   select(Country,Year,ygap,ygap_const,ygap_std) %>%
#   pivot_longer(
#     cols = c(ygap, ygap_const, ygap_std),
#     names_to = "variable",
#     values_to = "value"
#   ) %>%
#   mutate(
#     data_type = case_when(
#       variable == "ygap" ~ "Output Gap with projected dependency",
#       variable == "ygap_const" ~ "Output Gap with constant dependency",
#       variable == "ygap_std" ~ "Output Gap w/o dependency")
#   )
# ygap_plot <- ggplot(df_ygap_long, 
#                     aes(x = Year, y = value, color = Country, linetype = data_type)) +
#   geom_line(size = 1) +
#   geom_vline(xintercept = curr_year, linetype = "dashed", color = "red") +
#   labs(title = "Output Gap Projections",
#        y = "Output Gap",
#        x = "Year") +
#   scale_color_manual(values = grey_palette) +
#   scale_linetype_manual(
#     values = c("Output Gap with projected dependency" = "solid",
#                "Output Gap with constant dependency" = "dashed",
#                "Output Gap w/o dependency" = "dotted"),
#     name = "Data Type"
#   ) +
#   theme_minimal() +
#   #scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
#   guides(color = guide_legend(title = "Country"))
# ygap_plot
# ggsave("plots/output_gap_projections.pdf",ygap_plot)
# 
# df_dy_long <- df_proj_long %>%
#   filter(Year > curr_year - 2) %>%
#   filter(Year < 2060) %>%
#   select(Country,Year,dy,dy_const,dy_std) %>%
#   pivot_longer(
#     cols = c(dy,dy_const,dy_std),
#     names_to = "variable",
#     values_to = "value"
#   ) %>%
#   mutate(
#     data_type = case_when(
#       variable == "dy" ~ "Output Growth with projected dependency",
#       variable == "dy_const" ~ "Output Growth with constant dependency",
#       variable == "dy_std" ~ "Output Growth w/o dependency")
#     
#   )
# dy_plot <- ggplot(df_dy_long %>% filter(Country %in% c("Spain","France")), 
#                   aes(x = Year, y = value, color = Country, linetype = data_type)) +
#   geom_line(size = 1) +
#   geom_vline(xintercept = curr_year, linetype = "dashed", color = "red") +
#   labs(title = "Output Growth Projections",
#        y = "Output Growth",
#        x = "Year") +
#   scale_color_manual(values = grey_palette) +
#   scale_linetype_manual(
#     values = c("Output Growth with projected dependency" = "solid",
#                "Output Growth with constant dependency" = "dashed",
#                "Output Growth w/o dependency" = "dotted"),
#     name = "Data Type"
#   ) +
#   theme_minimal() +
#   #scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
#   guides(color = guide_legend(title = "Country"))
# dy_plot
# ggsave("plots/output_growth_projections.pdf",dy_plot)