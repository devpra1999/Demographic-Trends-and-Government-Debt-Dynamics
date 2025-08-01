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
library(ggplot2)
library(panelr)
library(RColorBrewer)
### Load and Process Databases--------------------------------------------------------------
df_macro <- read_csv("data/main/Macros_big5.csv")
df_debt <- read_csv("data/main/Debt_big5.csv")
df_debt_historical <- read_csv("data/main/Historical_Debt_big5.csv")
#df_macro$Year <- year(df_macro$Year)
df <- merge(df_macro,df_debt_historical, by = c("Country","Year"), all = TRUE)
#Merge alternate and main gdp for Germany
# df$GDP[df$Country == "Germany" & is.na(df$GDP)] <- df$GDP_alt[df$Country == "Germany" & is.na(df$GDP)]
# df$Deflator[df$Country == "Germany" & is.na(df$Deflator)] <- df$Deflator_alt[df$Country == "Germany" & is.na(df$Deflator)]
# df$GDP_r <- df$GDP*100/df$Deflator

### Make variables-------------------------------------------------------
df <- df %>%
  mutate(Country = str_replace(Country, "United States", "US")) %>%
  group_by(Country) %>% 
  mutate(
    GDP_lag = dplyr::lag(GDP_r),
    y = log(GDP_r),
    y_lag = log(GDP_lag),
    dy = y - y_lag,
    GDPPOT_lag = dplyr::lag(GDPPOT),
    ypot = log(GDPPOT),
    ypot_lag = log(GDPPOT_lag),
    ygap = y - ypot,
    ygap_lag= y_lag-ypot_lag,
    dypot = ypot - ypot_lag,
    dypot_lag = dplyr::lag(dypot),
    IR = IR/100,
    IR_r = IR - 0.02,
    IR_R = IR - pi,
    pi_target = 0.02,
    b = Total_Debt/GDP,
    b_lag = lag(b),
    s_star = (IR-dy)/(1+dy) * lag(b),
    s = Primary_Surplus/GDP,
    s_ex = s - s_star,
    log_IR = log(IR)) %>%
  ungroup()

### Make Plots------------------------------------------------------------------
countries <- c("Italy","France","Germany","Spain","Netherlands")
df_plot <- df

# Create named color palettes
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

df_plot$Country <- factor(df_plot$Country, 
                          levels = c(names(grey_palette),"US"))

###Define Color Palettes
blue_palette <- brewer.pal(n = 6, name = "Blues")[6:2]  # 5 shades, darkest to lighter
names(blue_palette) <- countries
plot_palette <- c(blue_palette, "US" = "red")

# 1. Debt/GDP Ratio (b) Plot
b_plot <- ggplot(df_plot %>% filter(Year >= 1960), aes(x = Year, y = b, color = Country)) +
  geom_line(linewidth = 1) +
  labs(
       y = "Debt/GDP",
       x = "Year") +
  #ylim(-0.05,0.1) +
  scale_color_manual(values = plot_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2025, by = 10)) + 
  theme(text=element_text(size=15))
b_plot
ggsave("plots/desc_debt_ratio_big5.pdf",b_plot)


dypot_plot <- ggplot(df_plot %>% filter(Year >= 1960), aes(x = Year, y = dypot, color = Country)) +
  geom_line(linewidth = 1) +
  labs(
    y = "Potentil GDP Growth",
    x = "Year") +
  #ylim(-0.05,0.1) +
  scale_color_manual(values = plot_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2025, by = 10)) + 
  theme(text=element_text(size=15))
dypot_plot
ggsave("plots/desc_potential_output_growth_big5.pdf",dypot_plot)

dy_plot <- ggplot(df_plot %>% filter(Year >= 1960), aes(x = Year, y = dy, color = Country)) +
  geom_line(linewidth = 1) +
  labs(
    y = "Real GDP Growth",
    x = "Year") +
  #ylim(-0.05,0.1) +
  scale_color_manual(values = plot_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2025, by = 10)) + 
  theme(text=element_text(size=15))
dy_plot
ggsave("plots/desc_real_output_growth_big5.pdf",dy_plot)

g_plot <- ggplot(df_plot %>% filter(Year >= 1960), aes(x = Year, y = dy + pi, color = Country)) +
  geom_line(linewidth = 1) +
  labs(
    y = "Nominal GDP Growth",
    x = "Year") +
  #ylim(-0.05,0.1) +
  scale_color_manual(values = plot_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2025, by = 10)) + 
  theme(text=element_text(size=15))
g_plot
ggsave("plots/desc_nominal_output_growth_big5.pdf",g_plot)

IR_r_plot <- ggplot(df_plot %>% filter(Year >= 1960), aes(x = Year, y = IR_R, color = Country)) +
  geom_line(linewidth = 1) +
  labs(
    y = "Real Cost of Debt",
    x = "Year") +
  #ylim(-0.05,0.1) +
  scale_color_manual(values = plot_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2025, by = 10)) + 
  theme(text=element_text(size=15))
IR_r_plot
ggsave("plots/desc_real_cost_of_debt_big5.pdf",IR_r_plot)

IR_plot <- ggplot(df_plot %>% filter(Year >= 1960), aes(x = Year, y = IR, color = Country)) +
  geom_line(linewidth = 1) +
  labs(
    y = "Nominal Cost of Debt",
    x = "Year") +
  #ylim(-0.05,0.1) +
  scale_color_manual(values = plot_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2025, by = 10)) + 
  theme(text=element_text(size=15))
dypot_plot
ggsave("plots/desc_nominal_cost_of_debt_big5.pdf",IR_plot)

surplus_plot <- ggplot(df_plot %>% filter(Year >= 1960), aes(x = Year, y = s, color = Country)) +
  geom_line(linewidth = 1) +
  labs(
    y = "Primary Surplus/GDP",
    x = "Year") +
  #ylim(-0.05,0.1) +
  scale_color_manual(values = plot_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2025, by = 10)) + 
  theme(text=element_text(size=15))
surplus_plot
ggsave("plots/desc_surplus_big5.pdf",surplus_plot)

# # 2. Growth (dy) and Cost of Debt (ir) Plot
# df_plot_long <- df_plot %>%
#   select(Country, Year, IR_R, dy) %>%
#   tidyr::pivot_longer(cols = c(IR_R, dy), 
#                       names_to = "Type", 
#                       values_to = "Value")
# 
# labels <- c("IR_R" = expression(rr[t]^{av}), "dy" = expression(dy[t]))
# dy_ir_plot <- ggplot(df_plot_long, aes(x = Year, y = Value, group = Type)) +
#   geom_line(aes(color = Type, linetype = Type), linewidth = 0.8) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(~ Country, scales = "free_y", ncol = 2) +
#   scale_color_manual(values = c("IR_R" = "red", "dy" = "blue"),
#                      labels = labels) +
#   scale_linetype_manual(values = c("IR_R" = "solid", "dy" = "solid"),
#                         labels = labels) +
#   labs(x = "Year", y = "") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   guides(color = guide_legend(title = NULL),
#          linetype = "none") 
# dy_ir_plot
# ggsave("plots/desc_r_g_big5.pdf",dy_ir_plot)
# #title = expression(rr[t]^{av}~"and"~dy[t]), 
# 
# # 3. Primary Surplus/GDP (s) Plot
# df_plot_long <- df_plot %>%
#   select(Country, Year, s, s_star) %>%
#   tidyr::pivot_longer(cols = c(s,s_star), 
#                       names_to = "Type", 
#                       values_to = "Value")
# labels <- c("s" = "Actual Surplus", "s_star" = "Stabilizing Surplus")
# surplus_plot <- ggplot(df_plot_long, aes(x = Year, y = Value, group = Type)) +
#   geom_line(aes(color = Type, linetype = Type), linewidth = 0.8) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(~ Country, scales = "free_y", ncol = 2) +
#   scale_color_manual(values = c("s" = "blue", "s_star" = "red"), label = labels) +
#   scale_linetype_manual(values = c("s" = "solid", "s_star" = "solid"), label = labels) +
#   labs(title = "Primary Surplus/GDP",
#        x = "Year",
#        y = "Surplus/GDP") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   guides(color = guide_legend(title = NULL),
#          linetype = "none") 
# surplus_plot
# ggsave("plots/desc_surplus_big5.pdf",surplus_plot)
