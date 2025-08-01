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
### Source Functions------------------------------------------------------------
source("code/3_aux_funs_calibration.R")

### Load Databases--------------------------------------------------------------
df_IT <- prepare_demographics_data("Italy")
df_FR <- prepare_demographics_data("France")
df_DE <- prepare_demographics_data("Germany")
df_ES <- prepare_demographics_data("Spain")
df_NL <- prepare_demographics_data("Netherlands")
df_US <- prepare_demographics_data("United States of America")
df_US$Country <- "United States"

### Merge Databases-------------------------------------------------------------
df_demo <- rbind(df_IT,df_FR,df_DE,df_ES,df_NL,df_US)

### Make Dependency Ratios-------------------------------------------------------
df_demo$D_Y <- df_demo$N_Y/(df_demo$N_M1 + df_demo$N_M2)
df_demo$D_R <- df_demo$N_R/(df_demo$N_M1 + df_demo$N_M2)
df_demo$MY <- df_demo$N_M2/df_demo$N_M1
df_demo$D <- df_demo$D_Y + df_demo$D_R 

### Make TFR--------------------------------------------------------------------
df_demo$TFR_imputed <- (df_demo$CBR*25/1000)/(df_demo$N_M1/(df_demo$TPopulation1July*2))

### Make Plots------------------------------------------------------------------
countries <- c("Italy","France","Germany","Spain","Netherlands")
df_plot <- df_demo
df_plot <- df_plot %>%
  mutate(across(c(mu_Y, mu_M1, mu_M2, mu_R), 
                ~ { attributes(.) <- NULL; . }))

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
                              levels = c(names(grey_palette),"United States"))

my_plot <- ggplot(df_plot, aes(x = Year, y = MY, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  labs(
       y = "MY Ratio",
       x = "Year") +
  scale_color_manual(values = c(blue_palette,"United States" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$MY), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$MY), 
           label = "Projected", color = "black") + 
  theme(text=element_text(size=15))
my_plot

# my_plot <- ggplot(df_plot %>% filter(Year >=1982) %>% filter(Year<=2022), aes(x = Year, y = MY, color = Country)) +
#   geom_line(size = 1) +
#   labs(
#        y = "Ratio",
#        x = "Year") +
#   scale_color_manual(values = c(blue_palette,"United States" = "red")) +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(1982, 2022, by = 5)) + 
#   theme(text=element_text(size=15))
# my_plot

# 1. Population Plot
pop_plot <- ggplot(df_plot, aes(x = Year, y = TPopulation1July/1000, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  labs(
       y = "Population (in millions)",
       x = "Year") +
  scale_color_manual(values = c(blue_palette,"United States" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$TPopulation1July/1000), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$TPopulation1July/1000), 
           label = "Projected", color = "black") + 
  theme(text=element_text(size=15))
pop_plot


# 2. Natality (CBR) Plot
CBR_plot <- ggplot(df_plot, aes(x = Year, y = CBR/10, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  labs(
       y = "Birth Rate (in %)",
       x = "Year") +
  scale_color_manual(values = c(blue_palette,"United States" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$CBR/10), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$CBR/10), 
           label = "Projected", color = "black") + 
  theme(text=element_text(size=15))
CBR_plot

# 3. Mortality Plots
mu_Y_plot <- ggplot(df_plot, aes(x = Year, y = mu_Y, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  labs(
       y = "Deaths/Population",
       x = "Year") +
  scale_color_manual(values = c(blue_palette,"United States" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$mu_R), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$mu_R), 
           label = "Projected", color = "black") + 
  theme(text=element_text(size=15))
mu_Y_plot

mu_R_plot <- ggplot(df_plot, aes(x = Year, y = mu_R, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  labs(
       y = "Deaths/Population",
       x = "Year") +
  scale_color_manual(values = c(blue_palette,"United States" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$mu_R), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$mu_R), 
           label = "Projected", color = "black") + 
  theme(text=element_text(size=15))
mu_R_plot

max_ratio <- max(df_plot$mu_Y, na.rm = TRUE) / max(df_plot$mu_R, na.rm = TRUE)
mortality_combined_plot <- ggplot(df_plot, aes(x = Year)) +
  geom_line(aes(y = mu_Y*100, color = Country, linetype = "Young Mortality"), size = 1) +
  geom_line(aes(y = mu_R * max_ratio*100, color = Country, linetype = "Old-Age Mortality"), size = 1) +
  geom_vline(xintercept = 2024, linetype = "dotted", color = "black") +
  scale_y_continuous(
    name = "Young Mortality Rate",
    sec.axis = sec_axis(~./max_ratio, name = "Old-Age Mortality Rate")
  ) +
  scale_color_manual(values = c(blue_palette, "United States" = "red")) +
  scale_linetype_manual(values = c("Young Mortality" = "solid", "Old-Age Mortality" = "solid")) +
  labs(
       x = "Year",
       color = "Country",
       linetype = "Mortality Type") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$mu_Y*100), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$mu_Y*100), 
           label = "Projected", color = "black") + 
  theme(text=element_text(size=15))
mortality_combined_plot



# 4. Migration (CNMR) Plot
migration_plot <- ggplot(df_plot, aes(x = Year, y = CNMR/10, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  labs(
       y = "Net Migration/Total Population",
       x = "Year") +
  scale_color_manual(values = c(blue_palette, "United States" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$CNMR/10), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$CNMR/10), 
           label = "Projected", color = "black") + 
  theme(text=element_text(size=15))
migration_plot

# 5. Dependency Ratio (D) Plot
dependency_plot <- ggplot(df_plot, aes(x = Year, y = D, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  labs(
       y = "Dependency Ratio",
       x = "Year") +
  scale_color_manual(values = c(blue_palette,"United States" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$D), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$D), 
           label = "Projected", color = "black") + 
  theme(text=element_text(size=15))
dependency_plot

dependency_plot_hist <- ggplot(df_plot %>% filter(Year<=2024) %>% filter(Year >= 1960), aes(x = Year, y = D_R, color = Country)) +
  geom_line(size = 1) +
  labs(
       y = "Old-Age Dependency Ratio",
       x = "Year") +
  scale_color_manual(values = c(blue_palette,"United States" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2024, by = 10)) + 
  theme(text=element_text(size=15))
dependency_plot_hist


# 5.b. Decomposition into young and retired
df_long <- df_plot %>%
  pivot_longer(cols = c(D_Y, D_R),
               names_to = "Dependency_Type",
               values_to = "Value") %>%
  mutate(Dependency_Type = ifelse(Dependency_Type == "D_R", "D_O", Dependency_Type))

# line_colors <- c(
#   setNames(blue_palette, paste0(countries, "_D_Y")),
#   setNames(red_palette, paste0(countries, "_D_O"))
# )
line_colors <- c(
  setNames(c(blue_palette,"United States" = "red"), paste0(c(countries,"United States"), "_D_Y")),
  setNames(c(red_palette,"United States" = "grey"), paste0(c(countries,"United States"), "_D_O"))
)
#Plot
dependency_decomp_plot <- ggplot(df_long, aes(x = Year, y = Value,
                                              color = interaction(Country, Dependency_Type, sep = "_"),
                                              group = interaction(Country, Dependency_Type, sep = "_"))) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  labs(
       y = "Dependency Ratio",
       x = "Year") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  scale_color_manual(values = line_colors) +
  annotate("text", x = 2000, y = max(df_long$Value, na.rm = TRUE),
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_long$Value, na.rm = TRUE),
           label = "Projected", color = "black") +
  guides(color = guide_legend(title = "Country & Type"))
dependency_decomp_plot

df_long <- df_plot %>%
  pivot_longer(cols = c(D_Y, D_R),
               names_to = "Dependency_Type",
               values_to = "Value") %>%
  mutate(Dependency_Type = recode(Dependency_Type, "D_R" = "D_O")) %>%
  filter(Year <= 2024) %>%
  filter(Year >= 1960)

line_colors <- c(
  setNames(c(blue_palette,"United States" = "grey"), paste0(c(countries,"United States"), "_D_Y")),
  setNames(c(red_palette,"United States" = "red"), paste0(c(countries,"United States"), "_D_O"))
)
#Plot
dependency_decomp_plot_hist <- ggplot(df_long, aes(x = Year, y = Value,
                                              color = interaction(Country, Dependency_Type, sep = "_"),
                                              group = interaction(Country, Dependency_Type, sep = "_"))) +
  geom_line(linewidth = 1) +
  labs(
       y = "Dependency Ratio",
       x = "Year") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2024, by = 10)) +
  scale_color_manual(values = line_colors) +
  guides(color = guide_legend(title = "Country & Type"))
dependency_decomp_plot_hist

# 5. Old-Age Dependency Ratio (D) Plot
dependency_decomp_plot <- ggplot(df_plot, aes(x = Year, y = D_R, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  labs(y = "Old Age Dependency Ratio",
       x = "Year") +
  scale_color_manual(values = c(blue_palette,"United States" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$D), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$D), 
           label = "Projected", color = "black") + 
  theme(text=element_text(size=15))
dependency_decomp_plot


#Fertility Rate
df_long <- df_plot %>%
  select(Country, Year, TFR, TFR_imputed) %>%
  pivot_longer(
    cols = c(TFR, TFR_imputed),
    names_to = "TFR_Type",
    values_to = "Value"
  )
tfr_plot <- ggplot(df_long, aes(x = Year, y = Value, color = Country, linetype = TFR_Type)) +
  geom_line(size = 1) +
  labs(
    y = "Total Fertility Rate",
    x = "Year",
    color = "Country",
    linetype = "TFR Type"
  ) +
  scale_color_manual(values = c(blue_palette,"United States" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  scale_linetype_manual(
    values = c("TFR" = "solid", "TFR_imputed" = "dashed"),
    labels = c("TFR (Observed)", "TFR (Imputed)")
  ) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 2.1, linetype = "solid", color = "darkred") +
  annotate("text", x = 2000, y = max(df_long$Value), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_long$Value), 
           label = "Projected", color = "black") +
  annotate("text", x = 2075, y = 2.15, 
           label = "Replacement = 2.1", color = "darkred") + 
  theme(text=element_text(size=15))
tfr_plot

# Plot with facets and a single legend
tfr_facet_plot <- ggplot(df_long, aes(x = Year, y = Value, 
                                      color = TFR_Type,
                                      linetype = TFR_Type)) +
  geom_line(size = 1) +
  facet_wrap(~ Country, ncol = 2) +
  labs(
    title = "Total Fertility Rate (TFR) and Imputed TFR by Country",
    y = "Total Fertility Rate",
    x = "Year",
    color = "TFR Type",
    linetype = "TFR Type"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  scale_color_manual(
    values = c("TFR" = "blue", "TFR_imputed" = "black"),
    labels = c("TFR (Observed)", "TFR (Imputed)")
  ) +
  scale_linetype_manual(
    values = c("TFR" = "solid", "TFR_imputed" = "dashed"),
    labels = c("TFR (Observed)", "TFR (Imputed)")
  ) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  theme(legend.position = "bottom",
        legend.box = "horizontal")
print(tfr_facet_plot)


### Save Plots------------------------------------------------------------------
ggsave("plots/desc_pop_big5.pdf",pop_plot)
ggsave("plots/desc_pop_big5.pdf",my_plot)
ggsave("plots/desc_natality_big5.pdf",CBR_plot)
ggsave("plots/desc_mortality_big5.pdf",mortality_combined_plot)
ggsave("plots/desc_migration_big5.pdf",migration_plot)
ggsave("plots/desc_dependency_big5.pdf",dependency_plot)
ggsave("plots/desc_dependency_big5_intro.pdf",dependency_plot_hist)
ggsave("plots/desc_dependency_young_retired_big5.pdf",dependency_decomp_plot)
ggsave("plots/desc_dependency_young_retired_big5_intro.pdf",dependency_decomp_plot_hist)
ggsave("plots/desc_TFR_grid_big5.pdf",tfr_facet_plot)
ggsave("plots/desc_TFR_big5.pdf",tfr_plot)
