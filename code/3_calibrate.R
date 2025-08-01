rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
### Packages-------------------------------------------------------------------
library(readr)
library(pracma)
library(panelr)
library(ggplot2)
library(purrr)
detach("package:dplyr",unload = TRUE)
library(dplyr)
library(RColorBrewer)

### Source Functions-----------------------------------------------------------
source("code/3_aux_funs_calibration.R")

# ### Make datasets------------------------------------------------------------------
df_IT <- get_calibrated_data(prepare_demographics_data("Italy"))
df_FR <- get_calibrated_data(prepare_demographics_data("France"))
df_DE <- get_calibrated_data(prepare_demographics_data("Germany"))
df_ES <- get_calibrated_data(prepare_demographics_data("Spain"))
df_NL <- get_calibrated_data(prepare_demographics_data("Netherlands"))
df_US <- get_calibrated_data(prepare_demographics_data("United States of America"))
df_US$Country <- "United States"
# ### Merge datasets into a panel dataframe--------------------------------------
df <- rbind(df_IT,df_FR,df_DE,df_ES,df_NL,df_US)
write_csv(df,"data/main/Demographics_big5.csv")
#df <- read_csv("data/main/Demographics_big5.csv")
df <- panel_data(df, id = Country, wave = Year)

### Validation-----------------------------------------------------------------
df <- validate_calibrated_data(df) #df needs to be of panel type

### Plots----------------------------------------------------------------------
countries <- c("Italy","France","Germany","Spain","Netherlands")
df_plot <- as_tibble(df)
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
countries <- c("Italy","France","Germany","Spain","Netherlands","United States")
df_plot$Country <- factor(df_plot$Country, 
                              levels = countries)

#Plot for transition probabilitities--------------------------------------------
#Country wise breakdown
ggplot(df_plot, aes(x = Year)) +
  geom_line(aes(y = p_y, color = "Young"), linewidth = 1) +
  geom_line(aes(y = p_m1, color = "Working 1"), linewidth = 1) +
  geom_line(aes(y = p_m2, color = "Working 2"), linewidth = 1) +
  facet_wrap(~ Country, ncol = 2, scales = "free_y") +
  labs(title = "Transition Probabilities by Country",y = "Probability",color = "Variable") +
  scale_color_manual(
    values = c(
      "Young" = "black",
      "Working 1" = "blue",
      "Working 2" = "red"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

#Age group wise breakdown
young_tp <- ggplot(df_plot, aes(x = Year, y = p_y, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Young to Working 1 Transtion Probabilities (1950-2100)",
       y = "Probability",
       x = "Year") +
  scale_color_manual(values = blue_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$p_y), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$p_y), 
           label = "Projected", color = "black")
young_tp
ggsave("plots/young_to_working1.pdf",young_tp)

middle1_tp <- ggplot(df_plot, aes(x = Year, y = p_m1, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Working 1 to Working 2 Transtion Probabilities (1950-2100)",
       y = "Probability",
       x = "Year") +
  scale_color_manual(values = green_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$p_m1), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$p_m1), 
           label = "Projected", color = "black")
middle1_tp
ggsave("plots/working1_to_working2.pdf",middle1_tp)

middle2_tp <- ggplot(df_plot, aes(x = Year, y = p_m2, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Working 2 to Retired Transtion Probabilities (1950-2100)",
       y = "Probability",
       x = "Year") +
  scale_color_manual(values = red_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df_plot$p_y), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$p_y), 
           label = "Projected", color = "black")
middle2_tp
ggsave("plots/working2_to_retired.pdf",middle2_tp)

#Plot for fatalities--------------------------------------------------------
fatality_young <- ggplot(df_plot, aes(x = Year, y = FAT_Y, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Young Fatalities (1950-2100)",
       y = "Fatalities (in thousands)",
       x = "Year") +
  scale_color_manual(values = blue_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df$FAT_Y), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df$FAT_Y), 
           label = "Projected", color = "black")
fatality_young
ggsave("plots/fatalities_young.pdf",fatality_young)

fatality_middle1 <- ggplot(df_plot, aes(x = Year, y = FAT_M1, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Working 1 Fatalities (1950-2100)",
       y = "Fatalities (in thousands)",
       x = "Year") +
  scale_color_manual(values = green_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df$FAT_M1), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df$FAT_M1), 
           label = "Projected", color = "black")
fatality_middle1
ggsave("plots/fatalities_working1.pdf",fatality_middle1)

fatality_middle2 <- ggplot(df_plot, aes(x = Year, y = FAT_M2, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Working 2 Fatalities (1950-2100)",
       y = "Fatalities (in thousands)",
       x = "Year") +
  scale_color_manual(values = orange_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df$FAT_M2), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df$FAT_M2), 
           label = "Projected", color = "black")
fatality_middle2
ggsave("plots/fatalities_working2.pdf",fatality_middle2)

fatality_retired <- ggplot(df_plot, aes(x = Year, y = FAT_R, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Retired Fatalities (1950-2100)",
       y = "Fatalities (in thousands)",
       x = "Year") +
  scale_color_manual(values = red_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df$FAT_R), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df$FAT_R), 
           label = "Projected", color = "black")
fatality_retired
ggsave("plots/fatalities_retired.pdf",fatality_retired)

#Plot for mortality rate--------------------------------------------------------
mortality_young <- ggplot(df_plot, aes(x = Year, y = mu_Y*1000, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Young Mortality (1950-2100)",
       y = "Mortalities (per thousand)",
       x = "Year") +
  scale_color_manual(values = blue_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df$mu_Y), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df$mu_Y), 
           label = "Projected", color = "black")
mortality_young
ggsave("plots/mortality_young.pdf",mortality_young)

mortality_middle1 <- ggplot(df_plot, aes(x = Year, y = mu_M1*1000, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Working 1 Mortality (1950-2100)",
       y = "Mortalities (per thousand)",
       x = "Year") +
  scale_color_manual(values = green_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df$mu_M1), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df$mu_M1), 
           label = "Projected", color = "black")
mortality_middle1
ggsave("plots/mortality_working1.pdf",mortality_middle1)

mortality_middle2 <- ggplot(df_plot, aes(x = Year, y = mu_M2*1000, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Working 2 Mortality (1950-2100)",
       y = "Mortalities (per thousand)",
       x = "Year") +
  scale_color_manual(values = orange_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df$mu_M2), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df$mu_M2), 
           label = "Projected", color = "black")
mortality_middle2
ggsave("plots/mortality_working2.pdf",mortality_middle2)

mortality_retired <- ggplot(df_plot, aes(x = Year, y = mu_R*1000, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Retired Mortality (1950-2100)",
       y = "Mortalities (per thousand)",
       x = "Year") +
  scale_color_manual(values = red_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df$mu_R), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df$mu_R), 
           label = "Projected", color = "black")
mortality_retired
ggsave("plots/mortality_retired.pdf",mortality_retired)

###Calibration Errors-----------------------------------------------------------------------
error_calibration <- ggplot(df_plot, aes(x = Year, y = u_t, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Error in Calibration (1950-2100)",
       y = "Error",
       x = "Year") +
  scale_color_manual(values = grey_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df$u_t,na.rm = TRUE), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df$u_t,na.rm = TRUE), 
           label = "Projected", color = "black")
error_calibration
ggsave("plots/error_calibration.pdf",error_calibration)

error_calibration_scale <- ggplot(df_plot, aes(x = Year, y = u_t/N, color = Country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Error in Calibration scaled to total population (1950-2100)",
       y = "Error/Total Population",
       x = "Year") +
  scale_color_manual(values = grey_palette) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  annotate("text", x = 2000, y = max(df$u_t/df$N,na.rm = TRUE), 
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df$u_t/df$N,na.rm = TRUE), 
           label = "Projected", color = "black")
error_calibration_scale
ggsave("plots/error_calibration_scaled.pdf",error_calibration_scale)


### Function for projections using calibrated data-----------------------------------------------------
run_projection <- function(country_data, curr_idx = 75) {
  #curr_idx=75 for 2024
  end_idx <- nrow(country_data)
  # Extract calibrated variables
  p_y_t <- country_data$p_y[curr_idx:end_idx]
  p_m1_t <- country_data$p_m1[curr_idx:end_idx]
  p_m2_t <- country_data$p_m2[curr_idx:end_idx]
  nu_t <- country_data$nu_t[curr_idx:end_idx]
  M_t <- country_data$NetMigrations[curr_idx:end_idx]
  FAT_y_t <- country_data$FAT_Y[curr_idx:end_idx]
  FAT_m1_t <- country_data$FAT_M1[curr_idx:end_idx]
  FAT_m2_t <- country_data$FAT_M2[curr_idx:end_idx]
  FAT_r_t <- country_data$FAT_R[curr_idx:end_idx]
  # Initialize vectors
  n <- end_idx - curr_idx + 1
  N_proj <- numeric(n)
  dr_y <- numeric(n)
  dr_r <- numeric(n)
  N_y_proj <- numeric(n)
  N_m1_proj <- numeric(n)
  N_m2_proj <- numeric(n)
  N_r_proj <- numeric(n)
  # Set initial values
  N_y_proj[1] <- country_data$N_Y[curr_idx]
  N_m1_proj[1] <- country_data$N_M1[curr_idx]
  N_m2_proj[1] <- country_data$N_M2[curr_idx]
  N_r_proj[1] <- country_data$N_R[curr_idx]
  N_proj[1] <- sum(N_y_proj[1], N_m1_proj[1], N_m2_proj[1], N_r_proj[1])
  dr_y[1] <- N_y_proj[1] / (N_m1_proj[1] + N_m2_proj[1])
  dr_r[1] <- N_r_proj[1] / (N_m1_proj[1] + N_m2_proj[1])
  
  # Run simulation
  for (i in 2:n) {
    N_y_proj[i] <- N_y_proj[i-1] + nu_t[i] * N_proj[i-1] - p_y_t[i] * N_y_proj[i-1] - FAT_y_t[i]
    N_m1_proj[i] <- N_m1_proj[i-1] + M_t[i] + p_y_t[i] * N_y_proj[i-1] - p_m1_t[i] * N_m1_proj[i-1] - FAT_m1_t[i]
    N_m2_proj[i] <- N_m2_proj[i-1] + p_m1_t[i] * N_m1_proj[i-1] - p_m2_t[i] * N_m2_proj[i-1] - FAT_m2_t[i]
    N_r_proj[i] <- N_r_proj[i-1] + p_m2_t[i] * N_m2_proj[i-1] - FAT_r_t[i]
    #Projections
    N_proj[i] <- sum(N_y_proj[i], N_m1_proj[i], N_m2_proj[i], N_r_proj[i])
    dr_y[i] <- N_y_proj[i] / (N_m1_proj[i] + N_m2_proj[i])
    dr_r[i] <- N_r_proj[i] / (N_m1_proj[i] + N_m2_proj[i])
  }
  
  tibble(
    Year = country_data$Year[curr_idx:end_idx],
    Country = country_data$Country[curr_idx:end_idx],
    N_proj, N_y_proj, N_m1_proj, N_m2_proj, N_r_proj,
    dr_y, dr_r
  )
}

### Get projections for N, DR_Y and DR_R----------------------------------------
projections <- df_plot %>%
  group_by(Country) %>%
  group_modify(~ run_projection(.x, curr_idx = 10)) %>%
  ungroup()

plot_data <- projections %>%
  left_join(
    df %>% select(Country, Year, TPopulation1July,D_UN,D_Y_UN,D_R_UN), 
    by = c("Country", "Year")
  )

### Validate projections for total population-------------------------------------

df_long <- plot_data %>%
  as_tibble() %>%
  pivot_longer(
    cols = c(TPopulation1July, N_proj),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    data_type = case_when(
      variable == "TPopulation1July" ~ "UN Projection",
      variable == "N_proj" ~ "Simulation Projection"
    )
  )

population_projections_plot <- ggplot(df_long, 
                                      aes(x = Year, y = value/1000, color = Country, linetype = data_type)) +
  geom_line(size = 1) +
  labs(title = "Total Population Projections (2024-2100)",
       y = "Population (in millions)",
       x = "Year") +
  scale_color_manual(values = c(blue_palette, "United States" = "red")) +
  scale_linetype_manual(
    values = c("UN Projection" = "solid", "Simulation Projection" = "dashed"),
    name = "Data Type"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  guides(color = guide_legend(title = "Country"))
population_projections_plot
ggsave("plots/model_projections_validation_population.pdf",population_projections_plot)

### Validate projections for dependency ratio-----------------------------------
plot_data$dr_y <- plot_data$dr_y*100
plot_data$dr_r <- plot_data$dr_r*100
plot_data$dr <- plot_data$dr_y + plot_data$dr_r

df_long <- plot_data %>%
  as_tibble() %>%
  pivot_longer(
    cols = c(dr, D_UN),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    data_type = case_when(
      variable == "D_UN" ~ "UN Projection",
      variable == "dr" ~ "Simulation Projection"
    )
  )

dependency_projections_plot <- ggplot(df_long, 
                                      aes(x = Year, y = value, color = Country, linetype = data_type)) +
  geom_line(size = 1) +
  labs(title = "Dependency Ratio Projections (2024-2100)",
       y = "Dependency Ratio",
       x = "Year") +
  scale_color_manual(values = c(blue_palette, "United States" = "red")) +
  scale_linetype_manual(
    values = c("UN Projection" = "solid", "Simulation Projection" = "dashed"),
    name = "Data Type"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  guides(color = guide_legend(title = "Country"))
dependency_projections_plot
ggsave("plots/model_projections_validation_dependency.pdf",dependency_projections_plot)


df_long <- plot_data %>%
  as_tibble() %>%
  pivot_longer(
    cols = c(D_Y_UN, D_R_UN, dr_y, dr_r),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    age_group = if_else(variable %in% c("D_Y_UN", "dr_y"), "Young", "Retired"),
    series_type = if_else(variable %in% c("D_Y_UN", "D_R_UN"), "UN Projections", "Simulation Projections"),
    legend_group = paste(Country, age_group, sep = " - ")
  )

custom_colors <- c(
  setNames(blue_palette, paste0(countries, " - Young")),
  setNames(red_palette, paste0(countries, " - Retired"))
)

dependency_projections_decomposed_plot <- ggplot(df_long, 
                          aes(x = Year, y = value, 
                              color = legend_group,
                              linetype = series_type)) +
  geom_line(size = 1) +
  labs(title = "Dependency Ratio Projections Decomposed (2024-2100)",
       y = "Value",
       x = "Year") +
  scale_color_manual(
    values = custom_colors,
    name = "Country & Age Group",
    breaks = c(paste(countries, "Young", sep = " - "),
               paste(countries, "Retired", sep = " - ")),
    guide = guide_legend(order = 1)
  ) +
  scale_linetype_manual(
    values = c("UN Projections" = "solid", "Simulation Projections" = "dashed"),
    name = "Series Type",
    guide = guide_legend(order = 2)
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  theme(legend.spacing.y = unit(0.5, "cm")) +
  guides(color = guide_legend(
    override.aes = list(linetype = "solid"),
    ncol = 1,
    byrow = TRUE
  ))
dependency_projections_decomposed_plot
ggsave("plots/model_projections_validation_dependency_decomposed.pdf",dependency_projections_decomposed_plot)



###Extras-------------------------------------------------------------------------
df_long <- df_plot %>%
  pivot_longer(cols = c(FAT_Y,FAT_M1,FAT_M2,FAT_R),
               names_to = "Age_group",
               values_to = "Value")

line_colors <- c(
  setNames(blue_palette, paste0(countries, "_FAT_Y")),
  setNames(green_palette, paste0(countries, "_FAT_M1")),
  setNames(orange_palette, paste0(countries, "_FAT_M2")),
  setNames(red_palette, paste0(countries, "_FAT_R"))
)
#Plot
ggplot(df_long, aes(x = Year, y = Value,
                    color = interaction(Country, Age_group, sep = "_"),
                    group = interaction(Country, Age_group, sep = "_"))) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Fatality Rates (1950-2100)",
       y = "Fatality",
       x = "Year") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  scale_color_manual(values = line_colors) +
  annotate("text", x = 2000, y = max(df_long$Value, na.rm = TRUE),
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_long$Value, na.rm = TRUE),
           label = "Projected", color = "black") +
  guides(color = guide_legend(title = "Country & Age Group"))



df_plot <- df_plot %>%
  pivot_longer(cols = c(p_y,p_m1,p_m2),
               names_to = "Age_group",
               values_to = "Value")

line_colors <- c(
  setNames(blue_palette, paste0(countries, "_p_y")),
  setNames(green_palette, paste0(countries, "_p_m1")),
  setNames(orange_palette, paste0(countries, "_p_m2"))
)
#Plot
ggplot(df_plot, aes(x = Year, y = Value,
                    color = interaction(Country, Age_group, sep = "_"),
                    group = interaction(Country, Age_group, sep = "_"))) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  labs(title = "Transition Probabilities (1950-2100)",
       y = "Probability",
       x = "Year") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  scale_color_manual(values = line_colors) +
  annotate("text", x = 2000, y = max(df_plot$Value, na.rm = TRUE),
           label = "Observed", color = "black") +
  annotate("text", x = 2050, y = max(df_plot$Value, na.rm = TRUE),
           label = "Projected", color = "black") +
  guides(color = guide_legend(title = "Country & Age Group"))

mortality_validation_plot <- ggplot(df_plot, aes(x = Year)) +
  geom_line(aes(y = FAT_Y, color = "Young"), linewidth = 1) +
  geom_line(aes(y = FAT_M1, color = "Working 1"), linewidth = 1) +
  geom_line(aes(y = FAT_M2, color = "Working 2"), linewidth = 1) +
  geom_line(aes(y = FAT_R, color = "Retired"), linewidth = 1) +
  geom_line(aes(y = FAT_R_model, color = "Retired"), linetype = "dashed", linewidth = 1) +
  facet_wrap(~ Country, ncol = 2, scales = "free_y") +
  labs(title = "Mortality Rates by Age Group",y = "Deaths",x = "Year",color = "Age Group") +
  scale_color_manual(
    values = c(
      "Young" = "black",
      "Working 1" = "blue",
      "Working 2" = "violet",
      "Retired" = "red"
    ),
    breaks = c("Young", "Working 1", "Working 2", "Retired")
  ) +
  scale_linetype_manual(
    values = c("solid", "solid", "solid", "solid", "dashed"),
    guide = "none"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )