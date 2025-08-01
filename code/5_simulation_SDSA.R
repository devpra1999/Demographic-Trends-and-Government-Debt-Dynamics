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

### Run model estimations and counterfactuals---------------------------------------------------------------------
source("code/4_model_estimation_new.R")
df_proj <- merge(df_output,df_debt, all.x = T)
df_proj <- df_proj %>%
  fill(PTR_US, .direction = "downup")
#mod_s_restricted <- mod_s_unrestricted
rm(list = setdiff(ls(), c("df", "df_proj","mod_y_restricted_OLS","mod_pi_US","mod_pi_restricted","mod_r_US","mod_r_DE","mod_r_restricted_OLS","mod_s_restricted")))
#df_proj <- df_proj %>% filter(Year > 1970)
### Set up parameters---------------------------------------------------------------------
curr_year <- 2024
curr_idx <- which(df_proj$Year==curr_year)
num_sim <- 1000
country_map <- c( "US" = "US","Germany" = "DE", "France" = "FR", "Italy" = "IT", 
                 "Netherlands" = "NL", "Spain" = "ES")

## Simulation (Optimized) -------------------------------------------------
# Coefficients
coefs_y <- coef(mod_y_restricted_OLS)
coefs_pi_US <- coef(mod_pi_US)
coefs_pi <- coef(mod_pi_restricted)
coef_r_US <- coef(mod_r_US)
coef_r_DE <- coef(mod_r_DE)
coefs_r <- coef(mod_r_restricted_OLS)
coefs_s <- coef(mod_s_restricted)
# Initialize projection matrices
country_names <- names(country_map)
n_proj <- nrow(df_proj) - curr_idx
matrix_template <- matrix(nrow = n_proj, ncol = num_sim)
dy_mat_list <- y_mat_list <- ygap_mat_list <- pi_mat_list <- g_mat_list <- ir_mat_list <- s_mat_list <- b_mat_list <- setNames(
  replicate(length(country_names), matrix_template, simplify = FALSE),
  country_names
)
# Error samples
e_y_df <- residuals(mod_y_restricted_OLS) # 1960-2100
e_pi_US_df <- residuals(mod_pi_US) #1968-2024
e_pi_df <- residuals(mod_pi_restricted) #1971-2024
#e_r_DE_df <- exp(fitted(mod_r_DE))*(exp(residuals(mod_r_DE)) - 1)# 1992-2024
e_r_US_df <- residuals(mod_r_US) #1968-2024
e_r_DE_df <- residuals(mod_r_DE) # 1992-2024
e_r_df <- residuals(mod_r_restricted_OLS) # 2000-2100
e_s_df <- residuals(mod_s_restricted) # 1991-2100
e_y_df <- e_y_df[41:65,] # 2000-2024
e_pi_US_df <- e_pi_US_df[33:57]
e_pi_df <- e_pi_df[30:54,]
e_r_US_df <- e_r_US_df[33:57]
e_r_DE_df <- e_r_DE_df[8:32] # 2000-2024
e_s_df <- e_s_df[10:34,] # 2000-2024
samples <- sapply(1:(num_sim*n_proj), function(x) sample(25, 1))
# Row indices
proj_rows <- (curr_idx+1):nrow(df_proj)
# Main Simulation Loop
for (n in 1:num_sim) {
  for (c in country_names) {
    dypot_init <- mean(na.omit(df_proj[(curr_idx-20):(curr_idx+2), paste0("dypot_", c)]))
    #Make initial vectors
    dy_vec <- y_vec <- ypot <- ygap_vec <- pi_vec <- numeric(n_proj)
    ir_vec <- s_vec <- b_vec <- spread_vec <- b_diff_vec <- numeric(n_proj)
    # Get initial values
    y_lag <- df_proj[curr_idx, paste0("y_", c)]
    ypot_lag <- df_proj[curr_idx, paste0("ypot_", c)]
    ygap_lag <- df_proj[curr_idx, paste0("ygap_", c)]
    pi_lag <- df_proj[curr_idx, paste0("pi_",c)]
    ir_lag <- df_proj[curr_idx, paste0("IR_", c)]
    b_lag <- df_proj[[paste0("b_", c)]][curr_idx]
    for (i in 1:n_proj) {
      e_y <- e_y_df[samples[(n-1)*n_proj + i],]
      e_pi_US <- e_pi_US_df[samples[(n-1)*n_proj + i]]
      e_pi <- e_pi_df[samples[(n-1)*n_proj + i],]
      e_r_US <- e_r_US_df[samples[(n-1)*n_proj + i]]
      e_r_DE <- e_r_DE_df[samples[(n-1)*n_proj + i]]
      e_r <- e_r_df[samples[(n-1)*n_proj + i],]
      e_s <- e_s_df[samples[(n-1)*n_proj + i],]
      idx <- proj_rows[i]
      # Project output growth
      dy_vec[i] <- coefs_y[paste0(c, "_dypot_", c)] * dypot_init +
        coefs_y[paste0(c, "_ygap_lag_", c)] * ygap_lag +
        coefs_y[paste0(c, "_D_", c)] * df_proj[idx, paste0("D_", c)] + 
        e_y[, c]
      # Project Level of output, potential output and output gap
      y_vec[i] <- y_lag + dy_vec[i]
      ypot[i] <- ypot_lag + dypot_init
      ygap_vec[i] <- y_vec[i] - ypot[i]
      if (c=="US"){
        #Project Inflation
        pi_vec[i] <- coefs_pi_US["pi_lag_US"] * pi_lag + 
          coefs_pi_US["PTR_US"] * df_proj$PTR_US[idx] +
          e_pi_US
        #Project Cost of debt
        ir_vec[i] <- coef_r_US["(Intercept)"] +
          coef_r_US["dypot_US"] * dypot_init +
          coef_r_US["PTR_US"] * df_proj$PTR_US[idx] +
          coef_r_US["MY_US"] * df_proj$MY_US[idx] +
          e_r_US
      } else if (c=="Germany"){
        #Project Inflation
        pi_vec[i] <- coefs_pi[paste0(c, "_pi_lag_", c)] * pi_lag +
          coefs_pi[paste0(c, "_pi_US")] * pi_mat_list[["US"]][i,n] + 
          e_pi[, c]
        ir_vec[i] <- coef_r_DE["(Intercept)"] + 
          coef_r_DE["IR_US"] * ir_mat_list[["US"]][i,n] +
          coef_r_DE["lag_IR_Germany"] * ir_lag +
          e_r_DE
      } else{
        #Project Inflation
        pi_vec[i] <- coefs_pi[paste0(c, "_pi_lag_", c)] * pi_lag +
          coefs_pi[paste0(c, "_pi_US")] * pi_mat_list[["US"]][i,n] + 
          e_pi[, c]
        #Make b_diff_lag
        if (i==1){
          b_diff_lag <- b_lag - df_proj$b_Germany[curr_idx]
        } else{
          b_diff_lag <- b_lag - b_mat_list[["Germany"]][i-1,n]
        }
        #Project spread
        spread_vec[i] <- coefs_r[paste0(c, "_(Intercept)")] + 
          coefs_r[paste0(c, "_b_diff_lag_", c)] * b_diff_lag +
          e_r[, c]
        #Project cost of debt
        ir_vec[i] <- spread_vec[i] + ir_mat_list[["Germany"]][i,n]
      }
      #Surplus
      s_vec[i] <- coefs_s[paste0(c, "_(Intercept)")] +
        coefs_s[paste0(c, "_ygap_", c)] * ygap_vec[i] +
        coefs_s[paste0(c, "_b_lag_", c)] * b_lag +
        #coefs_s[paste0(c, "_D_Y_", c)] * df_proj[[paste0("D_Y_", c)]][idx] +
        coefs_s[paste0(c, "_D_R_", c)] * df_proj[[paste0("D_R_", c)]][idx] +
        e_s[,c]
      #Debt
      b_vec[i] <- -s_vec[i] + 
        (1 + ir_vec[i])/(1 + dy_vec[i] + pi_vec[i]) * b_lag
      # Update lagged values for next iteration
      y_lag <- y_vec[i]
      ypot_lag <- ypot[i]
      ygap_lag <- ygap_vec[i]
      pi_lag <- pi_vec[i]
      ir_lag <- ir_vec[i]
      b_lag <- b_vec[i]
    }
    # Store results
    dy_mat_list[[c]][, n] <- dy_vec
    y_mat_list[[c]][, n] <- y_vec
    ygap_mat_list[[c]][, n] <- ygap_vec
    pi_mat_list[[c]][, n] <- pi_vec
    g_mat_list[[c]][, n] <- dy_vec + pi_vec
    ir_mat_list[[c]][, n] <- ir_vec
    s_mat_list[[c]][, n] <- s_vec
    b_mat_list[[c]][, n] <- b_vec
  }
}


### Plot for output simulations------------------------------------------------------
blue_palette <- brewer.pal(n = 6, name = "Blues")[6:2]   # light to dark blue
names(blue_palette) <- names(country_map)[1:5]
color_palette = c(blue_palette,"US" = "red")

plot_projection <- function(variable_name,
                            df_proj,
                            mat_list = NULL,
                            start_year = 2010,
                            end_year = 2050,
                            color_palette = NULL,
                            curr_idx = curr_idx) {

  # Determine the column naming pattern based on variable type
  if (variable_name == "ir") {
    df_prefix <- "IR_"
    mat_list_name <- "ir_mat_list"
  } else {
    df_prefix <- ifelse(variable_name %in% c("dy", "y", "ygap", "g", "pi", "b", "s"),
                        paste0(variable_name, "_"),
                        paste0(variable_name, "_"))
    mat_list_name <- paste0(variable_name, "_mat_list")
  }
  historical_data <- df_proj %>%
    filter(Year >= start_year & Year <= 2024) %>%
    select(Year, any_of(paste0(df_prefix, names(country_map)))) %>%
    pivot_longer(
      cols = -Year,
      names_to = "Country",
      names_prefix = df_prefix,
      values_to = "value"
    ) %>%
    mutate(
      mean = value,
      lower = value,
      upper = value
    )
  if (!is.null(mat_list)) {
    projected_data <- lapply(names(mat_list), function(country) {
      mat <- mat_list[[country]]
      data.frame(
        Year = 2025:2100,
        Country = country,
        mean = rowMeans(mat),
        sd = apply(mat, 1, sd),
        lower = rowMeans(mat) - 2*apply(mat, 1, sd),
        upper = rowMeans(mat) + 2*apply(mat, 1, sd)
      )
    }) %>%
      bind_rows() %>%
      filter(Year <= end_year)
  } else {
    projected_data <- NULL
  }
  combined_data <- bind_rows(historical_data, projected_data)
  combined_data <- combined_data %>%
    arrange(Country, Year)

  # Set y-axis label
  y_label <- case_when(
    variable_name == "dy" ~ "Real Growth Rate",
    variable_name == "g"  ~ "Nominal Growth Rate",
    variable_name == "pi" ~ "Inflation",
    variable_name == "ir" ~ "Nominal Cost of Debt",
    variable_name == "b"  ~ "Debt-to-GDP",
    variable_name == "s"  ~ "Primary Surplus",
    variable_name == "y"  ~ "Output",
    TRUE                  ~ "Output Gap"
  )
  # Capitalize variable name for title
  plot_title <- paste0(
    y_label,
    ": Historical Data and Projections"
  )
  # Create annotation data per country with max y for text placement
  annotation_df <- combined_data %>%
    group_by(Country) %>%
    summarise(y_pos = max(upper, na.rm = TRUE)) %>%
    mutate(
      hist_x = (start_year + 2024) / 2,
      proj_x = (2024 + end_year) / 2
    )
  # Build the plot
  p <- ggplot(combined_data, aes(x = Year, y = mean)) +
    geom_line(size = 1) +
    geom_ribbon(
      data = filter(combined_data, Year >= 2024),
      aes(ymin = lower, ymax = upper),
      inherit.aes = TRUE,
      alpha = 0.2,
      fill = "grey"
    ) +
    geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
    geom_text(data = annotation_df, aes(x = hist_x, y = y_pos, label = "Historical"),
              inherit.aes = FALSE, size = 3.5, color = "black") +
    geom_text(data = annotation_df, aes(x = proj_x, y = y_pos, label = "Projected"),
              inherit.aes = FALSE, size = 3.5, color = "black") +
    facet_wrap(~ Country, ncol = 2, scales = "free_y") +
    labs(
      title = plot_title,
      x = "Year",
      y = y_label
    ) +
    scale_x_continuous(breaks = seq(start_year, end_year, by = 5)) +
    theme_minimal()

  # Apply color palette if provided
  if (!is.null(color_palette)) {
    p <- p +
      scale_color_manual(values = color_palette) +
      scale_fill_manual(values = color_palette)
  }

  return(p)
}

library(ggplot2)
library(dplyr)
library(patchwork)

plot_country_projections <- function(df_proj, mat_lists, 
                                     start_year = 2010, end_year = 2050,
                                     countries = c("US", "Germany", "Netherlands", 
                                                   "France", "Spain", "Italy")) {
  
  # Create individual variable plots (without faceting)
  create_var_plot <- function(variable_name) {
    # Determine the column naming pattern
    df_prefix <- if (variable_name == "ir") "IR_" else paste0(variable_name, "_")
    
    # Historical data
    historical_data <- df_proj %>%
      filter(Year >= start_year & Year <= 2024) %>%
      select(Year, any_of(paste0(df_prefix, countries))) %>%
      pivot_longer(
        cols = -Year,
        names_to = "Country",
        names_prefix = df_prefix,
        values_to = "value"
      ) %>%
      mutate(mean = value, lower = value, upper = value)
    
    # Projected data
    if (!is.null(mat_lists[[paste0(variable_name, "_mat_list")]])) {
      projected_data <- lapply(countries, function(country) {
        mat <- mat_lists[[paste0(variable_name, "_mat_list")]][[country]]
        data.frame(
          Year = 2025:2100,
          Country = country,
          mean = rowMeans(mat),
          sd = apply(mat, 1, sd),
          lower = rowMeans(mat) - 2*apply(mat, 1, sd),
          upper = rowMeans(mat) + 2*apply(mat, 1, sd)
        )
      }) %>% bind_rows() %>% filter(Year <= end_year)
    } else {
      projected_data <- NULL
    }
    
    combined_data <- bind_rows(historical_data, projected_data) %>%
      arrange(Country, Year)
    
    # Set y-axis label
    y_label <- case_when(
      variable_name == "dy" ~ "Real Growth Rate",
      variable_name == "ir" ~ "Nominal Cost of Debt",
      variable_name == "b"  ~ "Debt-to-GDP",
      variable_name == "s"  ~ "Primary Surplus",
      TRUE ~ variable_name
    )
    
    ggplot(combined_data, aes(x = Year, y = mean)) +
      geom_line(size = 1) +
      geom_ribbon(
        data = filter(combined_data, Year >= 2024),
        aes(ymin = lower, ymax = upper),
        alpha = 0.2,
        fill = "grey"
      ) +
      geom_vline(xintercept = 2024, linetype = "dashed") +
      labs(y = y_label) +
      scale_x_continuous(breaks = seq(start_year, end_year, by = 5)) +
      theme_minimal() +
      theme(legend.position = "none")
  }
  
  # Create all variable plots for all countries
  var_plots <- list(
    dy = create_var_plot("dy"),
    ir = create_var_plot("ir"),
    s = create_var_plot("s"),
    b = create_var_plot("b")
  )
  
  # Function to create a country page by filtering the variable plots
  create_country_page <- function(country) {
    # Filter each variable plot for the current country
    country_plots <- lapply(var_plots, function(p) {
      p$data <- p$data %>% filter(Country == country)
      p + ggtitle(NULL)  # Remove individual titles
    })
    
    # Combine into 2x2 grid
    (country_plots$dy + ggtitle("Real Growth Rate") | 
        country_plots$ir + ggtitle("Nominal Cost of Debt")) /
      (country_plots$s + ggtitle("Primary Surplus") | 
         country_plots$b + ggtitle("Debt-to-GDP")) +
      plot_annotation(title = country,
                      theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))
  }
  
  # Create and return all country pages
  country_plots <- lapply(countries, create_country_page)
  names(country_plots) <- countries
  
  return(country_plots)
}

# Usage example:
mat_lists <- list(dy_mat_list = dy_mat_list,
                 ir_mat_list = ir_mat_list,
                 s_mat_list = s_mat_list,
                 b_mat_list = b_mat_list)

country_plots <- plot_country_projections(df_proj, mat_lists)
# 
# To access individual country plots:
country_plots$US  # US 2x2 plot
country_plots$Germany  # Germany 2x2 plot
# etc.

dy_plot <- plot_projection("dy", df_proj, dy_mat_list, end_year = 2050, color_palette = c(blue_palette,"US" = "red"))
dy_plot

pi_plot <- plot_projection("pi", df_proj, pi_mat_list, end_year = 2050)
pi_plot

g_plot <- plot_projection("g", df_proj, g_mat_list, end_year = 2050)
g_plot

ir_plot <- plot_projection("ir", df_proj, ir_mat_list, end_year = 2050)
ir_plot

b_plot <- plot_projection("b", df_proj, b_mat_list, end_year = 2050)
b_plot

s_plot <- plot_projection("s", df_proj, s_mat_list, end_year = 2050)
s_plot

ygap_plot <- plot_projection("ygap", df_proj, ygap_mat_list, end_year = 2050)
ygap_plot

ggsave("plots/sim_debt_ratio_big5_oos.pdf",b_plot)
ggsave("plots/sim_real_growth_big5_oos.pdf",dy_plot)
ggsave("plots/sim_nominal_growth_big5_oos.pdf",g_plot)
ggsave("plots/sim_cost_of_debt_big5_oos.pdf",ir_plot)
ggsave("plots/sim_inflation_big5_oos.pdf",pi_plot)
ggsave("plots/sim_surplus_big5_oos.pdf",s_plot)
ggsave("plots/sim_output_gap_big5_oos.pdf",ygap_plot)