# Main function to run all projections
run_all_projections <- function(df_demo, df_proj,
                                mod_y_restricted_OLS, mod_r_DE, 
                                mod_r_restricted_OLS, mod_s_restricted_OLS,
                                mod_pi_US, mod_pi_restricted, #change sofia
                                mig_per_thousand = "baseline", CBR_cf = "baseline", 
                                curr_year = 2024, demo_year = 2024) {
  country_map <- c("Germany" = "DE", "France" = "FR", "Italy" = "IT", 
                   "Netherlands" = "NL", "Spain" = "ES", "US" = "US")
  country_names <- names(country_map)
  # Run demographic projections
  projections_alt <- run_demographic_projections(df_demo, mig_per_thousand, CBR_cf, demo_year)
  df_proj <- merge_demographic_projections(df_proj, projections_alt)
  # Set the databases
  curr_idx <- which(df_proj$Year == curr_year)
  df_historical <- df_proj[1:curr_idx,]
  df_projected <- df_proj[curr_idx:nrow(df_proj),]
  #Get coefficients
  coefs_y <- coef(mod_y_restricted_OLS)
  coef_r_DE <- coef(mod_r_DE)
  coefs_r_US <- coef(mod_r_US)
  coefs_r <- coef(mod_r_restricted_OLS)
  coefs_s <- coef(mod_s_restricted_OLS)
  coefs_pi_US <- coef(mod_pi_US) #change sofia
  coefs_pi <- coef(mod_pi_restricted) #change sofia
  # for (c in country_names){
  #   df_proj <- set_constant_potential_growth(df_proj, c)
  #   # Run output simulations
  #   # Inputs - Number of years of simulation, Initial value of y, ypot and vector of dypot, D, coefficients of model and errors (optional for SDSA)
  #   # Outputs - Vectors of y, dy and ygap for country
  #   L <- run_output_simulations(n_proj = nrow(df_projected),
  #                               y_lag = df_proj[curr_idx, paste0("y_", c)],
  #                               ypot_lag = df_proj[curr_idx, paste0("ypot_", c)],
  #                               dypot = df_projected[,paste0("dypot_", c)],
  #                               D = df_projected[,paste0("D_", c)],
  #                               coefs_y = coefs_y,
  #                               e_y_vec = rep(0,nrow(df_projected)))
  #   df_projected[,paste0("y_", c)] <- L$y
  #   df_projected[,paste0("dy_", c)] <- L$dy
  #   df_projected[,paste0("ygap_", c)] <- L$ygap
  #   df_projected[,paste0("ypot_", c)] <- L$ypot
  #   # Run debt simulations
  #   # Inputs - Vectors of dy, ygap, dypot, D_Y, D_R, and initial value of b, coefficients for r_DE, spreads and surplus.
  #   # Outputs - Vectors of ir, s and b
  #   L <- run_output_simulations(n_proj = nrow(df_projected),
  #                               y_lag = df_proj[curr_idx, paste0("y_", c)],
  #                               ypot_lag = df_proj[curr_idx, paste0("ypot_", c)],
  #                               dypot = df_projected[,paste0("dypot_", c)],
  #                               D = df_projected[,paste0("D_", c)],
  #                               coefs_y = coefs_y,
  #                               e_y_vec = rep(0,n_proj))
  # }
  
  df_proj <- run_output_simulations(df_proj, mod_y_restricted_OLS, curr_year)
  for (c in country_names) {
    df_proj <- project_country_inflation(df_proj, c, coefs_pi, coefs_pi_US, curr_idx)
  } #change sofia
  df_proj <- run_debt_simulations(df_proj, mod_r_DE, mod_r_restricted_OLS, mod_s_restricted_OLS, curr_year)
  #change sofia2
  for (c in country_names) {
    df_proj[[paste0("g_", c)]] <- df_proj[[paste0("dy_", c)]] + df_proj[[paste0("pi_", c)]]
  } 
  
  # Process and save results
  L <- save_projections_results(df_proj, mig_per_thousand,CBR_cf,curr_year)
  
  return(L)
}

# Demographic projections
run_demographic_projections <- function(df_demo, mig_per_thousand,CBR_cf, curr_year) {
  curr_idx = which(df_demo$Year==curr_year)[1]
  projections_alt <- df_demo %>%
    group_by(Country) %>%
    group_modify(~ run_projection(.x, curr_idx = curr_idx, migration = mig_per_thousand, CBR = CBR_cf)) %>%
    ungroup()
  
  projections_alt$dr <- projections_alt$dr_y + projections_alt$dr_r
  colnames(projections_alt) <- c("Country", "Year", "D_Y_alt", "D_R_alt", "MY_alt", "CBR_alt", "M_alt", "D_alt")
  # "N_alt", "N_Y_alt", "N_M1_alt", "N_M2_alt", "N_R_alt",
  projections_alt$M_alt[is.na(projections_alt$M_alt)] <- df_demo$M[is.na(projections_alt$M_alt)]
  projections_alt %>%
    pivot_wider(names_from = Country, values_from = -c(Country, Year))
}

merge_demographic_projections <- function(df_proj, projections_alt) {
  merge(df_proj, projections_alt, all.x = TRUE)
}


### Function for demographic projections for each country for migration scenarios
run_projection <- function(country_data, curr_idx = 75,migration = "baseline", CBR = "baseline") {
  #curr_idx=75 for 2024
  end_idx <- nrow(country_data)
  n <- end_idx - curr_idx + 1
  # Extract calibrated variables
  # p_y_t <- country_data$p_y[curr_idx:end_idx]
  # p_m1_t <- country_data$p_m1[curr_idx:end_idx]
  # p_m2_t <- country_data$p_m2[curr_idx:end_idx]
  p_y_t <- country_data$p_y[curr_idx:end_idx]
  p_m1_t <- country_data$p_m1[curr_idx:end_idx]
  p_m2_t <- country_data$p_m2[curr_idx:end_idx]
  if (CBR=="baseline"){
    nu_t <- country_data$nu_t[curr_idx:end_idx]
  } else if (CBR=="last"){
    nu_t <- rep(country_data$nu_t[curr_idx],n)
  } else {
    nu_t <- rep(CBR/1000,n)
  }
  if (migration=="baseline"){
    M <- country_data$CNMR[curr_idx:end_idx]
  } else {
    M <- rep(migration,n)
  }
  FAT_y_t <- country_data$FAT_Y[curr_idx:end_idx]
  FAT_m1_t <- country_data$FAT_M1[curr_idx:end_idx]
  FAT_m2_t <- country_data$FAT_M2[curr_idx:end_idx]
  FAT_r_t <- country_data$FAT_R[curr_idx:end_idx]
  mu_y_t <- country_data$mu_Y[curr_idx:end_idx]
  mu_m1_t <- country_data$mu_M1[curr_idx:end_idx]
  mu_m2_t <- country_data$mu_M2[curr_idx:end_idx]
  mu_r_t <- country_data$mu_R[curr_idx:end_idx]
  # Initialize vectors
  N_proj <- numeric(n)
  dr_y <- numeric(n)
  dr_r <- numeric(n)
  my <- numeric(n)
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
  my[1] <- N_m2_proj[1]/N_m1_proj[1]
  # Run simulation
  for (i in 2:n) {
    N_y_proj[i] <- N_y_proj[i-1] + nu_t[i] * N_proj[i-1] - p_y_t[i] * N_y_proj[i-1] - mu_y_t[i] * N_y_proj[i-1]
    N_m1_proj[i] <- N_m1_proj[i-1] + M[i]*N_proj[i-1]/1000 + p_y_t[i] * N_y_proj[i-1] - p_m1_t[i] * N_m1_proj[i-1] - mu_m1_t[i] * N_m1_proj[i-1]
    N_m2_proj[i] <- N_m2_proj[i-1] + p_m1_t[i] * N_m1_proj[i-1] - p_m2_t[i] * N_m2_proj[i-1] - mu_m2_t[i] * N_m2_proj[i-1]
    N_r_proj[i] <- N_r_proj[i-1] + p_m2_t[i] * N_m2_proj[i-1] - mu_r_t[i] * N_r_proj[i-1]
    #Projections
    N_proj[i] <- sum(N_y_proj[i], N_m1_proj[i], N_m2_proj[i], N_r_proj[i])
    dr_y[i] <- N_y_proj[i] / (N_m1_proj[i] + N_m2_proj[i])
    dr_r[i] <- N_r_proj[i] / (N_m1_proj[i] + N_m2_proj[i])
    my[i] <- N_m2_proj[i]/N_m1_proj[i]
  }

  tibble(
    Year = country_data$Year,#[curr_idx:end_idx]
    Country = country_data$Country,#[curr_idx:end_idx]
    #N_proj, N_y_proj, N_m1_proj, N_m2_proj, N_r_proj,
    dr_y = c(country_data$D_Y_UN[1:curr_idx-1]/100,dr_y),
    dr_r = c(country_data$D_R_UN[1:curr_idx-1]/100,dr_r),
    my = c(country_data$MY[1:curr_idx-1],my),
    nu_t = c(country_data$nu_t[1:curr_idx-1],nu_t),
    M = c(country_data$CNMR[1:curr_idx-1],M)
  )
}

set_constant_potential_growth <- function(df_proj, country) {
  idx <- which(df_proj$Year == 2024)
  df_proj[[paste0("dypot_", country)]][df_proj$Year > 2024] <- 
    mean(df_proj[[paste0("dypot_", country)]][(idx-20):(idx+2)],na.rm = T)
  df_proj
}

# Output simulations
run_output_simulations <- function(df_proj, mod_y_restricted_OLS,curr_year) {
  curr_idx <- which(df_proj$Year == curr_year)
  country_map <- c("France" = "FR", "Germany" = "DE", "Italy" = "IT",
                   "Netherlands" = "NL", "Spain" = "ES", "US" = "US")

  coefs_y <- coef(mod_y_restricted_OLS)

  # Set constant potential growth rates for projection period
  for (c in names(country_map)) {
    df_proj <- set_constant_potential_growth(df_proj, c)
  }

  # Run output projections for each country
  for (c in names(country_map)) {
    df_proj <- project_country_output(df_proj, c, coefs_y, curr_idx)
  }

  return(df_proj)
}

project_country_output <- function(df_proj, country, coefs_y, curr_idx) {
  for (idx in (curr_idx+1):nrow(df_proj)) {
    # Projections with dependency in model
    df_proj[idx, paste0("dy_", country)] <-
      coefs_y[paste0(country, "_dypot_", country)] * df_proj[idx, paste0("dypot_", country)] +
      coefs_y[paste0(country, "_ygap_lag_", country)] * df_proj[idx-1, paste0("ygap_", country)] +
      coefs_y[paste0(country, "_D_", country)] * df_proj[idx, paste0("D_alt_", country)]

    # Level of output, potential output and output gap
    df_proj[idx, paste0("y_", country)] <- df_proj[idx-1, paste0("y_", country)] + df_proj[idx, paste0("dy_", country)]
    df_proj[idx, paste0("ypot_", country)] <- df_proj[idx-1, paste0("ypot_", country)] + df_proj[idx, paste0("dypot_", country)]
    df_proj[idx, paste0("ygap_", country)] <- df_proj[idx, paste0("y_", country)] - df_proj[idx, paste0("ypot_", country)]
  }
  df_proj
}

#change sofia
project_country_inflation <- function(df_proj, country, coefs_pi, coefs_pi_US, curr_idx) {
  for (idx in (curr_idx+1):nrow(df_proj)) {
    if (country == "US") {
      df_proj[idx, paste0("pi_", country)] <- 
        coefs_pi_US["pi_lag_US"] * df_proj[idx - 1, paste0("pi_", country)] +
        coefs_pi_US["PTR_US"] * df_proj[idx, "PTR_US"]
    } else {
      df_proj[idx, paste0("pi_", country)] <- 
        coefs_pi[paste0(country, "_pi_lag_", country)] * df_proj[idx - 1, paste0("pi_", country)] +
        coefs_pi[paste0(country, "_pi_US")] * df_proj[idx, "pi_US"]
    }
  }
  return(df_proj)
}


# run_output_simulations <- function(n_proj, y_lag, ypot_lag, dypot, D, coefs_y, e_y_vec) {
#   ygap_lag <- y_lag - ypot_lag
#   dy_vec <- y_vec <- ypot <- ygap_vec <- n_proj
#   for (i in 1:n_proj) {
#     dy_vec[i] <- coefs_y[paste0(c, "_dypot_", c)] * dypot[i] +
#       coefs_y[paste0(c, "_ygap_lag_", c)] * ygap_lag +
#       coefs_y[paste0(c, "_D_", c)] * df_proj[i, paste0("D_", c)] + 
#       e_y_vec[i]
#     # Project Level of output, potential output and output gap
#     y_vec[i] <- y_lag + dy_vec[i]
#     ypot[i] <- ypot_lag + dypot[i]
#     ygap_vec[i] <- y_vec[i] - ypot[i]
#   }
#   return(list(y = y_vec, dy = dy_vec, ygap = ygap_vec, ypot = ypot))
# }

# Debt simulations
run_debt_simulations <- function(df_proj, mod_r_DE, mod_r_restricted_OLS, mod_s_restricted_OLS,curr_year) {
  curr_idx <- which(df_proj$Year == curr_year)
  country_list <- c("France", "Italy", "Netherlands", "Spain")
  # Project US first
  df_proj <- project_US_debt(df_proj, mod_r_US, mod_s_restricted_OLS, curr_idx)
  # Project Germany second
  df_proj <- project_germany_debt(df_proj, mod_r_DE, mod_s_restricted_OLS, curr_idx)
  # Project other countries
  for (c in country_list) {
    df_proj[curr_idx-1, paste0("b_diff_", c)] <- df_proj[curr_idx-1, paste0("b_", c)] - df_proj$b_Germany[curr_idx-1]
    df_proj[curr_idx, paste0("b_diff_", c)] <- df_proj[curr_idx-1, paste0("b_", c)] - df_proj$b_Germany[curr_idx]
    df_proj <- project_country_debt(df_proj, c, mod_r_restricted_OLS, mod_s_restricted_OLS, curr_idx)
  }
  return(df_proj)
}


project_US_debt <- function(df_proj, mod_r_US, mod_s_restricted_OLS, curr_idx) {
  coef_r_US <- coef(mod_r_US)
  coefs_s <- coef(mod_s_restricted_OLS)
  c <- "US"
  # Project interest rates
  df_proj$IR_US[(curr_idx+1):nrow(df_proj)] <- coef_r_US["(Intercept)"] + 
    coef_r_US["dypot_US"] * df_proj$dypot_US[(curr_idx+1):nrow(df_proj)] +
    coef_r_US["PTR_US"] * df_proj$PTR_US[(curr_idx+1):nrow(df_proj)] + 
    coef_r_US["MY_US"] * df_proj$MY_alt_US[(curr_idx+1):nrow(df_proj)]
  # Project debt
  for (idx in (curr_idx+1):nrow(df_proj)) {
    df_proj[idx, paste0("s_", c)] <- 
      coefs_s[paste0(c, "_(Intercept)")] +
      coefs_s[paste0(c, "_ygap_", c)] * df_proj[idx, paste0("ygap_", c)] +
      coefs_s[paste0(c, "_b_lag_", c)] * df_proj[idx-1, paste0("b_", c)] +
      #coefs_s[paste0(c, "_D_Y_", c)] * df_proj[idx, paste0("D_Y_alt_", c)] +
      coefs_s[paste0(c, "_D_R_", c)] * df_proj[idx, paste0("D_R_alt_", c)]
    df_proj$b_US[idx] <- -df_proj$s_US[idx] +
      df_proj$b_US[idx-1] * (1 + df_proj$IR_US[idx]) / (1 + df_proj$dy_US[idx] + 0.02)
  }
  return(df_proj)
}

project_germany_debt <- function(df_proj, mod_r_DE, mod_s_restricted_OLS, curr_idx) {
  coef_r_DE <- coef(mod_r_DE)
  coefs_s <- coef(mod_s_restricted_OLS)
  c <- "Germany"
  # Project interest rates
  df_proj$IR_Germany[(curr_idx+1):nrow(df_proj)] <- coef_r_DE["(Intercept)"] +
          coef_r_DE["IR_US"] * df_proj$IR_US[(curr_idx+1):nrow(df_proj)] +
          coef_r_DE["lag_IR_Germany"] * df_proj$IR_Germany[curr_idx:(nrow(df_proj)-1)]
  for (idx in (curr_idx+1):nrow(df_proj)) {
    df_proj$IR_Germany[idx] <- coef_r_DE["(Intercept)"] +
      coef_r_DE["IR_US"] * df_proj$IR_US[idx] +
      coef_r_DE["lag_IR_Germany"] * df_proj$IR_Germany[idx-1]
  }  
  # Project debt
  for (idx in (curr_idx+1):nrow(df_proj)) {
    df_proj[idx, paste0("s_", c)] <- 
      coefs_s[paste0(c, "_(Intercept)")] +
      coefs_s[paste0(c, "_ygap_", c)] * df_proj[idx, paste0("ygap_", c)] +
      coefs_s[paste0(c, "_b_lag_", c)] * df_proj[idx-1, paste0("b_", c)] +
      #coefs_s[paste0(c, "_D_Y_", c)] * df_proj[idx, paste0("D_Y_alt_", c)] +
      coefs_s[paste0(c, "_D_R_", c)] * df_proj[idx, paste0("D_R_alt_", c)]
    df_proj$b_Germany[idx] <- -df_proj$s_Germany[idx] +
      df_proj$b_Germany[idx-1] * (1 + df_proj$IR_Germany[idx]) / (1 + df_proj$dy_Germany[idx] + 0.02)
  }
  return(df_proj)
}

project_country_debt <- function(df_proj, country, mod_r_restricted_OLS, mod_s_restricted_OLS, curr_idx) {
  coefs_r <- coef(mod_r_restricted_OLS)
  coefs_s <- coef(mod_s_restricted_OLS)
  c <- country
  for (idx in (curr_idx+1):nrow(df_proj)) {
    # Primary surplus
    df_proj[idx, paste0("s_", c)] <- 
      coefs_s[paste0(c, "_(Intercept)")] +
      coefs_s[paste0(c, "_ygap_", c)] * df_proj[idx, paste0("ygap_", c)] +
      coefs_s[paste0(c, "_b_lag_", c)] * df_proj[idx-1, paste0("b_", c)] +
      #coefs_s[paste0(c, "_D_Y_", c)] * df_proj[idx, paste0("D_Y_alt_", c)] +
      coefs_s[paste0(c, "_D_R_", c)] * df_proj[idx, paste0("D_R_alt_", c)]
    
    # Spread and interest rate
    df_proj[idx, paste0("Spread_", c)] <- 
      coefs_r[paste0(c, "_(Intercept)")] +
      coefs_r[paste0(c, "_b_diff_lag_", c)] * df_proj[idx-1, paste0("b_diff_", c)]
    df_proj[idx, paste0("IR_", c)] <- df_proj[idx, paste0("Spread_", c)] + df_proj[idx, "IR_Germany"]
    
    # Debt dynamics
    df_proj[idx, paste0("b_", c)] <- -df_proj[idx, paste0("s_", c)] +
      (1 + df_proj[idx, paste0("IR_", c)]) / (1 + df_proj[idx, paste0("dy_", c)] + 0.02) * 
      df_proj[idx-1, paste0("b_", c)]
    
    # Debt difference with Germany
    df_proj[idx, paste0("b_diff_", c)] <- log(df_proj[idx, paste0("b_", c)]) - log(df_proj[idx, "b_Germany"])
  }
  return(df_proj)
}

# Saving results
save_projections_results <- function(df_proj, mig_per_thousand, CBR_cf, curr_year) {
  country_map <- c("France" = "FR", "Germany" = "DE", "Italy" = "IT",
                   "Netherlands" = "NL", "Spain" = "ES", "US" = "US")
  blue_palette <- brewer.pal(n = 6, name = "Blues")[6:2]
  blue_palette <- c(blue_palette,"red")
  names(blue_palette) <- names(country_map)
  df_proj_long <- df_proj %>%
    select(Year, starts_with("b_"), starts_with("s_"), starts_with("dy_"), starts_with("IR_"),
           starts_with("pi_"), starts_with("g_"), #change sofia2
           starts_with("ygap_"), starts_with("D_Y_alt_"), starts_with("D_R_alt_"), starts_with("MY_alt_"),
           starts_with("CBR_alt_"), starts_with("M_alt_")) %>%
    pivot_longer(
      cols = -Year,
      names_to = c(".value", "Country"),
      names_pattern = "(.*)_(.*)"
    )
  df_proj_long$Country <- factor(df_proj_long$Country, levels = names(blue_palette))
  output_filename <- paste0("data/simulations/migration_", mig_per_thousand,
                            "_CBR_", CBR_cf,
                            "_year_",curr_year,
                            "_projections.rda")
  projections <- df_proj_long %>% 
    select(Country, Year, b, s, dy, IR, pi, g, ygap, D_Y_alt, D_R_alt, MY_alt, CBR_alt, M_alt) #change sofia2
  save(projections, file = output_filename)
  return(list(projections,df_proj))
}


# Modified reshape_actual_data function with year filtering
reshape_actual_data <- function(df_proj, variable, projection_years) {
  actual_var <- gsub("_alt$", "", variable)  # Remove "_alt" suffix if present
  if (actual_var=="M"){
    actual_var = "CNMR"
  }
  countries <- c("France","Germany","Italy","Netherlands","Spain","US")
  var_cols <- paste0(actual_var, "_", countries)
  df_long <- df_proj %>%
    select(Year, all_of(var_cols)) %>%
    filter(Year %in% projection_years) %>%  # Keep only years present in projections
    pivot_longer(
      cols = -Year,
      names_to = "Country",
      values_to = "Value"
    ) %>%
    mutate(
      Country = gsub(paste0("^", actual_var, "_"), "", Country),
      Scenario = "actual"
    )
  return(df_long)
}

# Then modify your plotting function to include actual data
plot_scenario_comparison <- function(data, variable, scenarios, scenario_names = NULL, 
                                     actual_data = NULL, curr_year = 2024) {
  valid_vars <- c("b", "s", "dy", "IR", "g", "ygap","D_Y_alt","D_R_alt","MY_alt","CBR_alt", "M_alt", "pi")
  if (!variable %in% valid_vars) {
    stop("Invalid variable. Please choose one of: ", paste(valid_vars, collapse = ", "))
  }
  # Default scenario names if not provided
  if (is.null(scenario_names)) {
    scenario_names <- setNames(
      gsub("_", " ", gsub("^_", "", scenarios)),
      scenarios
    )
  } else {
    if (length(scenario_names) != length(scenarios)) {
      stop("scenario_names must have same length as scenarios")
    }
    scenario_names <- setNames(scenario_names, scenarios)
  }
  plot_data <- data %>% 
    select(Country, Year, starts_with(paste0(variable, "_"))) %>%
    pivot_longer(
      cols = -c(Country, Year),
      names_to = "Scenario",
      values_to = "Value"
    ) %>%
    mutate(
      Scenario = str_remove(Scenario, paste0("^", variable, "_")),
      Scenario = factor(Scenario, levels = c("actual", scenarios))
    ) %>%
    filter(Scenario %in% scenarios)
  if (!is.null(actual_data)) {
    proj_years <- unique(plot_data$Year)
    proj_years <- (min(proj_years)-10):max(proj_years)
    actual_plot_data <- reshape_actual_data(actual_data, variable, proj_years)
    plot_data <- bind_rows(plot_data, actual_plot_data)
  }
  all_scenarios <- c("actual", scenarios)
  line_types <- setNames(
    c("solid", rep("dashed", length(scenarios))),
    all_scenarios
  )
  line_colors <- setNames(
    c("black", "red", brewer.pal(max(3, length(scenarios)), "Set1")[2:length(scenarios)]),
    all_scenarios
  )

  line_sizes <- setNames(
    c(1.2, rep(1, length(scenarios))),
    all_scenarios
  )
  
  p <- ggplot(plot_data, aes(x = Year, y = Value, color = Scenario, 
                             linetype = Scenario, size = Scenario)) +
    geom_line() +
    facet_wrap(~ Country, ncol = 2, scales = "free_y") +
    geom_vline(xintercept = curr_year, linetype = "dotted", color = "black") +
    labs(
      title = paste("Comparison of", 
                    switch(variable,
                           "b" = "Debt Ratio",
                           "s" = "Primary Surplus/GDP",
                           "dy" = "Real Growth",
                           "g" = "Nominal Growth",
                           "IR" = "Nominal Cost of Debt",
                           "ygap" = "Output Gap",
                           "D_Y_alt" = "Young Dependency",
                           "D_R_alt" = "Retired Dependency",
                           "CBR_alt" = "Birth Rates",
                           "M_alt" = "Migration Rates"),
                    "Across Scenarios"),
      y = switch(variable,
                 "b" = "Debt Ratio (% of GDP)",
                 "s" = "Primary Surplus (% of GDP)",
                 "dy" = "Real Growth Rate",
                 "g" = "Nominal Growth",
                 "IR" = "Nominal Interest Rate",
                 "ygap" = "Output Gap (% of potential GDP)",
                 "D_Y_alt" = "Young Dependency Ratio",
                 "D_R_alt" = "Retired Dependency Ratio",
                 "CBR_alt" = "Birth Rate (per 1000)",
                 "M_alt" = "Migration Rate (per 1000)"),
      color = "Scenario",
      linetype = "Scenario"
    ) +
    scale_color_manual(
      values = line_colors,
      labels = c("Actual" = "Actual", scenario_names)
    ) +
    scale_linetype_manual(
      values = line_types,
      labels = c("Actual" = "Actual", scenario_names)
    ) +
    scale_size_manual(
      values = line_sizes,
      guide = "none"  # Hide size from legend
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
  return(p)
}





# plot_scenario_comparison <- function(data, variable, scenarios, scenario_names = NULL, curr_year = 2024) {
#   # Check if variable is valid
#   valid_vars <- c("b", "s", "dy", "IR", "ygap","D_Y_alt","D_R_alt","MY_alt","CBR_alt", "M_alt")
#   if (!variable %in% valid_vars) {
#     stop("Invalid variable. Please choose one of: ", paste(valid_vars, collapse = ", "))
#   }
#   
#   # Check if scenarios are valid
#   if (!"baseline" %in% scenarios) {
#     stop("Baseline scenario must be included in scenarios vector")
#   }
#   
#   # Default scenario names if not provided
#   if (is.null(scenario_names)) {
#     scenario_names <- setNames(
#       gsub("_", " ", gsub("^_", "", scenarios)),
#       scenarios
#     )
#   } else {
#     if (length(scenario_names) != length(scenarios)) {
#       stop("scenario_names must have same length as scenarios")
#     }
#     scenario_names <- setNames(scenario_names, scenarios)
#   }
#   
#   # Prepare plot data
#   plot_data <- data %>% 
#     select(Country, Year, starts_with(paste0(variable, "_"))) %>%
#     pivot_longer(
#       cols = -c(Country, Year),
#       names_to = "Scenario",
#       values_to = "Value"
#     ) %>%
#     mutate(
#       Scenario = str_remove(Scenario, paste0("^", variable, "_")),
#       Scenario = factor(Scenario, levels = scenarios)
#     ) %>%
#     filter(Scenario %in% scenarios)
#   line_types <- setNames(
#     c("solid", rep("dashed", length(scenarios) - 1)),
#     scenarios
#   )
#   line_colors <- setNames(
#     c("black", brewer.pal(max(3, length(scenarios)), "Set1")[2:length(scenarios)]),
#     scenarios
#   )
#   p <- ggplot(plot_data, aes(x = Year, y = Value, color = Scenario, linetype = Scenario)) +
#     geom_line(linewidth = 1) +
#     facet_wrap(~ Country, ncol = 2, scales = "free_y") +
#     geom_vline(xintercept = curr_year, linetype = "dotted", color = "black") +
#     labs(
#       title = paste("Comparison of", 
#                     switch(variable,
#                            "b" = "Debt Ratio",
#                            "s" = "Primary Surplus/GDP",
#                            "dy" = "Real Growth",
#                            "IR" = "Nominal Cost of Debt",
#                            "ygap" = "Output Gap",
#                            "D_Y_alt" = "Young Dependency",
#                            "D_R_alt" = "Retired Dependency",
#                            "CBR_alt" = "Birth Rates",
#                            "M_alt" = "Migration Rates"),
#                     "Across Scenarios"),
#       y = switch(variable,
#                  "b" = "Debt Ratio (% of GDP)",
#                  "s" = "Primary Surplus (% of GDP)",
#                  "dy" = "Real Growth Rate",
#                  "IR" = "Nominal Interest Rate",
#                  "ygap" = "Output Gap (% of potential GDP)",
#                  "D_Y_alt" = "Young Dependency Ratio",
#                  "D_R_alt" = "Retired Dependency Ratio",
#                  "CBR_alt" = "Birth Rate (per 1000)",
#                  "M_alt" = "Migration Rate (per 1000)"),
#       color = "Scenario",
#       linetype = "Scenario"
#     ) +
#     scale_color_manual(
#       values = line_colors,
#       labels = scenario_names
#     ) +
#     scale_linetype_manual(
#       values = line_types,
#       labels = scenario_names
#     ) +
#     theme_minimal() +
#     theme(
#       legend.position = "bottom",
#       plot.title = element_text(hjust = 0.5)
#     )
#   return(p)
# }


# projections_alt <- df_demo %>%
#   group_by(Country) %>%
#   group_modify(~ run_projection(.x, curr_idx = 75, migration = mig_per_thousand)) %>%
#   ungroup()
# projections_alt$dr <- projections_alt$dr_y+projections_alt$dr_r
# colnames(projections_alt) <- c("Country","Year","N_alt","N_Y_alt","N_M1_alt","N_M2_alt",
#                            "N_R_alt","D_Y_alt","D_R_alt","D_alt")
# 
# projections_alt <- projections_alt %>%
#   pivot_wider(names_from = Country, values_from = -c(Country,Year))
# df_proj <- merge(df_proj,projections_alt, all.x = T)
# 
# ### Set up parameters---------------------------------------------------------------------
# curr_year <- 2024
# curr_idx = which(df_proj$Year==curr_year)
# country_map <- c("France" = "FR", "Germany" = "DE", "Italy" = "IT",
#                  "Netherlands" = "NL", "Spain" = "ES")
# grey_palette <- brewer.pal(n = 6, name = "Greys")[6:2]
# names(grey_palette) <- names(country_map)
# 
# 
# ### Output Simuation---------------------------------------------------------------------
# #Format Coefficients
# coefs_y <- coef(mod_y_restricted_OLS)
# df_proj$dypot_France[df_proj$Year>curr_year] <- mean(df_proj$dypot_France[(curr_idx-20):curr_idx+2])
# df_proj$dypot_Germany[df_proj$Year>curr_year] <- mean(df_proj$dypot_Germany[(curr_idx-20):curr_idx+2])
# df_proj$dypot_Italy[df_proj$Year>curr_year] <- mean(df_proj$dypot_Italy[(curr_idx-20):curr_idx+2])
# df_proj$dypot_Spain[df_proj$Year>curr_year] <- mean(df_proj$dypot_Spain[(curr_idx-20):curr_idx+2])
# df_proj$dypot_Netherlands[df_proj$Year>curr_year] <- mean(df_proj$dypot_Netherlands[(curr_idx-20):curr_idx+2])
# for (c in names(country_map)){
#   for (idx in (curr_idx+1):nrow(df_proj)){
#     #Projections with dependency in model
#     df_proj[idx,paste0("dy_",c)] <- coefs_y[paste0(c,"_dypot_",c)]*df_proj[idx,paste0("dypot_",c)] +
#       coefs_y[paste0(c,"_ygap_lag_",c)]*df_proj[idx-1,paste0("ygap_",c)] +
#       coefs_y[paste0(c,"_D_",c)]*df_proj[idx,paste0("D_alt_",c)]
#     #Level of output, potential output and output gap
#     df_proj[idx,paste0("y_",c)] <- df_proj[idx-1,paste0("y_",c)] + df_proj[idx,paste0("dy_",c)]
#     df_proj[idx,paste0("ypot_",c)] <- df_proj[idx-1,paste0("ypot_",c)] + df_proj[idx,paste0("dypot_",c)]
#     df_proj[idx,paste0("ygap_",c)] <- df_proj[idx,paste0("y_",c)] - df_proj[idx,paste0("ypot_",c)]
#   }
# }
# 
# ### Cost of debt (IR) Simulation----------------------------------------------------------
# coef_r_DE <- coef(mod_r_DE)
# country_list <- c("France","Italy","Netherlands","Spain")
# coefs_r <- coef(mod_r_restricted_OLS)
# coefs_s <- coef(mod_s_restricted_OLS)
# #Projections of r for Germany
# df_proj$IR_Germany[(curr_idx+1):nrow(df_proj)] <- exp(coef_r_DE["(Intercept)"] +
#   coef_r_DE["dypot_Germany"] * df_proj$dypot_Germany[(curr_idx+1):nrow(df_proj)] +
#   coef_r_DE["D_Y_Germany"] * df_proj$D_Y_alt_Germany[(curr_idx+1):nrow(df_proj)] +
#   coef_r_DE["D_R_Germany"] * df_proj$D_R_alt_Germany[(curr_idx+1):nrow(df_proj)])# + 0.02
# c <- "Germany"
# #Projections of b for Germany
# for (idx in (curr_idx+1):nrow(df_proj)){
#   df_proj[idx,paste0("s_",c)] <- coefs_s[paste0(c,"_(Intercept)")] +
#     coefs_s[paste0(c,"_ygap_",c)]*df_proj[idx,paste0("ygap_",c)] +
#     coefs_s[paste0(c,"_b_lag_",c)]*df_proj[idx-1,paste0("b_",c)] +
#     coefs_s[paste0(c,"_D_Y_",c)]*df_proj[idx,paste0("D_Y_alt_",c)] +
#     coefs_s[paste0(c,"_D_R_",c)]*df_proj[idx,paste0("D_R_alt_",c)]
#   df_proj$b_Germany[idx] <- -df_proj$s_Germany[idx] +
#     df_proj$b_Germany[idx-1]*(1+df_proj$IR_Germany[idx])/(1+df_proj$dy_Germany[idx] + 0.02)
# }
# 
# 
# #Projections of spreads for rest
# for (c in country_list){
#   for (idx in (curr_idx+1):nrow(df_proj)){
#     #s_t
#     df_proj[idx,paste0("s_",c)] <- coefs_s[paste0(c,"_(Intercept)")] +
#       coefs_s[paste0(c,"_ygap_",c)]*df_proj[idx,paste0("ygap_",c)] +
#       coefs_s[paste0(c,"_b_lag_",c)]*df_proj[idx-1,paste0("b_",c)] +
#       coefs_s[paste0(c,"_D_Y_",c)]*df_proj[idx,paste0("D_Y_alt_",c)] +
#       coefs_s[paste0(c,"_D_R_",c)]*df_proj[idx,paste0("D_R_alt_",c)]
#     #r_t
#     df_proj[idx,paste0("Spread_",c)] <- coefs_r[paste0(c,"_(Intercept)")] +
#       coefs_r[paste0(c,"_b_diff_lag_",c)]*df_proj[idx-1,paste0("b_diff_",c)]
#     df_proj[idx,paste0("IR_",c)] <- df_proj[idx,paste0("Spread_",c)] + df_proj[idx,"IR_Germany"]
#     #b_t
#     df_proj[idx,paste0("b_",c)] <- -df_proj[idx,paste0("s_",c)] +
#       (1 + df_proj[idx,paste0("IR_",c)])/(1 + df_proj[idx,paste0("dy_",c)]+0.02) * df_proj[idx-1,paste0("b_",c)]
#     #Make b_diff_t
#     df_proj[idx,paste0("b_diff_",c)] <- log(df_proj[idx,paste0("b_",c)]) - log(df_proj[idx,"b_Germany"])
#   }
# }
# 
# 
# df_proj_long$Country <- factor(df_proj_long$Country,
#                                levels = names(grey_palette))
# if (mig_per_thousand==5){
#   projections_migration_05 <- df_proj_long %>% select(Country, Year, b, s, dy, IR, ygap)
#   save(projections_migration_05, file = "data/main/migration05_projections.rda")
# }
# if (mig_per_thousand==0){
#   projections_migration_0 <- df_proj_long %>% select(Country, Year, b, s, dy, IR, ygap)
#   save(projections_migration_0, file = "data/main/migration0_projections.rda")
# }