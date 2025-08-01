build_restriction_matrix <- function(vars_to_restrict, country_names, regressors_per_eq) {
  n_eq <- length(country_names)
  k <- length(regressors_per_eq)
  total_coef <- n_eq * k
  
  restriction_list <- list()
  
  for (var in vars_to_restrict) {
    var_pos <- which(regressors_per_eq == var)#Get position of regressor to be restricted
    if (length(var_pos) == 0) stop(paste("Variable", var, "not found in regressor list."))
    for (i in 1:(n_eq - 1)) {
      row <- rep(0, total_coef)
      col1 <- (i - 1) * k + var_pos
      col2 <- i * k + var_pos
      row[col1] <- 1
      row[col2] <- -1
      restriction_list[[length(restriction_list) + 1]] <- row
    }
  }
  
  R.restr <- do.call(rbind, restriction_list)
  q.restr <- rep(0, nrow(R.restr))
  
  return(list(R = R.restr, q = q.restr))
}

generate_regression_table <- function(coefs, ses, pvals, model_names, covariate_names, type = "latex") {
  # Create a data frame to store results
  result_df <- data.frame(matrix(ncol = length(covariate_names) + 1, nrow = nrow(coefs) * 2))
  colnames(result_df) <- c("", covariate_names)
  
  # Fill the data frame with coefficients and standard errors
  for (i in seq_len(nrow(coefs))) {
    result_df[2 * i - 1, 1] <- model_names[i]  # Coefficients row
    for (j in seq_len(ncol(coefs))) {
      # Coefficients with significance levels
      significance <- ifelse(pvals[i, j] < 0.01, "***",
                             ifelse(pvals[i, j] < 0.05, "**",
                                    ifelse(pvals[i, j] < 0.1, "*", "")))
      result_df[2 * i - 1, j + 1] <- paste0(coefs[i, j], significance)
    }
    
    result_df[2 * i, 1] <- ""  # Empty row for standard errors
    for (j in seq_len(ncol(ses))) {
      result_df[2 * i, j + 1] <- paste0("(", ses[i, j], ")")  # Standard errors
    }
  }
  
  # LaTeX output
  if (type == "latex") {
    cat("\\begin{table}[H]\n")
    cat("\\centering\n")
    cat("\\begin{tabular}{", paste(rep("l", ncol(result_df)), collapse = ""), "}\n")
    cat("\\toprule\n")
    cat(paste(c("", colnames(result_df)[-1]), collapse = " & "), "\\\\\ \n")
    cat("\\midrule\n")
    
    for (row in 1:nrow(result_df)) {
      cat(paste(result_df[row, ], collapse = " & "), "\\\\\ \n")
      # if (row %% 2 == 1 && row < nrow(result_df)) {
      #   cat("     & \\\\ \n")  # Keep the formatting for separating coefficients and standard errors
      # }
    }
    
    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
    cat("\\end{table}\n")
  } else {
    # For text output, return the data frame
    return(result_df)
  }
}

process_single_model <- function(model, dataset, sysfit = T) {
  smry <- summary(model)
  # Extract coefficients
  coefs <- smry$coefficients
  #Extract Regressor Names
  coef_names <- rownames(coefs)
  if (sysfit==T){
    coef_names[coef_names!="(Intercept)"] <- paste0(sub("_[^_]*$", "", coef_names[coef_names!="(Intercept)"]),"_i")
  }
  #Stationarity Tests
  resids <- na.omit(residuals(model))
  lag_resids <- lag.xts(resids)
  diff_resids <- diff.xts(resids)
  adf_reg <- lm(diff.xts(resids) ~ 0 + lag_resids)
  rho <- coef(lm(resids ~ 0 + lag_resids))
  t_stat <- summary(adf_reg)$coefficients[1, 3]
  # Get model statistics
  if (!is.null(smry$sigma)){
    r_squared <- smry$r.squared
    sigma <- smry$sigma
    years <- range(dataset$Year[as.numeric(names(na.omit(fitted(model))))])
  } else {
    r_squared <- smry$R2.org
    sigma <- sqrt(smry$s2)
    years <- range(dataset$Year[as.numeric(rownames(na.omit(fitted(model))))])
  }
  #nobs <- nobs(model)
  sample <- paste(years, collapse = "-")
  list(
    coefficients = coefs,
    coef_names = coef_names,
    stats = list(
      r_squared = r_squared,
      sigma = sigma,
      sample = sample,
      rho = rho,
      adf = t_stat
    )
  )
}

generate_summary_regression_table <- function(model_results, format = "latex") {
  all_coefs <- unique(unlist(lapply(model_results, function(x) x$coef_names)))
  coef_mat <- matrix("", nrow = length(all_coefs), ncol = length(model_results))
  se_mat <- matrix("", nrow = length(all_coefs), ncol = length(model_results))
  for (i in seq_along(all_coefs)) {
    for (j in seq_along(model_results)) {
      coef_name <- all_coefs[i]
      model <- model_results[[j]]
      if (coef_name %in% model$coef_names) {
        idx <- which(model$coef_names == coef_name)
        coef_val <- model$coefficients[idx, 1]
        se_val <- model$coefficients[idx, 2]
        p_val <- model$coefficients[idx, 4]
        stars <- ifelse(p_val < 0.01, "^{***}", 
                        ifelse(p_val < 0.05, "^{**}", 
                               ifelse(p_val < 0.1, "^{*}", "")))
        coef_mat[i, j] <- sprintf("%.3f%s", coef_val, stars)
        se_mat[i, j] <- sprintf("(%.3f)", se_val)
      }
    }
  }
  # Clean coefficient names 
  clean_coef_names <- gsub("\\$|\\\\pi_|\\\\pi\\^\\{LR\\}_|\\\\beta_", "", all_coefs)
  clean_coef_names <- gsub("_", "-", clean_coef_names)  # Replace underscores with hyphens
  stats_list <- list(
    "$SMPL$" = sapply(model_results, function(x) x$stats$sample),
    "$R^2$" = sapply(model_results, function(x) sprintf("%.3f", x$stats$r_squared)),
    "$\\sigma$" = sapply(model_results, function(x) sprintf("%.4f", x$stats$sigma)),
    "$\\rho$" = sapply(model_results, function(x) sprintf("%.3f", x$stats$rho)),
    "$ADF$" = sapply(model_results, function(x) sprintf("%.3f", x$stats$adf))
  )
  if (format == "latex") {
    # Header
    model_names <- names(model_results)
    header <- paste0(
      "\\begin{table}[H]\n",
      "\\centering\n",
      "\\resizebox{\\textwidth}{!}{\n",
      "\\begin{tabular}{l", paste(rep("c", length(model_names)), collapse = ""), "}\n",
      "\\toprule\n",
      " & ", paste(model_names, collapse = " & "), " \\\\\n",
      "\\midrule\n"
    )
    
    # Coefficient rows
    coef_rows <- character()
    for (i in 1:nrow(coef_mat)) {
      coef_row <- paste(clean_coef_names[i], paste(coef_mat[i, ], collapse = " & "), sep = " & ")
      se_row <- paste(" ", paste(se_mat[i, ], collapse = " & "), sep = " & ")
      coef_rows <- c(coef_rows, coef_row, se_row)
    }
    
    # Statistics rows
    stat_rows <- character()
    for (stat in names(stats_list)) {
      stat_row <- paste(stat, paste(stats_list[[stat]], collapse = " & "), sep = " & ")
      stat_rows <- c(stat_rows, stat_row)
    }
    
    # Combine all parts
    latex_table <- paste0(
      header,
      paste(coef_rows, collapse = " \\\\\n"), " \\\\\n",
      "\\midrule\n",
      paste(stat_rows, collapse = " \\\\\n"), " \\\\\n",
      "\\bottomrule\n",
      "\\end{tabular}\n",
      "}\n",
      "\\caption{}\n",
      "\\label{}\n",
      "\\end{table}"
    )
    return(latex_table)
  } else {
    # For data frame output
    table_df <- data.frame(
      Coefficient = clean_coef_names,
      coef_mat,
      stringsAsFactors = FALSE
    )
    colnames(table_df)[-1] <- names(model_results)
    return(table_df)
  }
}