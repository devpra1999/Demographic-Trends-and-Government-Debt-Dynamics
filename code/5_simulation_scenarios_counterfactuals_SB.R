rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
### Run model estimations---------------------------------------------------------------------
source("code/4_model_estimation_new.R")

### Demographics Dataset--------------------------------------------------------------------
df_demo <- read_csv("data/main/Demographics_big5.csv")
df_demo <- df_demo %>% 
  mutate(Country = str_replace(Country, "United States", "US"))

### Econ Dataset----------------------------------------------------------------------------
df_proj <- merge(df_output,df_debt, all.x = T)
df_proj <- df_proj %>%
  fill(PTR_US, .direction = "downup") %>%
  merge(df_demo %>% pivot_wider(names_from = Country, values_from = -c(Country,Year)), , all.x = T)
rm(list = setdiff(ls(), c("df", "df_proj","df_demo","mod_y_restricted_OLS","mod_pi_US","mod_pi_restricted","mod_r_DE","mod_r_US","mod_r_restricted_OLS","mod_s_restricted")))

### Source simulation auxiliary functions------------------------------------------------------
source("code/5_aux_funs_simulations_SB.R")
curr_year <- 2000
demo_year <- 1950
end_year <- 2024
L <- run_all_projections(
  df_demo = df_demo,
  df_proj = df_proj,
  mod_y_restricted_OLS = mod_y_restricted_OLS,
  mod_r_DE = mod_r_DE,
  mod_r_restricted_OLS = mod_r_restricted_OLS,
  mod_s_restricted_OLS = mod_s_restricted,
  mod_pi_US = mod_pi_US, #change sofia
  mod_pi_restricted = mod_pi_restricted, #change sofia
  mig_per_thousand = "baseline",
  CBR_cf = "baseline",
  curr_year = curr_year,
  demo_year = demo_year
)
proj1 <- L[[1]]
df_proj1 <- L[[2]]
proj1$CBR_alt <- proj1$CBR_alt*1000 
proj1 <- proj1 %>% 
  filter(if_any(-Year, ~ !is.na(.)))

# proj2 <- run_all_projections(
#   df_demo = df_demo,
#   df_proj = df_proj,
#   mod_y_restricted_OLS = mod_y_restricted_OLS,
#   mod_r_DE = mod_r_DE,
#   mod_r_restricted_OLS = mod_r_restricted_OLS,
#   mod_s_restricted_OLS = mod_s_restricted_OLS,
#   mig_per_thousand = "baseline",
#   CBR_cf = "last",
#   curr_year = curr_year,
#   demo_year = demo_year
# )
# proj2$CBR_alt <- proj2$CBR_alt*1000 

### Make projections dataset------------------------------------------------------------
scenarios <- c("baseline")
names(proj1)[3:ncol(proj1)] <- paste0(names(proj1)[3:ncol(proj1)],"_",scenarios[1])
#names(proj2)[3:ncol(proj2)] <- paste0(names(proj2)[3:ncol(proj2)],"_",scenarios[2])
#names(proj3)[3:ncol(proj3)] <- paste0(names(proj3)[3:ncol(proj3)],"_",scenarios[3])
#projections <- merge(proj1,proj2, by = c("Country","Year"))#,proj3, by =c("Country","Year"))
projections <- proj1
projections <- projections %>% filter(Year>curr_year - 1) %>% filter(Year<=end_year)

### Plots--------------------------------------------------------------------------

CBR_plot <- plot_scenario_comparison(
  projections,
  "CBR_alt",
  scenarios = scenarios,
  actual_data = df_proj,
  curr_year = curr_year
)
CBR_plot
M_plot <- plot_scenario_comparison(projections, "M_alt",scenarios = scenarios,actual_data = df_proj, curr_year = curr_year)
M_plot
D_Y_plot <- plot_scenario_comparison(projections, "D_Y_alt",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year)
D_Y_plot
D_R_plot <- plot_scenario_comparison(projections, "D_R_alt",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year)
D_R_plot
MY_plot <- plot_scenario_comparison(projections, "MY_alt",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year)
MY_plot
IR_plot <- plot_scenario_comparison(projections, "IR",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year)
IR_plot
#change sofia2
g_plot <- plot_scenario_comparison(projections, "g",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year)
g_plot
b_plot <- plot_scenario_comparison(projections, "b",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year)
b_plot
surplus_plot <- plot_scenario_comparison(projections, "s",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year)
surplus_plot
dy_plot <- plot_scenario_comparison(projections, "dy",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year)
dy_plot
ygap_plot <- plot_scenario_comparison(projections, "ygap",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year)
ygap_plot
# ggsave("plots/sim_debt_ratio_scenario_big5.pdf",b_plot)
# ggsave("plots/sim_growth_scenario_big5.pdf",dy_plot)
# ggsave("plots/sim_cost_of_debt_scenario_big5.pdf",IR_plot)
# ggsave("plots/sim_surplus_scenario_big5.pdf",surplus_plot)


# Step 1: Extract actual IR and g from df_proj
actual_g_ir <- df_proj %>%
  select(Year, matches("^IR_|^g_")) %>%
  pivot_longer(
    cols = -Year,
    names_to = c("Variable", "Country"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  mutate(Scenario = "actual")

# Step 2: Extract projections from `projections` (which is already long)
proj_g_ir <- projections %>%
  select(Country, Year, IR_baseline, g_baseline) %>%
  pivot_longer(
    cols = c(IR_baseline, g_baseline),
    names_to = "Variable",
    names_prefix = "",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = case_when(
      Variable == "IR_baseline" ~ "IR",
      Variable == "g_baseline" ~ "g"
    ),
    Scenario = "baseline"
  )

# Step 3: Combine actual and projected
df_plot <- bind_rows(actual_g_ir, proj_g_ir)

# color_map <- c(
#   "IR_actual" = "firebrick",
#   "IR_baseline" = "indianred2",
#   "g_actual" = "navy",
#   "g_baseline" = "deepskyblue3"
# )
# 
# linetype_map <- c(
#   "IR_actual" = "solid",
#   "IR_baseline" = "dashed",
#   "g_actual" = "solid",
#   "g_baseline" = "dashed"
# )

df_plot <- df_plot %>%
  mutate(LineID = paste0(Variable, "_", Scenario))


# Step 4: Plot
p <- ggplot(df_plot, aes(x = Year, y = Value, color = LineID, linetype = Scenario, size = Scenario)) +
  geom_line() +
  facet_wrap(~ Country, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = 2000, linetype = "dotted", color = "black") +
  scale_color_manual(
    values = c(
      "g_actual" = "navy",       # blue
      "IR_actual" = "firebrick",      # red
      "g_baseline" = "deepskyblue3",     # light blue
      "IR_baseline" = "indianred2"     # light red-orange
    )
  ) +
  scale_linetype_manual(values = c("actual" = "solid", "baseline" = "dashed")) +
  scale_size_manual(values = c("actual" = 0.7, "baseline" = 0.5)) +
  labs(
    title = "Nominal Cost of Debt (IR) and Nominal Growth (g)",
    y = "Value",
    x = "Year",
    color = "Variable",
    linetype = "Scenario",
    size = NULL
  ) +
  xlim(1960, 2025) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position5 = "bottom"
  )
p