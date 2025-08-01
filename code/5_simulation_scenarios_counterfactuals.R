rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
### Run model estimations---------------------------------------------------------------------
source("code/4_model_estimation_new.R")
#mod_s_restricted <- mod_s_unrestricted

### Demographics Dataset--------------------------------------------------------------------
df_demo <- read_csv("data/main/Demographics_big5.csv")
df_demo <- df_demo %>% 
  mutate(Country = str_replace(Country, "United States", "US"))

### Econ Dataset----------------------------------------------------------------------------
df_proj <- merge(df_output,df_debt, all.x = T)
df_proj <- df_proj %>%
  fill(PTR_US, .direction = "downup") %>%
  merge(df_demo %>% pivot_wider(names_from = Country, values_from = -c(Country,Year)), , all.x = T)
rm(list = setdiff(ls(), c("country_map","df", "df_proj","df_demo","mod_y_restricted_OLS",
                          "mod_r_DE","mod_r_US","mod_r_restricted_OLS","mod_s_restricted")))
country_names <- names(country_map)
### Source simulation auxiliary functions------------------------------------------------------
source("code/5_aux_funs_simulations.R")
curr_year <- 2024
demo_year <- 2024
end_year <- 2050
# df_proj$b_Germany[which(df_proj$Year==1970)] <- 0.25
# df_proj$b_Germany[which(df_proj$Year==1971)] <- 0.25
# df_proj$b_Spain[which(df_proj$Year==1970)] <- 0.25
# df_proj$b_Spain[which(df_proj$Year==1971)] <- 0.25
# df_proj$IR_Germany[which(df_proj$Year==1971)] <- 0.05
# df_proj$IR_Germany[which(df_proj$Year==1970)] <- 0.05
# df_proj$IR_Germany[which(df_proj$Year==1969)] <- 0.05
# df_proj$dypot_Germany[which(df_proj$Year<1970)] <- 0.03
L <- run_all_projections(
  df_demo = df_demo,
  df_proj = df_proj,
  mod_y_restricted_OLS = mod_y_restricted_OLS,
  mod_r_DE = mod_r_DE,
  mod_r_restricted_OLS = mod_r_restricted_OLS,
  mod_s_restricted_OLS = mod_s_restricted,
  mig_per_thousand = "baseline",
  CBR_cf = "baseline",
  curr_year = curr_year,
  demo_year = demo_year,
  country_names = names(country_map)
)
proj1 <- L[[1]]
rm(L)
proj1$CBR_alt <- proj1$CBR_alt*1000 
proj1 <- proj1 %>% 
  filter(if_any(-Year, ~ !is.na(.)))

L <- run_all_projections(
  df_demo = df_demo,
  df_proj = df_proj,
  mod_y_restricted_OLS = mod_y_restricted_OLS,
  mod_r_DE = mod_r_DE,
  mod_r_restricted_OLS = mod_r_restricted_OLS,
  mod_s_restricted_OLS = mod_s_restricted,
  mig_per_thousand = 10,
  CBR_cf = "baseline",
  curr_year = curr_year,
  demo_year = demo_year,
  country_names = names(country_map),
  demo_countries = c("Italy","Spain")
)
proj2 <- L[[1]]
rm(L)
proj2$CBR_alt <- proj2$CBR_alt*1000
proj2 <- proj2 %>% 
  filter(if_any(-Year, ~ !is.na(.)))

L <- run_all_projections(
  df_demo = df_demo,
  df_proj = df_proj,
  mod_y_restricted_OLS = mod_y_restricted_OLS,
  mod_r_DE = mod_r_DE,
  mod_r_restricted_OLS = mod_r_restricted_OLS,
  mod_s_restricted_OLS = mod_s_restricted,
  mig_per_thousand = 20,
  CBR_cf = "baseline",
  curr_year = curr_year,
  demo_year = demo_year,
  country_names = names(country_map),
  demo_countries = c("Italy","Spain")
)
proj3 <- L[[1]]
rm(L)
proj3$CBR_alt <- proj3$CBR_alt*1000
proj3 <- proj3 %>% 
  filter(if_any(-Year, ~ !is.na(.)))

### Make projections dataset------------------------------------------------------------
scenarios <- c("baseline","migratio_10","migration_20")
names(proj1)[3:12] <- paste0(names(proj1)[3:12],"_",scenarios[1])
names(proj2)[3:12] <- paste0(names(proj2)[3:12],"_",scenarios[2])
names(proj3)[3:12] <- paste0(names(proj3)[3:12],"_",scenarios[3])
projections <- merge(merge(proj1,proj2, by = c("Country","Year")),proj3, by =c("Country","Year"))
projections <- projections %>% filter(Year>curr_year - 1) %>% filter(Year<=end_year)

### Plots--------------------------------------------------------------------------

CBR_plot <- plot_scenario_comparison(
  projections,
  "CBR_alt",
  scenarios = scenarios,
  actual_data = df_proj,
  curr_year = curr_year,
  country_names = c("Italy","Spain")
)

CBR_plot
M_plot <- plot_scenario_comparison(projections, "M_alt",scenarios = scenarios,actual_data = df_proj, curr_year = curr_year,country_names = c("Italy","Spain"))
M_plot
D_R_plot <- plot_scenario_comparison(projections, "D_R_alt",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year,country_names = c("Italy","Spain"))
D_R_plot
MY_plot <- plot_scenario_comparison(projections, "MY_alt",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year,country_names = c("Italy","Spain"))
MY_plot
b_plot <- plot_scenario_comparison(projections, "b",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year,country_names = c("Italy","Spain"))
b_plot
IR_plot <- plot_scenario_comparison(projections, "IR",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year,country_names = c("Italy","Spain"))
IR_plot
surplus_plot <- plot_scenario_comparison(projections, "s",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year,country_names = c("Italy","Spain"))
surplus_plot
dy_plot <- plot_scenario_comparison(projections, "dy",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year,country_names = c("Italy","Spain"))
dy_plot
ygap_plot <- plot_scenario_comparison(projections, "ygap",scenarios = scenarios,actual_data = df_proj,curr_year = curr_year,country_names = c("Italy","Spain"))
ygap_plot
ggsave("plots/sim_migration_big5_scenario.pdf",M_plot)
ggsave("plots/sim_dependency_old_big5_scenario.pdf",D_R_plot)
ggsave("plots/sim_debt_ratio_big5_scenario.pdf",b_plot)
ggsave("plots/sim_real_growth_big5_scenario.pdf",dy_plot)
ggsave("plots/sim_cost_of_debt_big5_scenario.pdf",IR_plot)
ggsave("plots/sim_surplus_big5_scenario.pdf",surplus_plot)
ggsave("plots/sim_output_gap_big5_scenario.pdf",ygap_plot)
