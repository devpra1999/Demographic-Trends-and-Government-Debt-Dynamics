rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
### Packages-------------------------------------------------------------------
library(dplyr)
library(readr)

### Read file------------------------------------------------------------------
df_IT <- read_csv("data/all_data.csv") %>% 
  filter(Location == "Italy", Time<=2100) %>%
  select(Location, Time, CBR, CDR)
colnames(df_IT)[1:2] <- c("Country","Year")

df <- read_csv("data/pop_estimates_big5.csv")
df2 <- read_csv("data/pop_projections_big5.csv")
df <- rbind(df,df2)
rm(df2)
df_IT2 <- df %>% filter(Country == "Italy")

### Set up variables-----------------------------------------------------------
df_IT$N_Y <- apply(df_IT2[3:17],1,sum)
df_IT$N_M <- apply(df_IT2[18:67],1,sum)
df_IT$N_R <- apply(df_IT2[68:ncol(df_IT)],1,sum)
rm(df_IT2)

### Calibration----------------------------------------------------------------
df_IT <- df_IT %>%
  mutate(
    n_t = CBR / 1000,
    s_t = 1 - (CDR / 1000),
    N = N_Y + N_M + N_R,
    N_lag = lag(N),
    p_y = (lag(N_Y) + n_t * N_lag - N_Y) / lag(N_M),
    p_m = (lag(N_M) + p_y * lag(N_M) - N_M) / lag(N_M)
  )

### Validation-----------------------------------------------------------------
df_IT <- df_IT %>%
  mutate(
    s_t_observed = 1 - (CDR / 1000),
    s_t_implied = 1 - (lag(N_R) + p_m * lag(N_M) - N_R) / lag(N_R)
  )
validation_results <- df_IT %>%
  select(Year, s_t_observed, s_t_implied) %>%
  filter(!is.na(s_t_observed) & !is.na(s_t_implied))

### Plots----------------------------------------------------------------------
plot(df_IT$Year,df_IT$p_m, col = "black", type = "l", lwd = 2, ylim = c(0,0.05))
lines(df_IT$Year, df_IT$p_y, col = "blue", lwd = 2)



