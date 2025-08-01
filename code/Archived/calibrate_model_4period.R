rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
### Packages-------------------------------------------------------------------
library(dplyr)
library(readr)


### Read file------------------------------------------------------------------
df_IT <- read_csv("data/all_data.csv") %>% 
  filter(Location == "Italy", Time<=2100) %>%
  select(Location, Time, CBR, CDR, NetMigrations,CNMR,TPopulation1July)
colnames(df_IT)[1:2] <- c("Country","Year")
df_IT$NetMigrations_smooth <- pracma::hampel(df_IT$NetMigrations, k = 3, t0=3)$y


df <- read_csv("data/raw/pop_estimates_big5.csv")
df2 <- read_csv("data/raw/pop_projections_big5.csv")
df <- rbind(df,df2)
rm(df2)
#df_IT2 has age wise population estimates
df_IT2 <- df %>% filter(Country == "Italy")


df <- read_csv("data/raw/mortality_by_age_estimates_big5.csv")
df2 <- read_csv("data/raw/mortality_by_age_projections_big5.csv")
df <- rbind(df,df2)
#df_IT3 has age wise mortality estimates
df_IT3 <- df %>% filter(Country == "Italy") %>% select(Country,Year,'0-19','20+','40+','50+','65+')
df_IT3$'45+' <- (df_IT3$`40+` + df_IT3$`50+`)/2
df_IT3$'20-44' <- df_IT3$`20+` - df_IT3$`45+`
df_IT3$'45-64' <- df_IT3$`45+` - df_IT3$`65+`
rm(df,df2)


### Set up variables-----------------------------------------------------------
df_IT$N_Y <- apply(df_IT2[3:22],1,sum)
df_IT$N_M1 <- apply(df_IT2[23:47],1,sum)
df_IT$N_M2 <- apply(df_IT2[48:67],1,sum)
df_IT$N_R <- apply(df_IT2[68:ncol(df_IT2)],1,sum)
#rm(df_IT2)


df_IT$FAT_Y <- df_IT3$`0-19`
df_IT$FAT_M1 <- df_IT3$`20-44`
df_IT$FAT_M2 <- df_IT3$`45-64`
df_IT$FAT_R <- df_IT3$`65+`
rm(df_IT3)


df_IT$mu_Y <- df_IT$FAT_Y/lag(df_IT$N_Y)
df_IT$mu_M1 <- df_IT$FAT_M1/lag(df_IT$N_M1)
df_IT$mu_M2 <- df_IT$FAT_M2/lag(df_IT$N_M2)
df_IT$mu_R <- df_IT$FAT_R/lag(df_IT$N_R)


### Calibration----------------------------------------------------------------
df_IT <- df_IT %>%
  mutate(
    nu_t = CBR/1000,
    N = N_Y + N_M1 + N_M2 + N_R,
    p_y = (lag(N_Y) + nu_t*lag(N) - FAT_Y - N_Y) / lag(N_Y),
    p_m1 = (lag(N_M1) + p_y*lag(N_Y) + NetMigrations - FAT_M1 - N_M1) / lag(N_M1),
    p_m2 = (lag(N_M2) + p_m1*lag(N_M1) - FAT_M2 - N_M2)/ lag(N_M2)
  )


# df_IT <- df_IT %>%
#   mutate(
#     nu_t = CBR/1000,
#     N = N_Y + N_M1 + N_M2 + N_R,
#     p_y = (lag(N_Y) + nu_t*lag(N) - mu_Y*lag(N_Y) - N_Y) / lag(N_Y),
#     p_m1 = (lag(N_M1) + p_y*lag(N_Y) + NetMigrations - mu_M*lag(N_M1) - N_M1) / lag(N_M1),
#     p_m2 = (lag(N_M2) + p_m1*lag(N_M1) - mu_M*lag(N_M2) - N_M2)/ lag(N_M2)
#   )


### Validation-----------------------------------------------------------------
validation_results <- df_IT %>%
  mutate(
    FAT_t_observed = FAT_R,
    FAT_t_implied = lag(N_R) + p_m2 * lag(N_M2) + NetMigrations*0 - N_R
  ) %>%
  select(Year, FAT_t_observed, FAT_t_implied) %>%
  filter(!is.na(FAT_t_observed) & !is.na(FAT_t_implied))


# validation_results <- df_IT %>%
#   mutate(
#     mu_t_observed = mu_R,
#     mu_t_implied = (lag(N_R) + p_m2 * lag(N_M2) - N_R) / lag(N_R)
#   ) %>%
#   select(Year, mu_t_observed, mu_t_implied) %>%
#   filter(!is.na(mu_t_observed) & !is.na(mu_t_implied))


### Plots----------------------------------------------------------------------
plot(df_IT$Year,df_IT$p_y, col = "black", type = "l", lwd = 2, ylim = c(0,0.2))
lines(df_IT$Year, df_IT$p_m1, col = "blue", lwd = 2)
lines(df_IT$Year, df_IT$p_m2, col = "red", lwd = 2)
lines(df_IT$Year, df_IT$nu_t*10, col = "orange", lwd = 2)


plot(df_IT$Year,df_IT$FAT_Y, col = "black", type = "l", lwd = 2, ylim = c(0,800), ylab = "mu", xlab = "Year", main = "Mortality Rates by Age Group")
lines(df_IT$Year, df_IT$FAT_M1, col = "blue", lwd = 2)
lines(df_IT$Year, df_IT$FAT_M2, col = "violet", lwd = 2)
lines(df_IT$Year, df_IT$FAT_R, col = "red", lwd = 2)
lines(validation_results$Year,validation_results$FAT_t_implied, col = "red", lty = "dotted", lwd = 2)
