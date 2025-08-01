rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
### Packages-------------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
### Demographics Estimates-----------------------------------------------------
filename = "data/pop_estimates.xlsx"
df = read_xlsx(filename,sheet = "Estimates", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% filter(Country %in% c("Germany","France","Italy","Spain","Netherlands"))
write_csv(df,"data/raw/pop_estimates_big5.csv")

### Demographics Projections----------------------------------------------------
filename = "data/pop_estimates.xlsx"
df = read_xlsx(filename,sheet = "Medium variant", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% filter(Country %in% c("Germany","France","Italy","Spain","Netherlands"))
write_csv(df,"data/raw/pop_projections_big5.csv")

### Dependency Estimates------------------------------------------------------
filename = "data/dependency_ratio.xlsx"
df = read_xlsx(filename,sheet = "Estimates", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% filter(Country %in% c("Germany","France","Italy","Spain","Netherlands"))
write_csv(df,"data/raw/dependency_ratio_estimates_big5.csv")

### Dependency Projections------------------------------------------------------
filename = "data/dependency_ratio.xlsx"
df = read_xlsx(filename,sheet = "Medium variant", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% filter(Country %in% c("Germany","France","Italy","Spain","Netherlands"))
write_csv(df,"data/raw/dependency_ratio_projections_big5.csv")

### Mortality (by age) Estimates---------------------------------------------------
filename = "data/mortality_by_age.xlsx"
df = read_xlsx(filename,sheet = "Estimates", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% filter(Country %in% c("Germany","France","Italy","Spain","Netherlands"))
write_csv(df,"data/raw/mortality_by_age_estimates_big5.csv")

### Mortality (by age) Projections--------------------------------------------------
filename = "data/mortality_by_age.xlsx"
df = read_xlsx(filename,sheet = "Medium variant", skip = 16)
df = df[,c(-1:-2)]
df = df[,c(-2:-8)]
colnames(df)[1] = "Country"
df = df %>% filter(Country %in% c("Germany","France","Italy","Spain","Netherlands"))
write_csv(df,"data/raw/mortality_by_age_projections_big5.csv")

