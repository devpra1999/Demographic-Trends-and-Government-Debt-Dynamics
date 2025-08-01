# Clear environment and set working directory
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load necessary package
listofpackages <- c("ggplot2","dplyr","readxl","rlist","rstudioapi")
for (j in listofpackages) {
  if (sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = TRUE)
}

# Manually input life expectancy data at age 65 for Italy
# Data sourced from reliable sources like ISTAT, OECD, and demographic studies
life_expectancy_data <- data.frame(
  Year = c(1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002, 2003, 2004, 2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023),
  Life_Expectancy = c(
    16.9,   # 1990 
    17.1,   # 1991
    17.4,   # 1992
    17.4,   # 1993 
    17.5,   # 1994
    17.7,   # 1995
    17.9,   # 1996
    18.0,   # 1997
    17.9,   # 1998
    18.2,   # 1999
    18.5,   # 1999
    18.8,   # 2001 
    18.9,   # 2002
    18.7,   # 2003
    19.3,   # 2004
    19.3,   # 2005
    19.6,   # 2006
    19.6,   # 2007
    19.6,   # 2008
    19.7,   # 2009
    20.0,   # 2010
    20.1,   # 2011
    20.1,   # 2012
    20.4,    # 2013
    20.6,   # 2014
    20.3,   # 2015
    20.8,   # 2016
    20.5,   # 2017
    20.9,    # 2018
    21.0,   # 2019
    20.0,   # 2020
    20.4,   # 2021
    20.4,   # 2022
    20.9    # 2023
    
  )
)

# Create visualization
ggplot(life_expectancy_data, aes(x = Year, y = Life_Expectancy)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 3) +
  theme_minimal() +
  labs(
    title = "Life Expectancy at Age 65 in Italy (1990-2023)",
    x = "Year",
    y = "Years Remaining",
    caption = "Compiled from ISTAT data"
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 3)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold")
  )