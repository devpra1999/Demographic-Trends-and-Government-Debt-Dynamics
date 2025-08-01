# Clear environment and set working directory
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load necessary package
listofpackages <- c("ggplot2","dplyr","readxl","rlist","rstudioapi","ameco")
for (j in listofpackages) {
  if (sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = TRUE)
}
