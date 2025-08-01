rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
### Load Projections------------------------------------------------------------
load("data/main/baseline_projections.rda")
load("data/main/migration0_projections.rda")
load("data/main/migration05_projections.rda")

