# MN_APCD Analysis

# read in files
diag <- read.csv("dPUF2013_diagnosis_v1.0.csv", header = TRUE)
util <- read.csv("dPUF2013_utilization_v1.0.csv", header = TRUE)

# load libraries
library(dplyr); library(ggplot2)
