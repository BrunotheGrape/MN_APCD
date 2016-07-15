# MN_APCD Analysis

# read in files
diag <- read.csv("dPUF2013_diagnosis_v1.0.csv", header = TRUE, 
                 colClasses = c("factor", "factor", "factor", 
                                "factor", "factor", "factor", 
                                "factor", "character", "numeric"))
options(digits = 12)
diag$TOTAL_PAID <- gsub("$", "", diag$TOTAL_PAID, fixed = TRUE)
diag$TOTAL_PAID <- as.numeric(as.character(gsub(",","",diag$TOTAL_PAID)))

util <- read.csv("dPUF2013_utilization_v1.0.csv", header = TRUE,
                 colClasses = c("factor", "factor", "factor",
                                "factor", "factor", "factor",
                                "character", "numeric", "numeric"))
util$TOTAL_PAID <- gsub("$", "", util$TOTAL_PAID, fixed = TRUE)
util$TOTAL_PAID <- as.numeric(as.character(gsub(",","",util$TOTAL_PAID)))

# load libraries
library(dplyr); library(ggplot2)




# Add variable for $/members and $/services
diag <- mutate(diag, IND = TOTAL_PAID / DISTINCT_MEMBERS) 
util <- mutate(util, IND = TOTAL_PAID / DISTINCT_MEMBERS)

# subset and aggregate data
diagred <- select(diag, DX1_CODE, IND)
diagmn <- aggregate(diagred$IND, list(diag$DX1_CODE), mean)
#spdiag <- split(diagred, diag$DX1_CODE)
