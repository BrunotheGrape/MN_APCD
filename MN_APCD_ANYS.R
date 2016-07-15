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

# subset, aggregate data, and add distance metrics
diagred <- select(diag, DX1_CODE, IND)
diagTOT <- select(diag, DX1_CODE, TOTAL_PAID)
diagmn <- aggregate(diagred$IND, list(diagred$DX1_CODE), mean)
diagsm <- aggregate(diagTOT$TOTAL_PAID, list(diag$DX1_CODE), sum)
diagmn <- rename(diagmn, DX1_CODE = Group.1, INDMN = x)
diagsm <- rename(diagsm, DX1_CODE = Group.1, TOTSUM = x)
diag <- inner_join(diag,diagmn, by = "DX1_CODE")
diag <- inner_join(diag,diagsm, by = "DX1_CODE")
diag <- mutate(diag, Dist = (IND - INDMN) / TOTSUM)
