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

######### subset, aggregate data, and add distance metrics  ##########
## Diagonosis ##
diagred <- select(diag, DX1_CODE, IND)
diagTOT <- select(diag, DX1_CODE, TOTAL_PAID)
diagmn <- aggregate(diagred$IND, list(diagred$DX1_CODE), mean)
diagsm <- aggregate(diagTOT$TOTAL_PAID, list(diag$DX1_CODE), sum)
diagmn <- rename(diagmn, DX1_CODE = Group.1, INDMN = x)
diagsm <- rename(diagsm, DX1_CODE = Group.1, TOTSUM = x)
diag <- inner_join(diag,diagmn, by = "DX1_CODE")
diag <- inner_join(diag,diagsm, by = "DX1_CODE")
diag <- mutate(diag, Dist = (IND - INDMN) / TOTSUM)

## Utilities ##
utilred <- select(util, UTILIZATION_CATEGORY, IND)
utilTOT <- select(util, UTILIZATION_CATEGORY, TOTAL_PAID)
utilmn <- aggregate(utilred$IND, list(utilred$UTILIZATION_CATEGORY), mean)
utilsm <- aggregate(utilTOT$TOTAL_PAID, list(util$UTILIZATION_CATEGORY), sum)
utilmn <- rename(utilmn, UTILIZATION_CATEGORY = Group.1, INDMN = x)
utilsm <- rename(utilsm, UTILIZATION_CATEGORY = Group.1, TOTSUM = x)
util <- inner_join(util,utilmn, by = "UTILIZATION_CATEGORY")
util <- inner_join(util,utilsm, by = "UTILIZATION_CATEGORY")
util <- mutate(util, Dist = (IND - INDMN) / TOTSUM)

################## Analysis ###################
# order the data by distance
diag <- arrange(diag, desc(Dist))
util <- arrange(util, desc(Dist))

write.csv(diag, file = "diag.csv")
write.csv(util, file = "util.csv")
