# Format large data files as Rdata
########################################################
setwd("/home/chao/Encfs/secure/dm_data/")

# ICPC codes for GOPC
########################################################

# ICD-9 codes for Inpatients, A&E, A&E follow-up, SOPC
########################################################
# diagnosis codes
# E codes (external injury) replaced as 9005. using gsub
# V codes (other details) replaced as 9022. using gsub
# --- Hidden codes replaced as 9999 using gsub

# Procedure codes
# --- Hidden codes replaced as 9999 using gsub

# format as numeric

library(foreign)

# A&E
d <- read.dta("data/ae_attn.dta")
str(d)
d$adate <- as.Date(paste("15", d$adate, sep = ""),"%d%b%Y")
d$ddate <- as.Date(paste("15", d$ddate, sep = ""),"%d%b%Y")

cols <- names(d)[grep("diag_cd", names(d))]
d[, cols] <- apply(d[, cols], 2, function(x) gsub("E", "9005.", x))
d[, cols] <- apply(d[, cols], 2, function(x) gsub("V", "9022.", x))
d[, cols] <- apply(d[, cols], 2, function(x) gsub("---", "9999", x))
d[, cols] <- lapply(d[, cols], as.numeric)

cols <- names(d)[grep("proc_cd", names(d))]
d[, cols] <- apply(d[, cols], 2, function(x) gsub("---", "9999", x))
d[, cols] <- lapply(d[, cols], as.numeric)

ae.attn <- d

# A&E f/u
d <- read.dta("data/ae_fu.dta")
str(d)
d$adate <- as.Date(paste("15", d$appo_date, sep = ""),"%d%b%Y")

cols <- names(d)[grep("diag_cd", names(d))]
d[, cols] <- apply(d[, cols], 2, function(x) gsub("E", "9005.", x))
d[, cols] <- apply(d[, cols], 2, function(x) gsub("V", "9022.", x))
d[, cols] <- apply(d[, cols], 2, function(x) gsub("---", "9999", x))
d[, cols] <- lapply(d[, cols], as.numeric)

cols <- names(d)[grep("proc_cd", names(d))]
d[, cols] <- apply(d[, cols], 2, function(x) gsub("---", "9999", x))
d[, cols] <- lapply(d[, cols], as.numeric)

ae.fu <- d

save(ae.attn, ae.fu, file = "Rdata/ae.Rdata")

# Medications
########################################################

library(data.table)
medication <- fread()
# fast read -> data.table
