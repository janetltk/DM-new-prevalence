rm(list = ls()); gc()
setwd("C:/Users/janet/Desktop/2017_data")

#######################################################
# Lab Glucose: Diagnosis of diabetes mellitus (DM)
#######################################################
library(data.table)

FunExplore <- function (x) {
  # Summary and percentiles
  # Convert to Data Table
  #
  print(head(d))
  cat("\n", "Full dataset dim = ", dim(d), "\n")
  if(any(is.na(d))) stop("MISSING DATA")
  d$ref_date <- as.Date(paste("15", d$ref_date, sep = ""), "%d%b%Y")
  d <- d[order(d$serial_no, d$ref_date),]
  cat("\n")
  print(summary(d[, -1]))
  cat("\n", "Lab values")
  # Summary of values in column 3
  names(d) [names(d) %in% x] <- "glucose"
  print(quantile(d$glucose, c(.00001, .0001, .001, .01, .99, .999, .9999, .99999)))
  cat("\n", "Unique patient IDs = ", length(unique(d$serial_no)), "\n")
  data.table(d, key = "serial_no")
}

FunDisplayOutliers <- function (lower, upper) {
    # Function: Number of diabetes patients between lab values cut-off values
    # Args:
    #   lower: above lower cut-off is diagnostic of DM
    #   upper: outliers (no. of DM patients above upper cut-off)
    # Returns:
    #   Number of (unique) DM patients
    #
  d <- as.data.table(d)
  all.dm <- (d[glucose >= lower, list(ref_date = min(ref_date)), 
               by = list(serial_no)])
    a <- length(unique(all.dm$serial_no))
    print(paste("All DM (>= ", lower, ") equals ", a," unique serial_no", sep = ""))
    clean.dm <- (d[which(glucose >= lower & glucose < upper), list(ref_date = min(ref_date)), by = list(serial_no)])
    b <- length(unique(clean.dm$serial_no))
    print(paste(">= ", lower," & <= ", upper," equals ",b," unique serial_no",sep = ""))
    print(paste("Difference in serial_no is", a - b))
    print(paste("Difference of ", round((a - b) / a * 100, d = 3), "%", sep = ""))
}

FunSubsetDM <- function (varname, lower, upper) {
    # Function: Number of diabetes patients between lab values cut-off values
    # Args:
    #   lower: above lower cut-off is diagnostic of DM
    #   upper: remove outliers ( DM patients above upper cut-off)
    # Returns:
    #   Subset DM patients, outliers removed
    pos <- d[(glucose >= lower & glucose < upper),] 
    print(paste("min = ", min(pos$glucose), " max = ", max(pos$glucose)))
    cat("\n")
    print(summary(pos))
    pos
    setkey(pos, serial_no, ref_date) # unique serial_no and months
    # pos <- unique(pos) seperate months?
    pos <- pos[, list(ref_date = min(ref_date), ref_date2 = (sort(ref_date, partial = 2)[2])), by = list(serial_no)]
    if(any(is.na(pos))) colSums(is.na(pos))	
    if(any(pos$ref_date2 < pos$ref_date, na.rm = T)) stop("2nd dx date is before 1st dx date") 
    if(anyDuplicated(pos$serial_no)) stop("Duplicate serial_no")
    setnames (pos, "ref_date", paste0(varname, ".date"))
    setnames (pos, "ref_date2", paste0(varname, ".date2"))
    cat("\n\n", "Unique patients = ", length(unique(pos$serial_no)), "\n")
    data.frame(pos)
    }

FunSubsetAJE <- function (varname, lower, upper) {
    # Function: Number of diabetes patients between lab values cut-off values (AJE criteria)
    # Args:
    #   lower: above lower cut-off is diagnostic of DM
    #   upper: remove outliers ( DM patients above upper cut-off)
    # Returns:
    #   Subset DM patients, outliers removed
    pos <- d[(glucose >= lower & glucose < upper),] 
    print(paste("min = ", min(pos$glucose), " max = ", max(pos$glucose)))
    cat("\n")
    pos[order(serial_no, ref_date), list(serial_no, ref_date)]
}

########################################################
# Hba1c >= 6.5% (>= 48mmol/mol) -> diagnostic of diabetes
# range: NGSP/DCCT >24% (IFCC 238.8 mmol/mol)
########################################################
d <- readRDS("hba1c.rds")
colSums(is.na(d))
d <- na.omit(d)
table(d$test_unit, exclude = NULL) #hba1c data in 3 units: %, %Hb and mmol/mol 

p <- ggplot(d, aes(x=hba1c, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 100))

d$ngsp <- ifelse(d$test_unit == "%" | d$test_unit == "% Hb", d$hba1c, (0.09148 * d$hba1c) + 2.152) #National Glycohemoglobin Standardization Program - http://www.ngsp.org/ifcc.asp
d$ifcc <- ifelse(d$test_unit == "mmol/mol", d$hba1c, (d$hba1c - 2.152) / 0.09148) #International Federation of Clinical Chemistry Working Group
head(d[d$test_unit == "%", ])
head(d[d$test_unit == "% Hb", ])
head(d[d$test_unit == "mmol/mol", ])

p <- ggplot(d, aes(x=ngsp, group=test_unit, colour=test_unit)) 
p + geom_density() + scale_x_continuous(limits = c(0, 10))

# round all hba1c readings to 1 decimal place
d[, c("ngsp", "ifcc")] <- round(d[, c("ngsp", "ifcc")], 1)
names(d)[names(d) == "hba1c"] <- "mixed_units"
names(d)[names(d) == "ngsp"] <- "hba1c"

d <- FunExplore ("hba1c")
# Unique patient IDs =  1265381 

# HbA1c of 24% = estimated Average Blood Glu of 35.7 mmol/L -> OUTLIER
# ref: https://professional.diabetes.org/diapro/glucose_calc
FunDisplayOutliers (lower = 6.5, upper = 24)
# 646,022 patients have HbA1c >= 6.5%
# 545,999 patients have HbA1c >= 6.5% & <= 24% 
# Difference of 23 serial no. 

hba1c <- FunSubsetDM (varname = "hba1c", lower = 6.5, upper = 24)
# Unique patients =  645999
aje.hba1c <- FunSubsetAJE (varname = "hba1c", lower = 6.5, upper = 24)
length(unique(aje.hba1c$serial_no))
# Unique patients =  645999

# prediabetes Hba1c 5.7% to 6.5%
prediabetes <- FunSubsetDM (varname = "hba1c", lower = 5.7, upper = 6.5)
# Unique patients =  864062 
prediabetes[,c("hba1c.date2")] <- NULL

###################################################################
# Fasting glucose >= 7.0mmol/l (126mg/dl) -> diagnostic of diabetes
###################################################################
d <- readRDS("fasting_gc.rds")
colSums(is.na(d)) #983 NA
d <- na.omit(d)
table(d$test_unit, exclude = NULL) # mmol/l   mmol/L

d <- FunExplore ("fasting_gc")
# Unique patient IDs =  1,557,368 

# Fasting glucose > 39mmol/L -> OUTLIER
FunDisplayOutliers (lower = 7, upper = 39)
# 601,335 patients have fasting glu >= 7.0
# 601,283 patients have fasting glu >= 7.0 & < = 39
# Difference = 52 serial no. 
# Difference of 0.009%

fasting <- FunSubsetDM (varname = "fasting", lower = 7, upper = 39)
# Unique patients =  601283 
aje.fasting <- FunSubsetAJE (varname = "fasting", lower = 7, upper = 39)
length(unique(aje.fasting$serial_no))

# impaired fasting glucose 5.6 mmol/l (100 mg/dl) to 7.0 (126)
IFG <- FunSubsetDM (varname = "fasting", lower = 5.6, upper = 7)
# Unique patients =  1127221
IFG[,3] <- NULL

#########################################################
# Random plasma glucose >= 11.1 mmol/l (200mg/dl) -> diagnostic of diabetes
#######################################################
d <- readRDS("random_gc.rds")
colSums(is.na(d)) #no NA
table(d$test_unit, exclude = NULL) # mmol/l   mmol/L

d <- FunExplore ("random_gc")
# Unique patient IDs =  1,165,060

# Random glucose >39mmol/L -> OUTLIER
FunDisplayOutliers(lower = 11.1, upper = 39)
# 422,279 patients have Random glucose >= 11.1 
# 421,040 patients have Random glucose >= 11.1 & < = 39
# Difference of 1239 serial no. 
# Difference of 0.293%

random <- FunSubsetDM (varname = "random", lower = 11.1, upper = 39)
# Unique patients =  421040
aje.random <- FunSubsetAJE (varname = "random", lower = 11.1, upper = 39)
length(unique(aje.random$serial_no))

#########################################################
# OGTT, 2-hour plasma glucose >= 11.1mmol/l (200mg/dl)
#########################################################
d <- readRDS("ogtt.rds")
colSums(is.na(d)) #no NA
table(d$test_unit, exclude = NULL) # mmol/l mmol/L

# flag_2hrs: indicate whether the result is 2 hours apart from the preceding one
table(d$flag_2hrs)
d <- d[d$flag_2hrs == "Y",]

d <- FunExplore ("ogtt")
# Unique patient IDs =  153804 

# Last OGTT value > 39mmol/L -> OUTLIER
FunDisplayOutliers(lower = 11.1, upper = 39)
# 33578 patients have last OGTT value >= 11.1 
# 33576 patients have last OGTT value >= 11.1 & < = 39
# Difference of 2 serial no. 

ogtt <- FunSubsetDM (varname = "ogtt", lower = 11.1, upper = 39)
# 33576 SELECTED

# Impaired glucose tolerance (>= 7.8 mmol/L and <11.1mmol/L)
IGT <- FunSubsetDM (varname = "ogtt", lower = 7.8, upper = 11.1)
# Unique patients = 81395
IGT[,3] <- NULL

################################################################
# Merge lab data
################################################################
rm(d)
# Merge positive diagnosis dates
lab <- Reduce(function(x, y) merge(x,y, by = "serial_no", all = T), list(hba1c, fasting, ogtt, random))
if(anyDuplicated(lab$serial_no)) stop("Duplicate serial_no")
length(unique(lab$serial_no))
# 867,618 patients positive from lab glucose tests

# AJE excluded OGTT data
save(aje.hba1c, aje.fasting, aje.random, file = "diagnosis/lab_glucose_aje.Rdata")

# Merge pre-diabetes
prediabetes <- Reduce(function(x, y) merge(x,y, by = "serial_no", all = T), list(prediabetes, IFG, IGT))
head(prediabetes)
prediabetes$prediabetes <- with(prediabetes, pmin(hba1c.date, ogtt.date, fasting.date, na.rm = T))
if(anyDuplicated(prediabetes$serial_no)) stop("Duplicate serial_no")
length(unique(prediabetes$serial_no))
# 1,392,331

save(lab, prediabetes, file = "diagnosis/lab_glucose.Rdata")

# Analysis
str(lab)
head(lab)
colSums(!is.na(lab))
length(lab[!is.na(lab$hba1c.date) | !is.na(lab$fasting.date) | !is.na(lab$ogtt.date), c("serial_no")])
#744,107
