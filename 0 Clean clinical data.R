#########################################
# DATA CLEAN 
#########################################
rm(list = ls())

#####################################################
# Patient list (self-reported diabetes diagnosis)
#####################################################
file_location <- "C:/Users/janet/Desktop/2017_data/"
d <- readRDS(paste0(file_location, "patient_list.rds"))

str(d)

# Format NA
d$death_date <- gsub(pattern = "      .", replacement = NA, x = d$death_date)
d$first_dm_diag_date <- gsub(pattern = "      .", replacement = NA, x = d$first_dm_diag_date)

if(any(is.na(d))) {
    print("MISSING DATA")
    colSums(is.na(d))
}
if(anyDuplicated(d$serial_no)) stop("Duplicate serial_no")

names(d)[names(d) %in% "year_of_birth"] <- "dob"
names(d)[names(d) %in% "first_dm_diag_date"] <- "self.reported"
names(d)[names(d) %in% "death_date"] <- "death.date"

# Format dates
head(d[ ,c("self.reported", "death.date")]) 
FunDate15 <- function(x) {
  # Replace missing date as 15
  as.Date(paste("15", x, sep = ""),"%d%b%Y")
}
d[,c("self.reported", "death.date")] <- lapply (d[ ,c("self.reported", "death.date")], FunDate15)
head(d[ ,c("self.reported", "death.date")]) 

sum(!is.na(d$self.reported))
sum(!is.na(d$death.date))

# female
names(d)[names(d) %in% "sex"] <- "female"
d$female[d$female == "F"] <- TRUE 
d$female[d$female == "M"] <- FALSE
d$female[d$female == "U"] <- NA
d$female <- as.logical(d$female)
table(d$female, exclude = NULL)
#FALSE (male)   TRUE (female)
#889249         958542 

# status: code unclassified as missing
status <- c("CSSA_status", "HA_staff", "HA_dep", "HA_retir", "CS_staff", "CS_dep", "CS_pen", "ramp", "nahc")
sapply(d[, c(status)], function (x) table (x, exclude = NULL))

FunUnclassified <- function (x) {
    x <- replace (x, x == "U", NA)
    x <- replace (x, x == "Y", TRUE)
    x <- replace (x, x == "N", FALSE)
    as.logical (x)
}

d[, c(status)] <- sapply(d[, c(status)], function (x) FunUnclassified(x)) 

# district
stopifnot(any(!is.na(d$district)))
table(d$district, exclude = NULL) # no unclassified or missing
d$district <- as.factor(d$district)
    
# smoking
table(d$curr_smoking_status, exclude = NULL) # More ex-smokers than smokers?
# HA coding for smoking (1 = smoker, 2 = ex-smoker, 3 = never smoker)
# code unknown status ("9") as = NA
d$smoking <- as.factor(d$curr_smoking_status)
levels (d$smoking) <- c("Yes", "Ex", "Non-smoker", NA)
table(d$smoking, exclude = NULL)
d$curr_smoking_status <- NULL

saveRDS(d, paste0(file_location, "sorted/patient_list.rds"))

# Exploratory analysis
colSums(is.na(d))
d$death.yr <- as.numeric(format(d$death.date, "%Y"))
table(d$death.yr)

d$death.age <- ifelse(is.na(d$death.yr), NA, d$death.yr - d$dob)

FunAgeCat <- function(x, lower = 0, upper = 85, by = 5, sep = "-", above.char = "+") {
  # Cut age into age categories (5-year strata)
  labs <- c(paste(seq(lower, upper - by, by = by), 
                  seq(lower + by - 1, upper - 1, by = by), sep = sep),
            paste(upper, above.char, sep = "")) 
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf), 
      right = FALSE, labels = labs)
}

# Deaths by year, age groups
d$death.agecat <- FunAgeCat(d$death.age)
table(d$death.agecat)
deaths <- table(d$death.agecat, d$death.yr)
deaths <- rbind(deaths, colSums(deaths))
deaths

#####################################################
# CLINICAL DATASETS
#####################################################
rm(list = setdiff(ls(), lsf.str()))
patient <- readRDS(paste0(file_location, "sorted/patient_list.rds"))
library(ggplot2)
  
FunDeaths <- function (d) {
# observations after death
if (any(d$ref_date > d$death.date & !is.na(d$death.date), na.rm = T)) {
    print("observations after death")
    print(sum(d$ref_date > d$death.date & !is.na(d$death.date)))
    d[d$ref_date <= d$death.date | is.na(d$death.date), ]
    } else { 
    d
    } 
}

FunBirth <- function (d) {
# observations before birth
    d$yr <- format(d$ref_date, "%Y")
if (any(d$yr < d$dob, na.rm = T)) {
    print("observations before birth")
    print(sum(d$yr < d$dob))
    d[d$yr >= d$dob, ]
    } else { 
    d
    } 
}

FunStat <- function(x){
  # Summary and percentiles
  #
  a1 <- summary(x)
  a2 <- paste(round(mean(x, 2)), "(", round(sd(x), 2), ")", sep = "")
  a3 <- quantile(x, c(.00001, .0001, .001, .01, .1, .9, .99, .999, .9999, .99999))
  result <- list(Summary = a1, Mean_sd = a2, Percentile = a3)
  return(result)
}

FunClean <- function (label, lower, upper){
  # clean dataset of outliers
  #
  # Args:
  #   label: variable
  #   upper/lower bounds
  #
  d$ref_date <- as.Date(paste("15", d$ref_date, sep = ""), "%d%b%Y")
  
  d <- merge(d, patient[, c("serial_no", "dob", "death.date")])
  d <- FunDeaths (d) # observations after death
  d <- FunBirth (d) # observations before birth
  d[, c("dob", "death.date", "yr")] <- list(NULL)
  
  print("dataset")
  print(dim(d))
  print(FunStat(d[, c(label)]))
  par(mfrow = c(1,2))
  plot(density(d[,c(label)]), main = paste(label, "(raw data)"))
  d <- d[d[, c(label)] >= lower & d[, c(label)] < upper, ]
  plot(density(d[, c(label)]), 
       main = substitute(paste(l <= lab) < u, list(l = lower, lab = label, u = upper)))
  length(unique(d$serial_no))
  print("CLEANED")
  print(summary(d)); cat("\n")
  print(paste("Mean (sd)", round(mean(d[, c(label)], 2)), "(", 
      round(sd(d[, c(label)]), 2), ")", sep = ""))
  d
}

# BMI range (FAMILY cohort): 15-150
#####################################################
d <- readRDS(paste0(file_location, "bmi.rds"))
colSums(is.na(d))

# check missing
d[is.na(d$height),]
d[is.na(d$weight),]

# check bmi = calculation
d$b <- with(d, weight/(height)^2)
d$dif <- d$bmi - d$b
summary(d$dif)
d[d$dif > 0.1, ]
d$bmi[!is.na(d$b)] <- round(d$b[!is.na(d$b)], digits = 2) #replace with calculated bmi corrected to nearest 2 d.p.
d[is.na(d$b),] #two observations with NA height/weight

d <- d[, c("serial_no", "ref_date", "bmi")]
bmi <- FunClean("bmi", lower = 15, upper = 150)
saveRDS(bmi, paste0(file_location, "sorted/bmi.rds"))

rm(list = setdiff(ls(), c(lsf.str(), "patient", "file_location")))
   
# SBP range (FAMILY cohort): 80-230
# DBP range: 20-200
# MAP range: 40-210
#######################################################
d <- readRDS(paste0(file_location, "bp.rds"))
colSums(is.na(d))

names (d)[names(d)%in% "systolic_bp"] <- "sbp"
names (d)[names(d)%in% "diastolic_bp"] <- "dbp"

bp <- d
d <- bp[, c("serial_no", "ref_date", "sbp")]
sbp <- FunClean("sbp", lower = 80, upper = 230)
sbp$dbp <- NULL

d <- bp[, c("serial_no", "ref_date", "dbp")]
dbp <- FunClean("dbp", lower = 20, upper = 200)
dbp$sbp <- NULL

bp$map <- with(bp, sbp/3 + (dbp * 2/3))
d <- bp[, c("serial_no", "ref_date", "map")]
map <- FunClean("map", lower = 40, upper = 210)

save(sbp, dbp, map, file = paste0(file_location, "sorted/bp.Rdata"))
rm(list = setdiff(ls(), c(lsf.str(), "patient", "file_location")))

# HbA1c range: NGSP/DCCT >3% (IFCC 39 mmol/mol), <24% (238.8 mmol/mol)
########################################################
# HbA1c of 3% = estimated Average Blood Glu of 2.2 mmol/L
# HbA1c of 24% = estimated Average Blood Glu of 35.7 mmol/L
# ref: http://professional.diabetes.org/GlucoseCalculator.aspx
# NGSP/IFCC conversion: http://www.ngsp.org/ifccngsp.asp
d <- readRDS(paste0(file_location, "hba1c.rds"))
colSums(is.na(d)) #7152 NA hba1c
d <- na.omit(d)
table(d$test_unit, exclude = NULL) #hba1c data in 3 units: %, %Hb and mmol/mol

tapply(d$hba1c, d$test_unit, summary)
p <- ggplot(d, aes(x=hba1c, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 100))

d$ngsp <- ifelse(d$test_unit == "%" | d$test_unit == "% Hb", d$hba1c, (0.09148 * d$hba1c) + 2.152) #National Glycohemoglobin Standardization Program - http://www.ngsp.org/ifcc.asp
d$ifcc <- ifelse(d$test_unit == "mmol/mol", d$hba1c, (d$hba1c - 2.152) / 0.09148) #International Federation of Clinical Chemistry Working Group
head(d[d$test_unit == "%", ])
head(d[d$test_unit == "% Hb", ])
head(d[d$test_unit == "mmol/mol", ])

tapply(d$ngsp, d$test_unit, summary)
p <- ggplot(d, aes(x=ngsp, group=test_unit, colour=test_unit)) 
p + geom_density() + scale_x_continuous(limits = c(0, 10))

hba1c <- FunClean("ngsp", lower = 3, upper = 24)

# see pattern of unit usage from 2006 to 2017
with(hba1c, table(format(ref_date, "%Y"), test_unit))
with(hba1c[format(hba1c$ref_date, "%Y") == 2011,], table(ref_date, test_unit)) #Only %/%Hb were used before 5/2011. mmol/m was used since 5/2011. 

# round all hba1c readings to 1 decimal place
head(hba1c[hba1c$test_unit == "%", ]); head(hba1c[hba1c$test_unit == "mmol/mol", ]) 
hba1c[, c("ngsp", "ifcc")] <- round(hba1c[, c("ngsp", "ifcc")], 1)

hba1c$hba1c <- NULL
hba1c$test_unit <- NULL
names(hba1c)[names(hba1c) == "ngsp"] <- "hba1c"

saveRDS(hba1c, paste0(file_location, "sorted/hba1c.rds"))
rm(list = setdiff(ls(), c(lsf.str(), "patient", "file_location")))

# TC >0.1 & <25 mmol/L
# HDL >0.1 & <5 mmol/L
# LDL >0.1 & <20 mmol/L
# Total Cholesterol: HDL Cholesterol ratio <1.5, >20 
########################################################
d <- readRDS(paste0(file_location, "cholesterol.rds"))
colSums(is.na(d))
table(c(d$test_unit_totclt, d$test_unit_hdl, d$test_unit_totclt)) #2: mmol/l, mmol/L(same)

names(d) <- c("serial_no", "tc", "unit_tc", "hdl", "unit_hdl", "ldl", "unit_ldl", "ref_date")
  
tapply(d$tc, d$unit_tc, function (x) summary(x, na.rm = T))
tapply(d$hdl, d$unit_hdl, function (x) summary(x, na.rm = T))
tapply(d$ldl, d$unit_ldl, function (x) summary(x, na.rm = T)) #same distribution of values of mmol/l and mmol/U units
chol <- d

d <- na.omit(chol[, c("serial_no", "ref_date", "tc")])
tc <- FunClean("tc", lower = 0.1, upper = 25) 

d <- na.omit(chol[, c("serial_no", "ref_date", "hdl")])
hdl <- FunClean("hdl", lower = 0.1, upper = 5) 

d <- na.omit(chol[, c("serial_no", "ref_date", "ldl")])
ldl <- FunClean("ldl", lower = 0.1, upper = 20) 

chol$lr <- with(chol, round(tc/hdl, 3)) #cholesterol ratio
d <- na.omit(chol[, c("serial_no", "ref_date", "lr")])
ggplot(d, aes(lr)) + geom_density() + scale_x_continuous(limits = c(0, 50))
lr <- FunClean("lr", lower = 1.5, upper = 20)

chol$nhdl <- with(chol, tc - hdl) #non-HDL
summary(chol$nhdl)
d <- na.omit(chol[, c("serial_no", "ref_date", "nhdl")])
ggplot(d, aes(nhdl)) + geom_density() + scale_x_continuous(limits = c(0, 50))
nhdl <- FunClean("nhdl", lower = 0.01, upper = 25)

save(tc, hdl, ldl, lr, nhdl, file = paste0(file_location, "sorted/cholesterol.Rdata"))

rm(list = setdiff(ls(), c(lsf.str(), "patient", "file_location")))

########################################################
# ACR range: >0, <1000
########################################################
d <- readRDS(paste0(file_location, "urine_acr.rds"))

colSums(is.na(d))
table(d$test_unit, exclude = NULL) # mg/mmol, mg/mmol Cr, mg/mmol Creat (same)

tapply(d$urine_acr, d$test_unit, summary)
p <- ggplot(d, aes(x=urine_acr, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 10))

d <- na.omit(d)
urine_acr <- FunClean("urine_acr", lower = 0.00001, upper = 1000)

saveRDS(urine_acr, paste0(file_location, "sorted/urine_acr.rds"))
rm(list = setdiff(ls(), c(lsf.str(), "patient", "file_location")))

########################################################
# Urine_alb range: 0-5000 mg/24h and 0-500 mg/L
########################################################
d <- readRDS(paste0(file_location, "urine_alb.rds"))
colSums(is.na(d)) #no NA
table(d$test_unit, exclude = NULL)
#mg/24 hr,   mg/24h,     mg/d,   mg/day,    mg/dL,     mg/l,     mg/L 

tapply(d$urine_alb, d$test_unit, summary) 

p <- ggplot(d, aes(x=urine_alb, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 500))

a <- d

# 24h urine albumin (mg/24h)
d <- a[a$test_unit == "mg/24h" | a$test_unit == "mg/24 hr" | a$test_unit == "mg/d" | a$test_unit == "mg/day",]
tapply(d$urine_alb, d$test_unit, summary)
p <- ggplot(d, aes(x=urine_alb, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 500))
d <- d[, c("serial_no", "ref_date", "urine_alb")]
urine_alb_24h <- FunClean("urine_alb", lower = 0, upper = 5000)
names(urine_alb_24h)[3] <- "urine_alb_24h"

# spot urine_alb (mg/L)
d <- a[a$test_unit != "mg/24h" & a$test_unit != "mg/24 hr" & a$test_unit != "mg/d" & a$test_unit != "mg/day",]
tapply(d$urine_alb, d$test_unit, summary)

d$urine_alb[d$test_unit == "mg/dL"] <- d$urine_alb[d$test_unit == "mg/dL"] * 10
p <- ggplot(d, aes(x=urine_alb, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 200))
d <- d[, c("serial_no", "ref_date", "urine_alb")]
urine_alb_spot <- FunClean("urine_alb", lower = 0, upper = 500)
names(urine_alb_spot)[3] <- "urine_alb_spot"

save(urine_alb_24h, urine_alb_spot, file = paste0(file_location, "sorted/urine_alb.Rdata"))
rm(list = setdiff(ls(), c(lsf.str(), "patient", "file_location")))

########################################################
# Haemoglobin range: 4-20 g/dL
########################################################
d <- readRDS(paste0(file_location, "haemoglobin.rds"))
colSums(is.na(d)) #no NA
table(d$test_unit, exclude = NULL)

tapply(d$haemoglobin, d$test_unit, summary)
p <- ggplot(d, aes(x=haemoglobin, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 150))

# g/L to g/dL -> not plausible values 
d$haemoglobin[d$test_unit == "g/L"] <- d$haemoglobin[d$test_unit == "g/L"] * 0.1

# mg/L to g/dL -> not plausible values 
d$haemoglobin[d$test_unit == "mg/L"] <- d$haemoglobin[d$test_unit == "mg/L"] * 0.0001

tapply(d$haemoglobin, d$test_unit, summary)
# not plausible -> remove 2195 values (0.008%)
d <- d[d$test_unit != "g/L" & d$test_unit != "mg/L", c("serial_no", "ref_date", "haemoglobin")]
haemoglobin <- FunClean("haemoglobin", lower = 4, upper = 20)

saveRDS(haemoglobin, paste0(file_location, "sorted/haemoglobin.rds"))
rm(list = setdiff(ls(), c(lsf.str(), "patient", "file_location")))

########################################################
# Serum Creatinine & eGFR Chinese
# Remove creatinine 0.1 to 2000 micromoles/L
########################################################
d <- readRDS(paste0(file_location, "creatinine.rds"))
colSums(is.na(d)) #12449 NAs
table(d$test_unit, exclude = NULL) ## umol/L,   umol/l,   umol/L, umol/L #

tapply(d$creatinine, d$test_unit, summary) #similar distribution, can assume they are same units

d <- na.omit(d) #remove all NAs
d <- d[, c("serial_no", "ref_date", "creatinine")]
d <- FunClean("creatinine", lower = 0.1, upper = 2000)

# merge with patient basic info to get eGRF
d <- merge(d, patient[, c("serial_no", "female", "dob")], by = "serial_no")
d$age <- as.numeric(format(d$ref_date, "%Y")) - d$dob

# MDRD formula
d$egfr_mdrd <- with(d, 186 * ((creatinine * 0.0113) ^ -1.154) * (age ^ -0.203) * (.742 ^ female))

# MDRD formula for Chinese (creatinine in umol/L)
# doi: 10.1681/ASN.2006040368
d$egfr_chinese <- d$egfr_mdrd * 1.227

# CKD-EPI formula
# CKD-EPI is also better than MDRD in mortality prediction in Chinese (http://doi.org/10.1016/j.diabres.2017.01.010)
library(nephro)
str(d) #convert all variables to numeric vectors
d$sex <- 1 - as.numeric(d$female) #in the function, male are coded as 1 and female as 0 
d$ethnicity <- 0 #assume all individuals are non-Black
d$creatinine_mg <- d$creatinine *0.0113
d$egfr_ckdepi <- CKDEpi.creat(creatinine = d$creatinine_mg, sex = d$sex, age = d$age, ethnicity = d$ethnicity)

colSums(is.na(d))
FunStat(na.omit(d$egfr_mdrd))
FunStat(na.omit(d$egfr_ckdepi))
summary(d[d$egfr_mdrd==Inf,]) # age == 0
length(unique(d$serial_no))
# remove if age = 0
d <- d[d$age > 0,] 

creatinine <- d[, c("serial_no", "ref_date", "creatinine", "egfr_chinese", "egfr_mdrd", "egfr_ckdepi")]
summary(creatinine)
save(creatinine, file = paste0(file_location, "sorted/creatinine.Rdata"))
rm(list = setdiff(ls(), c(lsf.str(), "patient", "file_location")))

########################################################

# Others

########################################################
# egfr range: 0.0001-1000 mL/min/1.73 m^2
########################################################
d <- readRDS(paste0(file_location, "egfr.rds"))
colSums(is.na(d)) #no NA
table(d$test_unit, exclude = NULL)

tapply(d$egfr, d$test_unit, summary) #all same unit
p <- ggplot(d, aes(x=egfr, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 500))

d <- d[, c("serial_no", "ref_date", "egfr")] 
egfr <- FunClean("egfr", lower = 0.0001, upper = max(d$egfr))

saveRDS(egfr, paste0(file_location, "sorted/egfr.rds"))
rm(list = setdiff(ls(), c(lsf.str(), "patient", "file_location")))


########################################################
# Pulse range: 25-200?
########################################################
d <- readRDS(paste0(file_location, "pulse.rds"))
colSums(is.na(d))

ggplot(d, aes(pulse)) + geom_density()
pulse <- FunClean("pulse", lower = 25, upper = 200)
saveRDS(pulse, paste0(file_location, "sorted/pulse.rds"))
rm(list = setdiff(ls(), c(lsf.str(), "patient", "file_location")))

########################################################
# Triglyceride range: 0-100 mmol/L
########################################################
d <- readRDS(paste0(file_location, "triglyceride.rds"))
colSums(is.na(d)) #784 NAs
table(d$test_unit, exclude = NULL) #all are mmol/L

ggplot(d, aes(triglyceride)) + geom_density()
d <- na.omit(d)
d <- d[, c("serial_no", "ref_date", "triglyceride")] 

triglyceride <- FunClean("triglyceride", lower = 0.1, upper = 100)
saveRDS(triglyceride, paste0(file_location, "sorted/triglyceride.rds"))
rm(list = setdiff(ls(), c(lsf.str(), "patient", "file_location")))

########################################################
# WBC range: 0.1-250 x10^9/L
########################################################
d <- readRDS(paste0(file_location, "wbc.rds"))
colSums(is.na(d))
table(d$test_unit, exclude = NULL)

# convert 10^9 and uL
tapply(d$wbc, d$test_unit, summary)
p <- ggplot(d, aes(x=wbc, group=test_unit, colour=test_unit)) 
p + geom_density() + scale_x_continuous(limits = c(0, 1000))

d$wbc[d$test_unit == "/uL"] <- d$wbc[d$test_unit == "/uL"] * 10^6 / 10^9
# "/ ul" = wrong units?

tapply(d$wbc, d$test_unit, summary)
p <- ggplot(d, aes(x=wbc, group=test_unit, colour=test_unit)) 
p + geom_density() + scale_x_continuous(limits = c(0, 250))

d <- d[, c("serial_no", "ref_date", "wbc")] 
wbc <- FunClean("wbc", lower = 0.1, upper = 250)
saveRDS(wbc, paste0(file_location, "sorted/wbc.rds"))
rm(list = setdiff(ls(), c(lsf.str(), "patient")))

