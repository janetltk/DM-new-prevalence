#########################################
# DATA CLEAN 
#########################################
rm(list = ls())
library(foreign)

#####################################################
# Patient list (self-reported diabetes diagnosis)
#####################################################
d <- read.dta("data/patient_list.dta")
str(d)
if(any(is.na(d))) {
  print("MISSING DATA")
  colSums(is.na(d))
}
if(anyDuplicated(d$serial_no)) stop("Duplicate serial_no")

names(d)[names(d) %in% "year_of_birth"] <- "dob"
names(d)[names(d) %in% "first_dm_diag_date"] <- "self.reported"
names(d)[names(d) %in% "death_date"] <- "death.date"

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

# status: code unclassified as missing
status <- c("cssa_status", "ha_staff", "ha_dep", "ha_retir", "cs_staff", "cs_dep", "cs_pen", "ramp", "nahc")
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
table(d$smoking, exclude = NULL)
levels (d$smoking) <- c("Yes", "Ex", "Non-smoker", NA)
table(d$smoking, exclude = NULL)
d$curr_smoking_status <- NULL

patient <- d
save(patient, file = "Rdata/patient.Rdata")

# Exploratory analysis
colSums(is.na(d))
d$death.yr <- as.numeric(format(d$death.date, "%Y"))
table(d$death.yr)

d$death.age <- ifelse(is.na(d$death.yr), NA, d$death.yr - d$dob)

FunAgeCat <- function(x, lower = 0, upper = 85, by = 5, sep = "-", above.char = "+") {
 
# Cut age into age categories (5-year strata)
# original
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

###

# Janet's codes from "10 age and sex standardisation" - modify later
__ <- mutate(__, group = cut(__, breaks=c(seq(from = 14, to = 84, by = 5), Inf), labels=c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))) %>% as.data.table
df <- df[, .(nv_50k = mean(nv_50k), nv_100k = mean(nv_100k), nv_200k = mean(nv_200k), diff.spend = mean(diff.spend.mod)), keyby = .(group, female)]

###

#####################################################
# CLINICAL DATASETS
#####################################################
rm(list = setdiff(ls(), lsf.str()))
load("Rdata/patient.Rdata") # line 70 above
library(foreign); library(ggplot2)

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
d <- read.dta("data/bmi.dta")
colSums(is.na(d))

# check missing
d[is.na(d$height),]
d[is.na(d$weight),]

# check bmi = calculation
d$b <- with(d, weight/(height)^2)
d$dif <- d$bmi - d$b
summary(d$dif)
d[d$dif > 0.1, ]
d$bmi[!is.na(d$b)] <- round(d$b[!is.na(d$b)], 2)

d <- d[, c("serial_no", "ref_date", "bmi")]
bmi <- FunClean("bmi", lower = 15, upper = 150)
save(bmi, file = "Rdata/bmi_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))

# SBP range (FAMILY cohort): 80-230
# DBP range: 20-200
# MAP range: 40-210
#######################################################
d <- read.dta("data/bp.dta")
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

save(sbp, dbp, map, file = "Rdata/bp_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))

# HbA1c range: NGSP/DCCT >3% (IFCC 39 mmol/mol), <24% (238.8 mmol/mol)
########################################################
# HbA1c of 3% = estimated Average Blood Glu of 2.2 mmol/L
# HbA1c of 24% = estimated Average Blood Glu of 35.7 mmol/L
# ref: http://professional.diabetes.org/GlucoseCalculator.aspx
# NGSP/IFCC conversion: http://www.ngsp.org/ifccngsp.asp
d <- read.dta("data/hba1c.dta")
colSums(is.na(d))
table(d$test_unit, exclude = NULL)

tapply(d$hba1c, d$test_unit, summary)
p <- ggplot(d, aes(x=hba1c, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 100))

d$ngsp <- ifelse(d$test_unit == "%" | d$test_unit == "% Hb", d$hba1c, (0.09148 * d$hba1c) + 2.152)
d$ifcc <- ifelse(d$test_unit == "mmol/mol", d$hba1c, (d$hba1c - 2.152) / 0.09148)
head(d[d$test_unit == "%", ])
head(d[d$test_unit == "% Hb", ])
head(d[d$test_unit == "mmol/mol", ])

tapply(d$ngsp, d$test_unit, summary)
p <- ggplot(d, aes(x=ngsp, group=test_unit, colour=test_unit)) 
p + geom_density() + scale_x_continuous(limits = c(0, 10))

hba1c <- FunClean("ngsp", lower = 3, upper = 24)

# both units (% and mmol/mol) used from 2011
with(hba1c, table(format(ref_date, "%Y"), test_unit))
with(hba1c[format(hba1c$ref_date, "%Y") == 2011,], table(ref_date, test_unit))

# 1 decimal place
head(hba1c[hba1c$test_unit == "%", ]) 
head(hba1c[hba1c$test_unit == "mmol/mol", ]) 
hba1c[, c("ngsp", "ifcc")] <- round(hba1c[, c("ngsp", "ifcc")], 1)

# 6% == 42 mmol/mol
head(hba1c[hba1c$test_unit == "%" & hba1c$hba1c == 6, ])
head(hba1c[hba1c$test_unit == "% Hb" & hba1c$hba1c == 6, ])
head(hba1c[hba1c$test_unit == "mmol/mol" & hba1c$hba1c == 42.0, ])

hba1c$hba1c <- NULL
hba1c$test_unit <- NULL
names(hba1c)[names(hba1c) == "ngsp"] <- "hba1c"

save(hba1c, file = "Rdata/hba1c_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))

# TC >0.1 & <25 mmol/L
# HDL >0.1 & <5 mmol/L
# LDL >0.1 & <20 mmol/L
# Total Cholesterol: HDL Cholesterol ratio <1.5, >20 
########################################################
d <- read.dta("data/cholesterol.dta")
colSums(is.na(d))
table(d$test_unit_totclt, exclude = NULL)
table(d$test_unit_hdl, exclude = NULL)
table(d$test_unit_ldl, exclude = NULL)

names (d)[names(d)%in% "total_clt"] <- "tc"
names (d)[names(d)%in% "hdl_c"] <- "hdl"
names (d)[names(d)%in% "ldl_c"] <- "ldl"

tapply(d$tc, d$test_unit_totclt, function (x) summary(x, na.rm = T))
tapply(d$hdl, d$test_unit_hdl, function (x) summary(x, na.rm = T))
tapply(d$ldl, d$test_unit_ldl, function (x) summary(x, na.rm = T))
chol <- d

d <- na.omit(chol[, c("serial_no", "ref_date", "tc")])
tc <- FunClean("tc", lower = 0.1, upper = 25) 

d <- na.omit(chol[, c("serial_no", "ref_date", "hdl")])
hdl <- FunClean("hdl", lower = 0.1, upper = 5) 

d <- na.omit(chol[, c("serial_no", "ref_date", "ldl")])
ldl <- FunClean("ldl", lower = 0.1, upper = 20) 

chol$lr <- with(chol, round(tc/hdl, 3))
d <- na.omit(chol[, c("serial_no", "ref_date", "lr")])
ggplot(d, aes(lr)) + geom_density() + scale_x_continuous(limits = c(0, 50))
lr <- FunClean("lr", lower = 1.5, upper = 20)

chol$nhdl <- with(chol, tc - hdl)
summary(chol$nhdl)
d <- na.omit(chol[, c("serial_no", "ref_date", "nhdl")])
ggplot(d, aes(nhdl)) + geom_density() + scale_x_continuous(limits = c(0, 50))
nhdl <- FunClean("nhdl", lower = 0.01, upper = 25)

save(tc, hdl, ldl, lr, nhdl, file = "Rdata/cholesterol_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))

########################################################
# ACR range: >0, <1000
########################################################
d <- read.dta("data/urine_acr.dta")
colSums(is.na(d))
table(d$test_unit, exclude = NULL)

tapply(d$urine_acr, d$test_unit, summary)
p <- ggplot(d, aes(x=urine_acr, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 10))

urine_acr <- FunClean("urine_acr", lower = 0.0001, upper = 1000)
save(urine_acr, file = "Rdata/urine_acr_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))

########################################################
# Urine_alb range: 0-5000 mg/24h and 0-500 mg/L
########################################################
d <- read.dta("data/urine_alb.dta")
colSums(is.na(d))
table(d$test_unit, exclude = NULL)

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

save(urine_alb_24h, urine_alb_spot, file = "Rdata/urine_alb_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))

########################################################
# Haemoglobin range: 4-20 g/dL
########################################################
d <- read.dta("data/haemoglobin.dta")
colSums(is.na(d))
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
# not plausible -> remove 992 values (0.006%)
d <- d[d$test_unit != "g/L" & d$test_unit != "mg/L", c("serial_no", "ref_date", "haemoglobin")]
haemoglobin <- FunClean("haemoglobin", lower = 4, upper = 20)
save(haemoglobin, file = "Rdata/haemoglobin_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))

########################################################
# Serum Creatinine & eGFR Chinese
# Remove creatinine 0.1 to 2000 micromoles/L
########################################################
d <- read.dta("data/creatinine.dta")
colSums(is.na(d))
table(d$test_unit, exclude = NULL)

tapply(d$creatinine, d$test_unit, summary)
p <- ggplot(d, aes(x=creatinine, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 500))

d <- d[, c("serial_no", "ref_date", "creatinine")]
d <- FunClean("creatinine", lower = 0.1, upper = 2000)

# eGRF
d <- merge(d, patient[, c("serial_no", "female", "dob")], by = "serial_no")
d$age <- as.numeric(format(d$ref_date, "%Y")) - d$dob

# MDRD formula
d$egfr_mdrd <- with(d, 186 * ((creatinine * 0.011) ^ -1.154) * (age ^ -0.203) * (.742 ^ female))

# MDRD formula for Chinese (creatinine in umol/L)
# doi: 10.1681/ASN.2006040368
d$egfr_chinese <- d$egfr_mdrd * 1.227

colSums(is.na(d))
FunStat(na.omit(d$egfr_mdrd))
summary(d[d$egfr_mdrd==Inf,]) # age == 0
length(unique(d$serial_no))
# rm if age = 0
d <- d[d$age > 0,] 
FunStat(na.omit(d$egfr_mdrd))
length(unique(d$serial_no))

creatinine <- d[, c("serial_no", "ref_date", "creatinine", "egfr_chinese", "egfr_mdrd")]
summary(creatinine)
save(creatinine, file = "Rdata/creatinine_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))

########################################################

# Others

########################################################
# egfr range: 0.0001-1000 mL/min/1.73 m^2
########################################################
d <- read.dta("data/egfr.dta")
colSums(is.na(d))
table(d$test_unit, exclude = NULL)

tapply(d$egfr, d$test_unit, summary)
p <- ggplot(d, aes(x=egfr, group=test_unit, colour=test_unit)) 
p + geom_density(aes(fill=test_unit), alpha=0.3)
p + geom_density() + scale_x_continuous(limits = c(0, 500))

d <- d[, c("serial_no", "ref_date", "egfr")] 
egfr <- FunClean("egfr", lower = 0.0001, upper = max(d$egfr))
save(egfr, file = "Rdata/egfr_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))


########################################################
# Pulse range: 25-200?
########################################################
d <- read.dta("data/pulse.dta")
colSums(is.na(d))

ggplot(d, aes(pulse)) + geom_density()
d <- d[, c("serial_no", "ref_date", "pulse")] 
pulse <- FunClean("pulse", lower = 25, upper = 200)
save(pulse, file = "Rdata/pulse_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))

########################################################
# Triglyceride range: 0-100 mmol/L
########################################################
d <- read.dta("data/triglyceride.dta")
colSums(is.na(d))
table(d$test_unit, exclude = NULL)

ggplot(d, aes(triglyceride)) + geom_density()
d <- d[, c("serial_no", "ref_date", "triglyceride")] 
triglyceride <- FunClean("triglyceride", lower = 0.1, upper = 100)
save(triglyceride, file = "Rdata/triglyceride_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))

########################################################
# WBC range: 0.1-250 x10^9/L
########################################################
d <- read.dta("data/wbc.dta")
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
save(wbc, file = "Rdata/wbc_clean.Rdata")
rm(list = setdiff(ls(), c(lsf.str(), "patient")))
