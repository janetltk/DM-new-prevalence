# CHD <- Sudden death (UKPDS)
########################################################
# Sudden death (ICD-9 code 798 and 798.9)
code1 <- 798
code2 <- 799

source(run_dx)
suddendeath <- data.frame(d)
save(suddendeath, gopc, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/suddendeath.Rdata")

chd.suddendeath <- data.table(rbind(chd, suddendeath))
chd.suddendeath <- chd.suddendeath[, list(adate = min(adate)), by = "serial_no"]
dim(chd); dim(chd.suddendeath)
chd.suddendeath <- data.frame(chd.suddendeath)
dim(gopc); dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
		  
table(format(chd$adate, "%Y"))
table(format(chd.suddendeath$adate, "%Y"))
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()

#####################
rm(list = ls())

########################################################
# Attendance codes: CHD
########################################################

# GOPC data: Jan 2006 to Dec 2013
# Hx of CHD
'
K74 Ischaemic heart disease w. angina
K75 Acute myocardial infarction
K76 Ischaemic heart disease w/o angina
'
load("D:/dm_data/Rdata/gopc.Rdata")
columns <- c("serial_no", "adate", paste0("icpc", 1:17))
d <- gopc[, columns]
str(d)

# Diagnosis codes (ICPC): columns 3-19
i1 <- apply(d[, 3:19], 1, function(x) length(grep("K74", x)) > 0)
i2 <- apply(d[, 3:19], 1, function(x) length(grep("K75", x)) > 0)
i3 <- apply(d[, 3:19], 1, function(x) length(grep("K76", x)) > 0)

index <- pmax(i1, i2, i3)
gopc <- d[index == 1, ]
print(length(unique(gopc$serial_no)))
# 34,685 patients
rm(d)

########################################################
# ICD-9 codes
########################################################

# Hx of CHD from 410 < 415
# CHD as per UKPDS MI (410) and sudden death (798)
# CHD as per CUHK CHD ICD9 MI(410) and IHD (411-414)
# STROKE as per CUHK  (430-434, 436), exclude 435 (TIA) ?death due to 437 & 438?

code1 <- 410
code2 <- 415

columns <- c("serial_no", "adate", paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:15))

FunICDcodes <- function (d) {
	d <- with(d, d[(diag_cd_01 >= code1 & diag_cd_01 < code2) | (diag_cd_02 >= code1 & diag_cd_02 < code2)
	| (diag_cd_03 >= code1 & diag_cd_03 < code2) | (diag_cd_04 >= code1 & diag_cd_04 < code2) 
	| (diag_cd_05 >= code1 & diag_cd_05 < code2) | (diag_cd_06 >= code1 & diag_cd_06 < code2) 
	| (diag_cd_07 >= code1 & diag_cd_07 < code2) | (diag_cd_08 >= code1 & diag_cd_08 < code2) 
	| (diag_cd_09 >= code1 & diag_cd_09 < code2) | (diag_cd_10 >= code1 & diag_cd_10 < code2) 
	| (diag_cd_11 >= code1 & diag_cd_11 < code2) | (diag_cd_12 >= code1 & diag_cd_12 < code2) 
	| (diag_cd_13 >= code1 & diag_cd_13 < code2) | (diag_cd_14 >= code1 & diag_cd_14 < code2) 
	| (diag_cd_15 >= code1 & diag_cd_15 < code2), ])
	print(head(d))
	print(length(unique(d$serial_no)))
	d[, c("serial_no", "adate")]
}

# SOPC data: 2000 - Dec 2013
load("D:/dm_data/Rdata/sopc.Rdata")
sopc <- sopc[, columns]
sopc <- FunICDcodes (sopc)
# 7,612 patients

# Inpatient data: Jan 1997 - Dec 2013
load("D:/dm_data/Rdata/inpatient.Rdata")
inpatient <- inpatient[, columns]
inpatient <- FunICDcodes (inpatient)
# 169,913 patients

# A&E & A&E FOLLOW-UP data: Jan 2000 - Dec 2013
load("D:/dm_data/Rdata/ae.Rdata")
ae <- ae[, columns]
ae <- FunICDcodes (ae)
# 13,887 patients

# Rbind diagnosis codes
if (any(ls() %in% "gopc")) {
    d <- rbind(gopc[, 1:2], inpatient, sopc, ae)
} else {
    d <- rbind(inpatient, sopc, ae)
}
print(length(unique(d$serial_no)))
# 183,489 total

library(data.table)
d <- data.table(d)
d <- d[, list(adate = min(adate)), by = "serial_no"]
if (anyDuplicated(d$serial_no)) stop("Duplicate serial_no")

chd <- data.frame(d)

########################################################
# Attendance codes: CHD <- Sudden death (UKPDS)
########################################################

# GOPC data: Jan 2006 to Dec 2013

########################################################
# ICD-9 codes
########################################################

# Sudden death (ICD-9 code 798 and 798.9);
code1 <- 798
code2 <- 799

# Inpatient data: Jan 1997 - Dec 2013
load("D:/dm_data/Rdata/inpatient.Rdata")
inpatient <- inpatient[, columns]
inpatient <- FunICDcodes (inpatient)
# 548 patients

# SOPC data: 2000 - Dec 2013
load("D:/dm_data/Rdata/sopc.Rdata")
sopc <- sopc[, columns]
sopc <- FunICDcodes (sopc)
# 1 patient

# A&E & A&E FOLLOW-UP data: Jan 2000 - Dec 2013
load("D:/dm_data/Rdata/ae.Rdata")
ae <- ae[, columns]
ae <- FunICDcodes (ae)
# 5910 patients

# Rbind diagnosis codes: NO GOPC codes
d <- rbind(inpatient, sopc, ae)
print(length(unique(d$serial_no)))
# 6442 total

# earliest recorded CHD 1997-
d <- data.table(d)
d <- d[, list(adate = min(adate)), by = "serial_no"]
if (anyDuplicated(d$serial_no)) stop("Duplicate serial_no")

d <- rbind(chd, d)
d <- data.table(d)
d <- d[, list(chdsuddendeath.adate = min(adate)), by = "serial_no"]

d <- merge(chd, d, all = T, by = "serial_no")

d$ref_date <- d$chdsuddendeath.adate
FunDeaths <- function (d) {
# observations after death
if (any(d$ref_date > d$death.date & !is.na(d$death.date))) {
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
if (any(d$yr < d$dob)) {
    print("observations before birth")
    print(sum(d$yr < d$dob))
    d[d$yr >= d$dob, ]
    } else { 
    d
    } 
}

load("D:/dm_data/Rdata/patient.Rdata")
d <- merge(d, patient[, c("serial_no", "dob", "death.date")], by = "serial_no")
d <- FunDeaths (d) # observations after death
d <- FunBirth (d) # observations before birth
d[, c("dob", "death.date", "yr", "ref_date")] <- list(NULL)

print(length(unique(d$serial_no)))
# 188,051 including CHD & sudden death

d$chd.cuhk <- d$adate
d$chd.ukpds <- d$chdsuddendeath.adate 

chd <- data.frame(d)
save(chd, file = "D:/dm_data/Rdata/chd.Rdata")
		  
'
No of CHD events
load("D:/vfm/chd.Rdata")
load("D:/vfm/periods.Rdata")
d<-merge(data.frame(serial_no=ob4$serial_no), chd,by="serial_no",all.x=T)
table(format(d$chd.cuhk, "%Y"))
table(format(d$chd.ukpds, "%Y"))

table(format(chd$chd.cuhk, "%Y"))
table(format(chd$chd.ukpds, "%Y"))
'
