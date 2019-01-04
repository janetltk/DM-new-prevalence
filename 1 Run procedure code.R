# Procedure codes 
# setwd("/home/chao/Encfs/secure/dm_data")

# SOPC data: 2000 - Dec 2014
########################################################
load("Rdata/sopc.Rdata")
names(sopc)
columns <- c("serial_no", "adate", paste0("proc_cd_0", 1:7))
d <- sopc[, columns]
rm(sopc); gc()

dx1 <- apply(d[, -(1:2)], 1, function(x) any(x >= code1 & x < code2, na.rm = T))
if (any(ls() %in% "code3")) {
  dx2 <- apply(d[, -(1:2)], 1, function(x) any(x >= code3 & x < code4, na.rm = T))
} else { 
  dx2 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code5")) {
  dx3 <- apply(d[, -(1:2)], 1, function(x) any(x >= code5 & x < code6, na.rm = T))
} else {
  dx3 <- rep(F, nrow(d))
  }
 
i <- as.logical(pmax(dx1, dx2, dx3))
sopc <- d[i, ]

# Inpatient data: Jan 1997 - Dec 2014
########################################################
load("Rdata/inpatient.Rdata")
names(inpatient)
columns <- c("serial_no", "adate", paste0("proc_cd_0", 1:9), paste0("proc_cd_", 10:15))
d <- inpatient[, columns]
rm(inpatient); gc()

dx1 <- apply(d[, -(1:2)], 1, function(x) any(x >= code1 & x < code2, na.rm = T))
if (any(ls() %in% "code3")) {
  dx2 <- apply(d[, -(1:2)], 1, function(x) any(x >= code3 & x < code4, na.rm = T))
} else { 
  dx2 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code5")) {
  dx3 <- apply(d[, -(1:2)], 1, function(x) any(x >= code5 & x < code6, na.rm = T))
} else {
  dx3 <- rep(F, nrow(d))
  }
 
i <- as.logical(pmax(dx1, dx2, dx3))
inpatient <- d[i, ]
 
# A&E & A&E FOLLOW-UP data: Jan 2000 - Dec 2014
########################################################
load("Rdata/ae.Rdata")
names(ae.attn)
columns <- c("serial_no", "adate", paste0("proc_cd_0", 1:8))
d <- ae.attn[, columns]

dx1 <- apply(d[, -(1:2)], 1, function(x) any(x >= code1 & x < code2, na.rm = T))
if (any(ls() %in% "code3")) {
  dx2 <- apply(d[, -(1:2)], 1, function(x) any(x >= code3 & x < code4, na.rm = T))
} else { 
  dx2 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code5")) {
  dx3 <- apply(d[, -(1:2)], 1, function(x) any(x >= code5 & x < code6, na.rm = T))
} else {
  dx3 <- rep(F, nrow(d))
  }
 
i <- as.logical(pmax(dx1, dx2, dx3))
ae.attn <- d[i,]

names(ae.fu)
columns <- c("serial_no", "adate", paste0("proc_cd_0", 1:4))
d <- ae.fu[, columns]

dx1 <- apply(d[, -(1:2)], 1, function(x) any(x >= code1 & x < code2, na.rm = T))
if (any(ls() %in% "code3")) {
  dx2 <- apply(d[, -(1:2)], 1, function(x) any(x >= code3 & x < code4, na.rm = T))
} else { 
  dx2 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code5")) {
  dx3 <- apply(d[, -(1:2)], 1, function(x) any(x >= code5 & x < code6, na.rm = T))
} else {
  dx3 <- rep(F, nrow(d))
  }
 
i <- as.logical(pmax(dx1, dx2, dx3))
ae.fu <- d[i,]

# GOPC data: no proc codes
######################################################## 

##################################################################
# Rbind proc codes
###################################################################
d <- rbind(inpatient[, 1:2], sopc[, 1:2], ae.attn[, 1:2], ae.fu[, 1:2])

print(length(unique(d$serial_no)))
library(data.table)
d <- data.table(d)
d <- d[, list(adate = min(adate)), by = "serial_no"]
if (anyDuplicated(d$serial_no)) stop("Duplicate serial_no")

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
if (any(d$yr < d$dob, na.rm = T)) {
    print("observations before birth")
    print(sum(d$yr < d$dob))
    d[d$yr >= d$dob, ]
    } else { 
    d
    } 
}

d$ref_date <- d$adate
load("Rdata/patient.Rdata")
d <- merge(d, patient[, c("serial_no", "dob", "death.date")], by = "serial_no")
d <- FunDeaths (d) # observations after death
d <- FunBirth (d) # observations before birth
d[, c("dob", "death.date", "yr", "ref_date")] <- list(NULL)

dim(d)

