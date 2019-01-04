# Diagnosis of attendance 

# SOPC data: 2000 - Dec 2014
########################################################
load("Rdata/sopc_dx_codes.Rdata")

d <- sopc_dx_codes
names(d)
dx1 <- apply(d[, 3:6], 1, function(x) any(grepl(code_grepl, x), na.rm = T))
dx2 <- apply(d[, 7:12], 1, function(x) any(grepl(code_grepl, x), na.rm = T))
dx3 <- apply(d[, 13:18], 1, function(x) any(grepl(code_grepl, x), na.rm = T))
dx4 <- apply(d[, 19:24], 1, function(x) any(grepl(code_grepl, x), na.rm = T))
dx5 <- apply(d[, 25:31], 1, function(x) any(grepl(code_grepl, x), na.rm = T))
i <- as.logical(pmax(dx1, dx2, dx3, dx4, dx5))

sopc <- d[i==T, ]
rm(d, sopc_dx_codes); gc()

# Inpatient data: Jan 1997 - Dec 2014
########################################################
load("Rdata/inpatient.Rdata")
names(inpatient)
columns <- c("serial_no", "adate", paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:15))
d <- inpatient[, columns]
rm(inpatient); gc()

i <- apply(d[, -(1:2)], 1, function(x) any(grepl(code_grepl, x), na.rm = T))
inpatient <- d[i==T, ]
 
# A&E & A&E FOLLOW-UP data: Jan 2000 - Dec 2014
########################################################
load("Rdata/ae.Rdata")
names(ae.attn)
columns <- c("serial_no", "adate", paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:20))
d <- ae.attn[, columns]

i <- apply(d[, -(1:2)], 1, function(x) any(grepl(code_grepl, x), na.rm = T))
ae.attn <- d[i==T,]

names(ae.fu)
columns <- c("serial_no", "adate", paste0("diag_cd_0", 1:8))

d <- ae.fu[, columns]
i <- apply(d[, -(1:2)], 1, function(x) any(grepl(code_grepl, x), na.rm = T))
ae.fu <- d[i==T,]

# GOPC data: Jan 2006 to Dec 2014
########################################################
if (any(ls() %in% "icpc")) {
  load("Rdata/gopc.Rdata")
  names(gopc)
  columns <- c("serial_no", "adate", paste0("icpc_", 1:17))

  d <- gopc[, columns]
  i <- apply(d[, -(1:2)], 1, function(x) length(grep(icpc, x)) > 0)
  gopc <- d[i,]
} 

##################################################################
# Rbind diagnosis codes
###################################################################
if (any(ls() %in% "gopc")) {
    d <- rbind(gopc[, 1:2], inpatient[, 1:2], sopc[, 1:2], ae.attn[, 1:2], ae.fu[, 1:2])
} else {
    d <- rbind(inpatient[, 1:2], sopc[, 1:2], ae.attn[, 1:2], ae.fu[, 1:2])
}

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

