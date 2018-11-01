# Diagnosis of diabetes mellitus (DM) from attendance data

rm(list = ls())
library(data.table)
setwd("/home/chao/Encfs/secure/dm_data")

########################################################
# ICD-9 Codes
complication.codes <- as.numeric(c(357.2, 366.41, paste0("362.0", c(1:7))))
t1.codes <- as.numeric(c(paste0("250.", 0:9, 1), paste0("250.", 0:9, 3)))

# SOPC data: 2000 - Dec 2014
########################################################
load("Rdata/sopc.Rdata")
names(sopc)
dx.col <- c(paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:29))

sopc$t1 <- apply(sopc[, dx.col], 1, function(x) any(x %in% t1.codes))
sopc$dm1 <- apply(sopc[, dx.col[1:9]], 1, function(x) any(x >= 250 & x < 251))
sopc$dm2 <- apply(sopc[, dx.col[10:19]], 1, function(x) any(x >= 250 & x < 251))
sopc$dm3 <- apply(sopc[, dx.col[20:29]], 1, function(x) any(x >= 250 & x < 251))
sopc$complication <- apply(sopc[, dx.col], 1, function(x) any(x %in% complication.codes))
sopc$dm <- with(sopc, as.logical(pmax(dm1, dm2, dm3, na.rm = T)))
sopc[, c("dm1", "dm2", "dm3")] <- list(NULL)

colSums(sopc[, c("t1", "dm", "complication")], na.rm = T) # 71,388 attendances

# Subset: admissions with ICD DM codes
i <- with(sopc, as.logical(pmax(t1, dm, complication, na.rm = T)))
save(i, file = "i.Rdata")

sopc <- sopc[i, ]
save(sopc, file = "Rdata/dm_sopc.Rdata")
gc()

# Inpatient data: Jan 1997 - Dec 2014
########################################################
load("Rdata/inpatient.Rdata")
names(inpatient)

dx.col <- c(paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:15))

inpatient$t1 <- apply(inpatient[, dx.col], 1, function(x) any(x %in% t1.codes))
inpatient$dm <- apply(inpatient[, dx.col], 1, function(x) any(x >= 250 & x < 251))
inpatient$complication <- apply(inpatient[, dx.col], 1, function(x) any(x %in% complication.codes))

colSums(inpatient[, c("t1", "dm", "complication")], na.rm = T) # 1,244,206 admissions 

# Subset: admissions with ICD DM codes
i <- with(inpatient, as.logical(pmax(t1, dm, complication, na.rm = T)))
inpatient <- inpatient[i, ]
save(inpatient, file = "Rdata/dm_inpatient.Rdata")
gc()


# A&E & A&E FOLLOW-UP data: Jan 2000 - Dec 2014
########################################################
load("Rdata/ae.Rdata")
names(ae.attn)
dx.col <- c(paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:20))

ae.attn$t1 <- apply(ae.attn[, dx.col], 1, function(x) any(x %in% t1.codes))
ae.attn$dm <- apply(ae.attn[, dx.col], 1, function(x) any(x >= 250 & x < 251))
ae.attn$complication <- apply(ae.attn[, dx.col], 1, function(x) any(x %in% complication.codes))
colSums(ae.attn[, c("t1", "dm", "complication")], na.rm = T) # 46,559 attendances
i <- with(ae.attn, as.logical(pmax(t1, dm, complication, na.rm = T)))
ae.attn <- ae.attn[i, ]
gc()

names(ae.fu)
dx.col <- c(paste0("diag_cd_0", 1:8))

ae.fu$t1 <- apply(ae.fu[, dx.col], 1, function(x) any(x %in% t1.codes))
ae.fu$dm <- apply(ae.fu[, dx.col], 1, function(x) any(x >= 250 & x < 251))
ae.fu$complication <- apply(ae.fu[, dx.col], 1, function(x) any(x %in% complication.codes))
colSums(ae.fu[, c("t1", "dm", "complication")], na.rm = T) # 706 attendances
i <- with(ae.fu, as.logical(pmax(t1, dm, complication, na.rm = T)))
ae.fu <- ae.fu[i, ]
gc()

save(ae.attn, ae.fu, file = "Rdata/dm_ae.Rdata")


########################################################
# GOPC data: Jan 2006 to Dec 2014
########################################################
load("Rdata/gopc.Rdata")
names(gopc)
dx.col <- c(paste0("icpc_", 1:17))

# T89 Diabetes insulin dependent = t1
gopc$t1 <- apply(gopc[, dx.col], 1, function(x) any(x %in% "T89"))

# T90 Diabetes non-insulin dependent == t2
gopc$t2 <- apply(gopc[, dx.col], 1, function(x) any(x %in% "T90"))

colSums(gopc[, c("t1", "t2")])

# Subset only attendances with ICPC DM codes
i <- with(gopc, as.logical(pmax(t1, t2, na.rm = T)))
gopc <- gopc[i, ]
save(gopc, file = "Rdata/dm_gopc.Rdata")
gc()

##################################################################
# Merge attendance data
###################################################################
library(data.table)
FunDmDate <- function (d) {
# earliest date of attendance with diabetes code
  any(d$complication == 1 & d$dm == 0, na.rm = T)
  total <- length(unique(d$serial_no))
  print(paste(total, "patients with 250.x ICD-9 codes"))
  DT <- data.table(d)
  nm <- deparse(substitute(d))
  DT <- DT[, list(adate = min(adate), adate2 = sort(adate)[2], t1 = max(t1), complication = max(complication), dm = max(dm)), by = serial_no]
  setnames(DT, "adate", paste0(nm, ".date"))
  setnames(DT, "adate2", paste0(nm, ".date2"))
  if (any(is.na(DT))) colSums(is.na(DT))
  if (anyDuplicated(DT$serial_no)) stop("Duplicate serial_no")
  colSums(!is.na(d[, c("t1", "dm", "complication")]))
  data.frame(DT)
}

load("Rdata/dm_sopc.Rdata")
aje.sopc <- sopc[, c("serial_no", "adate")]   # AJE SOPC
sopc <- FunDmDate(sopc)
# 56,386 patients 

load("Rdata/dm_inpatient.Rdata")
# AJE/Nichols uses one inpatient date
inpatient <- FunDmDate(inpatient)
# 320,037 patients 

load("Rdata/dm_ae.Rdata")
print(paste(length(unique(ae.attn$serial_no)), "patients with 250.x ICD-9 codes"))
# 35,509 patients 
print(paste(length(unique(ae.fu$serial_no)), "patients with 250.x ICD-9 codes"))
# 529 patients 
cols <- c("serial_no", "adate", "t1", "dm", "complication")  
ae <- rbind(ae.attn[, cols], ae.fu[, cols])
aje.ae <- ae[, c("serial_no", "adate")]   # AJE A&E
ae <- FunDmDate(ae)
# 35,796 patients

load("Rdata/dm_gopc.Rdata")
# AJE/Nichols does not use ICPC codes
gopc$complication <- 0 # no complication codes
gopc$dm <- 1 # dm = all diabetes of any type
gopc <- FunDmDate(gopc)
# 361,031 patients

# DM type 1 or 2
cols <- c("serial_no", "t1", "dm", "complication")  
d <- rbind(gopc[, cols], ae[, cols], inpatient[, cols], sopc[, cols])
d <- data.table(d)
d <- d[, list(t1 = max(t1), dm = max(dm), complication = max(complication)), by = serial_no]

d$dm.type <- NA
d$dm.type[d$complication == 1] <- "complication"
d$dm.type[d$t1 == 1] <- "t1"
d$dm.type[d$dm == 1] <- "t2"
stopifnot (!is.na(d$dm.type))
table(d$dm.type, exclude = NULL) 
dm.type <- data.frame(d[order(serial_no), list(serial_no, dm.type)])

# Merge attendance dates with dm type
dm <- Reduce(function(x, y) merge(x, y, by = "serial_no", all = TRUE), 
    list(gopc[, 1:3], ae[, 1:3], inpatient[, 1:3], sopc[, 1:3]))

if (identical((dm$serial_no), (dm.type$serial_no))) {
    d <- merge (dm, dm.type, by = "serial_no")
} else { stop ("Error: Serial numbers do not match")
}

names(d)
length(unique(d$serial_no))
# 561,087 patients with diabetes diagnosis codes (t1, t2, t3)

colSums(!is.na(d))
colSums(is.na(d))

# AJE methods does not use 2nd inpatient episode or ICPC-2 codes
head(aje.sopc)
head(aje.ae)
save(aje.sopc, aje.ae, file = "diagnosis/attendance_aje.Rdata")

d$inpatient.date2 <- NULL
d$gopc.date2 <- NULL

attn <- d
save(attn, file = "diagnosis/attendance.Rdata")
