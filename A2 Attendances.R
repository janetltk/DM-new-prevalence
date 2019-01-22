# Diagnosis of diabetes mellitus (DM) from attendance data

rm(list = ls()); gc()
library(data.table)
setwd("C:/Users/janet/Desktop/2017_data")
memory.limit(1e+13)

########################################################
# ICD-9 Codes
complication.codes <- as.numeric(c(357.2, 366.41, paste0("362.0", c(1:7))))
t1.codes <- as.numeric(c(paste0("250.", 0:9, 1), paste0("250.", 0:9, 3)))

# SOPC data: 2000 - Dec 2014
########################################################
sopc <- readRDS("sop_attn.rds")
names(sopc)
dx.col <- c(paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:36))

sopc$t1 <- apply(sopc[, dx.col[1:20]], 1, function(x) any(x %in% t1.codes))
sopc$t1.2 <- apply(sopc[, dx.col[21:36]], 1, function(x) any(x %in% t1.codes))
sopc$dm1 <- apply(sopc[, dx.col[1:9]], 1, function(x) any(x >= 250 & x < 251))
sopc$dm2 <- apply(sopc[, dx.col[10:19]], 1, function(x) any(x >= 250 & x < 251))
sopc$dm3 <- apply(sopc[, dx.col[20:29]], 1, function(x) any(x >= 250 & x < 251))
sopc$dm4 <- apply(sopc[, dx.col[30:36]], 1, function(x) any(x >= 250 & x < 251))
sopc$complication <- apply(sopc[, dx.col[1:20]], 1, function(x) any(x %in% complication.codes))
sopc$complication.2 <- apply(sopc[, dx.col[21:36]], 1, function(x) any(x %in% complication.codes))
sopc$dm <- with(sopc, as.logical(pmax(dm1, dm2, dm3, dm4, na.rm = T)))
sopc$type1 <- with(sopc, as.logical(pmax(t1, t1.2, na.rm = T)))
sopc$cx <- with(sopc, as.logical(pmax(complication, complication.2, na.rm = T)))
sopc[, c("dm1", "dm2", "dm3", "dm4", "t1", "t1.2", "complication", "complication.2")] <- NULL

colSums(sopc[, c("type1", "dm", "cx")], na.rm = T) 
# type1    dm    cx 
# 1819 94575 43348 

# Subset: admissions with ICD DM codes
i <- with(sopc, as.logical(pmax(type1, dm, cx, na.rm = T)))
'save(i, file = "diagnosis/i.Rdata")'

sopc <- sopc[i, ]
saveRDS(sopc, file = "diagnosis/dm_sopc.rds")

library(fst)
write_fst(sopc, path = "diagnosis/dm_sopc.fst")
rm(sopc); gc()

# Inpatient data: Jan 1997 - Dec 2014
########################################################
fst <- fst("inpatient.fst")
print(fst)
inpatient <- fst[, c(1:16, 32:38)]
rm(fst); gc()
  
names(inpatient)

dx.col <- c(paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:15))

inpatient$t1 <- apply(inpatient[, dx.col], 1, function(x) any(x %in% t1.codes))
inpatient$dm <- apply(inpatient[, dx.col], 1, function(x) any(x >= 250 & x < 251))
inpatient$complication <- apply(inpatient[, dx.col], 1, function(x) any(x %in% complication.codes))

colSums(inpatient[, c("t1", "dm", "complication")], na.rm = T)         #  t1           dm complication 
#37922      1505868       142694 

# Subset: admissions with ICD DM codes
i <- with(inpatient, as.logical(pmax(t1, dm, complication, na.rm = T)))
inpatient <- inpatient[i, ]

write_fst(inpatient, path = "diagnosis/dm_inpatient.fst")
rm(inpatient); gc()


# A&E & A&E FOLLOW-UP data: Jan 2000 - Dec 2014
########################################################
'ae.attn <- readRDS("ae_attn.rds")
names(ae.attn)
write_fst(ae.attn, path = "ae.attn.fst")

dx.col <- c(paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:20))

ae.attn$t1 <- apply(ae.attn[, dx.col], 1, function(x) any(x %in% t1.codes))
ae.attn$dm <- apply(ae.attn[, dx.col], 1, function(x) any(x >= 250 & x < 251))
ae.attn$complication <- apply(ae.attn[, dx.col], 1, function(x) any(x %in% complication.codes))
colSums(ae.attn[, c("t1", "dm", "complication")], na.rm = T) # 46,559 attendances
i <- with(ae.attn, as.logical(pmax(t1, dm, complication, na.rm = T)))
ae.attn <- ae.attn[i, ]
write_fst(ae.attn, path = "diagnosis/dm_ae_attn.fst")
rm(ae.attn); gc()

ae.fu <- readRDS("ae_fu.rds")
names(ae.fu)
dx.col <- c(paste0("diag_cd_0", 1:8))

ae.fu$t1 <- apply(ae.fu[, dx.col], 1, function(x) any(x %in% t1.codes))
ae.fu$dm <- apply(ae.fu[, dx.col], 1, function(x) any(x >= 250 & x < 251))
ae.fu$complication <- apply(ae.fu[, dx.col], 1, function(x) any(x %in% complication.codes))
colSums(ae.fu[, c("t1", "dm", "complication")], na.rm = T) # 706 attendances
i <- with(ae.fu, as.logical(pmax(t1, dm, complication, na.rm = T)))
ae.fu <- ae.fu[i, ]

write_fst(ae.fu, path = "diagnosis/dm_ae_fu.fst")
rm(ae.fu); gc()
'

########################################################
# GOPC data: Jan 2006 to Dec 2014
########################################################
fst <- fst("gop_attn.fst")
print(fst)
gopc <- fst[,]
rm(fst); gc()

dx.col <- c(paste0("icpc_", 1:17))

# T89 Diabetes insulin dependent = t1
gopc$t1 <- apply(gopc[, dx.col], 1, function(x) any(x %in% "T89"))

# T90 Diabetes non-insulin dependent == t2
gopc$t2 <- apply(gopc[, dx.col], 1, function(x) any(x %in% "T90"))

colSums(gopc[, c("t1", "t2")])
#t1       t2 
#24950 10354603 

# Subset only attendances with ICPC DM codes
i <- with(gopc, as.logical(pmax(t1, t2, na.rm = T)))
gopc <- gopc[i, ]
write_fst(gopc, path = "diagnosis/dm_gopc.fst")
rm(gopc, i); gc()

##################################################################
# Merge attendance data
###################################################################
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

fst <- fst("diagnosis/dm_sopc.fst")
print(fst)
sopc <- fst[,]
rm(fst); gc()

names(sopc)
colnames(sopc)[(names(sopc) == "appo_date")] <- "adate"
colnames(sopc)[(names(sopc) == "type1")] <- "t1"
colnames(sopc)[(names(sopc) == "cx")] <- "complication"

aje.sopc <- sopc[, c("serial_no", "appo_date")]   # AJE SOPC
sopc <- FunDmDate(sopc)
# 69716 patients 

fst <- fst("diagnosis/dm_inpatient.fst")
print(fst)
inpatient <- fst[,]
rm(fst); gc()

# AJE/Nichols uses one inpatient date
inpatient <- FunDmDate(inpatient)
# 368967 patients 

'fst <- fst("diagnosis/dm_ae_attn.fst")
print(fst)
ae.attn <- fst[row, col]
rm(fst); gc()
print(paste(length(unique(ae.attn$serial_no)), "patients with 250.x ICD-9 codes"))
# 35,509 patients 

fst <- fst("diagnosis/dm_ae_fu.fst")
print(fst)
ae.fu <- fst[row, col]
rm(fst); gc()
print(paste(length(unique(ae.fu$serial_no)), "patients with 250.x ICD-9 codes"))
# 529 patients 

cols <- c("serial_no", "adate", "t1", "dm", "complication")  
ae <- rbind(ae.attn[, cols], ae.fu[, cols])
aje.ae <- ae[, c("serial_no", "adate")]   # AJE A&E
ae <- FunDmDate(ae)
# 35,796 patients'

fst <- fst("diagnosis/dm_gopc.fst")
gopc <- fst[,]
rm(fst); gc()
# AJE/Nichols does not use ICPC codes
gopc$complication <- FALSE # no complication codes
gopc$dm <- TRUE # dm = all diabetes of any type
colnames(gopc)[(names(gopc) == "appo_date")] <- "adate"
gopc <- FunDmDate(gopc)
# 444271 patients

# DM type 1 or 2
cols <- c("serial_no", "t1", "dm", "complication")  
d <- rbind(gopc[, cols], inpatient[, cols], sopc[, cols])
d <- data.table(d)
d <- d[, list(t1 = max(t1), dm = max(dm), complication = max(complication)), by = serial_no]

d$dm.type <- NA
d$dm.type[d$dm == 1] <- "t2"
d$dm.type[d$complication == 1] <- "complication"
d$dm.type[d$t1 == 1] <- "t1"

stopifnot (!is.na(d$dm.type))
table(d$dm.type, exclude = NULL)
#complication           t1           t2 
#42071        16086       600770 
dm.type <- data.frame(d[order(serial_no), list(serial_no, dm.type)])

# Merge attendance dates with dm type
dm <- Reduce(function(x, y) merge(x, y, by = "serial_no", all = TRUE), 
    list(gopc[, 1:3], inpatient[, 1:3], sopc[, 1:3]))

if (identical((dm$serial_no), (dm.type$serial_no))) {
    d <- merge (dm, dm.type, by = "serial_no")
} else { stop ("Error: Serial numbers do not match")
}

 names(d)
length(unique(d$serial_no))
#  patients with diabetes diagnosis codes (t1, t2, t3)
# 658927

colSums(is.na(d))

# AJE methods does not use 2nd inpatient episode or ICPC-2 codes
head(aje.sopc)
saveRDS(aje.sopc, file = "diagnosis/attendance_aje.rds")

saveRDS(d, file = "diagnosis/attendance.rds")
