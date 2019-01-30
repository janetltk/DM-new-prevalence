# Diagnosis of diabetes mellitus (DM) from Medications data
###################################################################
rm(list = ls()); gc()
setwd("C:/Users/janet/Desktop/2017_data")
memory.limit(1e+13)

# Extract DM medications
# In 2017 data, medications are separated into 6 datsets: medication_ae / fmsc / gop / inpatient / sop / others
######################################
med_fmsc <- readRDS("medication_fmsc.rds")
d <- med_fmsc[, c("serial_no", "disp_date", "item_cd", "duration")]
head(d)
dim(d)
rm(med_fmsc); gc()

# drugcodes from HA datascheme table (sheet 2)
insulin.code <- c(paste0("DILU0", 1:4), 
    paste0("INSU0", 1:9), paste0("INSU", 20:51),
    "NOVO01", 
    "S00054", "S00094", "S00134", "S00195", "S00210", "S00211", "S00215", "S00306", "S00477", 
    "S00562", "S00644", "S00710", "S00795", "S00816", "S00817", "S00868", "S00906", 
    "SYRII1", "SYRII2", "SYRII3")
# remove glucagon?
non.insulin.code <- c("ACAR01", "ACAR02", 
    "EXEN01", "EXEN02", "EXEN03", "EXEN04", 
    "S00623", "S00624", 
    "GLIB01", "GLIB02", "GLIC01", "GLIC02", "GLIC03", 
    "S00089", 
    "GLIM01", "GLIM02", "GLIM03", 
    "S00248", "S00249", 
    "GLIP01", "GLIP02", 
    "GLUC01", "GLUC37", 
    "LINA01", 
    "S00893", 
    "METF01", "METF02", "METF03", "METF04", 
    "S00466", 
    "PIOG01", "PIOG02", 
    "S00016", "S00599", "S00790", 
    "SAXA01", 
    "S00600", "S00602", "S00675", 
    "SITA02", "SITA03", "SITA06", 
    "TOLB01",
    "S00731", 
    "VILD01")

d$insulin <- d$item_cd %in% insulin.code
d$non.insulin <- d$item_cd %in% non.insulin.code
if (any(d$insulin == TRUE & d$non.insulin == TRUE)) stop("Cannot be both insulin and non-insulin")
d <- d[d$insulin == T | d$non.insulin == T, ]
format(object.size(d), "auto") #56.8 Mb

stopifnot (sum(d$insulin) == sum(d$non.insulin == FALSE))
d$non.insulin <- NULL

length(unique(d$serial_no))
# 376640 patients with diabetes medications from fmsc
sort(table(d$item_cd), TRUE)

d$disp_date <- as.Date(paste("15", d$disp_date, sep = ""), "%d%b%Y")
d <- d[order(d$serial_no, d$disp_date),]
d_fmsc <- d

# gopc
med_gop <- readRDS("medication_gop.rds")
d <- med_gop[, c("serial_no", "disp_date", "item_cd", "duration")]
rm(med_gop); gc()
d$insulin <- d$item_cd %in% insulin.code
d$non.insulin <- d$item_cd %in% non.insulin.code
if (any(d$insulin == TRUE & d$non.insulin == TRUE)) stop("Cannot be both insulin and non-insulin")
d <- d[d$insulin == T | d$non.insulin == T, ]
format(object.size(d), "auto") #612.6 Mb

stopifnot (sum(d$insulin) == sum(d$non.insulin == FALSE))
d$non.insulin <- NULL

length(unique(d$serial_no))
# 376640 patients with diabetes medications from gopc
sort(table(d$item_cd), TRUE)

d$disp_date <- as.Date(paste("15", d$disp_date, sep = ""), "%d%b%Y")
d <- d[order(d$serial_no, d$disp_date),]
d_gop <- d
rm(d); gc()

# sopc
med_sop <- readRDS("medication_sop.rds")
d <- med_sop[, c("serial_no", "disp_date", "item_cd", "duration")]
rm(med_sop); gc()
d$insulin <- d$item_cd %in% insulin.code
d$non.insulin <- d$item_cd %in% non.insulin.code
if (any(d$insulin == TRUE & d$non.insulin == TRUE)) stop("Cannot be both insulin and non-insulin")
d <- d[d$insulin == T | d$non.insulin == T, ]
format(object.size(d), "auto") #334 Mb

stopifnot (sum(d$insulin) == sum(d$non.insulin == FALSE))
d$non.insulin <- NULL

length(unique(d$serial_no))
# 290979 patients with diabetes medications from sopc
sort(table(d$item_cd), TRUE)

d$disp_date <- as.Date(paste("15", d$disp_date, sep = ""), "%d%b%Y")
d <- d[order(d$serial_no, d$disp_date),]
d_sop <- d
rm(d); gc()

# inpatient
med_ip <- readRDS("medication_inpatient.rds")
d <- med_ip[, c("serial_no", "disp_date", "item_cd", "duration")]
rm(med_ip); gc()

d$insulin <- d$item_cd %in% insulin.code
d$non.insulin <- d$item_cd %in% non.insulin.code
if (any(d$insulin == TRUE & d$non.insulin == TRUE)) stop("Cannot be both insulin and non-insulin")
d <- d[d$insulin == T | d$non.insulin == T, ]
format(object.size(d), "auto") # 309.9Mb

stopifnot (sum(d$insulin) == sum(d$non.insulin == FALSE))
d$non.insulin <- NULL

length(unique(d$serial_no))
# 388366 patients with diabetes medications from inpatient
sort(table(d$item_cd), TRUE)

d$disp_date <- as.Date(paste("15", d$disp_date, sep = ""), "%d%b%Y")
d <- d[order(d$serial_no, d$disp_date),]
d_ip <- d
rm(d); gc()

# ae (? include)
med_ae <- readRDS("medication_ae.rds")
d <- med_ae[, c("serial_no", "disp_date", "item_cd", "duration")]
rm(med_ae); gc()

d$insulin <- d$item_cd %in% insulin.code
d$non.insulin <- d$item_cd %in% non.insulin.code
if (any(d$insulin == TRUE & d$non.insulin == TRUE)) stop("Cannot be both insulin and non-insulin")
d <- d[d$insulin == T | d$non.insulin == T, ]
format(object.size(d), "auto") # 8.3Mb

stopifnot (sum(d$insulin) == sum(d$non.insulin == FALSE))
d$non.insulin <- NULL

length(unique(d$serial_no))
# 58655 patients with diabetes medications from inpatient
sort(table(d$item_cd), TRUE)

d$disp_date <- as.Date(paste("15", d$disp_date, sep = ""), "%d%b%Y")
d <- d[order(d$serial_no, d$disp_date),]
d_ae <- d
rm(d); gc()

# others
med_others <- readRDS("medication_others.rds")
d <- med_others[, c("serial_no", "disp_date", "item_cd", "duration")]
rm(med_others); gc()

d$insulin <- d$item_cd %in% insulin.code
d$non.insulin <- d$item_cd %in% non.insulin.code
if (any(d$insulin == TRUE & d$non.insulin == TRUE)) stop("Cannot be both insulin and non-insulin")
d <- d[d$insulin == T | d$non.insulin == T, ]
format(object.size(d), "auto") # 62.5Mb

stopifnot (sum(d$insulin) == sum(d$non.insulin == FALSE))
d$non.insulin <- NULL

length(unique(d$serial_no))
# 110168 patients with diabetes medications from inpatient
sort(table(d$item_cd), TRUE)

d$disp_date <- as.Date(paste("15", d$disp_date, sep = ""), "%d%b%Y")
d <- d[order(d$serial_no, d$disp_date),]
d_others <- d
rm(d); gc()

# try following
d_ae$disp_location <- "ae"
d_fmsc$disp_location <- "fmsc"
d_gop$disp_location <- "gopc"
d_ip$disp_location <- "ip"
d_sop$disp_location <- "sopc"
d_others$disp_location <- "others"

d <- rbind(d_ae, d_fmsc, d_gop, d_ip, d_sop, d_others)
d$disp_location <- as.factor(d$disp_location)
d$item_cd <- as.factor(d$item_cd)

rm(list=setdiff(ls(), "d")); gc()

d <- d[order(d$serial_no, d$disp_date),]
format(object.size(d), "auto") #2.9Gb
length(unique(d$serial_no)) #615558 unique patients
sort(table(d$item_cd), TRUE)

saveRDS(d, file = "sorted/dm_meds.rds")

# Diagnosis of DM from DM medications
#########################################
d <- readRDS(d, file = "sorted/dm_meds.rds")

# Medication (AJE criteria)
######################################################################
# TWO prescription dates:
#   1st date includes metformin/thiazolidinediones
#   but EXCLUDING 2x prescriptions of metformin/thiazolidinediones
str(d)

# Prescription of metformin or thiazolidinediones excluded if 2 consecutive dx 
# any 2 events within 2 years
met.tzd <- c("METF01", "METF02", "METF04", "METF03", "S00466", 
"PIOG01", "ROSI01", "ROSI02", "ROSI03", "S00204", "S00205", "S00206", "PIOG02", "S00016", "S00599")
d$met.tzd <- d$item_cd %in% met.tzd
table(d$item_cd[d$met.tzd == TRUE])

aje.meds <- d[order(d$serial_no, d$disp_date), c("serial_no", "disp_date", "met.tzd")] 
saveRDS(aje.meds, file = "diagnosis/meds_dx_aje.rds")
rm(aje.meds); gc()

"# Sensitivity analysis
####################################################################
# use second prescription for HKU criteria (ie AJE medications)
# -> no point as second prescription ()by definition) = non metformin/thiazolidinediones

# HKU medication criteria: remove metformin and thiazolidinediones?
print('DM Medications not in HA inclusion criteria -> NOT USED!')
####################################################################"

library(data.table)

# including metformin and thiazolidinediones
# GML: cannot exclude metformin because of monotherapy
DT <- data.table(d)
DT <- DT[, .(med.date = min(disp_date)), by = serial_no]
length(unique(DT$serial_no)) #615,558 patients
if (anyDuplicated(DT$serial_no)) stop("Duplicate serial_no")
meds.dx <- data.frame(DT)

# excluding metformin and thiazolidinediones
DT <- data.table(d[d$met.tzd==FALSE, ])
DT <- DT[, list(med.exclude = min(disp_date)), by = serial_no]
length(unique(DT$serial_no))
# 464,648 patients
if (anyDuplicated(DT$serial_no)) stop("Duplicate serial_no")

meds.dx <- merge(meds.dx, data.frame(DT), all=TRUE)

saveRDS(meds.dx, file = "diagnosis/meds_dx.rds")
    

'# Drug code
insulin_cd <- "DILU01|DILU02|DILU03|DILU04|INSU01|INSU02|INSU03|INSU04|INSU05|INSU06|INSU07|INSU08|INSU09|INSU12|INSU13|INSU14|INSU15|INSU16|INSU17|INSU21|INSU22|INSU23|INSU25|INSU26|INSU27|INSU28|INSU29|INSU30|INSU31|INSU32|INSU33|INSU34|INSU35|INSU36|INSU37|INSU38|INSU39|INSU40|INSU41|INSU42|INSU43|INSU44|INSU45|INSU46|INSU47|INSU48|INSU49|INSU50|INSU51|NOVO01|S00054|S00094|S00134|S00195|S00210|S00211|S00215|S00306|S00477|S00562|S00644|S00710|S00795|S00816|S00817|S00868|S00906|SYRII1|SYRII2|SYRII3"
non_insulin_cd <- "ACAR01|ACAR02|EXEN01|EXEN02|EXEN03|EXEN04|S00623|S00624|GLIB01|GLIB02|GLIC01|GLIC02|GLIC03|S00089|GLIM01|GLIM02|GLIM03|S00248|S00249|GLIP01|GLIP02|GLUC01|GLUC37|LINA01|S00893|METF01|METF02|METF03|METF04|S00466|PIOG01|PIOG02|S00016|S00599|S00790|SAXA01|S00600|S00602|S00675|SITA02|SITA03|SITA06|TOLB01|S00731|VILD01"
'