# Diagnosis of diabetes mellitus (DM) from Medications data
###################################################################
rm(list = ls())

# Extract DM medications
######################################
load("Rdata/medication.Rdata")
d <- medication[, c("serial_no", "disp_date", "item_cd", "duration")]
head(d)
dim(d)
rm(medication); gc()

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
format(object.size(d), "auto")

stopifnot (sum(d$insulin) == sum(d$non.insulin == FALSE))
d$non.insulin <- NULL

length(unique(d$serial_no))
# 501,362 patients with diabetes medications

sort(table(d$item_cd), TRUE)

d$disp_date <- as.Date(paste("15", d$disp_date, sep = ""), "%d%b%Y")
d <- d[order(d$serial_no, d$disp_date),]

dm.meds <- d
save(dm.meds, file = "Rdata/dm_meds.Rdata")
rm(dm.meds)

# Diagnosis of DM from DM medications
#########################################
load("Rdata/dm_meds.Rdata")
d <- dm.meds

# Medication (AJE criteria)
######################################################################
# TWO prescription dates:
#   1st date includes metformin/thiazolidinediones
#   but EXCLUDING 2x prescriptions of metformin/thiazolidinediones
str(d)

# Prescription of metformin or thiazolidinediones excluded if 2 consecutive dx 
# any 2 events within 2 years
metformin.thiazolidinediones <- c("METF01", "METF02", "METF04", "METF03", "S00466", 
"PIOG01", "ROSI01", "ROSI02", "ROSI03", "S00204", "S00205", "S00206", "PIOG02", "S00016", "S00599")
d$metformin.thiazo <- d$item_cd %in% metformin.thiazolidinediones
table(d$item_cd[d$metformin.thiazo == TRUE])

aje.meds <- d[order(d$serial_no, d$disp_date), c("serial_no", "disp_date", "metformin.thiazo")] 
save(aje.meds, file = "diagnosis/meds_dx_aje.Rdata")
rm(aje.meds)
gc()

# Sensitivity analysis
####################################################################
# use second prescription for HKU criteria (ie AJE medications)
# -> no point as second prescription ()by definition) = non metformin/thiazolidinediones

# HKU medication criteria: remove metformin and thiazolidinediones?
print('DM Medications not in HA inclusion criteria -> NOT USED!')
####################################################################
library(data.table)

# including metformin and thiazolidinediones
# GML: cannot exclude metformin because of monotherapy
DT <- data.table(d)
DT <- DT[, list(med.date = min(disp_date)), by = serial_no]
length(unique(DT$serial_no))
if (anyDuplicated(DT$serial_no)) stop("Duplicate serial_no")
meds.dx <- data.frame(DT)

# excluding metformin and thiazolidinediones
DT <- data.table(d[d$metformin.thiazo==FALSE, ])
DT <- DT[, list(med.exclude = min(disp_date)), by = serial_no]
length(unique(DT$serial_no))
# 382,321 patients
if (anyDuplicated(DT$serial_no)) stop("Duplicate serial_no")

meds.dx <- merge(meds.dx, data.frame(DT), all=TRUE)

save(meds.dx, file="diagnosis/meds_dx.Rdata")
    

# Drug code
insulin_cd <- "DILU01|DILU02|DILU03|DILU04|INSU01|INSU02|INSU03|INSU04|INSU05|INSU06|INSU07|INSU08|INSU09|INSU12|INSU13|INSU14|INSU15|INSU16|INSU17|INSU21|INSU22|INSU23|INSU25|INSU26|INSU27|INSU28|INSU29|INSU30|INSU31|INSU32|INSU33|INSU34|INSU35|INSU36|INSU37|INSU38|INSU39|INSU40|INSU41|INSU42|INSU43|INSU44|INSU45|INSU46|INSU47|INSU48|INSU49|INSU50|INSU51|NOVO01|S00054|S00094|S00134|S00195|S00210|S00211|S00215|S00306|S00477|S00562|S00644|S00710|S00795|S00816|S00817|S00868|S00906|SYRII1|SYRII2|SYRII3"
non_insulin_cd <- "ACAR01|ACAR02|EXEN01|EXEN02|EXEN03|EXEN04|S00623|S00624|GLIB01|GLIB02|GLIC01|GLIC02|GLIC03|S00089|GLIM01|GLIM02|GLIM03|S00248|S00249|GLIP01|GLIP02|GLUC01|GLUC37|LINA01|S00893|METF01|METF02|METF03|METF04|S00466|PIOG01|PIOG02|S00016|S00599|S00790|SAXA01|S00600|S00602|S00675|SITA02|SITA03|SITA06|TOLB01|S00731|VILD01"
