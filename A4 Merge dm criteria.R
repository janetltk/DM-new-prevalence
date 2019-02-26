rm(list = ls()); gc()
setwd("C:/Users/janet/Desktop/2017_data")
library(dplyr)

##############################################################
# Merge lab glucose, medications, attendances, self-reported
##############################################################
# Merge with patient list (self-reported diagnosis of diabetes)
patient <- readRDS("sorted/patient_list.rds")
self <- patient %>% select(serial_no, female, dob, self.reported, death.date)
str(self)
rm(patient)

load("diagnosis/lab_glucose.Rdata")
# Merge Attendance data (diagnosis codes for t1, t2, t3 dm)
attn <- readRDS("diagnosis/attendance.rds")
meds.dx <- readRDS("diagnosis/meds_dx.rds")

d <- Reduce(function(x, y) merge(x, y, by = "serial_no", all = TRUE), 
     list(self, lab, attn, meds.dx))
head(d)
str(d)
colSums(!is.na(d))
if(anyDuplicated(d$serial_no)) stop("Duplicate serial_no")
# includes entire patient dataset of 1,847,791
# Includes Type 1 DM patients

#######################################################
# Cleaning
#######################################################

# Dx criteria groups (Lab, Attn, Medications): death date BEFORE dx event
####################################################################################
FunAfterDeath <- function (event) {
    # Remove if event occurred after death date
	if (any(!is.na(d[, c(event)]) & !is.na(d$death.date) & d$death.date < d[, c(event)])) {
    print(length(d[!is.na(d[, c(event)]) & !is.na(d$death.date) & d$death.date<d[, c(event)], c("serial_no")]))
    print("ERROR: Event happened AFTER death")
    print(d[!is.na(d[, c(event)]) & !is.na(d$death.date) & d$death.date < d[, c(event)], ])
    d[!is.na(d[, c(event)]) & !is.na(d$death.date) & d$death.date < d[, c(event)], c(event)] <- NA
    d
    } else {d
    }}

# self-reported
d <- FunAfterDeath("self.reported") # Nil

# Lab
d <- FunAfterDeath("random.date") # 6 patients died before their random glucose!
d <- FunAfterDeath("hba1c.date") # 1 patient
d <- FunAfterDeath("fasting.date") # Nil
d <- FunAfterDeath("ogtt.date") # Nil
# Attendances
d <- FunAfterDeath("gopc.date") # Nil
d <- FunAfterDeath("sopc.date") # Nil
d <- FunAfterDeath("inpatient.date") # 1 patient

# Medications - criteria to be defined!
d <- FunAfterDeath("med.date") # 6 patients
d <- FunAfterDeath("med.exclude") # 5 patients

# Clean: additional criteria
d <- FunAfterDeath("hba1c.date2") # Nil
d <- FunAfterDeath("fasting.date2") # Nil
d <- FunAfterDeath("random.date2") # Nil
d <- FunAfterDeath("sopc.date2") # Nil

dataset <- d

# AJE criteria
###########################
# Date = Inpatient or Later of TWO events within 24 months: 
#       HbA1c, Fasting, Random, ICD code (DM plus complications), Meds (AJE) 
#       separate days if same event type, accounted: type(x).date2 date MUST be after type(x).date?
#       used separate events rather than separate days because MM-YYYY data
# TWO prescription dates:
#       first date includes metformin/thiazolidinediones
#       but EXCLUDING 2x prescriptions of metformin/thiazolidinediones

save("dataset", file = "diagnosis/dataset_tmp.Rdata")

rm(list=ls()); gc()
load("diagnosis/dataset_tmp.Rdata")

library(dplyr)
library(data.table)

load("diagnosis/lab_glucose_aje.Rdata")
attn.op <- readRDS("diagnosis/attendance_aje.rds")
names(attn.op) <- c("serial_no", "ref_date")
attn.op <- data.table(attn.op)

aje.meds <- readRDS("diagnosis/meds_dx_aje.rds")
names(aje.meds) <- c("serial_no", "ref_date", "metformin.thiazo")

d <- bind_rows(aje.fasting, aje.hba1c, aje.random, attn.op, aje.meds)
str(d)
d$metformin.thiazo[is.na(d$metformin.thiazo)] <- FALSE
DT <- data.table(d)
DT <- DT[order(DT$serial_no, ref_date, metformin.thiazo)]
DT <- DT[, dif := c(NA, diff(as.numeric(ref_date))), by = serial_no]
DT$dif <- as.numeric(DT$dif)
if (any(!is.na(DT$dif) & DT$dif < 0)) stop ("ref_date NOT in chronological order")
#  any events within 2 years
DT <- DT[-which(dif > 732)] 

# exclude second metformin/thiazolidinediones prescription as qualifying event
stopifnot(!is.na(DT$metformin.thiazo))

no_metformin_thiazo <- DT[DT$metformin.thiazo == FALSE, list(serial_no, ref_date)]
DT <- DT[DT$metformin.thiazo == TRUE]
DT <- DT[, list(ref_date = min(ref_date)), by = serial_no] # select first prescription only

aje <- rbind(no_metformin_thiazo, DT)
aje <- aje[, list(aje = ref_date[2]), by = serial_no] # date of second qualifying event is diagnostic
aje
aje <- data.frame(aje)

d <- merge (dataset, aje[, c("serial_no", "aje")], all = TRUE, by = "serial_no")

# diagnosis date = Inpatient or Later of TWO qualifying events		
d$aje <- with(d, pmin(inpatient.date, aje, na.rm = T))
sum(!is.na(d$aje)) # 801,336 patient IDs

d <- as.data.table(d)
d <- FunAfterDeath("aje") # 3 patient removed

# WHO 
############
# HbA1c, Fasting, OGTT
#       diagnosis = earliest date
d$who <- with(d, pmin(hba1c.date, fasting.date, ogtt.date, na.rm = T))
d <- FunAfterDeath("who") # nil

# ADA (WHO + random glucose) 
###############################
# Random, HbA1c, Fasting, OGTT
#       diagnosis = earliest date
d$ada.tmp <- apply(d[, c("hba1c.date", "hba1c.date2", "fasting.date", "fasting.date2", "ogtt.date", "ogtt.date2")], 1, function(x) sort(x)[2])
d$ada.tmp <- as.Date(d$ada.tmp)
d$ada <- with(d, pmin(ada.tmp, random.date2, na.rm = T))

d <- FunAfterDeath("ada") # nil
			
# HKU	
############
" 2x Random, ADA, ICD code (T1 and T2 DM), ICPC (T89 & T90).
Date = Earliest date"	

d$hku <- with(d, pmin(ada, ae.date, gopc.date, inpatient.date, sopc.date, med.date, na.rm = T))    
d <- FunAfterDeath("hku") # nil

'# medications exclude metformin/thiazolidinediones
# Remove t3 diagnosis codes?
d$hku.exMetThia <- with(d, pmin(hba1c.date, fasting.date, ogtt.date, 
		ae.date, gopc.date, inpatient.date, sopc.date, 
		random.date2, med.exclude, na.rm = T))
        
d <- FunAfterDeath("hku.exMetThia") # nil'

#####################################################
# Exclude if died before Jan 1, 2006
####################################################
table(format(d$death.date, "%Y"), exclude = NULL)
table(!is.na(d$death.date) & d$death.date < as.Date("2006-01-01"))
d <- d[-which(!is.na(d$death.date) & d$death.date < as.Date("2006-01-01")), ]
dim(d) # 1,780,314

saveRDS(d, file = "diagnosis/dm_criteria.rds")
       

