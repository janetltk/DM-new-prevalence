rm(list = ls())
gc()

run_dx <- "dm-codes/01 Clean data/1_run_dx_code.R"
run_proc <- "dm-codes/01 Clean data/1_run_proc_code.R"

########################################################
# AF 
########################################################
# Assume all of the following dx codes:
# K78 Atrial fibrillation/flutter
# 427.31 Atrial fibrillation
# 427.32 Atrial flutter

code1 <- 427.3
code2 <- 427.4
icpc <- "K78"

source(run_dx)
af <- data.frame(d)
save(af, gopc, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/af.Rdata")
dim(gopc); dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()

########################################################
# Cancer
########################################################
# Hx of Cancer (Benign & Malignant)
'
A79 Malignancy NOS
B72 Hodgkins disease/lymphoma
B73 Leukaemia
B74 Malignant neoplasm blood other
B75 Benign/unspecified neoplasm blood
D74 Malignant neoplasm stomach
D75 Malignant neoplasm colon/rectum
D76 Malignant neoplasm pancreas
D77 Malig. neoplasm digest other/NOS
D78 Neoplasm digest benign/uncertain
F74 Neoplasm of eye/adnexa
H75 Neoplasm of ear
K72 Neoplasm cardiovascular
L71 Malignant neoplasm musculoskeletal
L97 Neoplasm benign/unspec musculo
N74 Malignant neoplasm nervous system
N75 Benign neoplasm nervous system
N76 Neoplasm nervous system unspec
R84 Malignant neoplasm bronchus/lung
R85 Malinant neoplasm respiratory, other
R86 Benign neoplasm respiratory
R92 Neoplasm respiratory unspecified
S77 Malignant neoplasm of skin
S78 Lipoma
S79 Neoplasm skin benign/unspecified
S80 Solar keratosis/sunburn
T71 Malignant neoplasm thyroid
T72 Benign neoplasm thyroid
T73 Neoplasm endocrine oth/unspecified
U75 Malignant neoplasm of kidney
U76 Malignant neoplasm of bladder
U77 Malignant neoplasm urinary other
U78 Benign neoplasm urinary tract
U79 Neoplasm urinary tract NOS
W72 Malignant neoplasm relate to preg.
W73 Benign/unspec. neoplasm/pregnancy
X75 Malignant neoplasm cervix
X76 Malignant neoplasm breast female
X77 Malignant neoplasm genital other (f)
X78 Fibromyoma uterus
X79 Benign neoplasm breast female
X80 Benign neoplasm female genital
X81 Genital neoplasm oth/unspecied (f)
Y77 Malignant neoplasm prostate
Y78 Malign neoplasm male genital other
Y79 Benign/unspec. neoplasm gen. (m)
'
icpc <- c("A79|B72|B73|B74|B75|D74|D75|D76|D77|D78|F74|H75|K72|L71|L97|N74|N75|N76|R84|R85|R86|R92|S77|S78|S79|S80|T71|T72|T73|U75|U76|U77|U78|U79|W72|W73|X75|X76|X77|X78|X79|X80|X81|Y77|Y78|Y79")

# Hx of Cancer (Benign & Malignant) 140-239
code1 <- 140
code2 <- 240

source(run_dx)
cancer <- data.frame(d)
save(cancer, gopc, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/cancer.Rdata")
dim(gopc); dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()

########################################################
# CHD
########################################################
# Hx of CHD

# K74 Ischaemic heart disease w. angina
# K75 Acute myocardial infarction
# K76 Ischaemic heart disease w/o angina

# CHD as per UKPDS: MI (410) and sudden death (798)
# CHD as per CUHK: CHD ICD9 MI(410) and IHD (411-414)
icpc <- c("K74|K75|K76")
code1 <- 410
code2 <- 415

source(run_dx)
chd <- data.frame(d)
save(chd, gopc, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/chd.Rdata")
dim(gopc); dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()

########################################################
# MI
########################################################
# K75 Acute myocardial infarction
# Acute MI ICD9 (410), Old MI ICD-9 (412)
icpc <- c("K75")
code1 <- 410; code2 <- 411
code3 <- 412; code4 <- 413

source(run_dx)
mi <- data.frame(d)
save(mi, gopc, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/mi.Rdata")
dim(gopc); dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()

########################################################
# Stroke (predictor variable: i.e non-fatal stroke) 
########################################################
# Stroke (1st or subsequent)defined as non-fatal stroke (ICD-9 code 430 and 434.9, or 436) or fatal stroke (ICD-9 code 430 and 438.9)
# ICD-9 code 435 TIA

# K90 Stroke/cerebrovascular accident
# K89 Transient cerebral ischaemia

# predictor variable as baseline: only include prior NON-FATAL stroke
icpc <- "K90" # exclude TIA
code1 <- 430; code2 <- 435
# exclude 435 TIA
code3 <- 436; code4 <- 437

source(run_dx)
stroke <- data.frame(d)
save(stroke, gopc, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/stroke.Rdata")
dim(gopc); dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()


# Stroke: haemorrhagic (ICD-9 code 430-432)
code1 <- 430; code2 <- 433
source(run_dx)
stroke_haemorrhagic <- data.frame(d)
save(stroke_haemorrhagic, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/stroke_haemorrhagic.Rdata")
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()


# Stroke: occulusion (ICD-9 code 433-434)
code1 <- 433; code2 <- 435
source(run_dx)
stroke_occlusion <- data.frame(d)
save(stroke_occlusion, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/stroke_occlusion.Rdata")
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()


########################################################
# CKD
########################################################
# no GOPC ICPC code

# Hx of CKD (ICD-9 code 585-587)
code1 <- 585
code2 <- 588

source(run_dx)
ckd <- data.frame(d)
save(ckd, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/ckd.Rdata")
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()

########################################################
# Secondary diabetes - none identified!
########################################################
# no GOPC ICPC code

# Hx of secondary diabetes (ICD-9 code 249)
code1 <- 249
code2 <- 250

source(run_dx)
dm_secondary <- data.frame(d)

# none identified
# save(dm_secondary, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/dm_secondary.Rdata")
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()

########################################################
# diabetes with complications
########################################################
# no GOPC ICPC code

# Diabetes mellitus with complication (ICD-9 code 250.1-250.9)
# 357.2 Polyneuropathy in diabetes
# 366.41 Diabetic cataract
# 362.0 Diabetic retinopathy

code1 <- 250.1; code2 <- 251
code3 <- 357.2; code4 <- 357.3
code5 <- 366.41; code6 <- 366.42
code7 <- 362.0; code8 <- 362.1

source(run_dx)
dm.complications <- data.frame(d)

save(dm.complications, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/dm_complications.Rdata")
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()

########################################################
# PAD & Amputation
########################################################
# No Hx of PAD ICPC code

# Hx of PAD (CUHK: lower limb amputation, revascularization of PAD, ABPI <0.9)
# UKPDS PVD: Defined from presence of intermittent claudication or ankle brachial pressure index < 0.9
# UKPDS Amputation: (1st or subsequent) of a digit or limb (ICD-9 code 5.845 and 5.848, or 250.6) or a fatal peripheral vascular event (ICD-9 code 997.2, 997.6, 250.6 or 440.2)

# 250.6 Diabetes with neurological manifestations
# Atherosclerosis
# 443.9 Peripheral vascular disease, unspecified: Intermittent claudication NOS
# 440.2 Of native arteries of the extremities
# 997.2 Peripheral vascular complications

# PAD
code1 <- 250.6; code2 <- 250.7
code3 <- 443.9; code4 <- 444.0
code5 <- 440.2; code6 <- 440.3
code7 <- 997.2; code8 <- 997.3

source(run_dx)
pad.dx <- data.frame(d)
save(pad.dx, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/pad_dx.Rdata")
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()

# Amputations
code1 <- 250.6; code2 <- 250.7
code3 <- 440.2; code4 <- 440.3
code5 <- 997.2; code6 <- 997.3

source(run_dx)
amp.dx <- data.frame(d)
save(amp.dx, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/amputation_dx.Rdata")
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()


# Procedures
# 84.1 Amputation of lower limb
############################################################
code1 <- 84.1
code2 <- 84.2

source(run_proc)
amp.proc <- data.frame(d)
save(amp.proc, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/amputation_proc.Rdata")

library(data.table)
load("Rdata/amputation_dx.Rdata")
amp <- data.table(rbind(amp.dx, amp.proc))
amp <- amp[, list(adate = min(adate)), by = "serial_no"]
dim(amp); dim(amp.dx); dim(amp.proc)
amp <- data.frame(amp)
save(amp, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/amputation.Rdata")
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()

load("Rdata/amputation_proc.Rdata")
load("Rdata/pad_dx.Rdata")
pad <- data.table(rbind(pad.dx, amp.proc))
pad <- pad[, list(adate = min(adate)), by = "serial_no"]
dim(pad); dim(pad.dx); dim(amp.proc)
pad <- data.frame(pad)
save(pad, sopc, inpatient, ae.attn, ae.fu, file = "Rdata/pad.Rdata")
rm(list = setdiff(ls(), c("run_dx", "run_proc")))
gc()


########################################################
# Haemodialysis 
########################################################
# Procedures
# 39.95 haemodialysis 
code1 <- 39.95
code2 <- 39.96
  
source(run_proc)

# Acute or Chronic charge / fee?? 
# Assume all A&E is acute, A&E follow-up and SOPC is chronic 
# Inpatients assume ALL LOS = 0 as chronic hemodialysis (day case)
# LOS of 1 or more days = acute hemodialysis

ae.attn$acute.chronic <- "acute"
ae.fu # no records
sopc$acute.chronic <- "chronic"

# need los for inpatient
load("Rdata/inpatient.Rdata")
names(inpatient)
columns <- c("serial_no", "adate", paste0("proc_cd_0", 1:9), paste0("proc_cd_", 10:15), "los_hr")
d <- inpatient[, columns]
rm(inpatient); gc()

dx1 <- apply(d[, -(1:2)], 1, function(x) any(x >= code1 & x < code2, na.rm = T))
i <- as.logical(dx1)
inpatient <- d[i, ]

table(inpatient$los_hr)
inpatient$acute.chronic <- ifelse(inpatient$los_hr < 24, "chronic", "acute")

# rbind
ae.attn$type <- "ae.attn"
sopc$type <- "sopc"
inpatient$type <- "ip"
cols <- c("serial_no", "adate", "acute.chronic", "type")
d <- rbind(ae.attn[, cols], sopc[, cols], inpatient[, cols])

d$ref_date <- d$adate
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

load("Rdata/patient.Rdata")
d <- merge(d, patient[, c("serial_no", "dob", "death.date")], by = "serial_no")
d <- FunDeaths (d) # observations after death
d <- FunBirth (d) # observations before birth
d[, c("dob", "death.date", "yr", "ref_date")] <- list(NULL)

d$yr <- format(d$adate, "%Y")
table(d$yr, exclude = NULL)
print(length(unique(d$serial_no)))
print(length(d$serial_no))

# from 2006 only
print(length(unique(d$serial_no[d$yr >= 2006])))
print(length(d$serial_no[d$yr >= 2006]))
table(d$type[d$yr >= 2006])
table(format(d$adate, "%Y"), d$type)

haemodialysis.items <- d

# Aggregate by patient, year and type of attendance
library(reshape2)
d <- dcast(d, serial_no + yr ~ acute.chronic, length)
head(d)
names(d)[3:4] <- c("h.acute", "h.chronic")

haemodialysis <- d

h.ae.attn <- ae.attn
h.sopc <- sopc
h.ip <- inpatient
save(haemodialysis, haemodialysis.items, h.ae.attn, h.sopc, h.ip, file = "Rdata/haemodialysis.Rdata")


