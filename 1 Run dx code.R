# Diagnosis of attendance 
# setwd("/home/chao/Encfs/secure/Bitlocker")

# SOPC data: 2000 - Dec 2014
########################################################
load("Rdata/sopc_dx_codes.Rdata")
names(sopc_dx_codes)
columns <- c("serial_no", "adate", paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:29))
d <- sopc_dx_codes[, columns]
rm(sopc_dx_codes); gc()

dx1 <- apply(d[, 3:10], 1, function(x) any(x >= code1 & x < code2, na.rm = T))
dx2 <- apply(d[, 11:20], 1, function(x) any(x >= code1 & x < code2, na.rm = T))
dx3 <- apply(d[, 21:31], 1, function(x) any(x >= code1 & x < code2, na.rm = T))
i1 <- as.logical(pmax(dx1, dx2, dx3))

if (any(ls() %in% "code3")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x >= code3 & x < code4, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x >= code3 & x < code4, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x >= code3 & x < code4, na.rm = T))
  i3 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i3 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code5")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x >= code5 & x < code6, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x >= code5 & x < code6, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x >= code5 & x < code6, na.rm = T))
  i5 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i5 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code7")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x >= code7 & x < code8, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x >= code7 & x < code8, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x >= code7 & x < code8, na.rm = T))
  i7 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i7 <- rep(F, nrow(d))
  }
  
if (any(ls() %in% "code9")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x >= code9 & x < code10, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x >= code9 & x < code10, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x >= code9 & x < code10, na.rm = T))
  i9 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i9 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code11")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x == code11, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x == code11, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x == code11, na.rm = T))
  i11 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i11 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code12")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x == code12, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x == code12, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x == code12, na.rm = T))
  i12 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i12 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code13")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x == code13, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x == code13, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x == code13, na.rm = T))
  i13 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i13 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code14")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x == code14, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x == code14, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x == code14, na.rm = T))
  i14 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i14 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code15")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x == code15, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x == code15, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x == code15, na.rm = T))
  i15 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i15 <- rep(F, nrow(d))
  }        

if (any(ls() %in% "code16")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x == code16, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x == code16, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x == code16, na.rm = T))
  i16 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i16 <- rep(F, nrow(d))
  }
  
if (any(ls() %in% "code17")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x == code17, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x == code17, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x == code17, na.rm = T))
  i17 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i17 <- rep(F, nrow(d))
  }
    
  
if (any(ls() %in% "code20")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x >= code20 & x < code21, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x >= code20 & x < code21, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x >= code20 & x < code21, na.rm = T))
  i20 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i20 <- rep(F, nrow(d))
  }
  
if (any(ls() %in% "code22")) {
  dx1 <- apply(d[, 3:10], 1, function(x) any(x >= code22 & x < code23, na.rm = T))
  dx2 <- apply(d[, 11:20], 1, function(x) any(x >= code22 & x < code23, na.rm = T))
  dx3 <- apply(d[, 21:31], 1, function(x) any(x >= code22 & x < code23, na.rm = T))
  i22 <- as.logical(pmax(dx1, dx2, dx3))
} else { 
  i22 <- rep(F, nrow(d))
  }
  
i <- as.logical(pmax(i1, i3, i5, i7, i9, i11, i12, i13, i14, i15, i16, i17, i20, i22))
rm(i1, i3, i5, i7, i9, i11, i12, i13, i14, i15, i16, i17, i20, i22)
sopc <- d[i, ]
rm(d); gc()

# Inpatient data: Jan 1997 - Dec 2014
########################################################
load("Rdata/inpatient.Rdata")
names(inpatient)
columns <- c("serial_no", "adate", paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:15))
d <- inpatient[, columns]
rm(inpatient); gc()

dx1 <- apply(d[, -(1:2)], 1, function(x) any(x >= code1 & x < code2, na.rm = T))
if (any(ls() %in% "code3")) {
  dx3 <- apply(d[, -(1:2)], 1, function(x) any(x >= code3 & x < code4, na.rm = T))
} else { 
  dx3 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code5")) {
  dx5 <- apply(d[, -(1:2)], 1, function(x) any(x >= code5 & x < code6, na.rm = T))
} else {
  dx5 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code7")) {
  dx7 <- apply(d[, -(1:2)], 1, function(x) any(x >= code7 & x < code8, na.rm = T))
} else {
  dx7 <- rep(F, nrow(d))
  }
 
if (any(ls() %in% "code9")) {
  dx9 <- apply(d[, -(1:2)], 1, function(x) any(x >= code9 & x < code10, na.rm = T))
} else {
  dx9 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code11")) {
  dx11 <- apply(d[, -(1:2)], 1, function(x) any(x == code11, na.rm = T))
} else {
  dx11 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code12")) {
  dx12 <- apply(d[, -(1:2)], 1, function(x) any(x == code12, na.rm = T))
} else {
  dx12 <- rep(F, nrow(d))
  }   
  
if (any(ls() %in% "code13")) {
  dx13 <- apply(d[, -(1:2)], 1, function(x) any(x == code13, na.rm = T))
} else {
  dx13 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code14")) {
  dx14 <- apply(d[, -(1:2)], 1, function(x) any(x == code14, na.rm = T))
} else {
  dx14 <- rep(F, nrow(d))
  }   
  
if (any(ls() %in% "code15")) {
  dx15 <- apply(d[, -(1:2)], 1, function(x) any(x == code15, na.rm = T))
} else {
  dx15 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code16")) {
  dx16 <- apply(d[, -(1:2)], 1, function(x) any(x == code16, na.rm = T))
} else {
  dx16 <- rep(F, nrow(d))
  }   

if (any(ls() %in% "code17")) {
  dx17 <- apply(d[, -(1:2)], 1, function(x) any(x == code17, na.rm = T))
} else {
  dx17 <- rep(F, nrow(d))
  }   


if (any(ls() %in% "code20")) {
  dx20 <- apply(d[, -(1:2)], 1, function(x) any(x >= code20 & x < code21, na.rm = T))
} else {
  dx20 <- rep(F, nrow(d))
  }
      
if (any(ls() %in% "code22")) {
  dx22 <- apply(d[, -(1:2)], 1, function(x) any(x >= code22 & x < code23, na.rm = T))
} else {
  dx22 <- rep(F, nrow(d))
  }
  
i <- as.logical(pmax(dx1, dx3, dx5, dx7, dx9, dx11, dx12, dx13, dx14, dx15, dx16, dx17, dx20, dx22))
rm(dx1, dx3, dx5, dx7, dx9, dx11, dx12, dx13, dx14, dx15, dx16, dx17, dx20, dx22)
inpatient <- d[i, ]
 
# A&E & A&E FOLLOW-UP data: Jan 2000 - Dec 2014
########################################################
load("Rdata/ae.Rdata")
names(ae.attn)
columns <- c("serial_no", "adate", paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:20))
d <- ae.attn[, columns]

dx1 <- apply(d[, -(1:2)], 1, function(x) any(x >= code1 & x < code2, na.rm = T))
if (any(ls() %in% "code3")) {
  dx3 <- apply(d[, -(1:2)], 1, function(x) any(x >= code3 & x < code4, na.rm = T))
} else { 
  dx3 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code5")) {
  dx5 <- apply(d[, -(1:2)], 1, function(x) any(x >= code5 & x < code6, na.rm = T))
} else {
  dx5 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code7")) {
  dx7 <- apply(d[, -(1:2)], 1, function(x) any(x >= code7 & x < code8, na.rm = T))
} else {
  dx7 <- rep(F, nrow(d))
  }
 
if (any(ls() %in% "code9")) {
  dx9 <- apply(d[, -(1:2)], 1, function(x) any(x >= code9 & x < code10, na.rm = T))
} else {
  dx9 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code11")) {
  dx11 <- apply(d[, -(1:2)], 1, function(x) any(x == code11, na.rm = T))
} else {
  dx11 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code12")) {
  dx12 <- apply(d[, -(1:2)], 1, function(x) any(x == code12, na.rm = T))
} else {
  dx12 <- rep(F, nrow(d))
  }   
  
if (any(ls() %in% "code13")) {
  dx13 <- apply(d[, -(1:2)], 1, function(x) any(x == code13, na.rm = T))
} else {
  dx13 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code14")) {
  dx14 <- apply(d[, -(1:2)], 1, function(x) any(x == code14, na.rm = T))
} else {
  dx14 <- rep(F, nrow(d))
  }   
  
if (any(ls() %in% "code15")) {
  dx15 <- apply(d[, -(1:2)], 1, function(x) any(x == code15, na.rm = T))
} else {
  dx15 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code16")) {
  dx16 <- apply(d[, -(1:2)], 1, function(x) any(x == code16, na.rm = T))
} else {
  dx16 <- rep(F, nrow(d))
  }   

if (any(ls() %in% "code17")) {
  dx17 <- apply(d[, -(1:2)], 1, function(x) any(x == code17, na.rm = T))
} else {
  dx17 <- rep(F, nrow(d))
  }   


if (any(ls() %in% "code20")) {
  dx20 <- apply(d[, -(1:2)], 1, function(x) any(x >= code20 & x < code21, na.rm = T))
} else {
  dx20 <- rep(F, nrow(d))
  }
      
if (any(ls() %in% "code22")) {
  dx22 <- apply(d[, -(1:2)], 1, function(x) any(x >= code22 & x < code23, na.rm = T))
} else {
  dx22 <- rep(F, nrow(d))
  }
  
i <- as.logical(pmax(dx1, dx3, dx5, dx7, dx9, dx11, dx12, dx13, dx14, dx15, dx16, dx17, dx20, dx22))
rm(dx1, dx3, dx5, dx7, dx9, dx11, dx12, dx13, dx14, dx15, dx16, dx17, dx20, dx22)
ae.attn <- d[i,]

names(ae.fu)
columns <- c("serial_no", "adate", paste0("diag_cd_0", 1:8))
d <- ae.fu[, columns]

dx1 <- apply(d[, -(1:2)], 1, function(x) any(x >= code1 & x < code2, na.rm = T))
if (any(ls() %in% "code3")) {
  dx3 <- apply(d[, -(1:2)], 1, function(x) any(x >= code3 & x < code4, na.rm = T))
} else { 
  dx3 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code5")) {
  dx5 <- apply(d[, -(1:2)], 1, function(x) any(x >= code5 & x < code6, na.rm = T))
} else {
  dx5 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code7")) {
  dx7 <- apply(d[, -(1:2)], 1, function(x) any(x >= code7 & x < code8, na.rm = T))
} else {
  dx7 <- rep(F, nrow(d))
  }
 
if (any(ls() %in% "code9")) {
  dx9 <- apply(d[, -(1:2)], 1, function(x) any(x >= code9 & x < code10, na.rm = T))
} else {
  dx9 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code11")) {
  dx11 <- apply(d[, -(1:2)], 1, function(x) any(x == code11, na.rm = T))
} else {
  dx11 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code12")) {
  dx12 <- apply(d[, -(1:2)], 1, function(x) any(x == code12, na.rm = T))
} else {
  dx12 <- rep(F, nrow(d))
  }   
  
if (any(ls() %in% "code13")) {
  dx13 <- apply(d[, -(1:2)], 1, function(x) any(x == code13, na.rm = T))
} else {
  dx13 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code14")) {
  dx14 <- apply(d[, -(1:2)], 1, function(x) any(x == code14, na.rm = T))
} else {
  dx14 <- rep(F, nrow(d))
  }   
  
if (any(ls() %in% "code15")) {
  dx15 <- apply(d[, -(1:2)], 1, function(x) any(x == code15, na.rm = T))
} else {
  dx15 <- rep(F, nrow(d))
  }

if (any(ls() %in% "code16")) {
  dx16 <- apply(d[, -(1:2)], 1, function(x) any(x == code16, na.rm = T))
} else {
  dx16 <- rep(F, nrow(d))
  }   

if (any(ls() %in% "code17")) {
  dx17 <- apply(d[, -(1:2)], 1, function(x) any(x == code17, na.rm = T))
} else {
  dx17 <- rep(F, nrow(d))
  }   


if (any(ls() %in% "code20")) {
  dx20 <- apply(d[, -(1:2)], 1, function(x) any(x >= code20 & x < code21, na.rm = T))
} else {
  dx20 <- rep(F, nrow(d))
  }
      
if (any(ls() %in% "code22")) {
  dx22 <- apply(d[, -(1:2)], 1, function(x) any(x >= code22 & x < code23, na.rm = T))
} else {
  dx22 <- rep(F, nrow(d))
  }
          
i <- as.logical(pmax(dx1, dx3, dx5, dx7, dx9, dx11, dx12, dx13, dx14, dx15, dx16, dx17, dx20, dx22))
rm(dx1, dx3, dx5, dx7, dx9, dx11, dx12, dx13, dx14, dx15, dx16, dx17, dx20, dx22)
ae.fu <- d[i,]

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

