rm(list = ls())

# Utilization
######################
# outpatient visits

# library(data.table)
FunVisits <- function (x) {

# rm ICD-9, ICPC codes
dropped <- c(grep("diag", names(x)), grep(("proc"), names(x)), grep(("icpc"), names(x)))
x <- x[, -(dropped)]
x$yr <- format(x$adate, "%Y")
x <- x[x$yr >= 2006, ] # 2006 - 2014
print(length(unique(x$serial_no)))
print(str(x))
print(table(x$yr, exclude = NULL))
x
}

# SOPC utilization (2000- )
# by clinics / new or fu
load("Rdata/sopc.Rdata")
sopc <- FunVisits (sopc)
gc()

# GOPC utilization (2006- )
load("Rdata/gopc.Rdata")
gopc <- FunVisits (gopc)
gc()

# Is A&E an outpatient visit?
load("Rdata/ae.Rdata")
ls() # ae.attn and ae.fu
ae.attn <- FunVisits (ae.attn)
ae.fu <- FunVisits (ae.fu)
gc()

# Inpatient utilization (1997- )
load("Rdata/inpatient.Rdata")
inpatient <- FunVisits (inpatient)
gc()


# Clean attendances
# Deaths before attendances
load("Rdata/patient.Rdata")
library(data.table)

FunAfterDeath <- function (y) {
  x <- merge(patient[, c("serial_no", "death.date")], y)
  x <- data.table(x)
if (any(x$adate > x$death.date, na.rm = T)) {
    print(nrow(x[adate > death.date]))
    print(x[adate > death.date, ])
    data.frame(x[-which(adate > death.date), ]) # includes is.na death.date: adate > is.na
} else {data.frame(x)
}}
ae.attn <- FunAfterDeath (ae.attn) # nil
ae.fu <- FunAfterDeath (ae.fu) # nil
gopc <- FunAfterDeath (gopc) # 1
sopc <- FunAfterDeath (sopc) # 12
inpatient <- FunAfterDeath (inpatient) # nil

save(sopc, gopc, ae.attn, ae.fu, inpatient, file = "Rdata/utilization.Rdata")

