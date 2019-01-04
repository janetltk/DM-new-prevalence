rm(list = ls())
load("diagnosis/dm_criteria.Rdata")

age <- 20

# Select patients diagnosed by age and type
#############################################################
table(dm.criteria$dm.type, exclude = NULL)
# All DM including type 1

FunBelowAge <- function (criteria) {
# Remove patients diagnosed below age x
    names(d) [names(d) %in% criteria] <- "dm.date"
    d <- d[!is.na(d$dm.date),]
    d$age.dx <- as.numeric(format(d$dm.date, format = "%Y")) - d$dob
    d$age.dx[is.na(d$age.dx)] <- 0
    cat("\n", criteria, "\n", "Age under", age, "at diagnosis")
    print(table(d$age.dx < age))
    cat("\n", "Age at diagnosis")
    print(table(d[d$age.dx < age, c("age.dx")]))
    d <- d[d$age.dx >= age, ]
    if (any(d$dm.date > d$death.date, na.rm = TRUE)) stop ("death before diagnosis")
    names(d) [names(d) %in% "dm.date"] <- criteria
    d[, c("serial_no", "female", "dob", "death.date", "dm.type", criteria)]
}

# Select patients diagnosed at age [age/20] years and over
#############################################################
d <- dm.criteria

aje <- FunBelowAge("aje") # 3280
who <- FunBelowAge("who") # 1958
ada <- FunBelowAge("ada") # 5047
hku <- FunBelowAge("hku") # 4556
hku.exMetThia <- FunBelowAge("hku.exMetThia") # 4479

names(d) [names(d) %in% "self.reported"] <- "self"
self <- FunBelowAge("self") # 1788
# FunBelowAge removed self-reported diagnosis dates before birth

d <- Reduce (function(x, y)
    merge(x, y, by = c("serial_no", "female", "dob", "death.date", "dm.type"), all = TRUE),
    list(aje, who, ada, hku, self, hku.exMetThia))

colSums(!is.na(d[, c("aje", "who", "ada", "hku", "self", "hku.exMetThia")]))
apply(d[, c("aje", "who", "ada", "hku", "self", "hku.exMetThia")], 2, function (x) min(x, na.rm = TRUE))
apply(d[, c("aje", "who", "ada", "hku", "self", "hku.exMetThia")], 2, function (x) max(x, na.rm = TRUE))

dm20 <- d
sapply(d[, c("aje", "who", "ada", "hku", "hku.exMetThia")], function(x) table(format(x, "%Y") >= 2006))
# type 1 dm
sapply(d[d$dm.type == "t1", c("aje", "who", "ada", "hku", "hku.exMetThia")], function(x) table(format(x, "%Y") >= 2006))

# Among dm criteria group, diagnosis = self.reported if earlier
# HKU plus SELF-REPORTED (for DURATION of diabetes)
#############################################################
sum(is.na(d$hku) & !is.na(d$self)) # Self-reported has extra 895 cases from 1940 to 2014

FunEarlierSelfDate <- function (x) {
# Among confirmed dm criteria group:
#       diagnosis date = self.reported if earlier
    a <- merge(x, d[, c("serial_no", "self")], all.x = TRUE, by = "serial_no")
    if (any(is.na(a[, 6]))) stop ("missing diagnosis date")
    cat("Self-reported date earlier than", names(a)[6], "\n")
    print(table(a$self < a[, 6], exclude = NULL))
    a[, 6] <- pmin(a[, 6], a[, 7], na.rm = TRUE)
    a
}

aje <- FunEarlierSelfDate(aje)
who <- FunEarlierSelfDate(who)
ada <- FunEarlierSelfDate(ada)
hku <- FunEarlierSelfDate(hku)
hku.exMetThia <- FunEarlierSelfDate(hku.exMetThia)

d <- Reduce (function(x, y)
    merge(x, y, by = c("serial_no", "female", "dob", "death.date", "dm.type"), all = TRUE),
    list(aje[, 1:6], who[, 1:6], ada[, 1:6], hku[, 1:6], self, hku.exMetThia[, 1:6]))
colSums(!is.na(d[, c("aje", "who", "ada", "hku", "self", "hku.exMetThia")]))

dm20.self <- d
sapply(d[, c("aje", "who", "ada", "hku", "hku.exMetThia")], function(x) table(format(x, "%Y") >= 2006))
# type 1 dm
sapply(d[d$dm.type == "t1", c("aje", "who", "ada", "hku", "hku.exMetThia")], function(x) table(format(x, "%Y") >= 2006))

# Merge pre-diabetes with earliest date of diagnosis including self (dm20.self)
load("diagnosis/lab_glucose.Rdata")
load("Rdata/patient.Rdata")
d <- prediabetes
d <- merge(dm20.self[, c("serial_no", "dm.type", "hku")], d, by = "serial_no", all.y = T)
d <- merge(patient[, c("serial_no", "female", "dob", "death.date")], d, by = "serial_no", all.y = T)

# remove prediabetes results after dm diagnosis (or death)
d$censordate <- with(d, pmin(hku, death.date, na.rm = T))
d <- d[with(d, prediabetes < censordate | is.na(censordate)), ]
d$hba1c.date[with(d, hba1c.date >= censordate & !is.na(censordate))] <- NA
d$fasting.date[with(d, fasting.date >= censordate & !is.na(censordate))] <- NA
d$ogtt.date[with(d, ogtt.date >= censordate & !is.na(censordate))] <- NA

# remove from prediabetes cases if dead or develop dm
# see "A7 Prediabetes.R" file

# age [age/20] or over
d <- d[with(d, as.numeric(format(prediabetes, "%Y")) - dob >= age), ]
d$hba1c.date[with(d, as.numeric(format(hba1c.date, "%Y")) - dob < age)] <- NA
d$fasting.date[with(d, as.numeric(format(fasting.date, "%Y")) - dob < age)] <- NA
d$ogtt.date[with(d, as.numeric(format(ogtt.date, "%Y")) - dob < age)] <- NA
str(d)
# 653,061 patients

prediabetes20 <- d
save(dm20, dm20.self, prediabetes20, file = "diagnosis/dm20.Rdata")
