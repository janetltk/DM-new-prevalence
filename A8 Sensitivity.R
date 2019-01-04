rm(list = ls())
############################################################
# DM criteria
load("diagnosis/dm20.Rdata")
ls()

# Age 20+ and All DM
# d <- dm20
d <- dm20.self     # Diagnosis = self.reported date if earlier

# type 1 dm
# d <- dm20.self[dm20.self$dm.type != "t1", ]

############################################################
# Sensitivity analysis: remove pregnancy
############################################################
load("diagnosis/pregnancy.Rdata")
a <- d$serial_no %in% serial_no
d <- d[!a, ]

# specify age strata breaks
agecat = c(20, 40, 60, 80) 

# source code
code <- "dm_codes/A_run_incidence.R"
# windows(record=TRUE)

############################################################
# Adjust for No follow-up or Private-use only
############################################################
adj.male = c(rep(0.8656706886, 3), 
	0.6545198739, 0.7391541486, 0.8468177181, 0.8523638329, 
	0.8247139608, 0.7985174687, 0.8475138448, 
	rep(0.9362282433, 4))
	
adj.female <- c(rep(0.8656706886, 3), 	
	0.7659163219, 0.8780285920, 0.8043123587, 0.8435265819, 
	0.7992926191, 0.8808036197, 0.9037039973, 
	rep(0.9109939244, 4))

Name <- "HKU"     
d$dm.date <- d$hku
source(code)
hku <- list(prev, inci)

Name <- "WHO"    
d$dm.date <- d$who
source(code)
who <- list(prev, inci)

Name <- "ADA"    
d$dm.date <- d$ada
source(code)
ada <- list(prev, inci)

Name <- "Nichols"    
d$dm.date <- d$aje
source(code)
aje <- list(prev, inci)

# results
############################################################
       
# table 1
groups <- list (WHO, ADA, Nichols, HKU)
names(groups) <- c("WHO", "ADA", "Nichols", "HKU")
age <- list()
female <- list()
for (i in 1:length(groups)) {
    x <- groups[[i]]
    # n
    r1 <- nrow(x[!is.na(x$dm.yr) & (x$death.yr>2005 | is.na(x$death.yr)), ])
    # incident cases
    r2 <- nrow(x[x$dm.yr %in% years[-1] & (x$death.yr>2005 | is.na(x$death.yr)), ])
    x$age <- x$dm.yr - x$dob
    age[[i]] <- x$age
    names(age)[[i]] <- names(groups)[[i]]
    # mean age at diagnosis
    r3 <- paste0(sprintf("%.1f", round(mean(x$age, na.rm = T), 1)), " (", sprintf("%.1f", round(sd(x$age, na.rm = T), 1)), ")")
    x$age.group <- cut(floor(x$age), breaks = c(agecat, Inf), right = FALSE, labels = labs)
    # % in age groups
    r4 <- sprintf("%.1f", round(table(x$age.group)/nrow(x)*100, 1))
    # % female
    female[[i]] <- x$female
    names(female)[[i]] <- names(groups)[[i]]
    r5 <- sprintf("%.1f", round(nrow(x[x$female==TRUE, ])/nrow(x)*100, 1))
    groups[[i]] <- c(format(c(r1, r2), big.mark=","), r3, r4, r5)   
}

# anova age
test <- data.frame(age=unlist(age), type=factor(rep(c(names(age)), 
    times=c(length(age[[1]]), length(age[[2]]), length(age[[3]]), length(age[[4]])))))
fm1 <- aov(age ~ type, data=test)
anova(fm1)
anova(fm1)[1,5]

test$age.group <- cut(floor(test$age), breaks = c(agecat, Inf), right = FALSE, labels = labs) 
table(test$age.group, test$type)
chisq.test(test$age.group, test$type)$p.value

sapply(female, table)
chisq.test(sapply(female, table))$p.value
groups$p.value <- FunFormatPvalue(c(NA, NA, anova(fm1)[1,5], chisq.test(test$age.group, test$type)$p.value, rep(NA, length(agecat)-1), chisq.test(sapply(female, table))$p.value))
tab1 <- sapply(groups, cbind)
row.names (tab1) <- c(paste0("Total cases ", years[1],"-", years[length(years)]), paste0("Incident cases ", years[2],"-", years[length(years)]), "Age, years", labs, "Female")
tab1

# table si - last year for HKU 
# not included
tab.si <- HKU.last.year
tab.si[, 2:3] <- sapply(tab.si[, 2:3], function(x) format(x, big.mark=","))
tab.si

# table 2 - overall incidence
tab <- rbind(inci.WHO, inci.ADA, inci.Nichols, inci.HKU)
tab2 <- tab[grep("overall", row.names(tab)), ]
t(tab2)

# table 3 & supplementary - HKU
tab3.si <- tab
t(tab3.si[tab3.si$id == "HKU", ])

# table 4 - overall prevalence
tab <- rbind(prev.WHO, prev.ADA, prev.Nichols, prev.HKU)
tab4 <- tab[grep("overall", row.names(tab)), ]
tab5.si <- tab
t(tab4)
t(tab5.si[tab5.si$id == "HKU", ])

