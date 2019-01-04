rm(list = ls())
setwd("/home/chao/Encfs/secure/dm_data")
############################################################
# DM criteria
load("diagnosis/dm20.Rdata")
ls()

# Age 20+ and All DM
# d <- dm20
d <- dm20.self     # Diagnosis = self.reported date if earlier

# type 1 dm
# d <- dm20.self[dm20.self$dm.type != "t1", ]

# specify age strata breaks
agecat <- c(20, 40, 60, 80)
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

Name <- "HKU.exMetThia" # excluding metformin/thiazolidinediones
d$dm.date <- d$hku.exMetThia
source(code)

Name <- "Self"
d$dm.date <- d$self
d$dm.date[d$dm.date > as.Date("2014-12-31")] <- NA
source(code)
self <- list(prev, inci)

save(list=ls(), file = "diagnosis/results_2016_Aug.Rdata")

# Results
############################################################
load("diagnosis/results_2016_Aug.Rdata")

# table 1
groups <- list (WHO, ADA, Nichols, HKU, HKU.exMetThia)
names(groups) <- c("WHO", "ADA", "Nichols", "HKU", "HKU.exMetThia")
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

# Other clinical characteristics
# load("Rdata/bmi_clean.Rdata")
# m <- merge(HKU, bmi, by.x = c("serial_no", "dm.date"), by.y = c("serial_no", "ref_date"))
dim(m)

# anova age
age$HKU.exMetThia <- NULL
test <- data.frame(age=unlist(age), type=factor(rep(c(names(age)),
    times=c(length(age[[1]]), length(age[[2]]), length(age[[3]]), length(age[[4]])))))
fm1 <- aov(age ~ type, data=test)
anova(fm1)
anova(fm1)[1,5]

test$age.group <- cut(floor(test$age), breaks = c(agecat, Inf), right = FALSE, labels = labs)
table(test$age.group, test$type)
chisq.test(test$age.group, test$type)$p.value

female$HKU.exMetThia <- NULL
sapply(female, table)
chisq.test(sapply(female, table))$p.value
groups$p.value <- FunFormatPvalue(c(NA, NA, anova(fm1)[1,5], chisq.test(test$age.group, test$type)$p.value, rep(NA, length(agecat)-1), chisq.test(sapply(female, table))$p.value))
tab1 <- sapply(groups, cbind)
row.names (tab1) <- c(paste0("Total cases ", years[1],"-", years[length(years)]), paste0("Incident cases ", years[2],"-", years[length(years)]), "Age, years", labs, "Female")
setwd("/home/chao/Dropbox/Diabetes/Results/")
write.csv(tab1, "tab1.csv")

# table 2 for overall, male and female + SI (age groups)
inci.HKU
a <- cbind(inci.HKU[, c(colnames(inci.HKU)[grep("adj.rate", colnames(inci.HKU))], "pois.quad", "RR")])
b <- cbind(inci.HKU[, c(colnames(inci.HKU)[grep("95% CI", colnames(inci.HKU))[1:8]], "pois.quad")])
b <- cbind(b, inci.HKU[, ncol(inci.HKU)])
colnames(b) <- colnames(a)
r <- list(NULL)
for (i in 1:nrow(inci.HKU)) {
   r1 <- rbind(a[i,], b[i,])
   r <- rbind(r, r1)
}
tab2i <- r
write.csv(tab2i, "tab2_i.csv")

prev.HKU
a <- cbind(prev.HKU[, c(colnames(prev.HKU)[grep("adj.rate", colnames(prev.HKU))], "log.quad", "RR")])
b <- cbind(prev.HKU[, c(colnames(prev.HKU)[grep("95% CI", colnames(prev.HKU))[1:9]], "log.quad")])
b <- cbind(b, prev.HKU[, ncol(prev.HKU)])
colnames(b) <- colnames(a)
r <- list(NULL)
for (i in 1:nrow(prev.HKU)) {
   r1 <- rbind(a[i,], b[i,])
   r <- rbind(r, r1)
}
tab2p <- r
write.csv(tab2p, "tab2_p.csv")

# supplementary table - overall incidence
tab <- rbind(inci.WHO, inci.ADA, inci.Nichols, inci.HKU, inci.HKU.exMetThia)
tab <- tab[grep("overall", row.names(tab)), ]
rownames(tab) <- c("WHO", "ADA", "Nichols", "HKU", "HKU.exMetThia")
a <- cbind(tab[, c(colnames(tab)[grep("adj.rate", colnames(tab))], "pois.quad")])
b <- cbind(tab[, c(colnames(tab)[grep("95% CI", colnames(tab))[1:8]], "pois.quad")])
colnames(b) <- colnames(a)
r <- list(NULL)
for (i in 1:nrow(tab)) {
   r1 <- rbind(a[i,], b[i,])
   r <- rbind(r, r1)
}
supp4i <- r
write.csv(supp4i, "supp4_i.csv")

# supplementary table - overall prevalence
tab <- rbind(prev.WHO, prev.ADA, prev.Nichols, prev.HKU, prev.HKU.exMetThia)
tab <- tab[grep("overall", row.names(tab)), ]
rownames(tab) <- c("WHO", "ADA", "Nichols", "HKU", "HKU.exMetThia")
a <- cbind(tab[, c(colnames(tab)[grep("adj.rate", colnames(tab))], "log.quad")])
b <- cbind(tab[, c(colnames(tab)[grep("95% CI", colnames(tab))[1:9]], "log.quad")])
colnames(b) <- colnames(a)
r <- list(NULL)
for (i in 1:nrow(tab)) {
   r1 <- rbind(a[i,], b[i,])
   r <- rbind(r, r1)
}
supp4p <- r
write.csv(supp4p, "supp4_p.csv")

# table - last year for HKU not included
tab.si <- HKU.last.year
tab.si[, 2:3] <- sapply(tab.si[, 2:3], function(x) format(x, big.mark=","))
write.csv(tab.si, "tab_si.csv")


library(data.table)
library(MASS)
library(ggplot2)

# P(Diabetes) = p / (1 - u * (1 - o))
# p = our predicted prevalence
#	= P(aware and overlapped cases)
# u = P(a diabetic patient being unaware)
# o = P(overlapped cases among the unaware cases)
adjust <- function(predicted=0.103,u,o)
{
return(predicted/(1-u*(1-o)))
}

#---------------------------------------
# Present as a two way table (if we are not sure about the distribution of u and o)
#---------------------------------------
p <- 0.103
u <- c(0.364, 0.409, 0.636, 0.644, 0.699)
o <- seq(0.0, 0.5, 0.1)

adjusted <- data.table(p=p,expand.grid(u,o))
names(adjusted) <- c("predicted","u","o")
# adjusted[,adjusted_prevalence:=predicted+u*(1-o)]
adjusted[,adjusted_prevalence:=adjust(u=u,o=o)]

#  Two-way sensitivity analysis table
tab.2way <- adjusted[,xtabs(adjusted_prevalence~u+o)] * 100
tab.2way
write.csv(tab.2way, "tab_2way_sensitivity.csv")


#---------------------------------------
# Present as a distribution (need to assign a bivariate distribution to u and o)
#---------------------------------------
# hist(rbinom(n=100000,size=100,prob=0.3)/100)
# hist(rnorm(n=100000,100*0.3,sqrt(100*0.3*0.7))/100)

# arbitrarily assume bivariate normal distribution
# assume u and o independent
n <- 10000
mu <- c(0.5, 0.1)
Sigma <- diag(2)*0.01

# sample from the bivariate distribution of u and o
u_o_sample <- mvrnorm(n=n,mu=mu,Sigma=Sigma)

# predict adjusted prevalence
prob_adjusted_prevalence <- data.table(p=p,u_o_sample)
names(prob_adjusted_prevalence) <- c("predicted","u","o")
# prob_adjusted_prevalence[,adjusted_prevalence:=predicted+u*(1-o)]
prob_adjusted_prevalence[,adjusted_prevalence:=adjust(u=u,o=o)]
ggplot(data=prob_adjusted_prevalence,aes(x=adjusted_prevalence)) +
geom_density() +
scale_x_continuous(breaks = seq(0.1, 1, 0.05), limits = c(0,1)) +
theme_classic()



# library(xlsx)
# xlsx.writeMultipleData
# file : the path to the output file
# ... : a list of data to write to the workbook
xlsx.write <- function (file, ...)
  {
    require(xlsx, quietly = TRUE)
    objects <- list(...)
    fargs <- as.list(match.call(expand.dots = TRUE))
    objnames <- as.character(fargs)[-c(1, 2)]
    nobjects <- length(objects)
    for (i in 1:nobjects) {
        if (i == 1)
            write.xlsx(objects[[i]], file, sheetName = objnames[i])
        else write.xlsx(objects[[i]], file, sheetName = objnames[i],
            append = TRUE)
    }
  }
# xlsx.writeMultipleData("myworkbook.xlsx", mtcars, Titanic, AirPassengers, state.
xlsx.write("results.xlsx", tab1, tab2, tab3, tab4.si, tab5, tab6.si)
#write.xlsx(tab, file="results.xlsx", sheetName="Table 5", append=TRUE)

# Multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.date.row = matchidx$row,
                                      layout.date.col = matchidx$col))
    }
  }
}
multiplot(WHO.plot1, ADA.plot1, Nichols.plot1, HKU.plot1, layout=matrix(c(1:4), nrow=2, byrow=TRUE))
multiplot(WHO.plot2, ADA.plot2, Nichols.plot2, HKU.plot2, layout=matrix(c(1:4), nrow=2, byrow=TRUE))
title("Main")

# prevalence plot
a <- do.call(rbind, list(who[[1]], ada[[1]], aje[[1]], hku[[1]]))
a$id <- factor(a$id, levels=c("WHO","ADA","Nichols","HKU"))
reshape(a[, c("year", "adj", "ci", "id")], timevar="year", direction="wide")

cbbPalette <- c("#56B4E9", "#D55E00", "#E69F00", "#009E73")

plot.prev <- ggplot(a, aes(x=year, group=id, colour=id, y=adj.rate, ymin=lci, ymax=uci)) +
    geom_errorbar(size = 0.5, width = 0.3) +
    scale_colour_manual(values = cbbPalette) +
    geom_point(shape = 16, size = 3) +
    labs(colour = "Criteria") +
    scale_x_continuous("Year", breaks = (2006:2013)) +
    scale_y_continuous("Prevalence, %", breaks=seq(0, 30, 1), limits=c(0, max(a$uci)+1)) +
    ggtitle("Prevalence, %") +
    geom_line(aes(year, fit)) + theme_bw()

# incidence plot
b <- do.call(rbind, list(who[[2]], ada[[2]], aje[[2]], hku[[2]]))
b$id <- factor(b$id, levels=c("WHO","ADA","Nichols","HKU"))
reshape(b[, c("year", "adj", "ci", "id")], timevar="year", direction="wide")

plot.inci <- ggplot(b, aes(x = year, group = id, colour = id,
    y = adj.rate, ymin = lci, ymax = uci)) +
    geom_errorbar( size = 0.5, width = 0.3) +
    scale_colour_manual(values = cbbPalette) +
    geom_point(shape = 16, size = 3) +
    labs(colour = "Criteria") +
    scale_x_continuous("Year", breaks = (2006:2013)) +
    scale_y_continuous("Incidence (per 1,000 person-years)", breaks=seq(0, 40, 1), limits=c(0, max(b$uci)+1)) +
    ggtitle ("Incidence (per 1,000 person-years)") +
    geom_line(aes(year, fit)) + theme_bw()

multiplot(plot.inci, plot.prev, cols=1)


############################################################
# Unadjusted
############################################################
adj.male = c(rep(1, 14))
adj.female = c(rep(1, 14))

Name <- "HKU"
d$dm.date <- d$hku
source(code)

Name <- "WHO"
d$dm.date <- d$who
source(code)

Name <- "ADA"
d$dm.date <- d$ada
source(code)

Name <- "AJE"
d$dm.date <- d$aje
source(code)


# Reviewer 2: Glucose at diagnosis
############################################################
setwd("/home/chao/Encfs/secure/dm_data")
load("diagnosis/results_2016_Aug.Rdata")
rm(list = setdiff(ls(), "HKU"))
head(HKU)

load("Rdata/hba1c_clean.Rdata")
d <- merge(HKU, hba1c, by.x = c("serial_no", "dm.date"), by.y = c("serial_no", "ref_date"))
tab1 <- sapply(d[, c("hba1c", "ifcc")], function(x) tapply(x, d[, "dm.yr"], mean))
summary(aov(hba1c ~ dm.yr, data = d))
pairwise.t.test(d$hba1c, d$dm.yr, p.adjust = "bonferroni")
# significant

library(foreign)
fasting <- read.dta("data/fasting_gc.dta")
fasting$ref_date <- as.Date(paste("15", fasting$ref_date, sep = ""),"%d%b%Y")
d2 <- merge(HKU, fasting, by.x = c("serial_no", "dm.date"), by.y = c("serial_no", "ref_date"))
tab2 <- tapply(d2[, "fasting_gc"], d2[, "dm.yr"], mean)

# glucose at diagnosis -> by definition: duration = 0
load("vfm/baseline_closest.Rdata")
head(closest2yr)
d3 <- merge(HKU, closest2yr[, c("serial_no", "duration")], by = "serial_no")
tab3 <- tapply(d3[, "duration"], d3[, "dm.yr"], function(x) (mean(x, na.rm = T)))

tab <- cbind(tab1, fasting_gc = tab2)
tab
setwd("/home/chao/Dropbox/Diabetes/Results/")
write.csv(tab, "glucose_diagnosis.csv")

# Reviewer 3: Diagnosis criteria by year
############################################################
setwd("/home/chao/Encfs/secure/dm_data")
load("diagnosis/results_2016_Aug.Rdata")
rm(list = setdiff(ls(), "HKU"))
load("diagnosis/dm_criteria.Rdata")
i <- dm.criteria$serial_no %in% HKU$serial_no
d <- dm.criteria[i, ]
stopifnot(d$serial_no %in% HKU$serial_no)
d$yr <- as.numeric(format(d$hku, "%Y"))
d$yr[d$yr<2007] <- 2006
table(d$yr)

'HKU CASE DEFINITION of DIABETES -> flowchart slide'
order.seq <- c("HbA1c", "Fasting glucose", "OGTT", "2x Random glucose", "Medication", "ICD")
tab <- list()
for (i in 1:length(unique(d$yr))) {
  year <- unique(d$yr)[order(unique(d$yr))][i]
  a <- d[d$yr == year, ]
  dim(a)
  # based on earliest date
  # a$type[a$self==a$hku]<-"Self"
  a$type[a$ae.date==a$hku] <-"ICD"
  a$type[a$gopc.date==a$hku] <-"ICD"
  a$type[a$sopc.date==a$hku] <-"ICD"
  a$type[a$inpatient.date==a$hku] <-"ICD"
  a$type[a$med.date==a$hku] <-"Medication"
  a$type[a$random.date2==a$hku] <-"2x Random glucose"
  a$type[a$ogtt.date==a$hku] <-"OGTT"
  a$type[a$fasting.date==a$hku] <-"Fasting glucose"
  a$type[a$hba1c.date==a$hku] <-"HbA1c"
  total <- data.frame(table(a$type))
  total$Var1 <-factor(total$Var1, levels = order.seq)
  tab[[i]] <- total[order(total$Var1), 2]
  names(tab)[i] <- year
}
names(tab)[1] <- "2006 and earlier"
tab <- do.call(cbind, tab)
tab <- rbind(tab, colSums(tab))
row.names(tab) <- c(order.seq, "Total")
setwd("/home/chao/Dropbox/Diabetes/Results/")
write.csv(tab, "tab_diagnosis.csv")

