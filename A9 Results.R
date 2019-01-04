rm(list=ls())
############################################################
# DISSERTATION
############################################################
# DM CRITERION
load("D:/dm_diagnosis/dm_20.RData"); ls()

d<-Reduce(function(x, y) merge(x, y, by="serial_no", all=TRUE),
          list(ada[,c(1,4)], who[,c(1,4)], hku[,c(1,4)]))

load("D:/dm_diagnosis/patient.Rdata")
# load("D:/dm_diagnosis/bmi_clean.Rdata")

d<-merge(patient, d, by="serial_no", all.y=T)
str(d)

# TABLE 1: Characteristics
# Cut age into age categories (5-year strata)
FUN.age.cat <- function(x, lower=20, upper=85, by=5, sep="-", above.char="+"){
  labs <- c(paste(seq(lower, upper-by, by=by),
                  seq(lower+by-1, upper-1, by=by), sep=sep),
            paste(upper, above.char, sep = ""))
  cut(floor(x), breaks=c(seq(lower, upper, by=by), Inf),
      right = FALSE, labels = labs)
}

d$curr_smoking_status[d$curr_smoking_status==9]<-3
FUN.table1 <-function (criterion){
d$age=as.numeric(format(d[,(criterion)], "%Y"))-d$dob
a<-d[!is.na(d[,(criterion)]),]
print(length(a$serial_no))
print(round(summary(a$age), d=1))
print(round(sd(a$age), d=1))
a$age_cat<-FUN.age.cat(a$age)
print(round(prop.table(table(a$age_cat))*100, d=1))
print(round(prop.table(table(a$female))*100, d=1))
print(round(prop.table(table(a$curr_smoking_status))*100, d=1))
a
}

who = FUN.table1 ("who")
ada = FUN.table1 ("ada")
hku = FUN.table1 ("hku")

# ANOVA age
y1 = data.frame(y=who$age, group="who")
y2 = data.frame(y=ada$age, group="ada")
y3 = data.frame(y=hku$age, group="hku")

y<-rbind(y1, y2, y3)
tmpfn = function(x) c(sum = sum(x), mean = mean(x), var = var(x), n = length(x))
tapply(y$y, y$group, tmpfn)
data = data.frame(y = y$y, group = factor(y$group))
fit = lm(y ~ group, data)
anova(fit)

# Chi-Sq sex, smoking
tbl = rbind(table(who$female), table(ada$female), table(hku$female))
tbl
chisq.test(tbl)

tbl = rbind(table(who$curr_smoking_status), table(ada$curr_smoking_status), table(hku$curr_smoking_status))
tbl
chisq.test(tbl)

#############################################################
# DM Diagnosis Criteria
#############################################################
load("D:/dm_diagnosis/dataset_raw.RData")
str(d_raw)
d<-d_raw

# HKU	
" 2x Random, HbA1c, Fasting, OGTT, ICD code (T2DM only), ICPC (T90).
Date = Earliest date"	
d$hku<-with(d, pmin(hba1c.pos, fasting.pos, ogtt.pos, 
		ae.pos, gopc.pos, inpatient.pos, sopc.pos,
		random.pos2, na.rm=T))

FUN.after.death <-function (event) {
	if(any(!is.na(event) & !is.na(d$death.date) & d$death.date<event)) {
		print("WARNING: Event happened AFTER death")
		print(length(d[!is.na(event) & !is.na(d$death.date) & d$death.date<event,c("serial_no")]))
		print(d[!is.na(event) & !is.na(d$death.date) & d$death.date<event,])}	
	}
FUN.after.death (d$hku) # Nil
'
# DM first of HKU or self-reported
d$dm_date<-pmin(d$hku, d$self.reported, na.rm=T)# Check cleaned
FUN.after.death (d$dm_date) # Nil
'
'CASE DEFINITION of DIABETES -> flowchart slide'
d<-d[!is.na(d$hku),]
# str(d)
# based on earliest date
order.seq<-c("HbA1c","Fasting glucose","OGTT","2x Random glucose",
	"Inpatient","Specialist outpatient","General outpatient","Accident & Emergency")
# d$type[d$self==d$hku]<-"Self"
d$type[d$ae.pos==d$hku]<-"Accident & Emergency"
d$type[d$gopc.pos==d$hku]<-"General outpatient"
d$type[d$inpatient.pos==d$hku]<-"Inpatient"
d$type[d$sopc.pos==d$hku]<-"Specialist outpatient"
d$type[d$random.pos2==d$hku]<-"2x Random glucose"
d$type[d$ogtt.pos==d$hku]<-"OGTT"
d$type[d$fasting.pos==d$hku]<-"Fasting glucose"
d$type[d$hba1c.pos==d$hku]<-"HbA1c"
total<-data.frame(table(d$type))
total$Var1<-factor(total$Var1, levels = order.seq)
total<-total[order(total$Var1),]
print(total)
print(paste(sum(total[,2]), sum(total[1:4,2]), sum(total[5:8,2])))

# Exclude Age <20
FUN.clean.20 <- function (event) {
	age<-20
	d$diagnosis<-event
	a<-subset(d, !is.na(d$diagnosis), select=-c(hba1c.pos2, fasting.pos2, ogtt.pos2, ae.pos2, sopc.pos2, aje.med:aje.med2))
	if (any(is.na(a$diagnosis))) stop("Missing data")
	cat("\n", "Exclude age at diagnosis under", age)
	a$age_dx<-as.numeric(format(a$diagnosis, format = "%Y"))-a$dob
	cat("\n", "Age under", age, "at diagnosis")
	print(table(a$age_dx<age))
	cat("\n","Age at diagnosis")
	print(table(a[a$age_dx<age, c("age_dx")]))
	a<-a[a$age_dx>=age,]
	if (any(a$age_dx<age)) stop (c("age under ", age))
	if (any(a$diagnosis>a$death.date & !is.na(a$death.date))) stop ("death before diagnosis")
	subset(a, select=-c(age_dx, diagnosis))
}
a<-FUN.clean.20(d$hku)
total<-data.frame(table(a$type))
total$Var1 <-factor(total$Var1, levels = order.seq)
total<-total[order(total$Var1),]
total
total<-matrix(total[,2])

table.all<-colSums(!is.na(subset (a, select=c(hba1c.pos, 
	fasting.pos, ogtt.pos, ae.pos, gopc.pos, inpatient.pos, 
	sopc.pos, random.pos2))))
table.all<-table.all[order(table.all, decreasing=T)]
table.all
m<-matrix(c(table.all, rep (0, 7*8)), ncol=8)
m<-rbind(t(total), m); m
colnames(m)<-order.seq
names(table.all)
rownames(m)<-c("All cases", "HbA1c","Fasting glucose",
	"General outpatient","Inpatient","2x Random glucose",
	"Specialist outpatient","Accident & Emergency","OGTT")
library (gplots)
par(mar= c(5, 10, 4, 2) + 0.1)
colour<-c("white","lightblue","pink","yellow","palegreen",
	"red","wheat","violet")
	
barplot2(t(m)/1000, horiz=T, axes=F, col=colour,
	las=1, xlab="'000 cases", xlim=c(0, 700),
	names.arg= c("All cases", colnames(m)))
axis(1, at=100*0:7)
#title(main="Number of diabetes cases identified by HKU diagnosis criteria",
	cex=1.5)
legend("topright", colnames(m), cex=1, fill=colour, bty="n")
text (100, 0.7, "HbA1c")
text (243, 0.7, "Fasting glucose")
text (380, 0.7, "Inpatient")
text (540, 0.7, "General outpatient")
hku<-m

# WHO 
d<-d_raw
" HbA1c, Fasting, OGTT. Date = Earliest date"
d$who<-with(d, pmin(hba1c.pos, fasting.pos, ogtt.pos, 
				na.rm=T))
FUN.after.death (d$who) # Nil
d<-d[-which(!is.na(d$who) & !is.na(d$death.date) & d$death.date<d$who),]	
d<-d[!is.na(d$who),]
# based on earliest date
order.seq<-c("HbA1c","Fasting glucose","OGTT")
d$type[d$ogtt.pos==d$who]<-"OGTT"
d$type[d$fasting.pos==d$who]<-"Fasting glucose"
d$type[d$hba1c.pos==d$who]<-"HbA1c"
total<-data.frame(table(d$type))
total$Var1 <-factor(total$Var1, levels = order.seq)
total<-total[order(total$Var1),]
print(total)
print(paste(sum(total[,2])))
a<-FUN.clean.20(d$who)

total<-data.frame(table(a$type))
total$Var1 <-factor(total$Var1, levels = order.seq)
total<-total[order(total$Var1),]
total
total<-matrix(total[,2])

table.all<-colSums(!is.na(subset (a, select=c(hba1c.pos, 
	fasting.pos, ogtt.pos))))
table.all<-table.all[order(table.all, decreasing=T)]
table.all
m<-matrix(c(table.all, rep (0, 2*3)), ncol=3)
m<-rbind(t(total), m); m
colnames(m)<-order.seq
names(table.all)
rownames(m)<-c("All cases","HbA1c","Fasting glucose","OGTT")
m
library (gplots)
par(mar= c(5, 10, 4, 2) + 0.1)
colour<-c("white","lightblue","pink","yellow","palegreen",
	"red","wheat","violet")
	
barplot2(t(m)/1000, horiz=T, axes=F, col=colour,
	las=1, xlab="'000 cases", xlim=c(0, 700),
	names.arg= c("All cases", colnames(m)))
axis(1, at=100*0:7)
#title(main="Number of diabetes cases identified by WHO diagnosis criteria",
	cex=1.5)
legend("topright", colnames(m), cex=1, fill=colour, bty="n")
text (200, 0.7, "HbA1c")
who<-m

# ADA (WHO + random glucose) 
d<-d_raw
" Random, HbA1c, Fasting, OGTT. Date = Earliest date"
d$ada<-with(d, pmin(random.pos, hba1c.pos, fasting.pos, ogtt.pos, 
		na.rm=T))
FUN.after.death (d$ada) # Nil
d<-d[!is.na(d$ada),]
# based on earliest date
order.seq<-c("HbA1c","Fasting glucose","OGTT","Random glucose")
d$type[d$random.pos==d$ada]<-"Random glucose"
d$type[d$ogtt.pos==d$ada]<-"OGTT"
d$type[d$fasting.pos==d$ada]<-"Fasting glucose"
d$type[d$hba1c.pos==d$ada]<-"HbA1c"
total<-data.frame(table(d$type))
total$Var1 <-factor(total$Var1, levels = order.seq)
total<-total[order(total$Var1),]
print(total)
print(paste(sum(total[,2])))
a<-FUN.clean.20(d$ada)

total<-data.frame(table(a$type))
total$Var1 <-factor(total$Var1, levels = order.seq)
total<-total[order(total$Var1),]
total
total<-matrix(total[,2])

table.all<-colSums(!is.na(subset (a, select=c(hba1c.pos, 
	fasting.pos, ogtt.pos, random.pos))))
table.all<-table.all[order(table.all, decreasing=T)]
table.all
m<-matrix(c(table.all, rep (0, 3*4)), ncol=4)
m<-rbind(t(total), m); m
colnames(m)<-order.seq
names(table.all)
rownames(m)<-c("All cases","HbA1c","Fasting glucose","Random glucose","OGTT")
m
library (gplots)
par(mar= c(5, 10, 4, 2) + 0.1)
colour<-c("white","lightblue","pink","yellow","palegreen",
	"red","wheat","violet")
	
barplot2(t(m)/1000, horiz=T, axes=F, col=colour,
	las=1, xlab="'000 cases", xlim=c(0, 700),
	names.arg= c(rownames(m)))
axis(1, at=100*0:7)
# title(main="Number of diabetes cases identified by ADA diagnosis criteria",
	cex=1.5)
legend("topright", colnames(m), cex=1, fill=colour, bty="n")
text (180, 0.7, "HbA1c")
ada<-m

#############################################################
# Prevalence & Incidence
#############################################################
'UNADJUSTED FOR NO F/U & PRIVATE PATIENTS'
setwd("C:/Users/qchao/Dropbox (Personal)/+ Diabetes/Valuation v6/epi unadjusted")
library(gplots); library(reshape)

FUN.plot.prev <- function (a) {
	a[,-1]<-apply(a[,-1], 2, function (x) as.numeric(substr(x, 1, nchar(x)-1)))
	a
	'PREVALENCE, by AGE GROUP'
	b = a[1:15,]
	levels(b$Age) [levels(b$Age)%in% "Total"]<-"All"
	b = melt(b)
	b$year = as.numeric(substr(b$variable,2,5))
	# convert factor to numeric for convenience
	b$factors = as.numeric(factor(b$Age))
	b
	ngroups = max(b$factors)

	# range for the x and y axis
	xrange <- c(min(b$year), max(b$year)+1)
	yrange <- c(0, 35)
	# set up the plot
	plot(xrange, yrange, type="n", axes=F, bty="L",
		xlab="Year", ylab="Prevalence, %", las=1, xpd=FALSE)
	axis(1, las=1, at=min(b$year):max(b$year))
	axis(2, las=1, at=0:7*5)
	colour <- c(rainbow(ngroups-1), "black")
	linetype <- c(rep(1, ngroups-1), 1)
	plotchar <- c(rep(20, ngroups-1), 15)
	# lines
	for (i in 1:ngroups) {
	  age <- subset(b, factors==i)
	  lines(age$year, age$value, type="b", lwd=1.5,
		lty=linetype[i], col=colour[i], pch=plotchar[i])
	}
	legend(max(b$year)+0.5, 35, unique(b$Age), cex=0.9, bty="n", col=colour,
	   pch=plotchar, lty=linetype, title="Age Group")
}

a<-read.csv("who_prevalence.csv")
FUN.plot.prev (a)   
title("Prevalence by age group, %  (WHO criteria)")

a<-read.csv("ada_prevalence.csv")
FUN.plot.prev (a)   
title("Prevalence by age group, %  (ADA criteria)")

a<-read.csv("hku_prevalence.csv")
FUN.plot.prev (a)   
title("Prevalence by age group, %  (HKU criteria)")

FUN.plot.inci <- function (a) {
'INCIDENCE RATE, by AGE GROUP'
b = a[1:15,]
levels(b$Age) [levels(b$Age)%in% "Total"]<-"All"
b = melt(b)
b$year = as.numeric(substr(b$variable,2,5))
# convert factor to numeric for convenience
b$factors = as.numeric(factor(b$Age))
b
ngroups = max(b$factors)

# range for the x and y axis
xrange <- c(min(b$year), max(b$year)+1)
# set up the plot
plot(xrange, yrange, type="n", axes=F, bty="L",
	xlab="Year",ylab="Incidence rate", las=1, xpd=FALSE)
axis(1, las=1, at=min(b$year):max(b$year))
axis(2, las=1, at=0:8*10)
colour <- c(rainbow(ngroups-1), "black")
linetype <- c(rep(1, ngroups-1), 1)
plotchar <- c(rep(20, ngroups-1), 15)
# lines
for (i in 1:ngroups) {
  age <- subset(b, factors==i)
  lines(age$year, age$value, type="b", lwd=1.5,
    lty=linetype[i], col=colour[i], pch=plotchar[i])
}
}

a<-read.csv("who_incidence.csv")
yrange <- c(0, 60)
FUN.plot.inci (a)   
legend(max(b$year)+0.5, 60, unique(b$Age), cex=0.9, bty="n", col=colour,
   pch=plotchar, lty=linetype, title="Age Group")
title("Incidence rate by age group, per 1,000 person-years  (WHO criteria)")

a<-read.csv("ada_incidence.csv")
yrange <- c(0, 70)
FUN.plot.inci (a)   
legend(max(b$year)+0.5, 70, unique(b$Age), cex=0.9, bty="n", col=colour,
   pch=plotchar, lty=linetype, title="Age Group")
title("Incidence rate by age group, per 1,000 person-years  (ADA criteria)")

a<-read.csv("hku_incidence.csv")
yrange <- c(0, 40)
FUN.plot.inci (a)   
legend(max(b$year)+0.5, 40, unique(b$Age), cex=0.9, bty="n", col=colour,
   pch=plotchar, lty=linetype, title="Age Group")  
title("Incidence rate by age group, per 1,000 person-years  (HKU criteria)")

#############################################################
# Prevalence & Incidence
#############################################################
'ADJUSTED FOR NO F/U & PRIVATE PATIENTS'
'Adjust for No follow-up or Private-use only'
adj.male = c(rep(0.8656706886, 3),
	0.6545198739,0.7391541486,0.8468177181,0.8523638329,
	0.8247139608,0.7985174687,0.8475138448,
	rep(0.9362282433, 4), 0.8656706886)
adj.female<-c(rep(0.8656706886, 3),	
	0.7659163219,0.8780285920,0.8043123587,0.8435265819,
	0.7992926191,0.8808036197,0.9037039973,
	rep(0.9109939244, 4), 0.8817769173)
adj.all<-c(rep(0.8656706886, 3), 0.7164816741, 0.8100480178,
	0.8300964211, 0.8480860118, 0.8139970040, 0.8393953090,
	0.8730333195, rep(0.9220783297,4), 0.8737944694)	
adj=matrix(c(adj.male, adj.female, adj.all), byrow=F, nrow=15, ncol=3)	
adj=as.vector(t(adj))	

setwd("C:/Users/qchao/Dropbox (Personal)/+ Diabetes/Valuation v6/epi unadjusted")
library(gplots)

'PREVALENCE'
a<-read.csv("hku_prevalence.csv")
a[,-1]<-apply(a[,-1], 2, function (x) as.numeric(substr(x, 1, nchar(x)-1)))
a
bar<-matrix(a$X2013, ncol=3)
b<-as.vector(t(bar))

bar = data.frame(public=b, non_public=b/adj-b)
bar$undiagnosed_us=(bar$public + bar$non_public)/(1-0.278)-(bar$public + bar$non_public)
bar$undiagnosed_hk=(bar$public + bar$non_public)/(1-0.644)-(bar$public+bar$non_public)

# US = 27.8%
i = seq(3,45,3); overall = bar[i,]
colour<-c("lightblue","orange","lightgrey","white")
barplot2(t(bar[i,1:3]), beside=F, names.arg=(a$Age[1:15]), las=1,
col=colour, ylab="%", xlab="Age")
legend("topleft", c("Public","Non-public","Undiagnosed = 27.8%"), cex=1.2, fill=colour, bty="n")
title (main="Prevalence in 2013, %  (HKU criteria)", cex=1.4)
sum(bar[45,1:3])
text(3, 25, "Unadjusted = 8.2% \n Adjusted = 12.0%", cex=1.3)	

'INCIDENCE'
a<-read.csv("hku_incidence.csv")
a
bar<-matrix(a$X2013, ncol=3)
b<-as.vector(t(bar))
bar = data.frame(public=b, non_public=b/adj-b)
bar$undiagnosed_us=(bar$public + bar$non_public)/(1-0.278)-(bar$public + bar$non_public)
bar$undiagnosed_hk=(bar$public + bar$non_public)/(1-0.644)-(bar$public+bar$non_public)

# US = 27.8%
i = seq(3,45,3); overall = bar[i,]
colour<-c("lightblue","orange","lightgrey","white")
barplot2(t(bar[i,1:3]), beside=F, names.arg=(a$Age[1:15]), las=1,
	col=colour, ylab="per 1,000 person-years", xlab="Age",
	ylim=c(0,50))
legend("topleft", c("Public","Non-public","Undiagnosed (US) = 27.8%"), cex=1.2, fill=colour, bty="n")
title (main="Incidence in 2013, per 1,000 person-years  (HKU criteria)", cex=1.4)
sum(bar[45,1:3])
text(3, 25, "Overall = 10.8 \n Adjusted = 12.4", cex=1.3)	


