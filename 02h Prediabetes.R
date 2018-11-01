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
agecat = c(20, 40, 60, 80)

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

############################################################
# Exclude prevalent cases of diabetes from incidence denominator (person-time at risk) for pre-diabetes
############################################################
d$dm.date <- d$hku
years <- 2006:2014

# Age strata labels
labs <- c(paste0(agecat[-length(agecat)],"-", (agecat[-1] - 1)), paste0(agecat[length(agecat)], "+"))

# Extract DM cases from all.dm data, and deaths
dm <- d[!is.na(d$dm.date), c("serial_no", "dm.date", "death.date", "dob", "female")]

# Format dates into numeric
dm[, c("dm.month", "death.month")] <- sapply(dm[, c("dm.date", "death.date")], function(x) as.numeric(format(x, "%m")))
dm[, c("dm.yr", "death.yr")] <- sapply(dm[, c("dm.date", "death.date")], function(x) as.numeric(format(x, "%Y")))

all.dm <- dm
table(all.dm$female)
male <- dm[dm$female == FALSE, ]
female <- dm[dm$female == TRUE, ]

# Population
# Male Population aged 20 and above by AGE groups (Census dept data)
population.male <- read.csv("source/population_male.csv", row.names = 1)
population.male <- population.male[rownames(population.male) != "Total", ]

# Female Population aged 20 and above by AGE groups (Census dept data)
population.female <- read.csv("source/population_female.csv", row.names = 1)
population.female <- population.female[rownames(population.female) != "Total", ]

# Match SPECIFIED age strata to census population 5-year age rows
FunAggregateAgeGroups <- function (population) {
    # Aggregate population age strata into SPECIFIED age strata
    a <- population[1:length(agecat), ]
    row.names(a) <- labs
    row.start <- rep(NA, length(agecat))
        for (i in 1:length(agecat)) {
            row.start[i] <- grep(agecat[i], row.names(population))
         }
    row.start
    row.end <- c(row.start[2:length(agecat)] - 1, nrow(population))
    a[,] <- 0
        for (j in 1:length(agecat)) {
        a[j, ] <- colSums(population[row.start[j]:row.end[j], ])
        }
    stopifnot(round(colSums(a), 2) == round(colSums(population), 2))
    # print(rbind(a, Total = colSums(a)))
    a
}
pop.male <- FunAggregateAgeGroups (population.male)
pop.female <- FunAggregateAgeGroups (population.female)
stopifnot(pop.female != pop.male)

# Cut age into age categories (5 - year strata)
FunAgeCat <- function (x, lower = 20, upper = 85, by = 5, sep = "-", above.char = "+") {
    labs <- c(paste(seq(lower, upper - by, by = by),
      seq(lower + by - 1, upper - 1, by = by), sep = sep), paste(upper, above.char, sep = ""))
    cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf), right = FALSE, labels = labs)
}

# Prevalence of diabetes
# existing cases on midpoint of year
timepoint <- as.Date(paste0(years, "-07-01"))

FunPrevCases <- function (x, adjust, year = years) {
    p <- list(NA)
    for(i in 1:length(year)) {
        dob <- x[x$dm.date < timepoint[i] & (x$death.date > timepoint[i] | is.na(x$death.date)), c("dob")]
        age <- year[i] - dob
        p[[i]] <- FunAgeCat(age)
        names(p)[i] <- year[i]
    }
    p <- sapply(p, table)
    #  Adjust for non-public patients by 5-year age strata
    p <- ceiling(p / adjust)
    stopifnot (row.names(population.male) == row.names(p))
    # Aggregate 5-year strat in SPECIFIED age strata
    FunAggregateAgeGroups (data.frame(p)) # data.frame - colSums  needs dim(2)
}

# standard population is pop_mid2014
# direct standardisation - adjust for age
FunAgeAdjust <- function (cases, population) {
    count <- as.matrix(cases)
    pop <- as.matrix(population[, grep("pop_mid", names(population))])
    standard <- pop[, c("pop_mid2014")]
    mylist <- vector("list", ncol(count))
        for (i in 1:ncol(count)) {
        mylist[[i]] <- c(years[i], ageadjust.direct(count[,i], pop[,i], stdpop=standard))
        }
    p <- data.frame(do.call(rbind, mylist))
    names(p)[1] <- "year"
    p[, 2:5]
    # print(p)
    p
}

dm.cases.male <- FunPrevCases(male, adj.male)
dm.cases.female <- FunPrevCases(female, adj.female)
rm(list = setdiff(ls(), c("dm.cases.male", "dm.cases.female", "agecat", "adj.male", "adj.female", "prediabetes20")))

############################################################
# Age 20+ and All DM
############################################################
# pre-diabetes
d <- prediabetes20

# remove from prediabetes cases if dead or develop dm
d$death.date <- d$censordate
d$death.date <- with(d, pmin(hku, death.date, na.rm = T)) # censor date

# source code
code <- "dm_codes/A_run_incidence_prediabetes.R"
# incidence: population at risk excludes existing diabetes
# windows(record=TRUE)

sapply(d[, c("prediabetes", "hba1c.date", "fasting.date", "ogtt.date")], function (x) table(format(na.omit(x), "%Y")))

Name <- "Prediabetes"
d$dm.date <- d$prediabetes
source(code)
prediabetes <- list(prev, inci)

Name <- "HbA1c"
d$dm.date <- d$hba1c.date
source(code)
hba1c <- list(prev, inci)

Name <- "IFG"
d$dm.date <- d$fasting.date
source(code)
fasting <- list(prev, inci)

Name <- "IGT"
d$dm.date <- d$ogtt.date
source(code)
ogtt <- list(prev, inci)

save (list=ls(), file = "diagnosis/prediabetes_results_2016_Aug.Rdata")

# Results
############################################################
load("diagnosis/results_2016_Aug.Rdata")
rm(list=setdiff(ls(), "HKU"))
load("diagnosis/prediabetes_results_2016_Aug.Rdata")
setwd("/home/chao/Dropbox/Diabetes/Results/")

# reviewer 2 stat comment: combine -> dm or prediabetes
i <- HKU$serial_no %in% Prediabetes$serial_no
d <- HKU[!i, ]
stopifnot((d$serial_no %in% Prediabetes$serial_no) == F)
stopifnot(names(Prediabetes) == names(d))
Prediabetes_dm <- rbind(Prediabetes, d)

# table 1
groups <- list(Prediabetes, Prediabetes_dm, HbA1c, IFG, IGT)
names(groups) <- c("Prediabetes", "Prediabetes or DM", "HbA1c", "Impaired fasting glucose", "Impaired glucose tolerance")

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
                                                    times=c(length(age[[1]]), length(age[[2]]), length(age[[3]]), length(age[[4]]), length(age[[5]])))))
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
write.csv(tab1, "tab1_prediabetes.csv")

# table 4 for overall, male and female + SI (age groups)
inci.Prediabetes
a <- cbind(inci.Prediabetes[, c(colnames(inci.Prediabetes)[grep("adj.rate", colnames(inci.Prediabetes))], "pois.quad", "RR")])
b <- cbind(inci.Prediabetes[, c(colnames(inci.Prediabetes)[grep("95% CI", colnames(inci.Prediabetes))[1:8]], "pois.quad")])
b <- cbind(b, inci.Prediabetes[, ncol(inci.Prediabetes)])
colnames(b) <- colnames(a)
r <- list(NULL)
for (i in 1:nrow(inci.Prediabetes)) {
   r1 <- rbind(a[i,], b[i,])
   r <- rbind(r, r1)
}
tab3i <- r
write.csv(tab3i, "tab3_i.csv")

prev.Prediabetes
a <- cbind(prev.Prediabetes[, c(colnames(prev.Prediabetes)[grep("adj.rate", colnames(prev.Prediabetes))], "log.quad", "RR")])
b <- cbind(prev.Prediabetes[, c(colnames(prev.Prediabetes)[grep("95% CI", colnames(prev.Prediabetes))[1:9]], "log.quad")])
b <- cbind(b, prev.Prediabetes[, ncol(prev.Prediabetes)])
colnames(b) <- colnames(a)
r <- list(NULL)
for (i in 1:nrow(prev.Prediabetes)) {
   r1 <- rbind(a[i,], b[i,])
   r <- rbind(r, r1)
}
tab3p <- r
write.csv(tab3p, "tab3_p.csv")

# table - overall incidence
tab <- rbind(inci.Prediabetes, inci.HbA1c, inci.IFG, inci.IGT)
tab.si <- tab[grep("overall", row.names(tab)), ]
# write.csv(t(tab.si), "tab_prediabetes.csv")

# table incidence
# write.csv(t(tab), "tab_prediabetes.csv")
t(tab[tab$id == "Prediabetes", ])

# table - overall prevalence
tab <- rbind(prev.Prediabetes, prev.HbA1c, prev.IFG, prev.IGT)
tab.si <- tab[grep("overall", row.names(tab)), ]
# write.csv(t(tab.si), "tab_prediabetes.csv")

# table prevalence
# write.csv(t(tab), "tab_prediabetes.csv")
t(tab[tab$id == "Prediabetes", ])

# multiplot
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
