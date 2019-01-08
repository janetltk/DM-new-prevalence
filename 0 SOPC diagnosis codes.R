
# SOPC data:redundency among diagnosis codes
########################################################
setwd("C:/Users/janet/Desktop/2017_data")
sopc <- readRDS("sop_attn.rds")
names(sopc)
columns <- c("serial_no", "adate", paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:29))
d <- sopc[, columns]
rm(sopc); gc()

head(d)
colSums(is.na(d)) # most diagnosis codes are empty
# find completely empty rows
d$icd <- rowSums(d[, c(paste0("diag_cd_0", 1:9), paste0("diag_cd_", 10:29))], na.rm = T)
table(d$icd==0)
head(d[d$icd!=0,]) # check
d[!is.na(d$diag_cd_01) & d$icd==0,] # check
d[!is.na(d$diag_cd_02) & d$icd==0,] # check

d <- d[d$icd!=0,] 
d$icd <- NULL
format(object.size(d), "auto")

sopc_dx_codes <- d
saveRDS(sopc_dx_codes, file = "sorted/sopc_dx_codes.rds")

