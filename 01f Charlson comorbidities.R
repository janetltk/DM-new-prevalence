# library(medicalrisk)
icd9cm_charlson_quan <- function (icd9) 
{
    data.frame(row.names = icd9, mi = grepl("^D(410|412)", icd9), 
        chf = grepl("^D(39891|402[019]1|404[019][13]|425[4-9]|428)", 
            icd9), perivasc = grepl("^D(0930|4373|44[01]|443[1-9]|4471|557[19]|V434)", 
            icd9), cvd = grepl("^D(36234|43[0-8])", icd9), dementia = grepl("^D(290|2941|3312)", 
            icd9), chrnlung = grepl("^D(416[89]|49|50[0-5]|5064|508[18])", 
            icd9), rheum = grepl("^D(4465|710[0-4]|714[0-2]|7148|725)", 
            icd9), ulcer = grepl("^D53[1-4]", icd9), liver = grepl("^D(070[23][23]|070[45]4|070[69]|57[01]|573[3489]|V427)", 
            icd9), dm = grepl("^D250[0-389]", icd9), dmcx = grepl("^D250[4-7]", 
            icd9), para = grepl("^D(3341|34[23]|344[0-69])", 
            icd9), renal = grepl("^D(403[019]1|404[019][23]|582|583[0-7]|58[56]|5880|V420|V451|V56)", 
            icd9), tumor = grepl("^D(1[4-6]|17[0-24-9]|18|19[0-5]|20[0-8]|2386)", 
            icd9), modliver = grepl("^D(456[0-2]|572[2-8])", 
            icd9), mets = grepl("^D19[6-9]", icd9), aids = grepl("^D04[2-4]", 
            icd9))
}

##
rm(list = ls()); gc()

run_dx <- "C:/Users/Chao/Documents/GitHub/dm-codes/1_run_dx_code_grepl.r"

# Charlson Comorbidities (Enhanced ICD-9-CM Quan)
########################################################
# r package medicalrisk
# (Quan) icd9cm_charlson_quan

# caret ^ is a metacharacter that match the empty string to the begining of a line

code_grepl <- "^(410|412)"
source(run_dx)
mi <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(mi, file = "Rdata/Charlson/mi.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(398.91|402.[019]1|404.[019][13]|425.[4-9]|428)"
source(run_dx)
chf <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(chf, file = "Rdata/Charlson/chf.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(93.0|437.3|44[01]|443[1-9]|447.1|557.[19]|9022.43.4)"   
source(run_dx)
perivasc <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(perivasc, file = "Rdata/Charlson/perivasc.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()    

code_grepl <- "^(362.34|43[0-8])"
source(run_dx)
cvd <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(cvd, file = "Rdata/Charlson/cvd.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(290|294.1|331.2)"
source(run_dx)
dementia <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(dementia, file = "Rdata/Charlson/dementia.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(416.[89]|49|50[0-5]|506.4|508.[18])"
source(run_dx)
chrnlung <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(chrnlung, file = "Rdata/Charlson/chrnlung.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(446.5|710.[0-4]|714.[0-2]|714.8|725)"
source(run_dx)
rheum <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(rheum, file = "Rdata/Charlson/rheum.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(53[1-4])"
source(run_dx)
ulcer <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(ulcer, file = "Rdata/Charlson/ulcer.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(70.[23][23]|70.[45]4|70.[69]|57[01]|573.[3489]|9022.42.7)"
source(run_dx)
liver <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(liver, file = "Rdata/Charlson/liver.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(250.[0-389])"
source(run_dx)
dm <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(dm, file = "Rdata/Charlson/dm.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^((250[4-7]))"     
source(run_dx)
dmcx <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(dmcx, file = "Rdata/Charlson/dmcx.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()    
 
code_grepl <- "^(334.1|34[23]|344.[0-69])"
source(run_dx)
para <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(para, file = "Rdata/Charlson/para.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(403.[019]1|404.[019][23]|582|583.[0-7]|58[56]|588.0|9022.42.0|9022.45.1|9022.56)"
source(run_dx)
renal <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(renal, file = "Rdata/Charlson/renal.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(1[4-6]|17[0-24-9]|18|19[0-5]|20[0-8]|238.6)"
source(run_dx)
tumor <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(tumor, file = "Rdata/Charlson/tumor.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(456.[0-2]|572.[2-8])"            
source(run_dx)
modliver <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(modliver, file = "Rdata/Charlson/modliver.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(19[6-9])"
source(run_dx)
mets <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(mets, file = "Rdata/Charlson/mets.Rdata")
rm(list = setdiff(ls(), "run_dx"))
gc()

code_grepl <- "^(4[2-4])"
source(run_dx)
aids <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(aids, file = "Rdata/Charlson/aids.Rdata")
