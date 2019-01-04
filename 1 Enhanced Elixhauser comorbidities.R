# Elixhauser's Enhanced ICD-9-CM (Quan et al. 2005)
########################################################
# Table 2 (row 3): ICD-9-CM and ICD-10 Coding Algorithms for Elixhauser Comorbidities

icd9cm_elixhauser_quan <- function (icd9){
    data.frame(row.names = icd9, 
            chf = grepl("^D(39891|402[019]1|404[019][13]|425[4-9]|428)", icd9), 
            arrhythmia = grepl("^D(426([079]|13)|426(10|12)|427[0-46-9]|7850|9960[14]|V450|V533)", icd9), 
            valve = grepl("^D(0932|39[4567]|424|746[3-6]|V422|V433)", icd9), 
            pulmcirc = grepl("^D(415[01]|416|417[089])", icd9), 
            perivasc = grepl("^D(0930|4373|44[01]|443[1-9]|4471|557[19]|V434)", icd9), 
            htn = grepl("^D401", icd9), 
            htncx = grepl("^D40[2-5]", icd9), para = grepl("^D(3341|34[23]|344[0-69])", icd9), 
            neuro = grepl("^D(3319|332[01]|333[45]|33392|33[45]|3362|34[015]|348[13]|7803|7843)", icd9), 
            chrnlung = grepl("^D(416[89]|49|50[0-5]|5064|508[18])", icd9), 
            dm = grepl("^D250[0-3]", icd9), 
            dmcx = grepl("^D250[4-9]", icd9), 
            hypothy = grepl("^D(2409|24[34]|246[18])", icd9), 
            renlfail = grepl("^D(403[019]1|404[019][23]|58[56]|5880|V420|V451|V56)", icd9), 
            liver = grepl("^D(070[23][23]|070[45]4|070[69]|456[0-2]|57[01]|572[2-8]|573[3489]|V427)", icd9), 
            ulcer = grepl("^D53[1-4][79]", icd9), 
            aids = grepl("^D04[2-4]", icd9), 
            lymph = grepl("^D(20[0-2]|2030|2386)", icd9), 
            mets = grepl("^D19[6-9]", icd9), tumor = grepl("^D(1[4-6]|17[0-24-9]|18|19[0-5])", icd9), 
            rheum = grepl("^D(446|7010|710[0-489]|7112|714|7193|72[05]|7285|72889|72930)", icd9), 
            coag = grepl("^D(286|287[13-5])", icd9), obese = grepl("^D2780", icd9),
            wghtloss = grepl("^D(26[0-3]|7832|7994)", icd9), 
            lytes = grepl("^D(2536|276)", icd9), 
            bldloss = grepl("^D2800", icd9), 
            anemdef = grepl("^D(280[1-9]|281)", icd9), 
            alcohol = grepl("^D(2652|291[1-35-9]|303[09]|3050|3575|4255|5353|571[0-3]|980|V113)", icd9), 
            drug = grepl("^D(292|304|305[2-9]|V6542)", icd9), 
            psych = grepl("^D(2938|295|296[0145]4|29[78])", icd9), 
            depress = grepl("^D(296[235]|3004|309|311)", icd9))
      }


##
rm(list = ls()); gc()
setwd("/Volumes/secure")
run_dx <- "/Users/chao/Documents/GitHub/dm-codes/01 Clean data/1_run_dx_code_grepl.r"


# Elixhauser's Comorbidities (Enhanced ICD-9-CM Quan)
########################################################
# r package medicalrisk
# (Quan) icd9cm_elixhauser_quan

# caret ^ is a metacharacter that match the empty string to the begining of a line

code_grepl <- "^(398.91|402.[019]1|404.[019][13]|425.[4-9]|428)"
source(run_dx)
chf <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(chf, file = "Rdata/Elixhauser (enhanced)/chf.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(426.([079]|13)|426.(10|12)|427.[0-46-9]|785|996.0[14]|9022.45|9022.533)"
source(run_dx)
arrhythmia <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(arrhythmia, file = "Rdata/Elixhauser (enhanced)/arrhythmia.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(93.2|39[4567]|424|746.[3-6]|9022.422|9022.433)"
source(run_dx)
valve <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(valve, file = "Rdata/Elixhauser (enhanced)/valve.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(415.[01]|416|417.[089])"
source(run_dx)
pulmcirc <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(pulmcirc, file = "Rdata/Elixhauser (enhanced)/pulmcirc.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(93$|93.|437.3|44[01]|443.[1-9]|447.1|557.[19]|9022.434)" # "93" or "93."
source(run_dx)
perivasc <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(perivasc, file = "Rdata/Elixhauser (enhanced)/perivasc.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()
      
code_grepl <- "^(401)"
source(run_dx)
htn <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(htn, file = "Rdata/Elixhauser (enhanced)/htn.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(40[2-5])"
source(run_dx)
htncx <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(htncx, file = "Rdata/Elixhauser (enhanced)/htncx.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()
            
code_grepl <- "^(334.1|34[23]|344.[0-69])"
source(run_dx)
para <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(para, file = "Rdata/Elixhauser (enhanced)/para.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()        

code_grepl <- "^(331.9|332.[01]|333.[45]|333.92|33[45]|336.2|34[015]|348.[13]|780.3|784.3)"
source(run_dx)
neuro <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(neuro, file = "Rdata/Elixhauser (enhanced)/neuro.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(416.[89]|49[0-6]|50[0-5]|506.4|508.[18])"
source(run_dx)
chrnlung <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(chrnlung, file = "Rdata/Elixhauser (enhanced)/chrnlung.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

# dm 
code_grepl <- "^(250.[0-3])"
# dmcx 
code_grepl <- "^(250.[4-9])"

code_grepl <- "^(240.9|24[34]|246.[18])"
source(run_dx)
hypothy <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(hypothy, file = "Rdata/Elixhauser (enhanced)/hypothy.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(403.[019]1|404.[019][23]|58[56]|588|9022.42|9022.451|9022.56)"
source(run_dx)
renlfail <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(renlfail, file = "Rdata/Elixhauser (enhanced)/renlfail.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()
            
code_grepl <- "^(70.[23][23]|70.[45]4|70.[69]|456.[0-2]|57[01]|572.[2-8]|573.[3489]|9022.427)"
source(run_dx)
liver <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(liver, file = "Rdata/Elixhauser (enhanced)/liver.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(53[1-4].[79])"
source(run_dx)
ulcer <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
# NIL 
save(ulcer, file = "Rdata/Elixhauser (enhanced)/ulcer.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(4[2-4])$" # ^$ need to include  codes in-between matches
# Hx of AIDS/HIV (ICD-9 code 0.42-0.45)
code1 <- 0.42
code2 <- 0.45
source("/Users/chao/Documents/GitHub/dm-codes/01 Clean data/1_run_dx_code.R")
aids <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
# NIL, peptic ulcer disease excluding bleeding
save(aids, file = "Rdata/Elixhauser (enhanced)/aids.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(20[0-2]|203|238.6)"
source(run_dx)
lymph <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(lymph, file = "Rdata/Elixhauser (enhanced)/lymph.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(19[6-9])"
source(run_dx)
mets <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(mets, file = "Rdata/Elixhauser (enhanced)/mets.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(1[4-6]|17[0-24-9]|18|19[0-5])"
source(run_dx)
tumor <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(tumor, file = "Rdata/Elixhauser (enhanced)/tumor.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(446|701|710.[0-489]|711.2|714|719.3|72[05]|728.5|728.89|729.30)"
source(run_dx)
rheum <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(rheum, file = "Rdata/Elixhauser (enhanced)/rheum.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()
            
code_grepl <- "^(286|287[13-5])"
source(run_dx)
coag <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(coag, file = "Rdata/Elixhauser (enhanced)/coag.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(278)"
source(run_dx)
obese <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(obese, file = "Rdata/Elixhauser (enhanced)/obese.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()            

code_grepl <- "^(26[0-3]|783.2|799.4)"
source(run_dx)
pulmcirc <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(wghtloss, file = "Rdata/Elixhauser (enhanced)/wghtloss.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()
            
code_grepl <- "^(253.6|276)"
source(run_dx)
lytes <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(lytes, file = "Rdata/Elixhauser (enhanced)/lytes.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()
            
code_grepl <- "^(280)"
source(run_dx)
bldloss <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(bldloss, file = "Rdata/Elixhauser (enhanced)/bldloss.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()
            
code_grepl <- "^(280.[1-9]|281)"
source(run_dx)
anemdef <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(anemdef, file = "Rdata/Elixhauser (enhanced)/anemdef.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()

code_grepl <- "^(265.2|291.[1-35-9]|303.[09]|305|357.5|425.5|535.3|571[0-3]|980|9022.113)"
source(run_dx)
alcohol <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(alcohol, file = "Rdata/Elixhauser (enhanced)/alcohol.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()        

code_grepl <- "^(292|304|305.[2-9]|9022.6542)"
source(run_dx)
drug <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(drug, file = "Rdata/Elixhauser (enhanced)/drug.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()
            
code_grepl <- "^(293.8|295|296.[0145]4|29[78])"
source(run_dx)
psych <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(psych, file = "Rdata/Elixhauser (enhanced)/psych.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()            

code_grepl <- "^(296.[235]|300.4|309|311)"
source(run_dx)
depress <- data.frame(d)
dim(sopc); dim(inpatient); dim(ae.attn); dim(ae.fu)
save(depress, file = "Rdata/Elixhauser (enhanced)/depress.Rdata")
rm(list = setdiff(ls(), "run_dx"));gc()            


