setwd("~/Path/SyntheticData")

library(parallel)
library(tree)

source("Code/99.02.cartdraw.R")
source("Code/99.03.cartdraw2.R")


load("Aux_Files/bs_instrctpgm.RData")
load("Aux_Files/bs_occ_cat.RData")
load("Aux_Files/bs_funcclas.RData")
load("Aux_Files/bs_flsa.RData")
load("Aux_Files/bs_appttype.RData")
load("Aux_Files/bs_polappttype.RData")
load("Aux_Files/bs_position.RData")
load("Aux_Files/bs_tenure.RData")
load("Aux_Files/bs_svsrstat.RData")

load("Synthetic_Files/bs_synthetic_agency.RData")
bs_synthetic_instrctpgm = bs_synthetic_agency
bs_synthetic_occ_cat = bs_synthetic_agency
bs_synthetic_funcclas = bs_synthetic_agency
bs_synthetic_flsa = bs_synthetic_agency
bs_synthetic_appttype = bs_synthetic_agency
bs_synthetic_polappttype = bs_synthetic_agency
bs_synthetic_position = bs_synthetic_agency
bs_synthetic_tenure = bs_synthetic_agency
bs_synthetic_svsrstat = bs_synthetic_agency

variables_instrctpgm.0 = c("agency", "gender", "race", "educ_lvl", "age", "init_yrsdegrng", "milmonths",
                           "total_year", "number_agency",  "M" , "occ", "collapse")
variables_occ_cat.0     = c(variables_instrctpgm.0, "instrctpgm")
variables_funcclas.0    = c(variables_occ_cat.0, "occ_cat")
variables_flsa.0        = c(variables_funcclas.0, "funcclas")
variables_appttype.0    = c(variables_flsa.0, "flsa")
variables_polappttype.0 = c(variables_appttype.0, "appttype")
variables_position.0    = c(variables_polappttype.0, "polappttype")
variables_tenure.0      = c(variables_position.0, "position")
variables_svsrstat.0    = c(variables_tenure.0, "tenure")


variables_instrctpgm.1  = c(variables_instrctpgm.0, "last.instrctpgm")
variables_occ_cat.1     = c(variables_occ_cat.0, "last.occ_cat")
variables_funcclas.1    = c(variables_funcclas.0, "last.funcclas")
variables_flsa.1        = c(variables_flsa.0, "last.flsa")
variables_appttype.1    = c(variables_appttype.0, "last.appttype")
variables_polappttype.1 = c(variables_polappttype.0, "last.polappttype")
variables_position.1    = c(variables_position.0, "last.position")
variables_tenure.1      = c(variables_tenure.0, "last.tenure")
variables_svsrstat.1    = c(variables_svsrstat.0, "last.svsrstat")



J1 = 1

load(paste("Aux_Files/data_other_variables_year_",J1,".RData",sep=""))
load(paste("Synthetic_Files/data_synthetic_other_variables_year_",J1,".RData",sep=""))


Ind_synthetic_other_variables = data_synthetic_other_variables$agency!=0
Ind_other_variables = data_other_variables$agency!=0

data_synthetic_other_variables = data_synthetic_other_variables[Ind_synthetic_other_variables,]
data_other_variables = data_other_variables[Ind_other_variables,]

###### Determining init_agerange

#### Collapsed variables -- used to split the data
#Previos race
mcl.collapse <- function(collapse)
  simplify2array(mclapply(1:nrow(collapse),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

collapse.1 = mcl.collapse(cbind(
  as.vector(data_other_variables$agency),
  as.vector(data_other_variables$occ)))

synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(data_synthetic_other_variables$agency),
  as.vector(data_synthetic_other_variables$occ)))

print(cbind(head(collapse.1,20),head(synthetic_collapse.1,20)))
print(mean(synthetic_collapse.1 %in% collapse.1))


collapse.2 = collapse.1
synthetic_collapse.2 = synthetic_collapse.1

print(cbind(head(collapse.2,20),head(synthetic_collapse.2,20)))
print(mean(synthetic_collapse.2 %in% collapse.2))

Collapse = cbind(
  as.vector(collapse.1),
  as.vector(collapse.2))

Synthetic_Collapse = cbind(
  as.vector(synthetic_collapse.1),
  as.vector(synthetic_collapse.2))

source("Code/07.02.07.collapsing.R")

################################
## instrctpgm
################################
variables = variables_instrctpgm.0
synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
Goal="instrctpgm"
source("Code/07.02.08.compiled.R")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("instrctpgm -- Year",J1))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(tail(sort(table(synthetic_variable))))
print(tail(sort(table(data_other_variables$instrctpgm))))
bs_synthetic_instrctpgm[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
data_synthetic_other_variables = data.frame(data_synthetic_other_variables, instrctpgm = synthetic_variable)
################################
## occ_cat
################################
variables = variables_occ_cat.0
synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
Goal="occ_cat"
source("Code/07.02.08.compiled.R")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("occ_cat -- Year",J1))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(tail(sort(table(synthetic_variable))))
print(tail(sort(table(data_other_variables$occ_cat))))
bs_synthetic_occ_cat[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
data_synthetic_other_variables = data.frame(data_synthetic_other_variables, occ_cat = synthetic_variable)
################################
## funcclas
################################
variables = variables_funcclas.0
synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
Goal="funcclas"
source("Code/07.02.08.compiled.R")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("funcclas -- Year",J1))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(tail(sort(table(synthetic_variable))))
print(tail(sort(table(data_other_variables$funcclas))))
bs_synthetic_funcclas[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
data_synthetic_other_variables = data.frame(data_synthetic_other_variables, funcclas = synthetic_variable)
################################
## flsa
################################
variables = variables_flsa.0
synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
Goal="flsa"
source("Code/07.02.08.compiled.R")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("flsa -- Year",J1))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(tail(sort(table(synthetic_variable))))
print(tail(sort(table(data_other_variables$flsa))))
bs_synthetic_flsa[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
data_synthetic_other_variables = data.frame(data_synthetic_other_variables, flsa = synthetic_variable)
################################
## appttype
################################
variables = variables_appttype.0
synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
Goal="appttype"
source("Code/07.02.08.compiled.R")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("appttype -- Year",J1))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(tail(sort(table(synthetic_variable))))
print(tail(sort(table(data_other_variables$appttype))))
bs_synthetic_appttype[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
data_synthetic_other_variables = data.frame(data_synthetic_other_variables, appttype = synthetic_variable)
################################
## polappttype
################################
variables = variables_polappttype.0
synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
Goal="polappttype"
source("Code/07.02.08.compiled.R")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("polappttype -- Year",J1))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(tail(sort(table(synthetic_variable))))
print(tail(sort(table(data_other_variables$polappttype))))
bs_synthetic_polappttype[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
data_synthetic_other_variables = data.frame(data_synthetic_other_variables, polappttype = synthetic_variable)
################################
## position
################################
variables = variables_position.0
synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
Goal="position"
source("Code/07.02.08.compiled.R")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("position -- Year",J1))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(tail(sort(table(synthetic_variable))))
print(tail(sort(table(data_other_variables$position))))
bs_synthetic_position[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
data_synthetic_other_variables = data.frame(data_synthetic_other_variables, position = synthetic_variable)
################################
## tenure
################################
variables = variables_tenure.0
synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
Goal="tenure"
source("Code/07.02.08.compiled.R")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("tenure -- Year",J1))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(tail(sort(table(synthetic_variable))))
print(tail(sort(table(data_other_variables$tenure))))
bs_synthetic_tenure[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
data_synthetic_other_variables = data.frame(data_synthetic_other_variables, tenure = synthetic_variable)
################################
## svsrstat
################################
variables = variables_svsrstat.0
synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
Goal="svsrstat"
source("Code/07.02.08.compiled.R")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("svsrstat -- Year",J1))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(tail(sort(table(synthetic_variable))))
print(tail(sort(table(data_other_variables$svsrstat))))
bs_synthetic_svsrstat[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
data_synthetic_other_variables = data.frame(data_synthetic_other_variables, svsrstat = synthetic_variable)


#####################################################
#####################################################
##########################
## Other Years
##########################

for(J1 in 2:24)
{
  
  load(paste("Aux_Files/data_other_variables_year_",J1,".RData",sep=""))
  load(paste("Synthetic_Files/data_synthetic_other_variables_year_",J1,".RData",sep=""))
  
  Ind_synthetic_other_variables = data_synthetic_other_variables$agency!=0
  Ind_other_variables = data_other_variables$agency!=0
  
  data_synthetic_other_variables = data_synthetic_other_variables[Ind_synthetic_other_variables,]
  data_other_variables = data_other_variables[Ind_other_variables,]
  
  
  f<-function(j,bs,Lag)
  {
    tmp = "0"
    if(sum(bs[j,1:(J1-1)]!="0")>0)
      tmp = tail(bs[j,1:(J1-1)][bs[j,1:(J1-1)]!="0"],5)[Lag]
    if(is.na(tmp))
      tmp = "Empty"
    tmp
  }
  
  
  bs = bs_instrctpgm[Ind_other_variables,]
  last.instrctpgm = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_other_variables = data.frame(data_other_variables, last.instrctpgm = last.instrctpgm)
  
  bs = bs_occ_cat[Ind_other_variables,]
  last.occ_cat = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_other_variables = data.frame(data_other_variables,last.occ_cat =last.occ_cat)
  
  bs = bs_funcclas[Ind_other_variables,]
  last.funcclas = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_other_variables = data.frame(data_other_variables,last.funcclas =last.funcclas)
  
  bs = bs_flsa[Ind_other_variables,]
  last.flsa = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_other_variables = data.frame(data_other_variables,last.flsa =last.flsa)
  
  bs = bs_appttype[Ind_other_variables,]
  last.appttype = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_other_variables = data.frame(data_other_variables,last.appttype = last.appttype)
  
  bs = bs_polappttype[Ind_other_variables,]
  last.polappttype = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_other_variables = data.frame(data_other_variables,last.polappttype =last.polappttype)
  
  bs = bs_position[Ind_other_variables,]
  last.position = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_other_variables = data.frame(data_other_variables,last.position =last.position)
  
  bs = bs_tenure[Ind_other_variables,]
  last.tenure = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_other_variables = data.frame(data_other_variables,last.tenure =last.tenure)
  
  bs = bs_svsrstat[Ind_other_variables,]
  last.svsrstat = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_other_variables = data.frame(data_other_variables,last.svsrstat = last.svsrstat)
  
  
  
  bs = bs_synthetic_instrctpgm[Ind_synthetic_other_variables,]
  last.instrctpgm = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables, last.instrctpgm = last.instrctpgm)
  
  bs = bs_synthetic_occ_cat[Ind_synthetic_other_variables,]
  last.occ_cat = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables,last.occ_cat =last.occ_cat)
  
  bs = bs_synthetic_funcclas[Ind_synthetic_other_variables,]
  last.funcclas = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables,last.funcclas =last.funcclas)
  
  bs = bs_synthetic_flsa[Ind_synthetic_other_variables,]
  last.flsa = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables,last.flsa =last.flsa)
  
  bs = bs_synthetic_appttype[Ind_synthetic_other_variables,]
  last.appttype = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables,last.appttype = last.appttype)
  
  bs = bs_synthetic_polappttype[Ind_synthetic_other_variables,]
  last.polappttype = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables,last.polappttype =last.polappttype)
  
  bs = bs_synthetic_position[Ind_synthetic_other_variables,]
  last.position = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables,last.position =last.position)
  
  bs = bs_synthetic_tenure[Ind_synthetic_other_variables,]
  last.tenure = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables,last.tenure =last.tenure)
  
  bs = bs_synthetic_svsrstat[Ind_synthetic_other_variables,]
  last.svsrstat = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables,last.svsrstat = last.svsrstat)
  
  

  ################################
  ## instrctpgm
  ################################
  collapse.1.instrctpgm = mcl.collapse(cbind(
    as.vector(data_other_variables$agency),
    as.vector(data_other_variables$occ),
    as.vector(data_other_variables$last.instrctpgm)))
  synthetic_collapse.1.instrctpgm = mcl.collapse(cbind(
    as.vector(data_synthetic_other_variables$agency),
    as.vector(data_synthetic_other_variables$occ),
    as.vector(data_synthetic_other_variables$last.instrctpgm)))
  print(cbind(head(collapse.1.instrctpgm,20),head(synthetic_collapse.1.instrctpgm,20)))
  print(mean(synthetic_collapse.1.instrctpgm %in% collapse.1.instrctpgm))
  ################################
  ## occ_cat
  ################################
  collapse.1.occ_cat = mcl.collapse(cbind(
    as.vector(data_other_variables$agency),
    as.vector(data_other_variables$occ),
    as.vector(data_other_variables$last.occ_cat)))
  synthetic_collapse.1.occ_cat = mcl.collapse(cbind(
    as.vector(data_synthetic_other_variables$agency),
    as.vector(data_synthetic_other_variables$occ),
    as.vector(data_synthetic_other_variables$last.occ_cat)))
  print(cbind(head(collapse.1.occ_cat,20),head(synthetic_collapse.1.occ_cat,20)))
  print(mean(synthetic_collapse.1.occ_cat %in% collapse.1.occ_cat))
  ################################
  ## funcclas
  ################################
  collapse.1.funcclas = mcl.collapse(cbind(
    as.vector(data_other_variables$agency),
    as.vector(data_other_variables$occ),
    as.vector(data_other_variables$last.funcclas)))
  synthetic_collapse.1.funcclas = mcl.collapse(cbind(
    as.vector(data_synthetic_other_variables$agency),
    as.vector(data_synthetic_other_variables$occ),
    as.vector(data_synthetic_other_variables$last.funcclas)))
  print(cbind(head(collapse.1.funcclas,20),head(synthetic_collapse.1.funcclas,20)))
  print(mean(synthetic_collapse.1.funcclas %in% collapse.1.funcclas))
  ################################
  ## flsa
  ################################
  collapse.1.flsa = mcl.collapse(cbind(
    as.vector(data_other_variables$agency),
    as.vector(data_other_variables$occ),
    as.vector(data_other_variables$last.flsa)))
  synthetic_collapse.1.flsa = mcl.collapse(cbind(
    as.vector(data_synthetic_other_variables$agency),
    as.vector(data_synthetic_other_variables$occ),
    as.vector(data_synthetic_other_variables$last.flsa)))
  print(cbind(head(collapse.1.flsa,20),head(synthetic_collapse.1.flsa,20)))
  print(mean(synthetic_collapse.1.flsa %in% collapse.1.flsa))
  ################################
  ## appttype
  ################################
  collapse.1.appttype = mcl.collapse(cbind(
    as.vector(data_other_variables$agency),
    as.vector(data_other_variables$occ),
    as.vector(data_other_variables$last.appttype)))
  synthetic_collapse.1.appttype = mcl.collapse(cbind(
    as.vector(data_synthetic_other_variables$agency),
    as.vector(data_synthetic_other_variables$occ),
    as.vector(data_synthetic_other_variables$last.appttype)))
  print(cbind(head(collapse.1.appttype,20),head(synthetic_collapse.1.appttype,20)))
  print(mean(synthetic_collapse.1.appttype %in% collapse.1.appttype))
  ################################
  ## polappttype
  ################################
  collapse.1.polappttype = mcl.collapse(cbind(
    as.vector(data_other_variables$agency),
    as.vector(data_other_variables$occ),
    as.vector(data_other_variables$last.polappttype)))
  synthetic_collapse.1.polappttype = mcl.collapse(cbind(
    as.vector(data_synthetic_other_variables$agency),
    as.vector(data_synthetic_other_variables$occ),
    as.vector(data_synthetic_other_variables$last.polappttype)))
  print(cbind(head(collapse.1.polappttype,20),head(synthetic_collapse.1.polappttype,20)))
  print(mean(synthetic_collapse.1.polappttype %in% collapse.1.polappttype))
  ################################
  ## position
  ################################
  collapse.1.position = mcl.collapse(cbind(
    as.vector(data_other_variables$agency),
    as.vector(data_other_variables$occ),
    as.vector(data_other_variables$last.position)))
  synthetic_collapse.1.position = mcl.collapse(cbind(
    as.vector(data_synthetic_other_variables$agency),
    as.vector(data_synthetic_other_variables$occ),
    as.vector(data_synthetic_other_variables$last.position)))
  print(cbind(head(collapse.1.position,20),head(synthetic_collapse.1.position,20)))
  print(mean(synthetic_collapse.1.position %in% collapse.1.position))
  ################################
  ## tenure
  ################################
  collapse.1.tenure = mcl.collapse(cbind(
    as.vector(data_other_variables$agency),
    as.vector(data_other_variables$occ),
    as.vector(data_other_variables$last.tenure)))
  synthetic_collapse.1.tenure = mcl.collapse(cbind(
    as.vector(data_synthetic_other_variables$agency),
    as.vector(data_synthetic_other_variables$occ),
    as.vector(data_synthetic_other_variables$last.tenure)))
  print(cbind(head(collapse.1.tenure,20),head(synthetic_collapse.1.tenure,20)))
  print(mean(synthetic_collapse.1.tenure %in% collapse.1.tenure))
  ################################
  ## svsrstat
  ################################
  collapse.1.svsrstat = mcl.collapse(cbind(
    as.vector(data_other_variables$agency),
    as.vector(data_other_variables$occ),
    as.vector(data_other_variables$last.svsrstat)))
  synthetic_collapse.1.svsrstat = mcl.collapse(cbind(
    as.vector(data_synthetic_other_variables$agency),
    as.vector(data_synthetic_other_variables$occ),
    as.vector(data_synthetic_other_variables$last.svsrstat)))
  print(cbind(head(collapse.1.svsrstat,20),head(synthetic_collapse.1.svsrstat,20)))
  print(mean(synthetic_collapse.1.svsrstat %in% collapse.1.svsrstat))
  
  
  collapse.2 = mcl.collapse(cbind(
    as.vector(data_other_variables$agency),
    as.vector(data_other_variables$occ)))
  
  synthetic_collapse.2 = mcl.collapse(cbind(
    as.vector(data_synthetic_other_variables$agency),
    as.vector(data_synthetic_other_variables$occ)))
  
  print(cbind(head(collapse.2,20),head(synthetic_collapse.2,20)))
  print(mean(synthetic_collapse.2 %in% collapse.2))
  
  
  ################################
  ## instrctpgm
  ################################
  Collapse = cbind(
    as.vector(collapse.1.instrctpgm),
    as.vector(collapse.2))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1.instrctpgm),
    as.vector(synthetic_collapse.2))
  source("Code/07.02.07.collapsing.R")
  variables = variables_instrctpgm.1
  synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
  Goal="instrctpgm"
  source("Code/07.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("instrctpgm -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_other_variables$instrctpgm))))
  bs_synthetic_instrctpgm[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables, instrctpgm = synthetic_variable)
  ################################
  ## occ_cat
  ################################
  Collapse = cbind(
    as.vector(collapse.1.occ_cat),
    as.vector(collapse.2))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1.occ_cat),
    as.vector(synthetic_collapse.2))
  source("Code/07.02.07.collapsing.R")
  variables = variables_occ_cat.1
  synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
  Goal="occ_cat"
  source("Code/07.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("occ_cat -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_other_variables$occ_cat))))
  bs_synthetic_occ_cat[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables, occ_cat = synthetic_variable)
  ################################
  ## funcclas
  ################################
  Collapse = cbind(
    as.vector(collapse.1.funcclas),
    as.vector(collapse.2))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1.funcclas),
    as.vector(synthetic_collapse.2))
  source("Code/07.02.07.collapsing.R")
  variables = variables_funcclas.1
  synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
  Goal="funcclas"
  source("Code/07.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("funcclas -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_other_variables$funcclas))))
  bs_synthetic_funcclas[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables, funcclas = synthetic_variable)
  ################################
  ## flsa
  ################################
  Collapse = cbind(
    as.vector(collapse.1.flsa),
    as.vector(collapse.2))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1.flsa),
    as.vector(synthetic_collapse.2))
  source("Code/07.02.07.collapsing.R")
  variables = variables_flsa.1
  synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
  Goal="flsa"
  source("Code/07.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("flsa -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_other_variables$flsa))))
  bs_synthetic_flsa[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables, flsa = synthetic_variable)
  ################################
  ## appttype
  ################################
  Collapse = cbind(
    as.vector(collapse.1.appttype),
    as.vector(collapse.2))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1.appttype),
    as.vector(synthetic_collapse.2))
  source("Code/07.02.07.collapsing.R")
  variables = variables_appttype.1
  synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
  Goal="appttype"
  source("Code/07.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("appttype -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_other_variables$appttype))))
  bs_synthetic_appttype[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables, appttype = synthetic_variable)
  ################################
  ## polappttype
  ################################
  Collapse = cbind(
    as.vector(collapse.1.polappttype),
    as.vector(collapse.2))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1.polappttype),
    as.vector(synthetic_collapse.2))
  source("Code/07.02.07.collapsing.R")
  variables = variables_polappttype.1
  synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
  Goal="polappttype"
  source("Code/07.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("polappttype -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_other_variables$polappttype))))
  bs_synthetic_polappttype[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables, polappttype = synthetic_variable)
  ################################
  ## position
  ################################
  Collapse = cbind(
    as.vector(collapse.1.position),
    as.vector(collapse.2))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1.position),
    as.vector(synthetic_collapse.2))
  source("Code/07.02.07.collapsing.R")
  variables = variables_position.1
  synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
  Goal="position"
  source("Code/07.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("position -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_other_variables$position))))
  bs_synthetic_position[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables, position = synthetic_variable)
  ################################
  ## tenure
  ################################
  Collapse = cbind(
    as.vector(collapse.1.tenure),
    as.vector(collapse.2))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1.tenure),
    as.vector(synthetic_collapse.2))
  source("Code/07.02.07.collapsing.R")
  variables = variables_tenure.1
  synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
  Goal="tenure"
  source("Code/07.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("tenure -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_other_variables$tenure))))
  bs_synthetic_tenure[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables, tenure = synthetic_variable)
  ################################
  ## svsrstat
  ################################
  Collapse = cbind(
    as.vector(collapse.1.svsrstat),
    as.vector(collapse.2))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1.svsrstat),
    as.vector(synthetic_collapse.2))
  source("Code/07.02.07.collapsing.R")
  variables = variables_svsrstat.1
  synthetic_variable = rep(0,nrow(data_synthetic_other_variables))
  Goal="svsrstat"
  source("Code/07.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("svsrstat -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_other_variables$svsrstat))))
  bs_synthetic_svsrstat[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  data_synthetic_other_variables = data.frame(data_synthetic_other_variables, svsrstat = synthetic_variable)
  
}

save(bs_synthetic_instrctpgm, file = "Synthetic_Files/bs_synthetic_instrctpgm.RData")
save(bs_synthetic_occ_cat, file = "Synthetic_Files/bs_synthetic_occ_cat.RData")
save(bs_synthetic_funcclas, file = "Synthetic_Files/bs_synthetic_funcclas.RData")
save(bs_synthetic_flsa, file = "Synthetic_Files/bs_synthetic_flsa.RData")
save(bs_synthetic_appttype, file = "Synthetic_Files/bs_synthetic_appttype.RData")
save(bs_synthetic_polappttype, file = "Synthetic_Files/bs_synthetic_polappttype.RData")
save(bs_synthetic_position, file = "Synthetic_Files/bs_synthetic_position.RData")
save(bs_synthetic_tenure, file = "Synthetic_Files/bs_synthetic_tenure.RData")
save(bs_synthetic_svsrstat, file = "Synthetic_Files/bs_synthetic_svsrstat.RData")

