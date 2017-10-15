setwd("~/Path/SyntheticData")

library(parallel)
library(tree)

source("Code/99.02.cartdraw.R")
source("Code/99.03.cartdraw2.R")

load("Aux_Files/bs_bargunit.RData")
for(j in 1:24)
{
  bs_bargunit[is.na(bs_bargunit[,j]),j]="NA.bargunit"
  bs_bargunit[bs_bargunit[,j]=="NA",j]="NA.bargunit"
}

load("Synthetic_Files/bs_synthetic_agency.RData")
bs_synthetic_bargunit = bs_synthetic_agency


variables_bargunit.0 = c("agency", "gender", "race", "educ_lvl", "age", "init_yrsdegrng", "milmonths",
                           "total_year", "number_agency",  "M" , "occ", "instrctpgm", "occ_cat", 
                           "funcclas", "flsa", "appttype", "polappttype", "position", "tenure", 
                           "svsrstat",  "collapse")

variables_bargunit.1  = c(variables_bargunit.0, "last.bargunit")


J1 = 1

load(paste("Aux_Files/data_bargunit_year_",J1,".RData",sep=""))
load(paste("Synthetic_Files/data_synthetic_bargunit_year_",J1,".RData",sep=""))


Ind_synthetic_other_variables = data_synthetic_bargunit$agency!=0
Ind_other_variables = data_bargunit$agency!=0

data_synthetic_bargunit = data_synthetic_bargunit[Ind_synthetic_other_variables,]
data_bargunit = data_bargunit[Ind_other_variables,]

###### Determining init_agerange

#### Collapsed variables -- used to split the data
#Previos race
mcl.collapse <- function(collapse)
  simplify2array(mclapply(1:nrow(collapse),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

collapse.1 = mcl.collapse(cbind(
  as.vector(data_bargunit$agency),
  as.vector(data_bargunit$occ)))

synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(data_synthetic_bargunit$agency),
  as.vector(data_synthetic_bargunit$occ)))

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

source("Code/08.02.07.collapsing.R")

################################
## bargunit
################################
variables = variables_bargunit.0
synthetic_variable = rep(0,nrow(data_synthetic_bargunit))
Goal="bargunit"
source("Code/08.02.08.compiled.R")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("bargunit -- Year",J1))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(tail(sort(table(synthetic_variable))))
print(tail(sort(table(data_bargunit$bargunit))))
bs_synthetic_bargunit[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
if(sum((bs_synthetic_bargunit[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
{
  print("Error: More zeros in synthetic_bargunit")
  print(which(((bs_synthetic_bargunit[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
  break()
}
data_synthetic_bargunit = data.frame(data_synthetic_bargunit, bargunit = synthetic_variable)

#####################################################
#####################################################
##########################
## Other Years
##########################

for(J1 in 2:24)
{
  
  load(paste("Aux_Files/data_bargunit_year_",J1,".RData",sep=""))
  load(paste("Synthetic_Files/data_synthetic_bargunit_year_",J1,".RData",sep=""))
  
  Ind_synthetic_other_variables = data_synthetic_bargunit$agency!=0
  Ind_other_variables = data_bargunit$agency!=0
  
  data_synthetic_bargunit = data_synthetic_bargunit[Ind_synthetic_other_variables,]
  data_bargunit = data_bargunit[Ind_other_variables,]
  
  
  f<-function(j,bs,Lag)
  {
    tmp = "0"
    if(sum(bs[j,1:(J1-1)]!="0")>0)
      tmp = rev(tail(bs[j,1:(J1-1)][bs[j,1:(J1-1)]!="0"],5))[Lag]
    if(is.na(tmp))
      tmp = "Empty"
    #print(j)
    tmp
  }
  
  
  bs = bs_bargunit[Ind_other_variables,]
  last.bargunit = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_bargunit = data.frame(data_bargunit, last.bargunit = last.bargunit)
  
  
  
  bs = bs_synthetic_bargunit[Ind_synthetic_other_variables,]
  last.bargunit = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_bargunit = data.frame(data_synthetic_bargunit, last.bargunit = last.bargunit)
  

  ################################
  ## bargunit
  ################################
  collapse.1.bargunit = mcl.collapse(cbind(
    as.vector(data_bargunit$agency),
    as.vector(data_bargunit$occ),
    as.vector(data_bargunit$last.bargunit)))
  synthetic_collapse.1.bargunit = mcl.collapse(cbind(
    as.vector(data_synthetic_bargunit$agency),
    as.vector(data_synthetic_bargunit$occ),
    as.vector(data_synthetic_bargunit$last.bargunit)))
  print(cbind(head(collapse.1.bargunit,20),head(synthetic_collapse.1.bargunit,20)))
  print(mean(synthetic_collapse.1.bargunit %in% collapse.1.bargunit))
  
  collapse.2 = mcl.collapse(cbind(
  as.vector(data_bargunit$agency),
  as.vector(data_bargunit$occ)))

  synthetic_collapse.2 = mcl.collapse(cbind(
  as.vector(data_synthetic_bargunit$agency),
  as.vector(data_synthetic_bargunit$occ)))

  ################################
  ## bargunit
  ################################
  Collapse = cbind(
    as.vector(collapse.1.bargunit),
    as.vector(collapse.2))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1.bargunit),
    as.vector(synthetic_collapse.2))
  source("Code/08.02.07.collapsing.R")
  variables = variables_bargunit.1
  synthetic_variable = rep(0,nrow(data_synthetic_bargunit))
  Goal="bargunit"
  source("Code/08.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_bargunit$bargunit))))
  bs_synthetic_bargunit[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  if(sum((bs_synthetic_bargunit[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in synthetic_bargunit")
    print(which(((bs_synthetic_bargunit[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  data_synthetic_bargunit = data.frame(data_synthetic_bargunit, bargunit = synthetic_variable)
  
}

sum((bs_synthetic_bargunit=="0") - (bs_synthetic_agency == "0"))

save(bs_synthetic_bargunit, file = "Synthetic_Files/bs_synthetic_bargunit.RData")

