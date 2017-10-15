setwd("~/Path/SyntheticData")
load("Synthetic_Files/data_synthetic_gender.RData")
load("Aux_Files/data_gender.RData")

library(parallel)
library(tree)

source("Code/99.02.cartdraw.R")
source("Code/99.03.cartdraw2.R")



#### Collapsed variables -- used to split the data
#Previos gender
collapse = cbind(
  as.vector(data_gender$init_year), 
  as.vector(data_gender$init_agency))
collapse = simplify2array(mclapply(1:nrow(data_gender),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

synthetic_collapse = cbind(
  as.vector(data_synthetic_gender$init_year), 
  as.vector(data_synthetic_gender$init_agency))
synthetic_collapse = simplify2array(mclapply(1:nrow(data_synthetic_gender),function(j){paste(synthetic_collapse[j,],collapse="-")},mc.cores=48))

print(cbind(head(collapse,20),head(synthetic_collapse,20)))
print(mean(synthetic_collapse %in% collapse))

data_gender = data.frame(data_gender,collapse=collapse)
data_synthetic_gender = data.frame(data_synthetic_gender,collapse=synthetic_collapse)

variables = c("init_year","last_year","total_year","mode_agency","init_agency",
              "number_agency","M","gaps","collapse")


synthetic_gender = matrix("0",nrow=nrow(data_synthetic_gender),ncol=1) 


data_gender_tmp = data.frame(data_gender)
data_synthetic_gender_tmp = data.frame(data_synthetic_gender)


Ind_collapse = data_gender_tmp$init_agency != "0"
Ind_synthetic_collapse = data_synthetic_gender_tmp$init_agency != "0"

## Start Algorithm
data_general=data_gender_tmp[Ind_collapse,,drop=FALSE]
data_synthetic_general=data_synthetic_gender_tmp[Ind_synthetic_collapse,,drop=FALSE]
Goal="gender"
variables=variables
N.samples=1


# Getting collapse for mono_table, mono_tree and multi_tree
cores.table=30
source("Code/02.03.01.Init_mono_multi.R")


# Matching with mono_table
pre.cores.table = 20
cores.table = 30
split.mono.table=15000
cores.mono.table=40
source("Code/02.03.03.mono_table.R")


# Fitting mono_tree
split.mono.tree=10000
cores.mono.tree=30
N.cartdraw = 10000
cores.cartdraw = 2
source("Code/02.03.04.mono_tree.R")


# End
synthetic_gender = synthetic_Goal
save(synthetic_gender,file=paste("Synthetic_Files/synthetic_gender.RData",sep=""))

load("Synthetic_Files/bs_synthetic_agency.RData")
bs_synthetic_gender = bs_synthetic_agency
for(J in 1:24)
bs_synthetic_gender[bs_synthetic_agency[,J]!=0,J] = synthetic_gender[bs_synthetic_agency[,J]!=0]

save(bs_synthetic_gender,file=paste("Synthetic_Files/bs_synthetic_gender.RData",sep=""))




