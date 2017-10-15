rm(list = ls())

library(parallel)
setwd("~/Path/SyntheticData")
load.aux.var<-c("bs_agency")
for(i1 in 1:length(load.aux.var)){load(paste("Aux_Files/",load.aux.var[i1],".RData",sep=""))}

# Moves
Z=bs_agency[,2:24]==bs_agency[,1:23]
tmp1 =  apply(Z,1,sum)
tmp2 = bs_agency[tmp1 == 23 ,]
Prob_Ag_24 = table(tmp2[,1])

save(Prob_Ag_24,file = "Aux_Files/Prob_Ag_24.RData")

