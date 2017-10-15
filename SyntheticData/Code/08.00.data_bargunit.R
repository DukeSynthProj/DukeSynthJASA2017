setwd("~/Path/SyntheticData")
rm(list=ls())
library(parallel)

load("Aux_Files/bs_bargunit.RData")
for(j in 1:24)
{
  bs_bargunit[is.na(bs_bargunit[,j]),j]="NA.bargunit"
  bs_bargunit[bs_bargunit[,j]=="NA",j]="NA.bargunit"
}

f<-function(J1)
{
  
  load(paste("Aux_Files/data_other_variables_year_",J1,".RData",sep=""))
  
  data_bargunit  = data.frame(data_other_variables, bargunit = bs_bargunit[,J1])
  
  print(J1)
  save(data_bargunit, file = paste("Aux_Files/data_bargunit_year_",J1,".RData",sep=""))
  
}

mclapply(1:24,f,mc.cores=24)

