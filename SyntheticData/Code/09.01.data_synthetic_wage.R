setwd("~/Path/SyntheticData")
rm(list=ls())
library(parallel)

load("Synthetic_Files/bs_synthetic_bargunit.RData")

f<-function(J1)
{
  
  load(paste("Synthetic_Files/data_synthetic_bargunit_year_",J1,".RData",sep=""))
  
  data_synthetic_wage  = data.frame(data_synthetic_bargunit, bargunit = bs_synthetic_bargunit[,J1])
  
  print(J1)
  save(data_synthetic_wage, file = paste("Synthetic_Files/data_synthetic_wage_year_",J1,".RData",sep=""))
  
}

mclapply(1:24,f,mc.cores=24)

