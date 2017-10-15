setwd("~/Path/SyntheticData")
load("Synthetic_Files/bs_synthetic_occ.RData")

library(parallel)



f<-function(J1)
{
  load(paste("Synthetic_Files/data_synthetic_occ_year_",J1,".RData",sep=""))
  data_synthetic_other_variables = data.frame(data_synthetic_occ, occ = bs_synthetic_occ[,J1])
  print(J1)
  save(data_synthetic_other_variables, file = paste("Synthetic_Files/data_synthetic_other_variables_year_",J1,".RData",sep=""))
  
}

mclapply(1:24,f,mc.cores=24)


