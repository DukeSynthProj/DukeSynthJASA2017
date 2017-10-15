setwd("~/Path/SyntheticData")

load("Synthetic_Files/bs_synthetic_instrctpgm.RData")
for(j in 1:24)
{
  bs_synthetic_instrctpgm[is.na(bs_synthetic_instrctpgm[,j]),j]="NA.instrctpgm"
  bs_synthetic_instrctpgm[bs_synthetic_instrctpgm[,j]=="NA",j]="NA.instrctpgm"
}
load("Synthetic_Files/bs_synthetic_occ_cat.RData")
for(j in 1:24)
{
  bs_synthetic_occ_cat[is.na(bs_synthetic_occ_cat[,j]),j]="NA.occ_cat"
  bs_synthetic_occ_cat[bs_synthetic_occ_cat[,j]=="NA",j]="NA.occ_cat"
}
load("Synthetic_Files/bs_synthetic_funcclas.RData")
for(j in 1:24)
{
  bs_synthetic_funcclas[is.na(bs_synthetic_funcclas[,j]),j]="NA.funcclas"
  bs_synthetic_funcclas[bs_synthetic_funcclas[,j]=="NA",j]="NA.funcclas"
}
load("Synthetic_Files/bs_synthetic_flsa.RData")
for(j in 1:24)
{
  bs_synthetic_flsa[is.na(bs_synthetic_flsa[,j]),j]="NA.flsa"
  bs_synthetic_flsa[bs_synthetic_flsa[,j]=="NA",j]="NA.flsa"
}
load("Synthetic_Files/bs_synthetic_appttype.RData")
for(j in 1:24)
{
  bs_synthetic_appttype[is.na(bs_synthetic_appttype[,j]),j]="NA.appttype"
  bs_synthetic_appttype[bs_synthetic_appttype[,j]=="NA",j]="NA.appttype"
}
load("Synthetic_Files/bs_synthetic_polappttype.RData")
for(j in 1:24)
{
  bs_synthetic_polappttype[is.na(bs_synthetic_polappttype[,j]),j]="NA.polappttype"
  bs_synthetic_polappttype[bs_synthetic_polappttype[,j]=="NA",j]="NA.polappttype"
}
load("Synthetic_Files/bs_synthetic_position.RData")
for(j in 1:24)
{
  bs_synthetic_position[is.na(bs_synthetic_position[,j]),j]="NA.position"
  bs_synthetic_position[bs_synthetic_position[,j]=="NA",j]="NA.position"
}
load("Synthetic_Files/bs_synthetic_tenure.RData")
for(j in 1:24)
{
  bs_synthetic_tenure[is.na(bs_synthetic_tenure[,j]),j]="NA.tenure"
  bs_synthetic_tenure[bs_synthetic_tenure[,j]=="NA",j]="NA.tenure"
}
load("Synthetic_Files/bs_synthetic_svsrstat.RData")
for(j in 1:24)
{
  bs_synthetic_svsrstat[is.na(bs_synthetic_svsrstat[,j]),j]="NA.svsrstat"
  bs_synthetic_svsrstat[bs_synthetic_svsrstat[,j]=="NA",j]="NA.svsrstat"
}

library(parallel)

f<-function(J1)
{
  
  load(paste("Synthetic_Files/data_synthetic_other_variables_year_",J1,".RData",sep=""))
  
  data_synthetic_bargunit  = data.frame(data_synthetic_other_variables,
                                     instrctpgm = bs_synthetic_instrctpgm[,J1],
                                     occ_cat = bs_synthetic_occ_cat[,J1],
                                     funcclas = bs_synthetic_funcclas[,J1],
                                     flsa = bs_synthetic_flsa[,J1],
                                     appttype = bs_synthetic_appttype[,J1],
                                     polappttype = bs_synthetic_polappttype[,J1],
                                     position = bs_synthetic_position[,J1],
                                     tenure = bs_synthetic_tenure[,J1],
                                     svsrstat = bs_synthetic_svsrstat[,J1])
  
  print(J1)
  save(data_synthetic_bargunit, file = paste("Synthetic_Files/data_synthetic_bargunit_year_",J1,".RData",sep=""))
  
}
library(parallel)
mclapply(1:24,f,mc.cores=24)

