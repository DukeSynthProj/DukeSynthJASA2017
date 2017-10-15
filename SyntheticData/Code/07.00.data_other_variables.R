setwd("~/Path/SyntheticData")

load("Aux_Files/bs_instrctpgm.RData")
for(j in 1:24)
{
  bs_instrctpgm[is.na(bs_instrctpgm[,j]),j]="NA.instrctpgm"
  bs_instrctpgm[bs_instrctpgm[,j]=="NA",j]="NA.instrctpgm"
}
load("Aux_Files/bs_occ_cat.RData")
for(j in 1:24)
{
  bs_occ_cat[is.na(bs_occ_cat[,j]),j]="NA.occ_cat"
  bs_occ_cat[bs_occ_cat[,j]=="NA",j]="NA.occ_cat"
}
load("Aux_Files/bs_funcclas.RData")
for(j in 1:24)
{
  bs_funcclas[is.na(bs_funcclas[,j]),j]="NA.funcclas"
  bs_funcclas[bs_funcclas[,j]=="NA",j]="NA.funcclas"
}
load("Aux_Files/bs_flsa.RData")
for(j in 1:24)
{
  bs_flsa[is.na(bs_flsa[,j]),j]="NA.flsa"
  bs_flsa[bs_flsa[,j]=="NA",j]="NA.flsa"
}
load("Aux_Files/bs_appttype.RData")
for(j in 1:24)
{
  bs_appttype[is.na(bs_appttype[,j]),j]="NA.appttype"
  bs_appttype[bs_appttype[,j]=="NA",j]="NA.appttype"
}
load("Aux_Files/bs_polappttype.RData")
for(j in 1:24)
{
  bs_polappttype[is.na(bs_polappttype[,j]),j]="NA.polappttype"
  bs_polappttype[bs_polappttype[,j]=="NA",j]="NA.polappttype"
}
load("Aux_Files/bs_position.RData")
for(j in 1:24)
{
  bs_position[is.na(bs_position[,j]),j]="NA.position"
  bs_position[bs_position[,j]=="NA",j]="NA.position"
}
load("Aux_Files/bs_tenure.RData")
for(j in 1:24)
{
  bs_tenure[is.na(bs_tenure[,j]),j]="NA.tenure"
  bs_tenure[bs_tenure[,j]=="NA",j]="NA.tenure"
}
load("Aux_Files/bs_svsrstat.RData")
for(j in 1:24)
{
  bs_svsrstat[is.na(bs_svsrstat[,j]),j]="NA.svsrstat"
  bs_svsrstat[bs_svsrstat[,j]=="NA",j]="NA.svsrstat"
}

library(parallel)

f<-function(J1)
{
  
  load(paste("Aux_Files/data_occ_year_",J1,".RData",sep=""))
  
  data_other_variables  = data.frame(data_occ,
                                     instrctpgm = bs_instrctpgm[,J1],
                                     occ_cat = bs_occ_cat[,J1],
                                     funcclas = bs_funcclas[,J1],
                                     flsa = bs_flsa[,J1],
                                     appttype = bs_appttype[,J1],
                                     polappttype = bs_polappttype[,J1],
                                     position = bs_position[,J1],
                                     tenure = bs_tenure[,J1],
                                     svsrstat = bs_svsrstat[,J1])
  
  print(J1)
  save(data_other_variables, file = paste("Aux_Files/data_other_variables_year_",J1,".RData",sep=""))
  
}

mclapply(1:24,f,mc.cores=24)

