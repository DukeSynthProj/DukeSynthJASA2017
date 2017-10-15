setwd("~/Path/SyntheticData")
load("Aux_Files/data_AgeYrsdMilm.RData")
load("Aux_Files/bs_agency.RData")
load("Aux_Files/bs_new_race.RData")
load("Aux_Files/bs_new_educ_lvl.RData")
load("Aux_Files/bs_occ.RData")

for(j in 1:24)
{
  bs_occ[is.na(bs_occ[,j]),j]="NA.occ"
  bs_occ[bs_occ[,j]=="NA",j]="NA.occ"
}

library(parallel)

Ind = which(data_AgeYrsdMilm$init_agerange == "UNSP")
educ_lvl = data_AgeYrsdMilm$init_educ
yrsdegrng = data_AgeYrsdMilm$init_yrsdegrng
age.0 = data_AgeYrsdMilm$init_agerange

educ_lvl_tmp = educ_lvl[Ind]
yrsdegrng_tmp = yrsdegrng[Ind]

f<-function(j)
{
  if(ceiling(j/1e2)*1e2 == j){print(j)}
  tmp = table(data_AgeYrsdMilm$init_agerange[educ_lvl_tmp[j] == educ_lvl & yrsdegrng_tmp[j] == yrsdegrng])
  tmp = tmp[-length(tmp)]
  sample(names(tmp),1,T,tmp)
}

tmp <- simplify2array(mclapply(1:length(Ind),f,mc.cores=48))
age.0[Ind] <- tmp
age.0 = as.numeric(substring(as.vector(age.0),0,2))+2



f<-function(J1)
{
  
  # 01 agency
  agency = bs_agency[,J1]
  Ind = agency=="0"
  
  # 02 gender
  gender = as.vector(data_AgeYrsdMilm$gender)
  gender[Ind] = "0"
  
  # 03 and 04 race
  race = bs_new_race[,J1]
  
  # 05 educ_lvl
  educ_lvl = bs_new_educ_lvl[,J1]
  
  # 06 age
  init_year = data_AgeYrsdMilm$init_year  
  age = age.0 + J1 - init_year
  age[Ind] = 0 
  
  # 07 bs_yrsdegrng
  init_yrsdegrng = as.vector(data_AgeYrsdMilm$init_yrsdegrng)
  init_yrsdegrng[Ind] = "0" 
  
  # 08 milmonths
  milmonths = as.vector(data_AgeYrsdMilm$milmonths)
  milmonths[Ind] = "0" 
  
  ## 09 Additional predictors
  total_year = data_AgeYrsdMilm$total_year
  number_agency = data_AgeYrsdMilm$number_agency
  M = data_AgeYrsdMilm$M
  
  total_year[Ind] = 0
  number_agency[Ind] = 0
  M[Ind] = 0

  # 10 occ
  occ = bs_occ[,J1]
  
  
  data_occ = data.frame(
    agency = agency,
    gender = gender,
    race = race,
    educ_lvl = educ_lvl,
    age = age,
    init_yrsdegrng = init_yrsdegrng,
    milmonths = milmonths,
    total_year = total_year,
    number_agency = number_agency,
    M = M,
    occ = occ)
  
  
  print(J1)
  save(data_occ, file = paste("Aux_Files/data_occ_year_",J1,".RData",sep=""))
  
}

mclapply(1:24,f,mc.cores=24)


