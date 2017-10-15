setwd("~/Path/SyntheticData")
load("Synthetic_Files/data_synthetic_AgeYrsdMilm.RData")
load("Synthetic_Files/bs_synthetic_agency.RData")
load("Synthetic_Files/bs_synthetic_new_race.RData")
load("Synthetic_Files/bs_synthetic_new_educ_lvl.RData")
load("Synthetic_Files/synthetic_init_agerange.RData")
load("Synthetic_Files/synthetic_init_yrsdegrng.RData")
load("Synthetic_Files/synthetic_milmonths.RData")


library(parallel)

Ind = which(synthetic_init_agerange == "UNSP")
educ_lvl = data_synthetic_AgeYrsdMilm$init_educ
yrsdegrng = synthetic_init_yrsdegrng
age.0 = synthetic_init_agerange

educ_lvl_tmp = educ_lvl[Ind]
yrsdegrng_tmp = yrsdegrng[Ind]

f<-function(j)
{
  if(ceiling(j/1e2)*1e2 == j){print(j)}
  tmp = table(synthetic_init_agerange[educ_lvl_tmp[j] == educ_lvl & yrsdegrng_tmp[j] == yrsdegrng])
  tmp = tmp[-length(tmp)]
  sample(names(tmp),1,T,tmp)
}

tmp <- simplify2array(mclapply(1:length(Ind),f,mc.cores=48))
age.0[Ind] <- tmp
age.0 = as.numeric(substring(as.vector(age.0),0,2))+2



f<-function(J1)
{
  
  # 01 agency
  agency = bs_synthetic_agency[,J1]
  Ind = agency=="0"
  
  # 02 gender
  gender = as.vector(data_synthetic_AgeYrsdMilm$gender)
  gender[Ind] = "0"
  
  # 03 and 04 race
  race = bs_synthetic_new_race[,J1]
  
  # 05 educ_lvl
  educ_lvl = bs_synthetic_new_educ_lvl[,J1]
  
  # 06 age
  init_year = data_synthetic_AgeYrsdMilm$init_year  
  age = age.0 + J1 - init_year
  age[Ind] = 0 
  
  # 07 bs_yrsdegrng
  init_yrsdegrng = as.vector(synthetic_init_yrsdegrng)
  init_yrsdegrng[Ind] = "0" 
  
  # 08 milmonths
  milmonths = as.vector(synthetic_milmonths)
  milmonths[Ind] = "0" 
  
  ## 09 Additional predictors
  total_year = data_synthetic_AgeYrsdMilm$total_year
  number_agency = data_synthetic_AgeYrsdMilm$number_agency
  M = data_synthetic_AgeYrsdMilm$M
  
  total_year[Ind] = 0
  number_agency[Ind] = 0
  M[Ind] = 0

  
  data_synthetic_occ = data.frame(
    agency = agency,
    gender = gender,
    race = race,
    educ_lvl = educ_lvl,
    age = age,
    init_yrsdegrng = init_yrsdegrng,
    milmonths = milmonths,
    total_year = total_year,
    number_agency = number_agency,
    M = M)
  
  
  print(J1)
  save(data_synthetic_occ, file = paste("Synthetic_Files/data_synthetic_occ_year_",J1,".RData",sep=""))
  
}

mclapply(1:24,f,mc.cores=24)


