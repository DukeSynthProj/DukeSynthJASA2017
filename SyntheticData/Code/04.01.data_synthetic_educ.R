setwd("~/Path/SyntheticData")
load("Synthetic_Files/data_synthetic_race.RData")

variables<-c("gender","init_year","last_year","total_year","mode_agency","init_agency",
"number_agency","M","gaps")
data_synthetic_educ = data_synthetic_race[,colnames(data_synthetic_race) %in% variables]

# Race
load("Synthetic_Files/bs_synthetic_race_1.18.RData")
load("Synthetic_Files/bs_synthetic_race_19.24.RData")

for(j in 1:18)
{
  bs_synthetic_race_1.18[is.na(bs_synthetic_race_1.18[,j]),]="NA.race"
  bs_synthetic_race_1.18[bs_synthetic_race_1.18[,j]=="NA",]="NA.race"
}

for(j in 1:6)
{
  bs_synthetic_race_19.24[is.na(bs_synthetic_race_19.24[,j]),]="NA.eribridge"
  bs_synthetic_race_19.24[bs_synthetic_race_19.24[,j]=="NA",]="NA.eribridge"
}


##### New Variable race
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="A","A",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="B","B",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="C","C",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="D","D",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="E","E",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="F","B",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="G","B",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="H","B",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="J","B",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="K","B",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="L","B",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="M","B",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="N","B",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="P","B",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="Q","B",bs_synthetic_race_1.18)
bs_synthetic_race_1.18=ifelse(bs_synthetic_race_1.18=="Y","E",bs_synthetic_race_1.18)


bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="A","A",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="B","B",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="D","B",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="C","C",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="G","D",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="H","D",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="I","D",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="J","D",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="K","D",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="L","D",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="E","E",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="M","D",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="F","F.eri",bs_synthetic_race_19.24)
bs_synthetic_race_19.24=ifelse(bs_synthetic_race_19.24=="NA.eribridge","NA.race",bs_synthetic_race_19.24)

bs_synthetic_new_race = cbind(bs_synthetic_race_1.18,bs_synthetic_race_19.24)
save(bs_synthetic_new_race,file="Synthetic_Files/bs_synthetic_new_race.RData")
library(parallel)
# Initial race
f<-function(j){bs_synthetic_new_race[j,data_synthetic_race$init_year[j]]}
init_race <- simplify2array(mclapply(1:nrow(bs_synthetic_new_race),f,mc.cores=48))
data_synthetic_educ = data.frame(data_synthetic_educ, init_race = init_race)


save(data_synthetic_educ,file="Synthetic_Files/data_synthetic_educ.RData")





