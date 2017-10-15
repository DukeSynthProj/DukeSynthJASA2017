setwd("~/Path/SyntheticData")
load("Aux_Files/bs_agency.RData")
load("Aux_Files/bs_race.RData")
load("Aux_Files/bs_eribridge.RData")
load("Aux_Files/data_gender.RData")

library(parallel)

# Preparing bs_race

for(j in 1:24)
{
  bs_race[is.na(bs_race[,j]),j]="NA.race"
  bs_race[bs_race[,j]=="NA",j]="NA.race"
  bs_eribridge[is.na(bs_eribridge[,j]),j]="NA.eribridge"
  bs_eribridge[bs_eribridge[,j]=="NA",j]="NA.eribridge"
}

bs_race = bs_race[,1:18]
bs_eribridge = bs_eribridge[,19:24]

tmp = simplify2array(mclapply(1:nrow(bs_race),function(j){length(unique(bs_race[j,bs_race[j,]!="0"]))},mc.cores=48))
unique_race = tmp
unique_race[tmp == 0] = "NS"
unique_race[tmp == 1] = "U"
unique_race[tmp >= 2] = "NU"


tmp = simplify2array(mclapply(1:nrow(bs_eribridge),function(j){length(unique(bs_eribridge[j,bs_eribridge[j,]!="0"]))},mc.cores=48))
unique_eribridge = tmp
unique_eribridge[tmp == 0] = "NS"
unique_eribridge[tmp == 1] = "U"
unique_eribridge[tmp >= 2] = "NU"

f<-function(j)
{
  tmp = "NS"
  if(sum(bs_race[j,]!="0")>0)
    tmp = bs_race[j,bs_race[j,]!="0"][1]
  tmp
}
init_race = simplify2array(mclapply(1:nrow(bs_race),f,mc.cores=48))

f<-function(j)
{
  tmp = "NS"
  if(sum(bs_eribridge[j,]!="0")>0)
    tmp = bs_eribridge[j,bs_eribridge[j,]!="0"][1]
  tmp
}
init_eribridge = simplify2array(mclapply(1:nrow(bs_eribridge),f,mc.cores=48))

table(init_race,unique_race)
table(init_eribridge,unique_eribridge)

######
# data race
######
data_race<-data.frame(data_gender, unique_race, init_race, unique_eribridge, init_eribridge)

colnames(data_race)

save(data_race,file="Aux_Files/data_race.RData")





