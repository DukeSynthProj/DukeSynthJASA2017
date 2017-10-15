setwd("~/Path/SyntheticData")
load("Aux_Files/data_race.RData")

variables<-c("gender","init_year","last_year","total_year","mode_agency","init_agency",
"number_agency","M","gaps")
data_educ = data_race[,colnames(data_race) %in% variables]

# Race
load("Aux_Files/bs_race.RData")
load("Aux_Files/bs_eribridge.RData")

for(j in 1:24)
{
  bs_race[is.na(bs_race[,j]),j]="NA.race"
  bs_race[bs_race[,j]=="NA",j]="NA.race"
  bs_eribridge[is.na(bs_eribridge[,j]),j]="NA.eribridge"
  bs_eribridge[bs_eribridge[,j]=="NA",j]="NA.eribridge"
}


##### New Variable race
bs_race=ifelse(bs_race=="A","A",bs_race)
bs_race=ifelse(bs_race=="B","B",bs_race)
bs_race=ifelse(bs_race=="C","C",bs_race)
bs_race=ifelse(bs_race=="D","D",bs_race)
bs_race=ifelse(bs_race=="E","E",bs_race)
bs_race=ifelse(bs_race=="F","B",bs_race)
bs_race=ifelse(bs_race=="G","B",bs_race)
bs_race=ifelse(bs_race=="H","B",bs_race)
bs_race=ifelse(bs_race=="J","B",bs_race)
bs_race=ifelse(bs_race=="K","B",bs_race)
bs_race=ifelse(bs_race=="L","B",bs_race)
bs_race=ifelse(bs_race=="M","B",bs_race)
bs_race=ifelse(bs_race=="N","B",bs_race)
bs_race=ifelse(bs_race=="P","B",bs_race)
bs_race=ifelse(bs_race=="Q","B",bs_race)
bs_race=ifelse(bs_race=="Y","E",bs_race)


bs_eribridge=ifelse(bs_eribridge=="A","A",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="B","B",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="D","B",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="C","C",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="G","D",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="H","D",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="I","D",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="J","D",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="K","D",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="L","D",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="E","E",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="M","D",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="F","F.eri",bs_eribridge)
bs_eribridge=ifelse(bs_eribridge=="NA.eribridge","NA.race",bs_eribridge)

bs_new_race = cbind(bs_race[,1:18],bs_eribridge[,19:24])
#save(bs_new_race,file="Aux_Files/bs_new_race.RData")
library(parallel)
# Initial race
f<-function(j){bs_new_race[j,data_race$init_year[j]]}
init_race <- simplify2array(mclapply(1:nrow(bs_new_race),f,mc.cores=48))
table(init_race)

data_educ = data.frame(data_educ, init_race = init_race)


## Educational Level without NAs

load("Aux_Files/bs_educ_lvl.RData")

for(j in 1:24)
{
  bs_educ_lvl[is.na(bs_educ_lvl[,j]),j] = "NA.educ_lvl"
  bs_educ_lvl[bs_educ_lvl[,j]=="NA",j]  = "NA.educ_lvl"
}

f<-function(j)
{
  if(ceiling(j/1e4)*1e4 == j){print(j)}
  x<-y<-bs_educ_lvl[j,]
  x<-x[x!="0"]
  if(length(x)>1)
  {
    for(j in (length(x)-1):1)
    {
      if(x[j]=="NA.educ_lvl" & x[j+1]!="0")
        x[j] = x[j+1]
    }
    for(j in 2:length(x))
    {
      if(x[j]=="NA.educ_lvl" & x[j-1]!="0")
        x[j] = x[j-1]
    }
  }
  y[y!="0"]<-x
  y
}

bs_new_educ_lvl = bs_educ_lvl
tmp=mclapply(1:nrow(bs_educ_lvl),f,mc.cores=48)
for(j in 1:nrow(bs_educ_lvl)){bs_new_educ_lvl[j,] <- tmp[[j]]}
save(bs_new_educ_lvl,file="Aux_Files/bs_new_educ_lvl.RData")

tmp = simplify2array(mclapply(1:nrow(bs_new_educ_lvl),function(j){length(unique(bs_new_educ_lvl[j,bs_new_educ_lvl[j,]!="0"]))},mc.cores=48))

unique_educ = tmp
unique_educ[tmp == 1] = "U"
unique_educ[tmp >= 2] = "NU"

f <- function(j)
{
tmp = unique_educ[j]
if(tmp == "NU")
{
  tmp1 = as.numeric(bs_new_educ_lvl[j,bs_new_educ_lvl[j,]!="0"])
  if(sum(diff(tmp1)<0)==0)
    tmp = "Mono"
  if(sum(diff(tmp1)<0)>0)
    tmp = "NoMono"
}
tmp
} 

unique_educ <- simplify2array(mclapply(1:nrow(bs_new_educ_lvl),f,mc.cores=48))

table(unique_educ)

data_educ = data.frame(data_educ, unique_educ = unique_educ)

save(data_educ,file="Aux_Files/data_educ.RData")





