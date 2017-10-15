setwd("~/Path/SyntheticData")
load("Aux_Files/data_educ.RData")

variables<-c("gender","init_year","last_year","total_year","mode_agency","init_agency",
             "number_agency","M","gaps","init_race")

data_AgeYrsdMilm = data_educ[,colnames(data_educ) %in% variables]

# Race
load("Aux_Files/bs_new_educ_lvl.RData")
load("Aux_Files/bs_agerange.RData")
load("Aux_Files/bs_yrsdegrng.RData")
load("Aux_Files/bs_milmonths.RData")


for(j in 1:24)
{
  bs_agerange[is.na(bs_agerange[,j]),j]="NA.agerange"
  bs_agerange[bs_agerange[,j]=="NA",j]="NA.agerange"
  bs_yrsdegrng[is.na(bs_yrsdegrng[,j]),j]="NA.yrsdegrng"
  bs_yrsdegrng[bs_yrsdegrng[,j]=="NA",j]="NA.yrsdegrng"
  bs_milmonths[is.na(bs_milmonths[,j]),j]="NA.milmonths"
  bs_milmonths[bs_milmonths[,j]=="NA",j]="NA.milmonths"
}

library(parallel)

# Initial educ
f<-function(j){bs_new_educ_lvl[j,data_educ$init_year[j]]}
init_educ <- simplify2array(mclapply(1:nrow(bs_new_educ_lvl),f,mc.cores=48))
table(init_educ)
data_AgeYrsdMilm = data.frame(data_AgeYrsdMilm, init_educ = init_educ)



# Initial agerange
f<-function(j){bs_agerange[j,data_educ$init_year[j]]}
init_agerange <- simplify2array(mclapply(1:nrow(bs_agerange),f,mc.cores=48))
table(init_agerange)
data_AgeYrsdMilm = data.frame(data_AgeYrsdMilm, init_agerange = init_agerange)

Ind = which(data_AgeYrsdMilm$init_agerange=="UNSP")

f<-function(j)
{
  print(paste(j,"of",length(Ind)))
  tmp1=colnames(bs_agerange)[bs_agerange[Ind[j],]!="0"][1]
  tmp2=colnames(bs_agerange)[bs_agerange[Ind[j],]!="0" & bs_agerange[Ind[j],]!="UNSP"][1]
  tmp3=bs_agerange[Ind[j],tmp2==colnames(bs_agerange)]
  c(tmp1,tmp2,tmp3)
}

tmp=mclapply(1:length(Ind),f,mc.cores=48)

f<-function(j)
{
  print(paste(j,"of",length(Ind)))
  tmp2 = "UNSP" 
  if(length(tmp[[j]])==3)
  {
    tmp1 = as.numeric(substring(tmp[[j]][3],0,2))+2+as.numeric(tmp[[j]][1])-as.numeric(tmp[[j]][2])
    for(j1 in 3:14)
    {
      if(tmp1 >= j1*5 & tmp1 <= j1*5+4){tmp2 = paste(j1*5,j1*5+4,sep="-")}   
    }
    if(tmp1 >= 75){tmp2 = "75+"} 
  }
  tmp2
}

tmp2 = simplify2array(mclapply(1:length(Ind),f,mc.cores=48))

data_AgeYrsdMilm$init_agerange[Ind] = tmp2



# Initial yrsdegrng
f<-function(j){bs_yrsdegrng[j,data_educ$init_year[j]]}
init_yrsdegrng <- simplify2array(mclapply(1:nrow(bs_yrsdegrng),f,mc.cores=48))
table(init_yrsdegrng)
data_AgeYrsdMilm = data.frame(data_AgeYrsdMilm, init_yrsdegrng = init_yrsdegrng)


bs_Ind = matrix(FALSE, nrow=nrow(bs_yrsdegrng), ncol=ncol(bs_yrsdegrng))
f<-function(j){bs_new_educ_lvl[j,] == bs_new_educ_lvl[j,data_educ$init_year[j]]}
tmp <- mclapply(1:nrow(bs_yrsdegrng),f,mc.cores=48)
for(j in 1:nrow(bs_yrsdegrng)){bs_Ind[j,]=tmp[[j]]}

bs_Ind = bs_Ind + (bs_yrsdegrng == "NA.yrsdegrng")
bs_Ind = bs_Ind == 2

f<-function(j)
{
   tmp1 = data_AgeYrsdMilm$init_year[j]
   tmp2 = as.numeric((1:24)[bs_Ind[j,]==TRUE][2])
   tmp3 = bs_yrsdegrng[j,tmp2][bs_yrsdegrng[j,tmp2] != "NA.yrsdegrng"][1]
   c(tmp1,tmp2,tmp3)
}
tmp <- t(simplify2array(mclapply(1:nrow(bs_yrsdegrng),f,mc.cores=48)))
table(tmp[,3])
####
# A lot of missing in  yrsdegrng
# > table(tmp[,3])
# < table of extent 0 >
####


# milmonths
f<-function(j){ifelse(sum(bs_milmonths[j,]=="0") == 24,"NoMilmonths","Milmonths")}
milmonths <- simplify2array(mclapply(1:nrow(bs_milmonths),f,mc.cores=48))
table(milmonths)
data_AgeYrsdMilm = data.frame(data_AgeYrsdMilm, milmonths = milmonths)


save(data_AgeYrsdMilm,file="Aux_Files/data_AgeYrsdMilm.RData")





