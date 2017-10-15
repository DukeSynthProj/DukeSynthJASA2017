setwd("~/Path/SyntheticData")

library(parallel)

rm(list=ls())
load.aux.var<-c("bs_agency")
for(i1 in 1:length(load.aux.var)){load(paste("Aux_Files/",load.aux.var[i1],".RData",sep=""))}

# Names -- Agencies
Names = unique(as.vector(bs_agency))
save(Names, file= "Aux_Files/Names.RData")


# Numeric bs_agency
library(OPMpackage)
system.time(bs_agency_num <- numeric_agency(bs_agency,Names,TRUE))
save(bs_agency_num, file= "Aux_Files/bs_agency_num.RData")


# bs_exist_agency
tmpf = function(j)
{
  print(j)
  1*(apply(bs_agency_num == j,2,sum)>0)
}
tmp1 = mclapply(1:length(Names),tmpf,mc.cores=48)
bs_exist_agency = tmp1[[1]]
for(j in 2:length(Names)){bs_exist_agency = rbind(bs_exist_agency ,tmp1[[j]])}
rownames(bs_exist_agency) = Names
save(bs_exist_agency, file= "Aux_Files/bs_exist_agency.RData")

# Moves
Z=bs_agency[,2:24]==bs_agency[,1:23]; Z=1-Z
M=apply(Z,1,sum)
Prob_M<-cbind(as.numeric(names(table(M))),as.numeric(table(M)))

for(j in 1:23){Z[,j]=(j+1)*Z[,j]}

Prob_Z<-list()

tmpf_2 <- function(m)
{
  if(m==1)
  {
    tmp1=Z[M==m,]
    tmpf<-function(j){sort(unique(tmp1[j,]))[2]}
    tmp2<-mclapply(1:nrow(tmp1),tmpf,mc.cores=20)
    tmp2<-simplify2array(tmp2)
    res<-cbind(as.numeric(names(table(tmp2))),as.numeric(table(tmp2)))
    print(m)
  }
  
  if(m > 1)
  {
    tmp1=Z[M==m,]
    tmpf<-function(j){paste(sort(unique(tmp1[j,]))[2:(m+1)],collapse='-')}

    tmp2<-mclapply(1:nrow(tmp1),tmpf,mc.cores=20)
    tmp2<-simplify2array(tmp2)

    tmp3<-table(tmp2)
    tmpf<-function(j){as.numeric(strsplit(names(tmp3)[j],"-")[[1]])}
    res<-cbind(t(sapply(1:length(tmp3),tmpf)),as.numeric(tmp3))
    print(m)
  }
  res
}

Prob_Z <- mclapply(1:17,tmpf_2,mc.cores=2)

save(Prob_M,file='Aux_Files/Prob_M.RData')
save(Prob_Z,file='Aux_Files/Prob_Z.RData')

