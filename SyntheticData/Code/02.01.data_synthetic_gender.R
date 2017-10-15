setwd("~/Path/SyntheticData")
load("Synthetic_Files/bs_synthetic_agency.RData")
library(parallel)


# Year
year <- matrix(1:24,ncol=24,nrow=nrow(bs_synthetic_agency),byrow = TRUE)
year <- (bs_synthetic_agency != "0")*year


# Initial Year
f<-function(j){min(year[j,][year[j,]>0])}
init_year <- simplify2array(mclapply(1:nrow(bs_synthetic_agency),f,mc.cores=48)) 

# Last Year
f<-function(j){max(year[j,][year[j,]>0])}
last_year <- simplify2array(mclapply(1:nrow(bs_synthetic_agency),f,mc.cores=48))  

# Total Year
f<-function(j){sum(year[j,]>0)}
total_year <- simplify2array(mclapply(1:nrow(bs_synthetic_agency),f,mc.cores=48))  

# Years of the agency mode
f<-function(j){max(table(bs_synthetic_agency[j,bs_synthetic_agency[j,]!="0"]))}
mode_agency <- simplify2array(mclapply(1:nrow(bs_synthetic_agency),f,mc.cores=48))

# Initial agency
f<-function(j){bs_synthetic_agency[j,init_year[j]]}
init_agency <- simplify2array(mclapply(1:nrow(bs_synthetic_agency),f,mc.cores=48))

# Number agencies
f<-function(j){length(unique(bs_synthetic_agency[j,bs_synthetic_agency[j,]!="0"]))}
number_agency <- simplify2array(mclapply(1:nrow(bs_synthetic_agency),f,mc.cores=48))

# Number of moves
Z<-bs_synthetic_agency[,2:24]==bs_synthetic_agency[,1:23]; Z=1-Z
M<-apply(Z,1,sum)

# Gaps
year <- matrix(1:24,ncol=24,nrow=nrow(bs_synthetic_agency),byrow = TRUE)
tmp=1*(bs_synthetic_agency!="0")*year
f<-function(j)
{
  #print(j)
  x=tmp[j,]
  x1=cumsum(x)
  tmp1=table(x1[x1>0])
  tmp2<-tmp1[tmp1>1]
  tmp3<-sum(tmp1>1)
  tmp3-sum(as.numeric(names(tmp2))==max(as.numeric(names(tmp1))))
}
tmp <-  simplify2array(mclapply(1:nrow(bs_synthetic_agency),f,mc.cores=48))
gaps <- rep(0,nrow(bs_synthetic_agency))
for(j in 1:nrow(bs_synthetic_agency)){gaps[j]<-tmp[[j]]}

#####
# data_synthetic_gender
#####
data_synthetic_gender<-data.frame(init_year,last_year,total_year,mode_agency,
                        init_agency,number_agency,M,gaps)

save(data_synthetic_gender,file="Synthetic_Files/data_synthetic_gender.RData")

