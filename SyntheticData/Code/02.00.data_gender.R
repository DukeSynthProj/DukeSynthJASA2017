setwd("~/Path/SyntheticData")
load("Aux_Files/bs_agency.RData")
load("Aux_Files/bs_sex.RData")

bs_gender = bs_sex
save(bs_gender,file="Aux_Files/bs_gender.RData")

library(parallel)

# Gender
# 0 Male and 1 Female
gender <- 1*(apply(bs_sex=="F",1,sum)>=1)
gender <- ifelse(gender == 1, "F","M")


# Year
year <- matrix(1:24,ncol=24,nrow=nrow(bs_agency),byrow = TRUE)
year <- (bs_agency != "0")*year


# Initial Year
f<-function(j){min(year[j,][year[j,]>0])}
init_year <- simplify2array(mclapply(1:nrow(bs_agency),f,mc.cores=48)) 
table(gender,init_year)

# Last Year
f<-function(j){max(year[j,][year[j,]>0])}
last_year <- simplify2array(mclapply(1:nrow(bs_agency),f,mc.cores=48))  
table(gender,last_year)

# Total Year
f<-function(j){sum(year[j,]>0)}
total_year <- simplify2array(mclapply(1:nrow(bs_agency),f,mc.cores=48))  
table(gender,total_year)

# Years of the agency mode
f<-function(j){max(table(bs_agency[j,bs_agency[j,]!="0"]))}
mode_agency <- simplify2array(mclapply(1:nrow(bs_agency),f,mc.cores=48))
table(gender,mode_agency)

# Initial agency
f<-function(j){bs_agency[j,init_year[j]]}
init_agency <- simplify2array(mclapply(1:nrow(bs_agency),f,mc.cores=48))
table(gender,init_agency)

# Number agencies
f<-function(j){length(unique(bs_agency[j,bs_agency[j,]!="0"]))}
number_agency <- simplify2array(mclapply(1:nrow(bs_agency),f,mc.cores=48))
table(gender,number_agency)

# Number of moves
Z<-bs_agency[,2:24]==bs_agency[,1:23]; Z=1-Z
M<-apply(Z,1,sum)
table(gender,M)

# Gaps
year <- matrix(1:24,ncol=24,nrow=nrow(bs_agency),byrow = TRUE)
tmp=1*(bs_agency!="0")*year
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
tmp <-  simplify2array(mclapply(1:nrow(bs_agency),f,mc.cores=48))
gaps <- rep(0,nrow(bs_agency))
for(j in 1:nrow(bs_agency)){gaps[j]<-tmp[[j]]}

#####
# data_gender
#####
data_gender<-data.frame(gender,init_year,last_year,total_year,mode_agency,
                        init_agency,number_agency,M,gaps)

save(data_gender,file="Aux_Files/data_gender.RData")

