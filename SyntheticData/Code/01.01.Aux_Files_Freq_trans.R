setwd("~/Path/SyntheticData")

rm(list=ls())
load.aux.var<-c("bs_agency_num","Prob_M")
for(i1 in 1:length(load.aux.var)){load(paste("Aux_Files/",load.aux.var[i1],".RData",sep=""))}
library(OPMpackage)
library(parallel)

m = max(bs_agency_num)

A0A1 = matrix(0,m^2,2)

i1=0;
for(i in 1:max(bs_agency_num))
{
  for(j in 1:max(bs_agency_num))
  {
    i1=i1+1
    A0A1[i1,] = c(i,j)
  }
}

tmp = A0A1[,1] != A0A1[,2] 
A0A1 = A0A1[tmp,]


Z=bs_agency_num[,1:(ncol(bs_agency_num)-1)] != bs_agency_num[,2:ncol(bs_agency_num)]
M = apply(Z,1,sum)


trans_f = function(m)
{
  X = bs_agency_num[M==m,]
  
  tmpf1 = function(i1)  
  {
    
    tmpf = function(j1)
    {
      
      A0 = A0A1[j1,1]
      A1 = A0A1[j1,2]
      
      if(m>=1)
      {
        a0 = apply(X==A0,1,sum)>0
        a1 = apply(X==A1,1,sum)>0     
      }
      
      #      if(m==2)
      #      {
      #        a0 = X[,1]==A0
      #        a1 = X[,24]==A1
      #      }
      
      a = a0==TRUE & a1==TRUE
      
      if(sum(a)==0){X=X[1,,drop=FALSE]}
      if(sum(a) > 0){X=X[a,,drop=FALSE]}
      
      #    print(paste(j1,"combination of", nrow(A0A1), "m =", m))
      
      tmp_print=100*(j1-II[i1,1])/II[1,2]
      #     print((tmp_print %% 10))
      if((tmp_print %% 10)==0)
        print(paste(tmp_print,"percentage", "m =", m))
      #         print(A0A1[j1,])
      freq_trans(X,A0,A1)
    }
    
    tmp1 = sapply(II[i1,1]:II[i1,2],tmpf)
    
    Ind = (1:length(II[i1,1]:II[i1,2]))[as.vector(as.matrix(tmp1[1,]))!=0]
    
    
    tmp2 = tmp1[,Ind[1]]$Z
    for(i in 2:length(Ind))
    {
      tmp2 = rbind(tmp2,tmp1[,Ind[i]]$Z)
    }
    
    tmp2 = cbind(m,tmp2)
    tmp2
  }
  
  ncores = 48
  II = cbind((0:(ncores-1))*floor(nrow(A0A1)/ncores)+1,(1:ncores)*floor(nrow(A0A1)/ncores))
  II[ncores,2] = nrow(A0A1)
  
  tmp1 = mclapply(1:ncores,tmpf1,mc.cores=ncores) 
  
  tmp2=tmp1[[1]]
  for(j1 in 2:ncores)
  {
    if(ncol(tmp1[[j1]])!=1)
      tmp2 = rbind(tmp2,tmp1[[j1]])
  }
  
  colnames(tmp2) = c("m","A0","A1","Y0","LAG1", "LAG2","Freq")
  rownames(tmp2) = 1:nrow(tmp2)
  
  tmp2 
}

#tmp1 = mclapply(1:11, trans_f, mc.cores=8)
#Freq_trans = tmp1[[1]]
#for(j in 2:11){Freq_trans = rbind(Freq_trans,tmp1[[j]])}

t17 = system.time(Freq_trans_17 <- trans_f(17))
t15 = system.time(Freq_trans_15 <- trans_f(15))
t14 = system.time(Freq_trans_14 <- trans_f(14))
t13 = system.time(Freq_trans_13 <- trans_f(13))
t12 = system.time(Freq_trans_12 <- trans_f(12))
t11 = system.time(Freq_trans_11 <- trans_f(11))
t10 = system.time(Freq_trans_10 <- trans_f(10))
t9 = system.time(Freq_trans_9 <- trans_f(9))
t8 = system.time(Freq_trans_8 <- trans_f(8))
t7 = system.time(Freq_trans_7 <- trans_f(7))
t6 = system.time(Freq_trans_6 <- trans_f(6))
t5 = system.time(Freq_trans_5 <- trans_f(5))
t4 = system.time(Freq_trans_4 <- trans_f(4))
t3 = system.time(Freq_trans_3 <- trans_f(3))
t2 = system.time(Freq_trans_2 <- trans_f(2))
t1 = system.time(Freq_trans_1 <- trans_f(1))

Freq_trans = rbind(Freq_trans_1,Freq_trans_2,Freq_trans_3,Freq_trans_4,Freq_trans_5,Freq_trans_6, Freq_trans_7,Freq_trans_8,Freq_trans_9,Freq_trans_10,Freq_trans_11,Freq_trans_12,Freq_trans_13,Freq_trans_14,
                   Freq_trans_15,Freq_trans_17)


tmp1 = apply(Freq_trans[,1:3],1,f<-function(x){paste(x,collapse="-")})
tmp_u = unique(tmp1)
Model = numeric_agency(cbind(tmp1),tmp_u,FALSE)
Freq_trans = cbind(Freq_trans,Model)
colnames(Freq_trans)[8] = "model"


save(Freq_trans, file = "Aux_Files/Freq_trans.RData")

##########################
##########################
load.aux.var<-c("bs_agency_num","Names","bs_exist_agency")
for(i1 in 1:length(load.aux.var)){load(paste("Aux_Files/",load.aux.var[i1],".RData",sep=""))}
load("Aux_Files/Freq_trans.RData")
library(parallel)

N = 24
tmp1 <- matrix(0, nrow = 24^3, ncol = 3 )
j4 <- 0
for(j1 in 1:N)
{
  for(j2 in 1:N)
  {
    for(j3 in 1:N)
    {
      j4 <- j4+1
      tmp1[j4,]=c(j1,j2,j3)
    } 
  }
}

tmp_base <- tmp1[apply(tmp1,1,sum)<= N+1,]

bs_exist_agency2 = bs_exist_agency
for(j in 1:N){bs_exist_agency2[,j]=bs_exist_agency2[,j]*j}


tmpf2 <- function(i1)
{
  A0 = Freq_trans[Freq_trans[,8]==i1,,drop=FALSE][1,2]
  A1 = Freq_trans[Freq_trans[,8]==i1,,drop=FALSE][1,3]
  M <-  Freq_trans[Freq_trans[,8]==i1,,drop=FALSE][1,1]
  
  aux1<-bs_exist_agency[c(A0,A1),]
  aux2<-bs_exist_agency2[c(A0,A1),]
  
  tmpf1 <- function(j1)
  {
    tmp1 = tmp_base[j1,]
    tmp2 = aux1[1,tmp1[1]:(tmp1[1]+tmp1[2]-1)]
    
    tmp3 = (tmp1[1]+tmp1[2]-1)
    tmp3 = aux1[2,(tmp3[length(tmp3)]+1):((tmp3[length(tmp3)]+1)+tmp1[3]-1)]
    
    tmp4 = sum(tmp1[1]==aux2[1,])==1 & sum(tmp2)==length(tmp2) & sum(tmp3)==length(tmp3)
    if(tmp4 == FALSE)
      tmp1 = c(0,0,0)
    cbind(tmp1)
  }
  
  X = t(sapply(1:nrow(tmp_base),tmpf1))
  print(i1)
  X = X[X[,1]>0, , drop = FALSE]
  
  if(M == 1)
  {
    X = X[apply(X,1,sum) == N+1 & X[,1]==1,,drop=FALSE]
  }
  if(M > 1 & nrow(X)>1)
  {
    X = X[apply(X[,2:3],1,sum) <= N-M+1, ,drop=FALSE]
  }
  print(paste(i1,"combination of",max(Freq_trans[,8])))
  X
}


output <- mclapply(1:max(Freq_trans[,8]), tmpf2, mc.cores = 48)


I1 <- c()
for(j1 in  1:max(Freq_trans[,8])){I1[j1] <-  nrow(output[[j1]])}


tmpf3 = function(j1)
{
  print(paste(j1,"combination of",max(Freq_trans[,8])))
  Freq_trans[Freq_trans[,8]==j1,,drop=FALSE][1,1]
}

output2 <- mclapply(1:max(Freq_trans[,8]), tmpf3, mc.cores = 48)

M <- c()
for(j1 in  1:max(Freq_trans[,8]))
{
  M[j1] <-  output2[[j1]]
}

save(Aux_prob_trans, file="Aux_Files/Aux_prob_trans.RData")



