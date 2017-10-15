#rm(list = ls())
setwd("~/Path/SyntheticData")
load(paste("Aux_Files/Probs_Theta_",theta,".RData",sep=""))
print(paste("Synthetizing Agency for theta =",theta))
load("Aux_Files/Names.RData")
load("Aux_Files/bs_exist_agency.RData")
library(OPMpackage)
library(parallel)

theta = Probs$theta; Prob_M = Probs$Prob_M;  Prob_Cond_Mov = Probs$Prob_Cond_Mov; 
Prob_trans = Probs$Prob_trans; Prob_Ag_24 = Probs$Prob_Ag_24


rTraj_agency = function(i1)
{
  
  M = sample(Prob_M[,1], size=1, replace=T, prob=Prob_M[,2] )
  #print(M)
  if(M==0)
  {
    traj = rep(sample(names(Prob_Ag_24), size=1, replace=T, prob=Prob_Ag_24),24)
  }
  if(M==1)
  {
    Z = sample(2:24, size=1, replace=T, prob=Prob_Cond_Mov[[M]])    
    prob1 = Prob_trans[Prob_trans[,1]==1, ,drop = FALSE]
    prob2 = prob1[prob1[,5] == Z-1, ,drop = FALSE]
    A0A1 = prob2[sample(1:nrow(prob2),1,F,prob2[,9]),][2:3]       
    traj = Names[c(rep(A0A1[1],(Z-1)),rep(A0A1[2],24-(Z-1)))]
  }
  # print(paste("M",M))
  if(M>1)
  { 
    t = 0
    traj=c()
    while(length(traj)<24)
    {
      t = t+1; if(t>1){print(paste("waiting",t,M))}
      
      
      tmp1 = sample(names(Prob_Cond_Mov[[M]]), size=1, replace=T, prob=Prob_Cond_Mov[[M]])
      Z = as.numeric(strsplit(tmp1,split = "-")[[1]])
      
      prob1 = Prob_trans[Prob_trans[,1]==M, ,drop = FALSE]
      prob2 = prob1[ prob1[,4] == 1 &   prob1[,5] == Z[1]-1, ,drop = FALSE]
      
      if(nrow(prob2)>0)
      {
        traj = rep(prob2[sample(1:nrow(prob2),1,F,prob2[,9]),][2] ,Z[1]-1)
        traj_tmp = traj
        Z1 = c(1,Z,25)
        
        
        for(j1 in 3:length(Z1))
        {
          
         #tmp1 =  prob1[,4] == Z1[j1-2]  & prob1[,2] == traj[length(traj)] &    
         #   prob1[,5] == Z1[j1-1]-Z1[j1-2] & prob1[,6] == Z1[j1]-Z1[j1-1] 
          
          #prob2 = prob1[tmp1 , ,drop = FALSE]; prob2

 

          X = sub_Prob_trans(prob1,c(traj[length(traj)], Z1[j1-2], Z1[j1-1]-Z1[j1-2], Z1[j1]-Z1[j1-1]))


#print(Z1[j1-1])
#print(Z1[j1])
#          print(Z1[j1-1]:(Z1[j1]-1))          

          Y = cbind((1:nrow(bs_exist_agency))[apply(bs_exist_agency[,Z1[j1-1]:(Z1[j1]-1),drop=FALSE],1,sum)==length(Z1[j1-1]:(Z1[j1]-1))],1e-8)

          Y=Y[Y[,1]!=traj[length(traj)],]

          X = rbind(X,Y)
           

#print(X)
     
          #if(nrow(prob2)>0)
          #  traj = c(traj,rep(prob2[sample(1:nrow(prob2),1,F,prob2[,9]),][3] ,Z1[j1]-Z1[j1-1]))
          if(nrow(X)==1)
             traj = c(traj,rep(X[1,1],Z1[j1]-Z1[j1-1]))
          
          if(nrow(X)>1)
             traj = c(traj,rep(sample(X[,1],1,F,X[,2]),Z1[j1]-Z1[j1-1]))

        }
      }   
      traj = Names[traj]
    }
    
  }
  
  print(i1)
  #  print(M) 
  #  if(M>0){print(Z)}
  #  print(traj) 
  traj
  
}  

#rTraj_agency(1)

t(sapply(1:100,rTraj_agency))



tmpf1 = function(j1)
{

#  if(j1<=47)
    output = t(sapply(1:70000,rTraj_agency))

#  if(j1==48)
#    output = t(sapply(1:32406,rTraj_agency))

  save(output,file=paste("Synthetic_Files/",j1,".RData",sep=""))
  print(j1)

}

mclapply(1:(3*48),tmpf1,mc.cores=48)


# Creating the bs_synthetic_agency 
output = mclapply(1:(3*48),function(j1){load(paste("Synthetic_Files/",j1,".RData",sep=""));output},mc.cores=48)

bs_synthetic_agency_long = matrix("0",ncol=24,nrow=7e4*(3*48))
for(j1 in 1:(3*48))
{
  bs_synthetic_agency_long[(1:70000)+70000*(j1-1),] = output[[j1]]
  print(j1)
}

colnames(bs_synthetic_agency_long) = colnames(bs_synthetic_agency_long)
rownames(bs_synthetic_agency_long) = 1:nrow(bs_synthetic_agency_long)


save(bs_synthetic_agency_long,file=paste("Synthetic_Files/bs_synthetic_agency_long_Theta_",theta,".RData",sep=""))
#for(j1 in 1:48){file.remove(paste("Synthetic_Files/",j1,".RData",sep=""))}


