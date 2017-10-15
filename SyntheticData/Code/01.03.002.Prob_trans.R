load.aux.var<-c("bs_agency_num","Names","bs_exist_agency")
for(i1 in 1:length(load.aux.var)){load(paste("Aux_Files/",load.aux.var[i1],".RData",sep=""))}
load("Aux_Files/Freq_trans.RData")

library(OPMpackage)
library(parallel)
load("Aux_Files/Aux_prob_trans.RData")


prob_trans_models <- function(i1)
{
  if(I1[i1]==1)
  {
    prob = Freq_trans[Freq_trans[,8]==i1,,drop=FALSE]
    Prob_trans = cbind(prob, prob[,7])
    colnames(Prob_trans)[9] = "pred"
  }
  
  if(I1[i1]>1)
  {
    m = M[i1]
    prob = Freq_trans[Freq_trans[,8]==i1,,drop=FALSE]
    support_base = output[[i1]]
    
    tmp2 = prob[,4:6, drop = FALSE]
    alpha = rbind(2*tmp2-1)
    alpha = cbind(alpha, 2*25-apply(alpha,1,sum))
    
    for(i in  1:nrow(alpha))
    {
      x = alpha[i,]
      j = 1
      while(min(x)<1)
      {
        j = j+1
        x[1:m]=2*x[1:m]-1
        x = c(x[1:m], (2^j)*25-sum(x[1:m]))
      }
      alpha[i,] = x
    }
    
    alpha = theta*alpha
    
    # Estimating probabilities
    rsample = function(j)
    {
      set.seed(123)
      if(sum(prob[,7])>1e4)
        nrec=prob[j,7]+1
      if(sum(prob[,7])<1e4)
        nrec=1e4*prob[j,7]/sum(prob[,7])+1
      aux1<-rgamma(nrec,alpha[j,1],1)
      for(j1 in 2:4){aux1<-cbind(aux1,rgamma(nrec,alpha[j,j1],1))}
      tmpf = function(x){x/sum(x)}
      aux2 = t(apply(aux1,1,tmpf))
      aux3 = rbind(ceiling(aux2[,1:3]*25))
      count_freq_trans(aux3,support_base)[,4]
    }
    output2 <- cbind(sapply(1:nrow(prob),rsample))
    if(ncol(output2)>1)
      output2 <- apply(output2,1,sum)
    
    Prob_trans = cbind(prob[1,1],prob[1,2],prob[1,3],support_base,0,prob[1,8],sum(prob[,7])*output2/sum(output2))
    colnames(Prob_trans) = c(colnames(prob),"pred")
    
    for(j in 1:nrow(prob))
    {
      for(i in 1:nrow(Prob_trans))
      {
        if(prob[j,4]==Prob_trans[i,4] & prob[j,5]==Prob_trans[i,5] & prob[j,6]==Prob_trans[i,6])
          Prob_trans[i,7] = prob[j,7]
      }  
    }
    
    tmp1 = Prob_trans[,7]>0
    tmp2 = Prob_trans[,9]>0
    tmp3 = tmp1 | tmp2
    
    Prob_trans = Prob_trans[tmp3,]
  }
  print(paste(i1,"combination of",max(Freq_trans[,8])))
  list(Prob_trans,nrow(Prob_trans))
}

fit <- mclapply(1:max(Freq_trans[,8]),prob_trans_models,mc.cores = 48)


I1 <- c()
for(j in 1:max(Freq_trans[,8]))
{
  fit[[j]][[1]] <- rbind(fit[[j]][[1]])
  I1[j] <- nrow(fit[[j]][[1]])
}


Prob_trans <- matrix(0,nrow=sum(I1),ncol=ncol(fit[[1]][[1]]))

I2 = cumsum(I1)
I3 = c(1,I2+1)[1:length(I2)]
I4 = I2
I5 = cbind(I3,I4)

for(j in 1:max(Freq_trans[,8])){Prob_trans[I5[j,1]:I5[j,2],] <- fit[[j]][[1]]}

colnames(Prob_trans)=colnames(fit[[1]][[1]])

Range = range(Prob_trans[,7]/sum(Prob_trans[,7]),Prob_trans[,9]/sum(Prob_trans[,9]))

plot(Prob_trans[,7]/sum(Prob_trans[,7]),Prob_trans[,9]/sum(Prob_trans[,9]), lwd=2, col = 'gray',main="trans",cex=0.5,xlim=Range,ylim=Range)
lines(seq(1e-5,1e4,len=1e4), (seq(1e-5,1e4,len=1e4)) , col = 2)

Range[2] = Range[2]*0.1

plot(Prob_trans[,7]/sum(Prob_trans[,7]),Prob_trans[,9]/sum(Prob_trans[,9]), lwd=2, col = 'gray',main="trans",cex=0.5,xlim=Range,ylim=Range)
lines(seq(1e-5,1e4,len=1e4), (seq(1e-5,1e4,len=1e4)) , col = 2)


#save(Prob_trans,file=paste("Prob_trans_",theta,".RData",sep=""))


