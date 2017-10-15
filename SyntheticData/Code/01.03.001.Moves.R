library(parallel)

# Loading counts
load.aux.var<-c("Prob_Z","Prob_M")
for(i1 in 1:length(load.aux.var)){load(paste("Aux_Files/",load.aux.var[i1],".RData",sep=""))}

tmpf1 = function(m)
{
  #######################
  # Number of Moves = 1 #
  #######################
  if(m == 1)
  {
    tmp1 =  Prob_Z[[1]]
    tmp1[,1] =  tmp1[,1]-1
    tmp1[,2] =  tmp1[,2]/sum(tmp1[,2])
    
    alpha = theta*cbind((2*tmp1[,1]-1) , 2*23-(2*tmp1[,1])+1) + .9^(theta)
    
    Prob_fit = rep(0,23)
    for(j in 1:23)
    {
      for(i in 1:23)
      {
        Prob_fit[j] = Prob_fit[j] + 
          tmp1[i,2]*(pbeta(j/23,alpha[i,1],alpha[i,2])-pbeta((j-1)/23,alpha[i,1],alpha[i,2]))
        
      }
    }
  }
  
  
  #######################
  # Number of Moves > 1 #
  #######################
  
  if(m  > 1)
  {
    
    tmp1 =  Prob_Z[[m]]
    tmp1[,1:m] =  tmp1[,1:m] -1
    tmp2 = tmp1[,1:m,drop = FALSE]
    for(j in 2:m){tmp2[,j] =  tmp1[,j]-tmp1[,j-1]}
    
    ##    alpha = rbind(theta*cbind(2*tmp2-1,2*3-apply(2*tmp2-1,1,sum))) + .9^(theta)
    alpha = rbind(2*tmp2-1)
    alpha = cbind(alpha, 2*max(tmp2)-apply(alpha,1,sum))
    alpha
    
    aux2 = t(apply(alpha,1,f<-function(x){x/sum(x)}))
    aux1.1 = t(apply(rbind(ceiling(aux2[,1:m]*(24-m))),1,cumsum))+1
    J=0
    while(sum(aux1.1- Prob_Z[[m]][,1:m])!=0)
    {
      J=J+1
      alpha = rbind(2*tmp2-1)
      alpha = cbind(alpha, 2*max(tmp2+J)-apply(alpha,1,sum))
      aux2 = t(apply(alpha,1,f<-function(x){x/sum(x)}))
      aux1.1 = t(apply(rbind(ceiling(aux2[,1:m]*(24-m))),1,cumsum))+1
    }
    
    if(m==15)
    {
      J=5
      alpha = rbind(2*tmp2-1)
      alpha = cbind(alpha, 2*max(tmp2+J)-apply(alpha,1,sum))
    }
    
    if(m==17)
    {
      J=4
      alpha = rbind(2*tmp2-1)
      alpha = cbind(alpha, 2*max(tmp2+J)-apply(alpha,1,sum))
      sum(aux1.1- Prob_Z[[m]][,1:m])
      
    }
    
    for(i in  1:nrow(alpha))
    {
      x = alpha[i,]
      j = 1
      while(min(x)<1)
      {
        j = j+1
        #x=x/sum(x)
        x[1:m]=2*x[1:m]-1
        #x[1:m]=x[1:m]-1/(j*10)
        x = c(x[1:m], (2^j)*max(tmp2+J)-sum(x[1:m]))
        #print( sum(cumsum(ceiling(x/sum(x)*(24-m)))[1:m]-tmp1[i,1:m]))
      }
      alpha[i,] = x
    }
    
    alpha = theta*alpha
    
    aux2 = t(apply(alpha,1,f<-function(x){x/sum(x)}))
    aux1.1 = t(apply(rbind(ceiling(aux2[,1:m]*(24-m))),1,cumsum))+1
    cbind(aux1.1,Prob_Z[[m]][,1:m], aux1.1- Prob_Z[[m]][,1:m])
    print(sum(aux1.1- Prob_Z[[m]][,1:m]))
    
    
    
    
    # Estimating probabilities
    rsample = function(j)
    {
      set.seed(123)
      #      nrec=1e2*tmp1[j,m+1]/sum(tmp1[,m+1])+1
      nrec=2e6*tmp1[j,m+1]/sum(tmp1[,m+1])+1
      aux1<-rgamma(nrec,alpha[j,1],1)
      for(j1 in 2:(m+1)){aux1<-cbind(aux1,rgamma(nrec,alpha[j,j1],1))}
      tmpf = function(x){x/sum(x)}
      aux2 = t(apply(aux1,1,tmpf))
      aux3 = t(apply(rbind(ceiling(aux2[,1:m]*(24-m))),1,cumsum))+1
      tmpf = function(x){paste(x,collapse="-")}
      aux4 = apply(aux3,1,tmpf)
      #      if(sum(aux4=="1-10")==1)
      #      print(j)
      #      print(aux4)
      aux4
    }
    
    aux1 = mclapply(1:nrow(alpha),rsample, mc.cores=48)
    aux2 = aux1[[1]]
    if(nrow(alpha) >1)
    {
      for(j in 2:nrow(alpha)){aux2 = c(aux2,aux1[[j]])}
    }
    aux1 = table(aux2)
    aux2 = names(aux1)
    aux3 = rep(0,length(aux2))
    
    #     if(m == 2)
    #     aux1 = aux1*c(2,4,4)^(1-theta/(1+theta))
    
    Prob_fit = aux1/sum(aux1)
    
    
  }
  
  print(paste(m,"moves of",17))
  
  Prob_fit
}

Prob_Cond_Mov <- sapply((1:17)[-16],tmpf1)

freq1 = 1+1:length(Prob_Cond_Mov[[1]])
freq2 = Prob_Cond_Mov[[1]]*Prob_M[2,2]
for(j in 2:16)
{
  freq1 = c(freq1,names(Prob_Cond_Mov[[j]]))
  freq2 = c(freq2,Prob_Cond_Mov[[j]]*Prob_M[j+1,2])
}

prob1 = Prob_Z[[1]][,1]
prob2 = Prob_Z[[1]][,2]

for(j in c(2:15,17))
{
  prob1 = c(prob1,apply(Prob_Z[[j]][,1:j],1,tmpf<- function(x){paste(x,collapse="-")}))
  prob2 = c(prob2,Prob_Z[[j]][,j+1])
}


tmp3 = unique(c(freq1,prob1))

freq3 = numeric_agency(cbind(freq1),tmp3,TRUE)
prob3 = numeric_agency(cbind(prob1),tmp3,TRUE)


freq4 = rep(0,length(tmp3)); freq4[freq3]=freq2
prob4 = rep(0,length(tmp3)); prob4[prob3]=prob2

tmp1 = freq4>0 & prob4>0

Range = range(freq4[tmp1]/sum(freq4[tmp1]),prob4[tmp1]/sum(prob4[tmp1]))

plot(freq4[tmp1]/sum(freq4[tmp1]),prob4[tmp1]/sum(prob4[tmp1]), lwd=2, col = 'gray',main="MOV",cex=0.5,
xlim=Range,ylim=Range)
lines(seq(1e-5,1e4,len=1e4), (seq(1e-5,1e4,len=1e4)) , col = 2)

Range[2] = Range[2]*0.1

plot(freq4[tmp1]/sum(freq4[tmp1]),prob4[tmp1]/sum(prob4[tmp1]), lwd=2, col = 'gray',main="MOV",cex=0.5,
xlim=Range,ylim=Range)
lines(seq(1e-5,1e4,len=1e4), (seq(1e-5,1e4,len=1e4)) , col = 2)

tmp3[abs(prob4-freq4)==max(abs(prob4-freq4))]

