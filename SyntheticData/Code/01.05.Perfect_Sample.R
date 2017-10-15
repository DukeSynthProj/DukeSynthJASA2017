setwd("~/Path/SyntheticData")

library(parallel)

load("Aux_Files/bs_agency.RData")
bs = bs_agency
bs_num = 1*(bs == 0)

last_year = simplify2array(mclapply(1:nrow(bs),function(j){tail((1:24)[bs_num[j,]==0],1)},mc.cores=48))
last_agency = simplify2array(mclapply(1:nrow(bs),function(j){ paste(tail(bs[j,][1:last_year[j]],1),collapse="-")},mc.cores=48))

init_year = simplify2array(mclapply(1:nrow(bs),function(j){head((1:24)[bs_num[j,]==0],1)},mc.cores=48))
init_agency = simplify2array(mclapply(1:nrow(bs),function(j){paste(head(bs[j,][bs_num[j,]==0],1),collapse="-")},mc.cores=48))

#total_year = simplify2array(mclapply(1:nrow(bs),function(j){sum(bs_num[j,]==0)},mc.cores=48))

collapse = cbind(init_year,init_agency,last_year,last_agency)#,total_year)
collapse = simplify2array(mclapply(1:nrow(bs),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))


load(paste("Synthetic_Files/bs_synthetic_agency_long_Theta_",theta,".RData",sep=""))
bs_synthetic = bs_synthetic_agency_long

bs_synthetic_num = 1*(bs_synthetic == 0)

synthetic_last_year = simplify2array(mclapply(1:nrow(bs_synthetic),function(j){tail((1:24)[bs_synthetic_num[j,]==0],1)},mc.cores=48))
synthetic_last_agency = simplify2array(mclapply(1:nrow(bs_synthetic),function(j){paste(tail(bs_synthetic[j,][1:synthetic_last_year[j]],1),collapse="-")},mc.cores=48))


synthetic_init_year = simplify2array(mclapply(1:nrow(bs_synthetic),function(j){head((1:24)[bs_synthetic_num[j,]==0],1)},mc.cores=48))
synthetic_init_agency = simplify2array(mclapply(1:nrow(bs_synthetic),function(j){paste(head(bs_synthetic[j,][bs_synthetic_num[j,]==0],1),collapse="-")},mc.cores=48))

#synthetic_total_year = simplify2array(mclapply(1:nrow(bs_synthetic),function(j){sum(bs_synthetic_num[j,]==0)},mc.cores=48))

head(bs_synthetic,5)
head(synthetic_last_agency,5)
head(synthetic_last_year,5)
head(synthetic_init_agency,5)
head(synthetic_init_year,5)


synthetic_collapse = cbind(synthetic_init_year,synthetic_init_agency,synthetic_last_year,synthetic_last_agency) #,synthetic_total_year)
synthetic_collapse = simplify2array(mclapply(1:nrow(bs_synthetic),function(j){paste(synthetic_collapse[j,],collapse="-")},mc.cores=48))



table_collapse = table(collapse)
synthetic_table_collapse = table(synthetic_collapse)

tmp0 <- collapse
tmp1 <- synthetic_collapse
Tr <- c(rep(0,length(collapse)),rep(1,length(synthetic_collapse)))
tmp <- as.factor(c(as.vector(tmp0),as.vector(tmp1)))
#Levels <- levels(tmp) 
#tmp <- as.numeric(tmp)
output <- table(tmp,Tr)
tail(output[order(output[,2]),])


synthetic_names_collapse = names(synthetic_table_collapse)


Perfect.sample <- function(J1)
{
  N.collapse = table_collapse[J1]
  
  N.synthetic.collapse = 0
  synthetic.records = 0
  
  if(sum(names(N.collapse)==synthetic_names_collapse)==1)
    N.synthetic.collapse = synthetic_table_collapse[names(N.collapse)==synthetic_names_collapse]
  
  if(N.collapse < N.synthetic.collapse & N.synthetic.collapse>0)
    synthetic.records = sample(which(synthetic_collapse == names(N.collapse)),N.collapse)
  
  if(N.collapse>=N.synthetic.collapse & N.synthetic.collapse>0)
    synthetic.records = which(synthetic_collapse == names(N.collapse))
  
  if(ceiling(J1/100)-J1/100 ==0)
    print(paste(J1,"of",length(table_collapse)))
  
  synthetic.records
}


Sample = mclapply(1:length(table_collapse),Perfect.sample,mc.cores=48)
#load("Sample.RData")

N.Sample = simplify2array(mclapply(1:length(Sample),function(j){length(Sample[[j]])},mc.cores=48))
tmp = rep(0,sum(N.Sample))
tmp[1:N.Sample[1]] = Sample[[1]]
for(J1 in 2:length(Sample)){tmp[sum(N.Sample[1:(J1-1)])+1:N.Sample[J1]] = Sample[[J1]]}
Sample.1 = tmp[tmp!=0]

#save(Sample,file="Sample.RData")
#Sample.1 = sample(Sample.1,(1-Noise)*length(Sample.1))

Ind = rep(0,nrow(bs_synthetic))
Ind[Sample.1] = 1
tmp = sample(which(Ind == 0),nrow(bs)-sum(Ind))
Ind[tmp] = 1

bs_synthetic_agency = bs_synthetic[Ind==1,]
save(bs_synthetic_agency,file="Synthetic_Files/bs_synthetic_agency.RData")

##################
##################

Year = 1:2

collapse1 = simplify2array(mclapply(1:nrow(bs),function(j){paste(bs_agency[j,Year],collapse="-")},mc.cores=48))

collapse2 = simplify2array(mclapply(1:nrow(bs),function(j){paste(bs_synthetic_agency[j,Year],collapse="-")},mc.cores=48))

tmp = table(data.frame(rbind(cbind(collapse1,1),cbind(collapse2,2))))
tmp= tmp[-which(rownames(tmp) == paste(rep(0,length(Year)),collapse="-")),]

par(mfrow=c(2,3))
plot(tmp[,1],tmp[,2],xlab="original",ylab="synthetic",ylim=c(0,max(tmp)),xlim=c(0,max(tmp)))
abline(a=0,b=1,col=2)
plot(tmp[,1],tmp[,2],xlab="original",ylab="synthetic",ylim=c(0,5e4),xlim=c(0,5e4))
abline(a=0,b=1,col=2)
plot(tmp[,1],tmp[,2],xlab="original",ylab="synthetic",ylim=c(0,5e3),xlim=c(0,5e3))
abline(a=0,b=1,col=2)
plot(tmp[,1],tmp[,2],xlab="original",ylab="synthetic",ylim=c(0,1e3),xlim=c(0,1e3))
abline(a=0,b=1,col=2)
plot(tmp[,1],tmp[,2],xlab="original",ylab="synthetic",ylim=c(0,5e2),xlim=c(0,5e2))
abline(a=0,b=1,col=2)
plot(tmp[,1],tmp[,2],xlab="original",ylab="synthetic",ylim=c(0,1e2),xlim=c(0,1e2))
abline(a=0,b=1,col=2)




