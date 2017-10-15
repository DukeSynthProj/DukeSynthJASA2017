setwd("~/Path/SyntheticData")

J1 = 1

load(paste("Aux_Files/data_occ_year_",J1,".RData",sep=""))
load("Aux_Files/bs_occ.RData")
for(j in 1:24)
{
  bs_occ[is.na(bs_occ[,j]),j]="NA.occ"
  bs_occ[bs_occ[,j]=="NA",j]="NA.occ"
}

load(paste("Synthetic_Files/data_synthetic_occ_year_",J1,".RData",sep=""))
load("Synthetic_Files/bs_synthetic_agency.RData")
bs_synthetic_occ = bs_synthetic_agency

library(Matching)
library(parallel)
library(tree)

source("Code/99.02.cartdraw.R")
source("Code/99.03.cartdraw2.R")

Ind_synthetic_occ = data_synthetic_occ$agency!=0
Ind_occ = data_occ$agency!=0

data_synthetic_occ = data_synthetic_occ[Ind_synthetic_occ,]
data_occ = data_occ[Ind_occ,]

###### Determining init_agerange

#### Collapsed variables -- used to split the data
#Previos race
mcl.collapse <- function(collapse)
  simplify2array(mclapply(1:nrow(collapse),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

collapse.1 = mcl.collapse(cbind(
  as.vector(data_occ$agency),
  as.vector(data_occ$gender),
  as.vector(data_occ$educ_lvl),
  as.vector(cut(data_occ$total_year,breaks=(0:5)*5))))

synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(data_synthetic_occ$agency),
  as.vector(data_synthetic_occ$gender),
  as.vector(data_synthetic_occ$educ_lvl),
  as.vector(cut(data_synthetic_occ$total_year,breaks=(0:5)*5))))

print(cbind(head(collapse.1,20),head(synthetic_collapse.1,20)))
print(mean(synthetic_collapse.1 %in% collapse.1))


collapse.2 = mcl.collapse(cbind(
  as.vector(data_occ$agency),
  as.vector(data_occ$gender),
  as.vector(data_occ$educ_lvl)))

synthetic_collapse.2 = mcl.collapse(cbind(
  as.vector(data_synthetic_occ$agency),
  as.vector(data_synthetic_occ$gender),
  as.vector(data_synthetic_occ$educ_lvl)))

print(cbind(head(collapse.2,20),head(synthetic_collapse.2,20)))
print(mean(synthetic_collapse.2 %in% collapse.2))


collapse.3 = mcl.collapse(cbind(
  as.vector(data_occ$agency),
  as.vector(data_occ$gender)))

synthetic_collapse.3 = mcl.collapse(cbind(
  as.vector(data_synthetic_occ$agency),
  as.vector(data_synthetic_occ$gender)))

print(cbind(head(collapse.3,20),head(synthetic_collapse.3,20)))
print(mean(synthetic_collapse.3 %in% collapse.3))



Collapse = cbind(
  as.vector(collapse.1),
  as.vector(collapse.2),
  as.vector(collapse.3))

Synthetic_Collapse = cbind(
  as.vector(synthetic_collapse.1),
  as.vector(synthetic_collapse.2),
  as.vector(synthetic_collapse.3))


Ind_collapse = matrix(FALSE,nrow = nrow(Collapse),ncol = ncol(Collapse))
Ind_synthetic_collapse = matrix(FALSE,nrow = nrow(Synthetic_Collapse),ncol = ncol(Synthetic_Collapse))

# -- 1
Ind = Collapse[,1] %in% Synthetic_Collapse[,1]
unique_collapse = unique(Collapse[Ind])

Ind_synthetic_collapse[,1] = Synthetic_Collapse[,1] %in% unique_collapse
Ind_collapse[,1] = Collapse[,1] %in% unique_collapse
print(apply(Ind_synthetic_collapse,2,sum))


for(j in 2:ncol(Synthetic_Collapse))
{
  
  Ind = Collapse[,j] %in% Synthetic_Collapse[,j] 
  unique_collapse = unique(Collapse[Ind,j])
  tmp = unique(Synthetic_Collapse[apply(Ind_synthetic_collapse[,1:(j-1),drop=FALSE],1,sum)==0,j])
  
  unique_collapse = unique_collapse[unique_collapse %in% tmp]
  
  Ind_collapse[,j] = Collapse[,j] %in% unique_collapse
  
  Ind_synthetic_collapse[,j] = Synthetic_Collapse[,j] %in% unique_collapse & apply(Ind_synthetic_collapse[,1:(j-1),drop=FALSE],1,sum)==0
  
  print(apply(Ind_synthetic_collapse,2,sum)); print(table(apply(Ind_synthetic_collapse,1,sum)))
}

variables = c("agency", "gender", "race", "educ_lvl", "age", "init_yrsdegrng", "milmonths", "total_year",
              "number_agency",  "M" , "collapse")

synthetic_occ = rep(0,nrow(data_synthetic_occ))

for(J in ncol(Synthetic_Collapse):1)
{
  if(sum( Ind_synthetic_collapse[,J]) > 0)
  {
    
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J, "occ"))
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
    
    
    
    data_occ_tmp = data.frame(data_occ,collapse=Collapse[,J])
    data_synthetic_occ_tmp = data.frame(data_synthetic_occ, collapse=Synthetic_Collapse[,J])
    
    
    ## Start Algorithm
    data_general=data_occ_tmp[Ind_collapse[,J] ,,drop=FALSE]
    data_synthetic_general=data_synthetic_occ_tmp[Ind_synthetic_collapse[,J],,drop=FALSE]
    Goal="occ"
    variables=variables
    N.samples=1
    
    
    # Getting collapse for mono_table, mono_tree and multi_tree
    cores.table=30
    source("Code/06.02.01.Init_mono_multi.R")
    
    
    # Matching with mono_table
    pre.cores.table = 20
    cores.table = 30
    split.mono.table=15000
    cores.mono.table=40
    source("Code/06.02.02.mono_table.R")
    
    
    # Fitting mono_tree
    split.mono.tree=10000
    cores.mono.tree=30
    N.cartdraw = 10000
    cores.cartdraw = 2
    source("Code/06.02.03.mono_tree.R")
    
    
    # Fitting multi_tree
    if(length(multi_tree)>0)
    {
      BIG = 1e4
      backup_multi_tree = multi_tree
      source("Code/06.02.04.Init_multi_tree.R")
      # Big
      if(length(BIG_multi_tree)>0)
      {
        
        data_general_multi_tree_Big = data_general_multi_tree
        data_synthetic_general_multi_tree_Big = data_synthetic_general_multi_tree
        
        multi_tree = BIG_multi_tree
        N.split = min(c(BIG,1e4))
        cores.forest = 40
        N.cartdraw = 10000
        cores.cartdraw.pred = 4
        source("Code/06.02.05.Big_multi_tree.R")
      }
      # Small
      if(length(Small_multi_tree)>0)
      {
        multi_tree = Small_multi_tree
        split.multi.tree=5000
        cores.multi.tree=30
        N.cartdraw = 10000
        cores.cartdraw = 2
        source("Code/06.02.06.multi_tree.R")
      }
    }
    
    
    # End
    synthetic_occ[Ind_synthetic_collapse[,J]] = synthetic_Goal
    
  }
}

print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("synthetic_occ"))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(tail(sort(table(synthetic_occ))))
print(tail(sort(table(data_occ$occ))))

data_synthetic_occ = data.frame(data_synthetic_occ, occ = synthetic_occ)



collapse.1 = mcl.collapse(cbind(as.vector(data_occ$agency),as.vector(data_occ$occ)))
synthetic_collapse.1 = mcl.collapse(cbind(as.vector(data_synthetic_occ$agency),synthetic_occ))
print(cbind(head(collapse.1,20),head(synthetic_collapse.1,20)))
print(mean(synthetic_collapse.1 %in% collapse.1))




data_tmp1 = data.frame(data_occ[collapse.1 %in% names(table(collapse.1))[table(collapse.1)<5],], Tr = 1)
data_tmp2 = data.frame(data_synthetic_occ[data_synthetic_occ$agency  %in% unique(data_tmp1$agency) ,], Tr = 0)
data_tmp1 = data_tmp1[data_tmp1$agency  %in% unique(data_tmp2$agency) ,]

data_collapse  = rbind(data_tmp1,data_tmp2)


J2 = 1

collapse_agency = as.vector(data_collapse$agency)
unique_agency = names(sort(table(collapse_agency),decreasing=F))
unique_agency = unique_agency[unique_agency != "0"]


f_occ<-function(J2)
{
  Ind2 = collapse_agency == unique_agency[J2]
  data_tmp = data_collapse[Ind2,]
  
  
  educ_lvl = data_tmp$educ_lvl
  Tr_educ = (data_tmp$educ_lvl == "NA.educ_lvl")*1
  
  if(sum(Tr_educ)>=1 & mean(Tr_educ==1)<0.5)
  {
    
    fit = list()
    
    try(fit$age <- glm(Tr~age,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$age)){fit$age = rep(0,nrow(data_tmp))}
    
    try(fit$gender <- glm(Tr~gender,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$gender)){fit$gender = rep(0,nrow(data_tmp))}
    
    try(fit$race <- glm(Tr~race,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$race)){fit$race = rep(0,nrow(data_tmp))}
    
    #      try(fit$init_instrctpgm <- glm(Tr~init_instrctpgm,data=data_tmp, family = binomial)$fitted.values)
    #      if(is.null(fit$init_instrctpgm)){fit$init_instrctpgm = rep(0,nrow(data_tmp))}
    
    w = c(40,5,5,40)/100
    pTr_edu = 
      w[1]*fit$age + 
      w[2]*fit$gender + 
      w[3]*fit$race   
    #        w[4]*fit$init_instrctpgm
    
    
    Match.tmp = Match(Tr=Tr_educ, X = pTr_edu, M=1, ties=FALSE,replace=FALSE)$Ma 
    Ind5 = Match.tmp[,1]
    Ind6 = Match.tmp[,2]
    educ_lvl[Ind5] = educ_lvl[Ind6]
    
    data_tmp$educ_lvl = educ_lvl
  }
  
  
  Ind3 = data_tmp$Tr
  Tr = data_tmp$Tr
  G_O = as.vector(data_tmp$occ)
  
  
  if(sum(Tr_educ)==0 | mean(Tr_educ)>=0.5)
  {
    fit = list()
    try(fit$age <- glm(Tr~age,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$age)){fit$age = rep(0,nrow(data_tmp))}
    
    try(fit$gender <- glm(Tr~gender,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$gender)){fit$gender = rep(0,nrow(data_tmp))}
    
    try(fit$race <- glm(Tr~race,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$race)){fit$race = rep(0,nrow(data_tmp))}
    
    #      try(fit$init_instrctpgm <- glm(Tr~init_instrctpgm,data=data_tmp, family = binomial)$fitted.values)
    #      if(is.null(fit$init_instrctpgm)){fit$init_instrctpgm = rep(0,nrow(data_tmp))}
    
  }
  
  if(mean(Tr_educ==1)<0.5)
    try(fit$educ <- glm(Tr~as.numeric(educ_lvl),data=data_tmp, family = binomial)$fitted.values)
  if(mean(Tr_educ==1)>=0.5)
    try(fit$educ <- glm(Tr~educ_lvl,data=data_tmp, family = binomial)$fitted.values)
  if(is.null(fit$educ)){fit$educ = rep(0,nrow(data_tmp))}
  
  try(fit$init_yrsdegrng <- glm(Tr~init_yrsdegrng,data=data_tmp, family = binomial)$fitted.values)
  if(is.null(fit$init_yrsdegrng)){fit$init_yrsdegrng = rep(0,nrow(data_tmp))}
  
  try(fit$milmonths <- glm(Tr~milmonths,data=data_tmp, family = binomial)$fitted.values)
  if(is.null(fit$milmonths)){fit$milmonths = rep(0,nrow(data_tmp))}
  
  try(fit$total_year <- glm(Tr~total_year,data=data_tmp, family = binomial)$fitted.values)
  if(is.null(fit$total_year)){fit$total_year = rep(0,nrow(data_tmp))}
  
  try(fit$number_agency <- glm(Tr~number_agency,data=data_tmp, family = binomial)$fitted.values)
  if(is.null(fit$number_agency)){fit$number_agency = rep(0,nrow(data_tmp))}
  
  try(fit$M  <- glm(Tr~M,data=data_tmp, family = binomial)$fitted.values)
  if(is.null(fit$M)){fit$M = rep(0,nrow(data_tmp))}
  
  table_occ = table(as.vector(data_tmp$occ),as.vector(data_tmp$Tr))
  table_occ[,2] = table_occ[,2]/apply(table_occ,1,sum)
  fit$occ = rep(0,nrow(data_tmp))
  for(j in 1:nrow(table_occ)){fit$occ[data_tmp$occ == rownames(table_occ)[j]] = table_occ[j,2] }

#  try(fit$occ  <- glm(Tr~occ,data=data_tmp, family = binomial)$fitted.values)
#  if(is.null(fit$occ)){fit$occ = rep(0,nrow(data_tmp))}
  
  
  w = c(200,100,50,50,5,1,1,1,1,1)/100
  pTr_glmnet = 
    w[1]*fit$occ + 
    w[2]*fit$educ + 
    w[3]*fit$age + 
    w[4]*fit$race  + 
    w[5]*fit$gender +
    w[6]*fit$init_yrsdegrng +
    w[7]*fit$milmonths +
    w[8]*fit$total_year +
    w[9]*fit$number_agency+ 
    w[10]*fit$M
  
  
  G_O_2 =  G_O
  
  n_O = sum(data_tmp$Tr == 1)
  n_S = sum(data_tmp$Tr == 0)
  Tr  = data_tmp$Tr     
  
  if(n_O > n_S)
  {
    print(paste("J2 - n_O > n_S",J2))
    Match.tmp = Match(Tr=1-Tr, X = pTr_glmnet, Z = data_tmp$occ, M=1, ties=FALSE,replace=FALSE)$Ma 
    Ind5 = Match.tmp[,1]
    Ind6 = Match.tmp[,2]
    G_O_2[Ind6] = G_O[Ind5]
  }
  
  if(n_O <= n_S)
  {
    Match.tmp = Match(Tr=Tr, X = pTr_glmnet, Z = data_tmp$occ, M=1, ties=FALSE,replace=FALSE)$Ma 
    Ind5 = Match.tmp[,2]
    Ind6 = Match.tmp[,1]
    G_O_2[Ind5] = G_O[Ind6]
  }
  
  print(paste("J2",J2))
  print(sort(table(as.vector(data_occ[data_occ$agency==unique_agency[J2],]$occ))))
  print(sort(table(G_O_2[Tr==0])))
  
  list(J2 = J2, G_O_2[Tr==0])
  
}

system.time(tmp <- mclapply(length(unique_agency):1,f_occ,mc.cores=48,mc.preschedule = FALSE))

for(j in length(unique_agency):1)
  synthetic_occ[data_synthetic_occ$agency == unique_agency[tmp[[j]]$J2]]=tmp[[j]][[2]]

bs_synthetic_occ[bs_synthetic_occ[,J1] != "0",J1] = synthetic_occ


#####################################################
#####################################################
##########################
## Other Years
##########################

for(J1 in 2:24)
{
  
  load(paste("Aux_Files/data_occ_year_",J1,".RData",sep=""))
  load(paste("Synthetic_Files/data_synthetic_occ_year_",J1,".RData",sep=""))
  
  Ind_synthetic_occ = data_synthetic_occ$agency!=0
  Ind_occ = data_occ$occ!=0
  
  f<-function(j,Lag)
  {
    tmp = "0"
    if(sum(bs_occ[j,1:(J1-1)]!="0")>0)
      tmp = rev(tail(bs_occ[j,1:(J1-1)][bs_occ[j,1:(J1-1)]!="0"],5))[Lag]
    tmp
  }
  last_occ.1 = simplify2array(mclapply(1:nrow(bs_occ),f,Lag=1,mc.cores=48))
  last_occ.2 = simplify2array(mclapply(1:nrow(bs_occ),f,Lag=2,mc.cores=48))
  last_occ.2[is.na(last_occ.2)]="Empty"
  last_occ.3 = simplify2array(mclapply(1:nrow(bs_occ),f,Lag=3,mc.cores=48))
  last_occ.3[is.na(last_occ.3)]="Empty"
  
  data_occ = data.frame(data_occ, last_occ.1=last_occ.1, last_occ.2=last_occ.2, last_occ.3=last_occ.3)
  data_occ =  data_occ[Ind_occ,]

  
  f<-function(j,Lag)
  {
    tmp = "0"
    if(sum(bs_synthetic_occ[j,1:(J1-1)]!="0")>0)
      tmp = rev(tail(bs_synthetic_occ[j,1:(J1-1)][bs_synthetic_occ[j,1:(J1-1)]!="0"],5))[Lag]
    tmp
  }
  last_occ.1 = simplify2array(mclapply(1:nrow(bs_synthetic_occ),f,Lag=1,mc.cores=48))
  last_occ.2 = simplify2array(mclapply(1:nrow(bs_synthetic_occ),f,Lag=2,mc.cores=48))
  last_occ.2[is.na(last_occ.2)]="Empty"
  last_occ.3 = simplify2array(mclapply(1:nrow(bs_synthetic_occ),f,Lag=3,mc.cores=48))
  last_occ.3[is.na(last_occ.3)]="Empty"
  
  data_synthetic_occ = data.frame(data_synthetic_occ, last_occ.1=last_occ.1, last_occ.2=last_occ.2, last_occ.3=last_occ.3)
  data_synthetic_occ = data_synthetic_occ[Ind_synthetic_occ,]
  
  
  collapse.1 = mcl.collapse(cbind(
    as.vector(data_occ$agency),
    as.vector(data_occ$gender),
    as.vector(data_occ$educ_lvl),
    as.vector(cut(data_occ$total_year,breaks=(0:5)*5)),
    as.vector(data_occ$last_occ.1),
    as.vector(data_occ$last_occ.2),
    as.vector(data_occ$last_occ.3)))
  
  synthetic_collapse.1 = mcl.collapse(cbind(
    as.vector(data_synthetic_occ$agency),
    as.vector(data_synthetic_occ$gender),
    as.vector(data_synthetic_occ$educ_lvl),
    as.vector(cut(data_synthetic_occ$total_year,breaks=(0:5)*5)),
    as.vector(data_synthetic_occ$last_occ.1),
    as.vector(data_synthetic_occ$last_occ.2),
    as.vector(data_synthetic_occ$last_occ.3)))
  
  print(cbind(head(collapse.1,20),head(synthetic_collapse.1,20)))
  print(mean(synthetic_collapse.1 %in% collapse.1))
  
  
  collapse.2 = mcl.collapse(cbind(
    as.vector(data_occ$agency),
    as.vector(data_occ$gender),
    as.vector(data_occ$educ_lvl),
    as.vector(data_occ$last_occ.1),
    as.vector(data_occ$last_occ.2),
    as.vector(data_occ$last_occ.3)))
  
  synthetic_collapse.2 = mcl.collapse(cbind(
    as.vector(data_synthetic_occ$agency),
    as.vector(data_synthetic_occ$gender),
    as.vector(data_synthetic_occ$educ_lvl),
    as.vector(data_synthetic_occ$last_occ.1),
    as.vector(data_synthetic_occ$last_occ.2),
    as.vector(data_synthetic_occ$last_occ.3)))
  
  print(cbind(head(collapse.2,20),head(synthetic_collapse.2,20)))
  print(mean(synthetic_collapse.2 %in% collapse.2))
  
  
  collapse.3 = mcl.collapse(cbind(
    as.vector(data_occ$agency),
    as.vector(data_occ$gender),
    as.vector(data_occ$educ_lvl),
    as.vector(data_occ$last_occ.1),
    as.vector(data_occ$last_occ.2)))
  
  synthetic_collapse.3 = mcl.collapse(cbind(
    as.vector(data_synthetic_occ$agency),
    as.vector(data_synthetic_occ$gender),
    as.vector(data_synthetic_occ$educ_lvl),
    as.vector(data_synthetic_occ$last_occ.1),
    as.vector(data_synthetic_occ$last_occ.2)))
  
  print(cbind(head(collapse.3,20),head(synthetic_collapse.3,20)))
  print(mean(synthetic_collapse.3 %in% collapse.3))
  
  
  collapse.4 = mcl.collapse(cbind(
    as.vector(data_occ$agency),
    as.vector(data_occ$gender),
    as.vector(data_occ$educ_lvl),
    as.vector(data_occ$last_occ.1)))
  
  synthetic_collapse.4 = mcl.collapse(cbind(
    as.vector(data_synthetic_occ$agency),
    as.vector(data_synthetic_occ$gender),
    as.vector(data_synthetic_occ$educ_lvl),
    as.vector(data_synthetic_occ$last_occ.1)))
  
  print(cbind(head(collapse.4,20),head(synthetic_collapse.4,20)))
  print(mean(synthetic_collapse.4 %in% collapse.4))
  
  
  collapse.5 = mcl.collapse(cbind(
    as.vector(data_occ$agency),
    as.vector(data_occ$gender),
    as.vector(data_occ$educ_lvl)))
  
  synthetic_collapse.5 = mcl.collapse(cbind(
    as.vector(data_synthetic_occ$agency),
    as.vector(data_synthetic_occ$gender),
    as.vector(data_synthetic_occ$educ_lvl)))
  
  print(cbind(head(collapse.5,20),head(synthetic_collapse.5,20)))
  print(mean(synthetic_collapse.5 %in% collapse.5))
  

  collapse.6 = mcl.collapse(cbind(
    as.vector(data_occ$agency),
    as.vector(data_occ$gender)))
  
  synthetic_collapse.6 = mcl.collapse(cbind(
    as.vector(data_synthetic_occ$agency),
    as.vector(data_synthetic_occ$gender)))
  
  print(cbind(head(collapse.6,20),head(synthetic_collapse.6,20)))
  print(mean(synthetic_collapse.5 %in% collapse.5))
  
  collapse.7 = mcl.collapse(cbind(
    as.vector(data_occ$agency)))
  
  synthetic_collapse.7 = mcl.collapse(cbind(
    as.vector(data_synthetic_occ$agency)))
  
  print(cbind(head(collapse.7,20),head(synthetic_collapse.7,20)))
  print(mean(synthetic_collapse.6 %in% collapse.6))
  
  
  Collapse = cbind(
    as.vector(collapse.1),
    as.vector(collapse.2),
    as.vector(collapse.3),
    as.vector(collapse.4),
    as.vector(collapse.5),
    as.vector(collapse.6),
    as.vector(collapse.7))
  
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1),
    as.vector(synthetic_collapse.2),
    as.vector(synthetic_collapse.3),
    as.vector(synthetic_collapse.4),
    as.vector(synthetic_collapse.5),
    as.vector(synthetic_collapse.6),
    as.vector(synthetic_collapse.7))
  
  
  Ind_collapse = matrix(FALSE,nrow = nrow(Collapse),ncol = ncol(Collapse))
  Ind_synthetic_collapse = matrix(FALSE,nrow = nrow(Synthetic_Collapse),ncol = ncol(Synthetic_Collapse))
  
  # -- 1
  Ind = Collapse[,1] %in% Synthetic_Collapse[,1]
  unique_collapse = unique(Collapse[Ind])
  
  Ind_synthetic_collapse[,1] = Synthetic_Collapse[,1] %in% unique_collapse
  Ind_collapse[,1] = Collapse[,1] %in% unique_collapse
  print(apply(Ind_synthetic_collapse,2,sum))
  
  
  for(j in 2:ncol(Synthetic_Collapse))
  {
    
    Ind = Collapse[,j] %in% Synthetic_Collapse[,j] 
    unique_collapse = unique(Collapse[Ind,j])
    tmp = unique(Synthetic_Collapse[apply(Ind_synthetic_collapse[,1:(j-1),drop=FALSE],1,sum)==0,j])
    
    unique_collapse = unique_collapse[unique_collapse %in% tmp]
    
    Ind_collapse[,j] = Collapse[,j] %in% unique_collapse
    
    Ind_synthetic_collapse[,j] = Synthetic_Collapse[,j] %in% unique_collapse & apply(Ind_synthetic_collapse[,1:(j-1),drop=FALSE],1,sum)==0
    
    print(apply(Ind_synthetic_collapse,2,sum)); print(table(apply(Ind_synthetic_collapse,1,sum)))
  }
  
  variables = c("agency", "gender", "race", "educ_lvl", "age", "init_yrsdegrng", "milmonths", "total_year",
                "number_agency",  "M",  "last_occ.1" , "collapse")
  
  
#  data_synthetic_occ = data_synthetic_occ[Ind_synthetic_occ,]
#  data_occ = data_occ[Ind_occ,]
  
  synthetic_occ = rep(0,nrow(data_synthetic_occ))
  
  
  for(J in ncol(Synthetic_Collapse):1)
  {
    if(sum( Ind_synthetic_collapse[,J]) > 0)
    {
      
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J, "occ -- Year",J1))
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
      
      
      
      data_occ_tmp = data.frame(data_occ,collapse=Collapse[,J])
      data_synthetic_occ_tmp = data.frame(data_synthetic_occ, collapse=Synthetic_Collapse[,J])
      
      
      ## Start Algorithm
      data_general=data_occ_tmp[Ind_collapse[,J] ,,drop=FALSE]
      data_synthetic_general=data_synthetic_occ_tmp[Ind_synthetic_collapse[,J],,drop=FALSE]
      Goal="occ"
      variables=variables
      N.samples=1
      
      
      # Getting collapse for mono_table, mono_tree and multi_tree
      cores.table=30
      source("Code/06.02.01.Init_mono_multi.R")
      
      
      # Matching with mono_table
      pre.cores.table = 20
      cores.table = 30
      split.mono.table=15000
      cores.mono.table=40
      source("Code/06.02.02.mono_table.R")
      
      
      # Fitting mono_tree
      split.mono.tree= 5000
      cores.mono.tree=30
      N.cartdraw = 10000
      cores.cartdraw = 2
      source("Code/06.02.03.mono_tree.R")
      
      
      # Fitting multi_tree
      if(length(multi_tree)>0)
      {
        BIG = 1e6
        backup_multi_tree = multi_tree
        source("Code/06.02.04.Init_multi_tree.R")
        # Big
        if(length(BIG_multi_tree)>0)
        {
          
          data_general_multi_tree_Big = data_general_multi_tree
          data_synthetic_general_multi_tree_Big = data_synthetic_general_multi_tree
          
          multi_tree = BIG_multi_tree
          N.split = min(c(BIG,1e4))
          cores.forest = 40
          N.cartdraw = 10000
          cores.cartdraw.pred = 4
          source("Code/06.02.05.Big_multi_tree.R")
        }
        # Small
        if(length(Small_multi_tree)>0)
        {
          multi_tree = Small_multi_tree
          split.multi.tree=5000
          cores.multi.tree=30
          N.cartdraw = 10000
          cores.cartdraw = 2
          source("Code/06.02.06.multi_tree.R")
        }
      }
      
      
      # End
      synthetic_occ[Ind_synthetic_collapse[,J]] = synthetic_Goal
      
    }
  }
  
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("synthetic_occ -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_occ))))
  print(tail(sort(table(data_occ$occ))))
  
  
  
  data_synthetic_occ = data.frame(data_synthetic_occ, occ = synthetic_occ)
  
  
  
  collapse.1 = mcl.collapse(cbind(as.vector(data_occ$agency),as.vector(data_occ$occ)))
  synthetic_collapse.1 = mcl.collapse(cbind(as.vector(data_synthetic_occ$agency),synthetic_occ))
  print(cbind(head(collapse.1,20),head(synthetic_collapse.1,20)))
  print(mean(synthetic_collapse.1 %in% collapse.1))
  
  
  
  
  data_tmp1 = data.frame(data_occ[collapse.1 %in% names(table(collapse.1))[table(collapse.1)<5],], Tr = 1)
  data_tmp2 = data.frame(data_synthetic_occ[data_synthetic_occ$agency  %in% unique(data_tmp1$agency) ,], Tr = 0)
  data_tmp1 = data_tmp1[data_tmp1$agency  %in% unique(data_tmp2$agency) ,]
  
  data_collapse  = rbind(data_tmp1,data_tmp2)
  
  
  J2 = 50
  
  collapse_agency = as.vector(data_collapse$agency)
  unique_agency = names(sort(table(collapse_agency),decreasing=F))
  unique_agency = unique_agency[unique_agency != "0"]
  
  
  f_occ<-function(J2)
  {
    Ind2 = collapse_agency == unique_agency[J2]
    data_tmp = data_collapse[Ind2,]
    
    
    educ_lvl = data_tmp$educ_lvl
    Tr_educ = (data_tmp$educ_lvl == "NA.educ_lvl")*1
    
    if(sum(Tr_educ)>=1 & mean(Tr_educ==1)<0.5)
    {
      
      fit = list()
      
      try(fit$age <- glm(Tr~age,data=data_tmp, family = binomial)$fitted.values)
      if(is.null(fit$age)){fit$age = rep(0,nrow(data_tmp))}
      
      try(fit$gender <- glm(Tr~gender,data=data_tmp, family = binomial)$fitted.values)
      if(is.null(fit$gender)){fit$gender = rep(0,nrow(data_tmp))}
      
      try(fit$race <- glm(Tr~race,data=data_tmp, family = binomial)$fitted.values)
      if(is.null(fit$race)){fit$race = rep(0,nrow(data_tmp))}
      
      #      try(fit$init_instrctpgm <- glm(Tr~init_instrctpgm,data=data_tmp, family = binomial)$fitted.values)
      #      if(is.null(fit$init_instrctpgm)){fit$init_instrctpgm = rep(0,nrow(data_tmp))}
      
      w = c(40,5,5,40)/100
      pTr_edu = 
        w[1]*fit$age + 
        w[2]*fit$gender + 
        w[3]*fit$race   
      #        w[4]*fit$init_instrctpgm
      
      
      Match.tmp = Match(Tr=Tr_educ, X = pTr_edu, M=1, ties=FALSE,replace=FALSE)$Ma 
      Ind5 = Match.tmp[,1]
      Ind6 = Match.tmp[,2]
      educ_lvl[Ind5] = educ_lvl[Ind6]
      
      data_tmp$educ_lvl = educ_lvl
    }
    
    
    Ind3 = data_tmp$Tr
    Tr = data_tmp$Tr
    G_O = as.vector(data_tmp$occ)
    
    
    if(sum(Tr_educ)==0 | mean(Tr_educ)>=0.5)
    {
      fit = list()
      try(fit$age <- glm(Tr~age,data=data_tmp, family = binomial)$fitted.values)
      if(is.null(fit$age)){fit$age = rep(0,nrow(data_tmp))}
      
      try(fit$gender <- glm(Tr~gender,data=data_tmp, family = binomial)$fitted.values)
      if(is.null(fit$gender)){fit$gender = rep(0,nrow(data_tmp))}
      
      try(fit$race <- glm(Tr~race,data=data_tmp, family = binomial)$fitted.values)
      if(is.null(fit$race)){fit$race = rep(0,nrow(data_tmp))}
      
      #      try(fit$init_instrctpgm <- glm(Tr~init_instrctpgm,data=data_tmp, family = binomial)$fitted.values)
      #      if(is.null(fit$init_instrctpgm)){fit$init_instrctpgm = rep(0,nrow(data_tmp))}
      
    }
    
    if(mean(Tr_educ==1)<0.5)
      try(fit$educ <- glm(Tr~as.numeric(educ_lvl),data=data_tmp, family = binomial)$fitted.values)
    if(mean(Tr_educ==1)>=0.5)
      try(fit$educ <- glm(Tr~educ_lvl,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$educ)){fit$educ = rep(0,nrow(data_tmp))}
    
    try(fit$init_yrsdegrng <- glm(Tr~init_yrsdegrng,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$init_yrsdegrng)){fit$init_yrsdegrng = rep(0,nrow(data_tmp))}
    
    try(fit$milmonths <- glm(Tr~milmonths,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$milmonths)){fit$milmonths = rep(0,nrow(data_tmp))}
    
    try(fit$total_year <- glm(Tr~total_year,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$total_year)){fit$total_year = rep(0,nrow(data_tmp))}
    
    try(fit$number_agency <- glm(Tr~number_agency,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$number_agency)){fit$number_agency = rep(0,nrow(data_tmp))}
    
    try(fit$M  <- glm(Tr~M,data=data_tmp, family = binomial)$fitted.values)
    if(is.null(fit$M)){fit$M = rep(0,nrow(data_tmp))}
    
    table_occ = table(as.vector(data_tmp$occ),as.vector(data_tmp$Tr))
    table_occ[,2] = table_occ[,2]/apply(table_occ,1,sum)
    fit$occ = rep(0,nrow(data_tmp))
    for(j in 1:nrow(table_occ)){fit$occ[data_tmp$occ == rownames(table_occ)[j]] = table_occ[j,2] }

    table_occ = table(as.vector(data_tmp$last_occ.1),as.vector(data_tmp$Tr))
    table_occ[,2] = table_occ[,2]/apply(table_occ,1,sum)
    fit$last_occ.1 = rep(0,nrow(data_tmp))
    for(j in 1:nrow(table_occ)){fit$last_occ.1[data_tmp$last_occ.1 == rownames(table_occ)[j]] = table_occ[j,2] }


#    try(fit$occ  <- glm(Tr~occ,data=data_tmp, family = binomial)$fitted.values)
#    if(is.null(fit$occ)){fit$occ = rep(0,nrow(data_tmp))}
    
#    try(fit$last_occ.1  <- glm(Tr~last_occ.1,data=data_tmp, family = binomial)$fitted.values)
#    if(is.null(fit$last_occ.1)){fit$last_occ.1 = rep(0,nrow(data_tmp))}
    
    
    w = c(200,100,20,5,5,1,1,1,1,1,100)/100
    pTr_glmnet = 
      w[1]*fit$occ + 
      w[2]*fit$educ + 
      w[3]*fit$age + 
      w[4]*fit$race  + 
      w[5]*fit$gender +
      w[6]*fit$init_yrsdegrng +
      w[7]*fit$milmonths +
      w[8]*fit$total_year +
      w[9]*fit$number_agency+ 
      w[10]*fit$M + 
      w[11]*fit$last_occ.1
    
    
    G_O_2 =  G_O
    
    n_O = sum(data_tmp$Tr == 1)
    n_S = sum(data_tmp$Tr == 0)
    Tr  = data_tmp$Tr     
    
    if(n_O > n_S)
    {
      print(paste("J2 - n_O > n_S",J2))
      Match.tmp = Match(Tr=1-Tr, X = pTr_glmnet, Z = data_tmp$occ, M=1, ties=FALSE,replace=FALSE)$Ma 
      Ind5 = Match.tmp[,1]
      Ind6 = Match.tmp[,2]
      G_O_2[Ind6] = G_O[Ind5]
    }
    
    if(n_O <= n_S)
    {
      Match.tmp = Match(Tr=Tr, X = pTr_glmnet, Z = data_tmp$occ, M=1, ties=FALSE,replace=FALSE)$Ma 
      Ind5 = Match.tmp[,2]
      Ind6 = Match.tmp[,1]
      G_O_2[Ind5] = G_O[Ind6]
    }
    
    print(paste("J2",J2))
    print(sort(table(as.vector(data_occ[data_occ$agency==unique_agency[J2],]$occ))))
    print(sort(table(G_O_2[Tr==0])))
    
    list(J2 = J2, G_O_2[Tr==0])
    
  }
  
  system.time(tmp <- mclapply(length(unique_agency):1,f_occ,mc.cores=48,mc.preschedule = FALSE))
  
  for(j in length(unique_agency):1)
    synthetic_occ[data_synthetic_occ$agency == unique_agency[tmp[[j]]$J2]]=tmp[[j]][[2]]
  


  if(sum((bs_synthetic_occ[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in occ")
    print(which(((bs_synthetic_occ[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  print( t(head(bs_synthetic_occ,10)))

  bs_synthetic_occ[bs_synthetic_occ[,J1] != "0",J1] = synthetic_occ
  
J=J1
tmp = rbind(cbind(bs_occ[bs_occ[,J]!="0",J],0),cbind(bs_synthetic_occ[bs_synthetic_occ[,J]!="0",J],1))
table.occ = table(tmp[,1],tmp[,2])
table.occ = cbind(table.occ,abs(apply(table.occ,1,diff)))
table.occ=table.occ[order(table.occ[,3]),]
print(tail(table.occ,20))

  
}

# Checking zeros
print(paste("checking zeros",sum((bs_synthetic_occ=="0")-(bs_synthetic_agency=="0"))))

save(bs_synthetic_occ,file="Synthetic_Files/bs_synthetic_occ.RData")

