setwd("~/Path/SyntheticData")

load("Synthetic_Files/data_synthetic_educ.RData")
load("Synthetic_Files/bs_synthetic_agency.RData")

load("Aux_Files/data_educ.RData")
load("Aux_Files/bs_agency.RData")
load("Aux_Files/bs_new_educ_lvl.RData")

library(parallel)
library(tree)

source("Code/99.02.cartdraw.R")
source("Code/99.03.cartdraw2.R")


###### Determining whether Race is unique or not
data_educ_backup            = data_educ
bs_agency_backup    	    = bs_agency
bs_new_educ_lvl_backup      = bs_new_educ_lvl

data_synthetic_educ_backup = data_synthetic_educ
bs_synthetic_agency_backup = bs_synthetic_agency

#### Collapsed variables -- used to split the data
#Previos race
collapse = cbind(
  as.vector(data_educ$init_year), 
  as.vector(data_educ$init_agency),
  as.vector(data_educ$gender))
collapse = simplify2array(mclapply(1:nrow(data_educ),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

synthetic_collapse = cbind(
  as.vector(data_synthetic_educ$init_year), 
  as.vector(data_synthetic_educ$init_agency),
  as.vector(data_synthetic_educ$gender))
synthetic_collapse = simplify2array(mclapply(1:nrow(data_synthetic_educ),function(j){paste(synthetic_collapse[j,],collapse="-")},mc.cores=48))

print(cbind(head(collapse,20),head(synthetic_collapse,20)))
print(mean(synthetic_collapse %in% collapse))

data_educ = data.frame(data_educ,collapse=collapse)
data_synthetic_educ = data.frame(data_synthetic_educ,collapse=synthetic_collapse)

variables = c("init_year","last_year","total_year","mode_agency","init_agency",
              "number_agency","M","gaps","gender","init_race","collapse")


data_educ_tmp = data.frame(data_educ)
data_synthetic_educ_tmp = data.frame(data_synthetic_educ)


Ind_collapse = data_educ_tmp$init_agency != "0"
Ind_synthetic_collapse = data_synthetic_educ_tmp$init_agency != "0"

## Start Algorithm
data_general=data_educ_tmp[Ind_collapse,,drop=FALSE]
data_synthetic_general=data_synthetic_educ_tmp[Ind_synthetic_collapse,,drop=FALSE]
Goal="unique_educ"
data_general=data_general[data_general[,Goal]!="0",,drop=FALSE]
variables=variables
N.samples=1


# Getting collapse for mono_table, mono_tree and multi_tree
cores.table=30
source("Code/04.02.01.Init_mono_multi.R")


# Matching with mono_table
pre.cores.table = 20
cores.table = 30
split.mono.table=15000
cores.mono.table=40
source("Code/04.02.02.mono_table.R")


# Fitting mono_tree
split.mono.tree=10000
cores.mono.tree=30
N.cartdraw = 10000
cores.cartdraw = 2
source("Code/04.02.03.mono_tree.R")


# End
synthetic_unique_educ = synthetic_Goal
save(synthetic_unique_educ,file=paste("Synthetic_Files/synthetic_unique_educ.RData",sep=""))

print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print("unique_educ")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(table(synthetic_unique_educ))
print(table(data_educ$unique_educ))


#########################################################
#########################################################

###### Determining Race when race is unique
data_educ = data_educ_backup
data_synthetic_educ = data_synthetic_educ_backup 
bs_new_educ_lvl   = bs_new_educ_lvl_backup

f<-function(j){bs_new_educ_lvl[j,data_educ$init_year[j]]}
init_educ <- simplify2array(mclapply(1:nrow(bs_new_educ_lvl),f,mc.cores=48))
table(init_educ)

init_educ = init_educ[data_educ$unique_educ =="U"]
data_educ = data_educ[data_educ$unique_educ =="U", ]
data_educ = data.frame(data_educ,init_educ=init_educ)


data_synthetic_educ = data_synthetic_educ[synthetic_unique_educ =="U", ]



#### Collapsed variables -- used to split the data
#Previos race
collapse = cbind(
  as.vector(data_educ$init_year), 
  as.vector(data_educ$init_agency))
collapse = simplify2array(mclapply(1:nrow(data_educ),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

synthetic_collapse = cbind(
  as.vector(data_synthetic_educ$init_year), 
  as.vector(data_synthetic_educ$init_agency))
synthetic_collapse = simplify2array(mclapply(1:nrow(data_synthetic_educ),function(j){paste(synthetic_collapse[j,],collapse="-")},mc.cores=48))

print(cbind(head(collapse,20),head(synthetic_collapse,20)))
print(mean(synthetic_collapse %in% collapse))

data_educ = data.frame(data_educ,collapse=collapse)
data_synthetic_educ = data.frame(data_synthetic_educ,collapse=synthetic_collapse)

variables = c("init_year","last_year","total_year","mode_agency","init_agency",
              "number_agency","M","gaps","gender","init_race","collapse")


synthetic_init_educ = matrix("0",nrow=nrow(data_synthetic_educ),ncol=1) 


data_educ_tmp = data.frame(data_educ)
data_synthetic_educ_tmp = data.frame(data_synthetic_educ)


Ind_collapse = data_educ_tmp$init_agency != "0"
Ind_synthetic_collapse = data_synthetic_educ_tmp$init_agency != "0"

## Start Algorithm
data_general=data_educ_tmp[Ind_collapse,,drop=FALSE]
data_synthetic_general=data_synthetic_educ_tmp[Ind_synthetic_collapse,,drop=FALSE]
Goal="init_educ"
data_general=data_general[data_general[,Goal]!="0",,drop=FALSE]
variables=variables
N.samples=1


# Getting collapse for mono_table, mono_tree and multi_tree
cores.table=30
source("Code/04.02.01.Init_mono_multi.R")


# Matching with mono_table
pre.cores.table = 20
cores.table = 30
split.mono.table=15000
cores.mono.table=40
source("Code/04.02.02.mono_table.R")


# Fitting mono_tree
split.mono.tree=10000
cores.mono.tree=30
N.cartdraw = 10000
cores.cartdraw = 2
source("Code/04.02.03.mono_tree.R")


# End
save(synthetic_init_educ,file=paste("Synthetic_Files/synthetic_init_educ.RData",sep=""))
synthetic_init_educ = synthetic_Goal

print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print("init_educ")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(table(synthetic_init_educ))
print(table(data_educ$init_educ))


#########################################################
#########################################################
# YEAR 1
###### Determining educ_lvl when educ_lvl is not unique
data_educ = data_educ_backup
bs_agency = bs_agency_backup
bs_new_educ_lvl   = bs_new_educ_lvl_backup

data_synthetic_educ = data_synthetic_educ_backup 
bs_synthetic_agency = bs_synthetic_agency_backup

bs_agency = bs_agency[data_educ$unique_educ =="Mono", ]
bs_new_educ_lvl   = bs_new_educ_lvl[data_educ$unique_educ =="Mono", ]
data_educ = data_educ[data_educ$unique_educ =="Mono", ]

data_synthetic_educ = data_synthetic_educ[synthetic_unique_educ =="Mono", ]
bs_synthetic_agency = bs_synthetic_agency[synthetic_unique_educ =="Mono", ]

bs_synthetic_new_educ_lvl_Mono = bs_synthetic_agency

J1 = 1

#### Collapsed variables -- used to split the data
#Previos race
collapse = cbind(
  as.vector(data_educ$gender), 
  as.vector(bs_agency[,J1]))
collapse = simplify2array(mclapply(1:nrow(data_educ),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

synthetic_collapse = cbind(
  as.vector(data_synthetic_educ$gender), 
  as.vector(bs_synthetic_agency[,J1]))
synthetic_collapse = simplify2array(mclapply(1:nrow(data_synthetic_educ),function(j){paste(synthetic_collapse[j,],collapse="-")},mc.cores=48))

print(cbind(head(collapse,20),head(synthetic_collapse,20)))
print(mean(synthetic_collapse %in% collapse))

data_educ = data.frame(data_educ,educ_lvl = bs_new_educ_lvl[,J1],agency = bs_agency[,J1],collapse=collapse)
data_synthetic_educ = data.frame(data_synthetic_educ,agency = bs_synthetic_agency[,J1],collapse=synthetic_collapse)

variables = c("init_year","last_year","total_year","mode_agency","init_agency",
              "number_agency","M","gaps","gender","init_race","collapse")


data_educ_tmp = data.frame(data_educ)
data_synthetic_educ_tmp = data.frame(data_synthetic_educ)


Ind_collapse = data_educ_tmp$agency != "0"
Ind_synthetic_collapse = data_synthetic_educ_tmp$agency != "0"

## Start Algorithm
data_general=data_educ_tmp[Ind_collapse,,drop=FALSE]
data_synthetic_general=data_synthetic_educ_tmp[Ind_synthetic_collapse,,drop=FALSE]
Goal="educ_lvl"
data_general=data_general[data_general[,Goal]!="0",,drop=FALSE]
variables=variables
N.samples=1


# Getting collapse for mono_table, mono_tree and multi_tree
cores.table=30
source("Code/04.02.01.Init_mono_multi.R")


# Matching with mono_table
pre.cores.table = 20
cores.table = 30
split.mono.table=15000
cores.mono.table=40
source("Code/04.02.02.mono_table.R")


# Fitting mono_tree
split.mono.tree=10000
cores.mono.tree=30
N.cartdraw = 10000
cores.cartdraw = 2
source("Code/04.02.03.mono_tree.R")


# End
bs_synthetic_new_educ_lvl_Mono[Ind_synthetic_collapse,J1] = synthetic_Goal

print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print("educ_lvl year 1")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(table(bs_synthetic_new_educ_lvl_Mono[,J1]))
print(table(bs_new_educ_lvl[,J1]))

if(sum((bs_synthetic_new_educ_lvl_Mono[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
{
  print("Error: More zeros in synthetic_new_educ_lvl")
  print(which(((bs_synthetic_new_educ_lvl_Mono[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
  break()
}



#########################################################
#########################################################
# YEAR >= 2
###### Determining Race when race is not unique
for( J1 in 2:24)
{
  data_educ = data_educ_backup
  bs_agency = bs_agency_backup
  bs_new_educ_lvl   = bs_new_educ_lvl_backup
  
  data_synthetic_educ = data_synthetic_educ_backup 
  bs_synthetic_agency = bs_synthetic_agency_backup
  
  
  bs_agency = bs_agency[data_educ$unique_educ =="Mono", ]
  bs_new_educ_lvl   = bs_new_educ_lvl[data_educ$unique_educ =="Mono", ]
  data_educ = data_educ[data_educ$unique_educ =="Mono", ]
  
  data_synthetic_educ = data_synthetic_educ[synthetic_unique_educ =="Mono", ]
  bs_synthetic_agency = bs_synthetic_agency[synthetic_unique_educ =="Mono", ]
  
  #bs_synthetic_new_educ_lvl_Mono = bs_synthetic_agency
  
  f<-function(j,Lag)
  {
    tmp = "0"
    if(sum(bs_new_educ_lvl[j,1:(J1-1)]!="0")>0)
      tmp = rev(tail(bs_new_educ_lvl[j,1:(J1-1)][bs_new_educ_lvl[j,1:(J1-1)]!="0"],5))[Lag]
    tmp
  }
  last_educ.1 = simplify2array(mclapply(1:nrow(bs_new_educ_lvl),f,Lag=1,mc.cores=48))
  last_educ.2 = simplify2array(mclapply(1:nrow(bs_new_educ_lvl),f,Lag=2,mc.cores=48))
  last_educ.2[is.na(last_educ.2)]="Empty"
  last_educ.3 = simplify2array(mclapply(1:nrow(bs_new_educ_lvl),f,Lag=3,mc.cores=48))
  last_educ.3[is.na(last_educ.3)]="Empty"

  data_educ = data.frame(data_educ, last_educ.1=last_educ.1, last_educ.2=last_educ.2, last_educ.3=last_educ.3)
  
  f<-function(j,Lag)
  {
    tmp = "0"
    if(sum(bs_synthetic_new_educ_lvl_Mono[j,1:(J1-1)]!="0")>0)
      tmp = rev(tail(bs_synthetic_new_educ_lvl_Mono[j,1:(J1-1)][bs_synthetic_new_educ_lvl_Mono[j,1:(J1-1)]!="0"],5))[Lag]
    tmp
  }
  last_educ.1 = simplify2array(mclapply(1:nrow(bs_synthetic_new_educ_lvl_Mono),f,Lag=1,mc.cores=48))
  last_educ.2 = simplify2array(mclapply(1:nrow(bs_synthetic_new_educ_lvl_Mono),f,Lag=2,mc.cores=48))
  last_educ.2[is.na(last_educ.2)]="Empty"
  last_educ.3 = simplify2array(mclapply(1:nrow(bs_synthetic_new_educ_lvl_Mono),f,Lag=3,mc.cores=48))
  last_educ.3[is.na(last_educ.3)]="Empty"

  data_synthetic_educ = data.frame(data_synthetic_educ, last_educ.1=last_educ.1, last_educ.2=last_educ.2, last_educ.3=last_educ.3)
  
  
  #### Collapsed variables -- used to split the data
  #Previos new_educ_lvl
  mcl.collapse <- function(collapse)
    simplify2array(mclapply(1:nrow(collapse),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

  collapse.1 = mcl.collapse(cbind(
    as.vector(bs_agency[,J1]),
    as.vector(data_educ$last_educ.3),
    as.vector(data_educ$last_educ.2),
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.1 = mcl.collapse(cbind(
    as.vector(bs_synthetic_agency[,J1]),
    as.vector(data_synthetic_educ$last_educ.3),
    as.vector(data_synthetic_educ$last_educ.2),
    as.vector(data_synthetic_educ$last_educ.1)))
  

  collapse.2 = mcl.collapse(cbind(
    as.vector(bs_agency[,J1]),
    as.vector(data_educ$last_educ.2),
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.2 = mcl.collapse(cbind(
    as.vector(bs_synthetic_agency[,J1]),
    as.vector(data_synthetic_educ$last_educ.2),
    as.vector(data_synthetic_educ$last_educ.1)))


  collapse.3 = mcl.collapse(cbind(
    as.vector(bs_agency[,J1]),
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.3 = mcl.collapse(cbind(
    as.vector(bs_synthetic_agency[,J1]),
    as.vector(data_synthetic_educ$last_educ.1)))


  collapse.4 = mcl.collapse(cbind(
    as.vector(data_educ$last_educ.3),
    as.vector(data_educ$last_educ.2),
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.4 = mcl.collapse(cbind(
    as.vector(data_synthetic_educ$last_educ.3),
    as.vector(data_synthetic_educ$last_educ.2),
    as.vector(data_synthetic_educ$last_educ.1)))
  

  collapse.5 = mcl.collapse(cbind(
    as.vector(data_educ$last_educ.2),
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.5 = mcl.collapse(cbind(
    as.vector(data_synthetic_educ$last_educ.2),
    as.vector(data_synthetic_educ$last_educ.1)))


  collapse.6 = mcl.collapse(cbind(
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.6 = mcl.collapse(cbind(
    as.vector(data_synthetic_educ$last_educ.1)))


  print(cbind(head(collapse.1,20),head(synthetic_collapse.1,20)))
  print(mean(synthetic_collapse.1 %in% collapse.1))
  print(cbind(head(collapse.2,20),head(synthetic_collapse.2,20)))
  print(mean(synthetic_collapse.2 %in% collapse.2))
  print(cbind(head(collapse.3,20),head(synthetic_collapse.3,20)))
  print(mean(synthetic_collapse.3 %in% collapse.3))
  print(cbind(head(collapse.4,20),head(synthetic_collapse.4,20)))
  print(mean(synthetic_collapse.4 %in% collapse.4))
  print(cbind(head(collapse.5,20),head(synthetic_collapse.5,20)))
  print(mean(synthetic_collapse.5 %in% collapse.5))
  print(cbind(head(collapse.6,20),head(synthetic_collapse.6,20)))
  print(mean(synthetic_collapse.6 %in% collapse.6))

  
  Collapse = cbind(
    as.vector(collapse.1),
    as.vector(collapse.2),
    as.vector(collapse.3),
    as.vector(collapse.4),
    as.vector(collapse.5),
    as.vector(collapse.6))[bs_new_educ_lvl[,J1]!="0",]
  
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1),
    as.vector(synthetic_collapse.2),
    as.vector(synthetic_collapse.3),
    as.vector(synthetic_collapse.4),
    as.vector(synthetic_collapse.5),
    as.vector(synthetic_collapse.6))
  
  
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
  
  
  data_educ = data.frame(data_educ,educ_lvl = bs_new_educ_lvl[,J1],agency = bs_agency[,J1])[bs_new_educ_lvl[,J1]!="0",]
  data_synthetic_educ = data.frame(data_synthetic_educ,agency = bs_synthetic_agency[,J1])
  
  variables = c("init_year","last_year","total_year","mode_agency","init_agency",
                "number_agency","M","gaps","gender","agency","init_race","collapse")
  
  
  for(J in ncol(Synthetic_Collapse):1)
  {
    if(sum( Ind_synthetic_collapse[,J] & bs_synthetic_agency[,J1]!="0") > 0)
    {
      
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J, "new_educ_lvl","-- Year",J1))
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
      
      
      
      data_educ_tmp = data.frame(data_educ,collapse=Collapse[,J])
      data_synthetic_educ_tmp = data.frame(data_synthetic_educ, collapse=Synthetic_Collapse[,J])
      
      
      ## Start Algorithm
      data_general=data_educ_tmp[Ind_collapse[,J] ,,drop=FALSE]
      data_synthetic_general=data_synthetic_educ_tmp[Ind_synthetic_collapse[,J] & bs_synthetic_agency[,J1]!="0",,drop=FALSE]
      Goal="educ_lvl"
      data_general=data_general[data_general[,Goal]!="0",,drop=FALSE]
      variables=variables
      N.samples=1
      
      
      # Getting collapse for mono_table, mono_tree and multi_tree
      cores.table=30
      source("Code/04.02.01.Init_mono_multi.R")
      
      
      # Matching with mono_table
      pre.cores.table = 20
      cores.table = 30
      split.mono.table=15000
      cores.mono.table=40
      source("Code/04.02.02.mono_table.R")
      
      
      # Fitting mono_tree
      split.mono.tree=10000
      cores.mono.tree=30
      N.cartdraw = 10000
      cores.cartdraw = 2
      source("Code/04.02.03.mono_tree.R")
      
      
      # End
      bs_synthetic_new_educ_lvl_Mono[Ind_synthetic_collapse[,J] & bs_synthetic_agency[,J1]!="0",J1] = synthetic_Goal
      
    }
  }
  
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("new_educ_lvl_Mono","-- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(table(bs_synthetic_new_educ_lvl_Mono[,J1]))
  print(table(bs_new_educ_lvl[,J1]))

  if(sum((bs_synthetic_new_educ_lvl_Mono[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in synthetic_new_educ_lvl")
    print(which(((bs_synthetic_new_educ_lvl_Mono[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  print( t(head(bs_synthetic_new_educ_lvl_Mono,10)))
}


###################################
### NO MONO
###################################
#########################################################
#########################################################
# YEAR 1
###### Determining educ_lvl when educ_lvl is not unique
data_educ = data_educ_backup
bs_agency = bs_agency_backup
bs_new_educ_lvl   = bs_new_educ_lvl_backup

data_synthetic_educ = data_synthetic_educ_backup 
bs_synthetic_agency = bs_synthetic_agency_backup

bs_agency = bs_agency[data_educ$unique_educ =="NoMono", ]
bs_new_educ_lvl   = bs_new_educ_lvl[data_educ$unique_educ =="NoMono", ]
data_educ = data_educ[data_educ$unique_educ =="NoMono", ]

data_synthetic_educ = data_synthetic_educ[synthetic_unique_educ =="NoMono", ]
bs_synthetic_agency = bs_synthetic_agency[synthetic_unique_educ =="NoMono", ]

bs_synthetic_new_educ_lvl_NoMono = bs_synthetic_agency

J1 = 1

#### Collapsed variables -- used to split the data
#Previos race
collapse = cbind(
  as.vector(data_educ$gender), 
  as.vector(bs_agency[,J1]))
collapse = simplify2array(mclapply(1:nrow(data_educ),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

synthetic_collapse = cbind(
  as.vector(data_synthetic_educ$gender), 
  as.vector(bs_synthetic_agency[,J1]))
synthetic_collapse = simplify2array(mclapply(1:nrow(data_synthetic_educ),function(j){paste(synthetic_collapse[j,],collapse="-")},mc.cores=48))

print(cbind(head(collapse,20),head(synthetic_collapse,20)))
print(mean(synthetic_collapse %in% collapse))

data_educ = data.frame(data_educ,educ_lvl = bs_new_educ_lvl[,J1],agency = bs_agency[,J1],collapse=collapse)
data_synthetic_educ = data.frame(data_synthetic_educ,agency = bs_synthetic_agency[,J1],collapse=synthetic_collapse)

variables = c("init_year","last_year","total_year","mode_agency","init_agency",
              "number_agency","M","gaps","gender","init_race","collapse")


data_educ_tmp = data.frame(data_educ)
data_synthetic_educ_tmp = data.frame(data_synthetic_educ)


Ind_collapse = data_educ_tmp$agency != "0"
Ind_synthetic_collapse = data_synthetic_educ_tmp$agency != "0"

## Start Algorithm
data_general=data_educ_tmp[Ind_collapse,,drop=FALSE]
data_synthetic_general=data_synthetic_educ_tmp[Ind_synthetic_collapse,,drop=FALSE]
Goal="educ_lvl"
data_general=data_general[data_general[,Goal]!="0",,drop=FALSE]
variables=variables
N.samples=1


# Getting collapse for mono_table, mono_tree and multi_tree
cores.table=30
source("Code/04.02.01.Init_mono_multi.R")


# Matching with mono_table
pre.cores.table = 20
cores.table = 30
split.NoMono.table=15000
cores.NoMono.table=40
source("Code/04.02.02.mono_table.R")


# Fitting mono_tree
split.NoMono.tree=10000
cores.NoMono.tree=30
N.cartdraw = 10000
cores.cartdraw = 2
source("Code/04.02.03.mono_tree.R")


# End
bs_synthetic_new_educ_lvl_NoMono[Ind_synthetic_collapse,J1] = synthetic_Goal

print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print("educ_lvl year 1")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(table(bs_synthetic_new_educ_lvl_NoMono[,J1]))
print(table(bs_new_educ_lvl[,J1]))



#########################################################
#########################################################
# YEAR >= 2
###### Determining Race when race is not unique
for( J1 in 2:24)
{
  data_educ = data_educ_backup
  bs_agency = bs_agency_backup
  bs_new_educ_lvl   = bs_new_educ_lvl_backup
  
  data_synthetic_educ = data_synthetic_educ_backup 
  bs_synthetic_agency = bs_synthetic_agency_backup
  
  
  bs_agency = bs_agency[data_educ$unique_educ =="NoMono", ]
  bs_new_educ_lvl   = bs_new_educ_lvl[data_educ$unique_educ =="NoMono", ]
  data_educ = data_educ[data_educ$unique_educ =="NoMono", ]
  
  bs_agency = bs_agency[bs_new_educ_lvl[,J1]!="0",]
  data_educ = data_educ[bs_new_educ_lvl[,J1]!="0",]
  bs_new_educ_lvl   = bs_new_educ_lvl[bs_new_educ_lvl[,J1]!="0",]

  data_synthetic_educ = data_synthetic_educ[synthetic_unique_educ =="NoMono", ]
  bs_synthetic_agency = bs_synthetic_agency[synthetic_unique_educ =="NoMono", ]
  
  #bs_synthetic_new_educ_lvl_NoMono = bs_synthetic_agency
  
  f<-function(j,Lag)
  {
    tmp = "0"
    if(sum(bs_new_educ_lvl[j,1:(J1-1)]!="0")>0)
      tmp = rev(tail(bs_new_educ_lvl[j,1:(J1-1)][bs_new_educ_lvl[j,1:(J1-1)]!="0"],5))[Lag]
    tmp
  }
  last_educ.1 = simplify2array(mclapply(1:nrow(bs_new_educ_lvl),f,Lag=1,mc.cores=48))
  last_educ.2 = simplify2array(mclapply(1:nrow(bs_new_educ_lvl),f,Lag=2,mc.cores=48))
  last_educ.2[is.na(last_educ.2)]="Empty"
  last_educ.3 = simplify2array(mclapply(1:nrow(bs_new_educ_lvl),f,Lag=3,mc.cores=48))
  last_educ.3[is.na(last_educ.3)]="Empty"

  data_educ = data.frame(data_educ, last_educ.1=last_educ.1, last_educ.2=last_educ.2, last_educ.3=last_educ.3)
  
  f<-function(j,Lag)
  {
    tmp = "0"
    if(sum(bs_synthetic_new_educ_lvl_NoMono[j,1:(J1-1)]!="0")>0)
      tmp = rev(tail(bs_synthetic_new_educ_lvl_NoMono[j,1:(J1-1)][bs_synthetic_new_educ_lvl_NoMono[j,1:(J1-1)]!="0"],5))[Lag]
    tmp
  }
  last_educ.1 = simplify2array(mclapply(1:nrow(bs_synthetic_new_educ_lvl_NoMono),f,Lag=1,mc.cores=48))
  last_educ.2 = simplify2array(mclapply(1:nrow(bs_synthetic_new_educ_lvl_NoMono),f,Lag=2,mc.cores=48))
  last_educ.2[is.na(last_educ.2)]="Empty"
  last_educ.3 = simplify2array(mclapply(1:nrow(bs_synthetic_new_educ_lvl_NoMono),f,Lag=3,mc.cores=48))
  last_educ.3[is.na(last_educ.3)]="Empty"

  data_synthetic_educ = data.frame(data_synthetic_educ, last_educ.1=last_educ.1, last_educ.2=last_educ.2, last_educ.3=last_educ.3)
  
  
  #### Collapsed variables -- used to split the data
  #Previos race
  mcl.collapse <- function(collapse)
    simplify2array(mclapply(1:nrow(collapse),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

  collapse.1 = mcl.collapse(cbind(
    as.vector(bs_agency[,J1]),
    as.vector(data_educ$last_educ.3),
    as.vector(data_educ$last_educ.2),
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.1 = mcl.collapse(cbind(
    as.vector(bs_synthetic_agency[,J1]),
    as.vector(data_synthetic_educ$last_educ.3),
    as.vector(data_synthetic_educ$last_educ.2),
    as.vector(data_synthetic_educ$last_educ.1)))
  

  collapse.2 = mcl.collapse(cbind(
    as.vector(bs_agency[,J1]),
    as.vector(data_educ$last_educ.2),
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.2 = mcl.collapse(cbind(
    as.vector(bs_synthetic_agency[,J1]),
    as.vector(data_synthetic_educ$last_educ.2),
    as.vector(data_synthetic_educ$last_educ.1)))


  collapse.3 = mcl.collapse(cbind(
    as.vector(bs_agency[,J1]),
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.3 = mcl.collapse(cbind(
    as.vector(bs_synthetic_agency[,J1]),
    as.vector(data_synthetic_educ$last_educ.1)))


  collapse.4 = mcl.collapse(cbind(
    as.vector(data_educ$last_educ.3),
    as.vector(data_educ$last_educ.2),
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.4 = mcl.collapse(cbind(
    as.vector(data_synthetic_educ$last_educ.3),
    as.vector(data_synthetic_educ$last_educ.2),
    as.vector(data_synthetic_educ$last_educ.1)))
  

  collapse.5 = mcl.collapse(cbind(
    as.vector(data_educ$last_educ.2),
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.5 = mcl.collapse(cbind(
    as.vector(data_synthetic_educ$last_educ.2),
    as.vector(data_synthetic_educ$last_educ.1)))


  collapse.6 = mcl.collapse(cbind(
    as.vector(data_educ$last_educ.1)))

  synthetic_collapse.6 = mcl.collapse(cbind(
    as.vector(data_synthetic_educ$last_educ.1)))


  print(cbind(head(collapse.1,20),head(synthetic_collapse.1,20)))
  print(mean(synthetic_collapse.1 %in% collapse.1))
  print(cbind(head(collapse.2,20),head(synthetic_collapse.2,20)))
  print(mean(synthetic_collapse.2 %in% collapse.2))
  print(cbind(head(collapse.3,20),head(synthetic_collapse.3,20)))
  print(mean(synthetic_collapse.3 %in% collapse.3))
  print(cbind(head(collapse.4,20),head(synthetic_collapse.4,20)))
  print(mean(synthetic_collapse.4 %in% collapse.4))
  print(cbind(head(collapse.5,20),head(synthetic_collapse.5,20)))
  print(mean(synthetic_collapse.5 %in% collapse.5))
  print(cbind(head(collapse.6,20),head(synthetic_collapse.6,20)))
  print(mean(synthetic_collapse.6 %in% collapse.6))

  
  Collapse = cbind(
    as.vector(collapse.1),
    as.vector(collapse.2),
    as.vector(collapse.3),
    as.vector(collapse.4),
    as.vector(collapse.5),
    as.vector(collapse.6))
  
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1),
    as.vector(synthetic_collapse.2),
    as.vector(synthetic_collapse.3),
    as.vector(synthetic_collapse.4),
    as.vector(synthetic_collapse.5),
    as.vector(synthetic_collapse.6))
  
  
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
  
  
  data_educ = data.frame(data_educ,educ_lvl = bs_new_educ_lvl[,J1],agency = bs_agency[,J1])
  data_synthetic_educ = data.frame(data_synthetic_educ,agency = bs_synthetic_agency[,J1])
  
  variables = c("init_year","last_year","total_year","mode_agency","init_agency",
                "number_agency","M","gaps","gender","agency","init_race","collapse")
  
  
  for(J in ncol(Synthetic_Collapse):1)
  {
    if(sum( Ind_synthetic_collapse[,J] & bs_synthetic_agency[,J1]!="0") > 0)
    {
      
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J, "race","-- Year",J1))
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
      
      
      
      data_educ_tmp = data.frame(data_educ,collapse=Collapse[,J])
      data_synthetic_educ_tmp = data.frame(data_synthetic_educ, collapse=Synthetic_Collapse[,J])
      
      
      ## Start Algorithm
      data_general=data_educ_tmp[Ind_collapse[,J] ,,drop=FALSE]
      data_synthetic_general=data_synthetic_educ_tmp[Ind_synthetic_collapse[,J] & bs_synthetic_agency[,J1]!="0",,drop=FALSE]
      Goal="educ_lvl"
      data_general=data_general[data_general[,Goal]!="0",,drop=FALSE]
      variables=variables
      N.samples=1
      
      
      # Getting collapse for mono_table, mono_tree and multi_tree
      cores.table=30
      source("Code/04.02.01.Init_mono_multi.R")
      
      
      # Matching with mono_table
      pre.cores.table = 20
      cores.table = 30
      split.NoMono.table=15000
      cores.NoMono.table=40
      source("Code/04.02.02.mono_table.R")
      
      
      # Fitting mono_tree
      split.NoMono.tree=10000
      cores.NoMono.tree=30
      N.cartdraw = 10000
      cores.cartdraw = 2
      source("Code/04.02.03.mono_tree.R")
      
      
      # End
      bs_synthetic_new_educ_lvl_NoMono[Ind_synthetic_collapse[,J] & bs_synthetic_agency[,J1]!="0",J1] = synthetic_Goal
      
    }
  }
  
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("new_educ_lvl_NoMono","-- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(table(bs_synthetic_new_educ_lvl_NoMono[,J1]))
  print(table(bs_new_educ_lvl[,J1]))


  if(sum((bs_synthetic_new_educ_lvl_NoMono[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in synthetic_new_educ_lvl")
    print(which(((bs_synthetic_new_educ_lvl_NoMono[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  print( t(head(bs_synthetic_new_educ_lvl_NoMono,10)))

}



bs_synthetic_new_educ_lvl = bs_synthetic_agency_backup
for(J1 in 1:24)
{
  bs_synthetic_new_educ_lvl[synthetic_unique_educ=="U",J1] = synthetic_init_educ
  bs_synthetic_new_educ_lvl[bs_synthetic_agency_backup[,J1] == "0", J1] = "0"
  bs_synthetic_new_educ_lvl[synthetic_unique_educ=="Mono", J1] = bs_synthetic_new_educ_lvl_Mono[,J1]
  bs_synthetic_new_educ_lvl[synthetic_unique_educ=="NoMono", J1] = bs_synthetic_new_educ_lvl_NoMono[,J1]
}

if(sum((bs_synthetic_new_educ_lvl=="0") - (bs_synthetic_agency_backup == "0"))>1)
{
  print("Error: More zeros in synthetic_race")
  print(which(apply((bs_synthetic_new_educ_lvl=="0") - (bs_synthetic_agency_backup == "0"),1,sum)>0))
  break()
}

save(bs_synthetic_new_educ_lvl,file=paste("Synthetic_Files/bs_synthetic_new_educ_lvl.RData",sep=""))

