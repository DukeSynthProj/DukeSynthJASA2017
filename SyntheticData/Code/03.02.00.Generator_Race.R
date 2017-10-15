setwd("~/Path/SyntheticData")

load("Synthetic_Files/data_synthetic_race.RData")
load("Synthetic_Files/bs_synthetic_agency.RData")

load("Aux_Files/data_race.RData")
load("Aux_Files/bs_agency.RData")
load("Aux_Files/bs_race.RData")
load("Aux_Files/bs_eribridge.RData")
for(j in 1:24)
{
  bs_race[is.na(bs_race[,j]),j]="NA.race"
  bs_race[bs_race[,j]=="NA",j]="NA.race"
  bs_eribridge[is.na(bs_eribridge[,j]),j]="NA.eribridge"
  bs_eribridge[bs_eribridge[,j]=="NA",j]="NA.eribridge"
}


library(parallel)
library(tree)

source("Code/99.02.cartdraw.R")
source("Code/99.03.cartdraw2.R")


###### Determining whether Race is unique or not
data_race_backup    = data_race
bs_agency_backup    = bs_agency
bs_race_backup      = bs_race
bs_eribridge_backup = bs_eribridge

data_synthetic_race_backup = data_synthetic_race
bs_synthetic_agency_backup = bs_synthetic_agency

bs_synthetic = bs_synthetic_agency[,1:18]
Ind_unique_race = simplify2array(mclapply(1:nrow(bs_synthetic),function(j){length(unique(bs_synthetic[j,bs_synthetic[j,]!="0"]))},mc.cores=48))

data_race = data_race[data_race$unique_race != "NS",]
data_synthetic_race = data_synthetic_race[Ind_unique_race != 0,]

#### Collapsed variables -- used to split the data
#Previos race
collapse = cbind(
  as.vector(data_race$init_year), 
  as.vector(data_race$init_agency),
  as.vector(data_race$gender))
collapse = simplify2array(mclapply(1:nrow(data_race),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

synthetic_collapse = cbind(
  as.vector(data_synthetic_race$init_year), 
  as.vector(data_synthetic_race$init_agency),
  as.vector(data_synthetic_race$gender))
synthetic_collapse = simplify2array(mclapply(1:nrow(data_synthetic_race),function(j){paste(synthetic_collapse[j,],collapse="-")},mc.cores=48))

print(cbind(head(collapse,20),head(synthetic_collapse,20)))
print(mean(synthetic_collapse %in% collapse))

data_race = data.frame(data_race,collapse=collapse)
data_synthetic_race = data.frame(data_synthetic_race,collapse=synthetic_collapse)

variables = c("init_year","last_year","total_year","mode_agency","init_agency",
              "number_agency","M","gaps","gender","collapse")


synthetic_unique_race = matrix("0",nrow=nrow(data_synthetic_race),ncol=1) 


data_race_tmp = data.frame(data_race)
data_synthetic_race_tmp = data.frame(data_synthetic_race)


Ind_collapse = data_race_tmp$init_agency != "0"
Ind_synthetic_collapse = data_synthetic_race_tmp$init_agency != "0"

## Start Algorithm
data_general=data_race_tmp[Ind_collapse,,drop=FALSE]
data_synthetic_general=data_synthetic_race_tmp[Ind_synthetic_collapse,,drop=FALSE]
Goal="unique_race"
variables=variables
N.samples=1


# Getting collapse for mono_table, mono_tree and multi_tree
cores.table=30
source("Code/03.02.01.Init_mono_multi.R")


# Matching with mono_table
pre.cores.table = 20
cores.table = 30
split.mono.table=15000
cores.mono.table=40
source("Code/03.02.03.mono_table.R")


# Fitting mono_tree
split.mono.tree=10000
cores.mono.tree=30
N.cartdraw = 10000
cores.cartdraw = 2
source("Code/03.02.04.mono_tree.R")


# End
synthetic_unique_race = synthetic_Goal
save(synthetic_unique_race,file=paste("Synthetic_Files/synthetic_unique_race.RData",sep=""))

print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print("unique_race")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(table(synthetic_unique_race))
print(table(data_race$unique_race))


#########################################################
#########################################################

###### Determining Race when race is unique
data_race = data_race_backup
data_synthetic_race = data_synthetic_race_backup 

data_race = data_race[data_race$unique_race =="U", ]
data_synthetic_race = data_synthetic_race[Ind_unique_race != 0, ]
data_synthetic_race = data_synthetic_race[synthetic_unique_race =="U", ]


#### Collapsed variables -- used to split the data
#Previos race
collapse = cbind(
  as.vector(data_race$init_year), 
  as.vector(data_race$init_agency))
collapse = simplify2array(mclapply(1:nrow(data_race),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

synthetic_collapse = cbind(
  as.vector(data_synthetic_race$init_year), 
  as.vector(data_synthetic_race$init_agency))
synthetic_collapse = simplify2array(mclapply(1:nrow(data_synthetic_race),function(j){paste(synthetic_collapse[j,],collapse="-")},mc.cores=48))

print(cbind(head(collapse,20),head(synthetic_collapse,20)))
print(mean(synthetic_collapse %in% collapse))

data_race = data.frame(data_race,collapse=collapse)
data_synthetic_race = data.frame(data_synthetic_race,collapse=synthetic_collapse)

variables = c("init_year","last_year","total_year","mode_agency","init_agency",
              "number_agency","M","gaps","gender","collapse")


synthetic_init_race = matrix("0",nrow=nrow(data_synthetic_race),ncol=1) 


data_race_tmp = data.frame(data_race)
data_synthetic_race_tmp = data.frame(data_synthetic_race)


Ind_collapse = data_race_tmp$init_agency != "0"
Ind_synthetic_collapse = data_synthetic_race_tmp$init_agency != "0"

## Start Algorithm
data_general=data_race_tmp[Ind_collapse,,drop=FALSE]
data_synthetic_general=data_synthetic_race_tmp[Ind_synthetic_collapse,,drop=FALSE]
Goal="init_race"
variables=variables
N.samples=1


# Getting collapse for mono_table, mono_tree and multi_tree
cores.table=30
source("Code/03.02.01.Init_mono_multi.R")


# Matching with mono_table
pre.cores.table = 20
cores.table = 30
split.mono.table=15000
cores.mono.table=40
source("Code/03.02.03.mono_table.R")


# Fitting mono_tree
split.mono.tree=10000
cores.mono.tree=30
N.cartdraw = 10000
cores.cartdraw = 2
source("Code/03.02.04.mono_tree.R")


# End
synthetic_init_race = synthetic_Goal
save(synthetic_init_race,file=paste("Synthetic_Files/synthetic_init_race.RData",sep=""))

print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print("init_race")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(table(synthetic_init_race))
print(table(data_race$init_race))


#########################################################
#########################################################
# YEAR 1
###### Determining Race when race is not unique
data_race = data_race_backup
bs_agency = bs_agency_backup
bs_race   = bs_race_backup
bs_eribridge = bs_eribridge_backup

data_synthetic_race = data_synthetic_race_backup 
bs_synthetic_agency = bs_synthetic_agency_backup


bs_agency = bs_agency[data_race$unique_race =="NU", ]
bs_race   = bs_race[data_race$unique_race =="NU", ]
bs_eribridge = bs_eribridge[data_race$unique_race =="NU", ]
bs_race = cbind(bs_race[,1:18],bs_eribridge[,19:20])

data_race = data_race[data_race$unique_race =="NU", ]

data_synthetic_race = data_synthetic_race[Ind_unique_race != 0, ]
data_synthetic_race = data_synthetic_race[synthetic_unique_race =="NU", ]

bs_synthetic_agency = bs_synthetic_agency[Ind_unique_race != 0, ]
bs_synthetic_agency = bs_synthetic_agency[synthetic_unique_race =="NU", ]

bs_synthetic_race = bs_synthetic_agency

J1 = 1

#### Collapsed variables -- used to split the data
#Previos race
collapse = cbind(
  as.vector(data_race$init_year), 
  as.vector(bs_agency[,J1]))
collapse = simplify2array(mclapply(1:nrow(data_race),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

synthetic_collapse = cbind(
  as.vector(data_synthetic_race$init_year), 
  as.vector(bs_synthetic_agency[,J1]))
synthetic_collapse = simplify2array(mclapply(1:nrow(data_synthetic_race),function(j){paste(synthetic_collapse[j,],collapse="-")},mc.cores=48))

print(cbind(head(collapse,20),head(synthetic_collapse,20)))
print(mean(synthetic_collapse %in% collapse))

data_race = data.frame(data_race,race = bs_race[,J1],agency = bs_agency[,J1],collapse=collapse)
data_synthetic_race = data.frame(data_synthetic_race,agency = bs_synthetic_agency[,J1],collapse=synthetic_collapse)

variables = c("init_year","last_year","total_year","mode_agency","init_agency",
              "number_agency","M","gaps","gender","collapse")


data_race_tmp = data.frame(data_race)
data_synthetic_race_tmp = data.frame(data_synthetic_race)


Ind_collapse = data_race_tmp$agency != "0"
Ind_synthetic_collapse = data_synthetic_race_tmp$agency != "0"

## Start Algorithm
data_general=data_race_tmp[Ind_collapse,,drop=FALSE]
data_synthetic_general=data_synthetic_race_tmp[Ind_synthetic_collapse,,drop=FALSE]
Goal="race"
variables=variables
N.samples=1


# Getting collapse for mono_table, mono_tree and multi_tree
cores.table=30
source("Code/03.02.01.Init_mono_multi.R")


# Matching with mono_table
pre.cores.table = 20
cores.table = 30
split.mono.table=15000
cores.mono.table=40
source("Code/03.02.03.mono_table.R")


# Fitting mono_tree
split.mono.tree=10000
cores.mono.tree=30
N.cartdraw = 10000
cores.cartdraw = 2
source("Code/03.02.04.mono_tree.R")


# End
bs_synthetic_race[Ind_synthetic_collapse,J1] = synthetic_Goal

print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print("race year 1")
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(table(bs_synthetic_race[,J1]))
print(table(bs_race[,J1]))


if(sum((bs_synthetic_race[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>1)
{
  print("Error: More zeros in synthetic_race")
  break()
}


#########################################################
#########################################################
# YEAR >= 2
###### Determining Race when race is not unique
for( J1 in 2:18)
{
  data_race = data_race_backup
  bs_agency = bs_agency_backup
  bs_race   = bs_race_backup
  bs_eribridge = bs_eribridge_backup
  
  data_synthetic_race = data_synthetic_race_backup 
  bs_synthetic_agency = bs_synthetic_agency_backup
  
  
  bs_agency = bs_agency[data_race$unique_race =="NU", ]
  bs_race   = bs_race[data_race$unique_race =="NU", ]
  bs_eribridge = bs_eribridge[data_race$unique_race =="NU", ]
  bs_race = cbind(bs_race[,1:18],bs_eribridge[,19:20])
  
  data_race = data_race[data_race$unique_race =="NU", ]
  
  data_synthetic_race = data_synthetic_race[Ind_unique_race != 0, ]
  data_synthetic_race = data_synthetic_race[synthetic_unique_race =="NU", ]
  
  bs_synthetic_agency = bs_synthetic_agency[Ind_unique_race != 0, ]
  bs_synthetic_agency = bs_synthetic_agency[synthetic_unique_race =="NU", ]
  
  #bs_synthetic_race = bs_synthetic_agency
  
  f<-function(j)
  {
    tmp = "0"
    if(sum(bs_race[j,1:(J1-1)]!="0")>0)
      tmp = tail(bs_race[j,1:(J1-1)][bs_race[j,1:(J1-1)]!="0"],1)
    tmp
  }
  last_race = simplify2array(mclapply(1:nrow(bs_race),f,mc.cores=48))
  data_race = data.frame(data_race,last_race = last_race)
  
  f<-function(j)
  {
    tmp = "0"
    if(sum(bs_synthetic_race[j,1:(J1-1)]!="0")>0)
      tmp = tail(bs_synthetic_race[j,1:(J1-1)][bs_synthetic_race[j,1:(J1-1)]!="0"],1)
    tmp
  }
  synthetic_last_race = simplify2array(mclapply(1:nrow(bs_synthetic_race),f,mc.cores=48))
  data_synthetic_race = data.frame(data_synthetic_race,last_race = synthetic_last_race)
  
  
  #### Collapsed variables -- used to split the data
  #Previos race
  collapse = cbind(
    as.vector(bs_agency[,J1]),
    as.vector(data_race$last_race))
  collapse.1 = simplify2array(mclapply(1:nrow(collapse),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))
  
  synthetic_collapse = cbind(
    as.vector(bs_synthetic_agency[,J1]),
    as.vector(data_synthetic_race$last_race))
  synthetic_collapse.1 = simplify2array(mclapply(1:nrow(synthetic_collapse),function(j){paste(synthetic_collapse[j,],collapse="-")},mc.cores=48))
  
  
  collapse = cbind(
    as.vector(data_race$last_race))
  collapse.2 = simplify2array(mclapply(1:nrow(data_race),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))
  
  synthetic_collapse = cbind(
    as.vector(data_synthetic_race$last_race))
  synthetic_collapse.2 = simplify2array(mclapply(1:nrow(data_synthetic_race),function(j){paste(synthetic_collapse[j,],collapse="-")},mc.cores=48))
  
  print(cbind(head(collapse.1,20),head(synthetic_collapse.1,20)))
  print(mean(synthetic_collapse.1 %in% collapse.1))
  print(cbind(head(collapse.2,20),head(synthetic_collapse.2,20)))
  print(mean(synthetic_collapse.2 %in% collapse.2))
  
  Collapse = cbind(
    as.vector(collapse.1),
    as.vector(collapse.2))
  
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.1),
    as.vector(synthetic_collapse.2))
  
  
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
  
  
  data_race = data.frame(data_race,race = bs_race[,J1],agency = bs_agency[,J1])
  data_synthetic_race = data.frame(data_synthetic_race,agency = bs_synthetic_agency[,J1])
  
  variables = c("init_year","last_year","total_year","mode_agency","init_agency",
                "number_agency","M","gaps","gender","agency","collapse")
  
  
  for(J in ncol(Synthetic_Collapse):1)
  {
    if(sum( Ind_synthetic_collapse[,J]) > 0)
    {
      
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J, "race","-- Year",J1))
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
            
      data_race_tmp = data.frame(data_race,collapse=Collapse[,J])
      data_synthetic_race_tmp = data.frame(data_synthetic_race, collapse=Synthetic_Collapse[,J])
      
      
      ## Start Algorithm
      data_general=data_race_tmp[Ind_collapse[,J] ,,drop=FALSE]
      data_general=data_general[data_general$race != "0" ,,drop=FALSE]
      data_synthetic_general=data_synthetic_race_tmp[Ind_synthetic_collapse[,J] & bs_synthetic_agency[,J1]!="0",,drop=FALSE]
      Goal="race"
      variables=variables
      N.samples=1
      
      
      # Getting collapse for mono_table, mono_tree and multi_tree
      cores.table=30
      source("Code/03.02.01.Init_mono_multi.R")
      
      
      # Matching with mono_table
      pre.cores.table = 20
      cores.table = 30
      split.mono.table=15000
      cores.mono.table=40
      source("Code/03.02.03.mono_table.R")
      
      
      # Fitting mono_tree
      split.mono.tree=10000
      cores.mono.tree=30
      N.cartdraw = 10000
      cores.cartdraw = 2
      source("Code/03.02.04.mono_tree.R")
      
      
      # End
      bs_synthetic_race[Ind_synthetic_collapse[,J] & bs_synthetic_agency[,J1]!="0",J1] = synthetic_Goal
      
    }
  }
  
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("race","-- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(table(bs_synthetic_race[,J1]))
  print(table(bs_race[,J1]))
  if(sum((bs_synthetic_race[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>1)
  {
    print("Error: More zeros in synthetic_race")
    print(which(((bs_synthetic_race[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
}


bs_synthetic_race_1.18 = bs_synthetic_agency_backup[,1:18]
for(J1 in 1:18)
{
  bs_synthetic_race_1.18[Ind_unique_race != 0, J1][synthetic_unique_race=="U"] = synthetic_init_race
  bs_synthetic_race_1.18[bs_synthetic_agency_backup[,J1] == "0", J1] = "0"
  bs_synthetic_race_1.18[Ind_unique_race != 0, J1][synthetic_unique_race=="NU"] = bs_synthetic_race[,J1]
}

if(sum((bs_synthetic_race_1.18=="0") - (bs_synthetic_agency_backup[,1:18] == "0"))>1)
{
  print("Error: More zeros in synthetic_race")
  print(which(((bs_synthetic_race_1.18=="0") - (bs_synthetic_agency_backup[,1:18] == "0"))>0))
  break()
}


save(bs_synthetic_race_1.18,file=paste("Synthetic_Files/bs_synthetic_race_1.18.RData",sep=""))


