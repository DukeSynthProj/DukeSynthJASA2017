setwd("~/Path/SyntheticData")

load("Synthetic_Files/data_synthetic_AgeYrsdMilm.RData")
load("Aux_Files/data_AgeYrsdMilm.RData")

library(parallel)
library(tree)

source("Code/99.02.cartdraw.R")
source("Code/99.03.cartdraw2.R")



###### Determining init_agerange

#### Collapsed variables -- used to split the data
#Previos race
mcl.collapse <- function(collapse)
  simplify2array(mclapply(1:nrow(collapse),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

collapse.1 = mcl.collapse(cbind(
  as.vector(data_AgeYrsdMilm$init_agency),
  as.vector(data_AgeYrsdMilm$init_year),
  as.vector(data_AgeYrsdMilm$init_educ),
  as.vector(data_AgeYrsdMilm$gender)))

synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(data_synthetic_AgeYrsdMilm$init_agency),
  as.vector(data_synthetic_AgeYrsdMilm$init_year),
  as.vector(data_synthetic_AgeYrsdMilm$init_educ),
  as.vector(data_synthetic_AgeYrsdMilm$gender)))

print(cbind(head(collapse.1,20),head(synthetic_collapse.1,20)))
print(mean(synthetic_collapse.1 %in% collapse.1))


collapse.2 = mcl.collapse(cbind(
  as.vector(data_AgeYrsdMilm$init_agency),
  as.vector(data_AgeYrsdMilm$init_year),
  as.vector(data_AgeYrsdMilm$init_educ)))

synthetic_collapse.2 = mcl.collapse(cbind(
  as.vector(data_synthetic_AgeYrsdMilm$init_agency),
  as.vector(data_synthetic_AgeYrsdMilm$init_year),
  as.vector(data_synthetic_AgeYrsdMilm$init_educ)))

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

variables = c("init_year", "last_year", "total_year", "mode_agency", 
              "init_agency", "number_agency", "M", "gaps", "gender", 
              "init_race", "init_educ", "collapse")

synthetic_init_agerange = rep(0,nrow(data_synthetic_AgeYrsdMilm))

for(J in ncol(Synthetic_Collapse):1)
{
  if(sum( Ind_synthetic_collapse[,J]) > 0)
  {
    
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J, "agerange"))
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
    
    
    
    data_AgeYrsdMilm_tmp = data.frame(data_AgeYrsdMilm,collapse=Collapse[,J])
    data_synthetic_AgeYrsdMilm_tmp = data.frame(data_synthetic_AgeYrsdMilm, collapse=Synthetic_Collapse[,J])
    
    
    ## Start Algorithm
    data_general=data_AgeYrsdMilm_tmp[Ind_collapse[,J] ,,drop=FALSE]
    data_synthetic_general=data_synthetic_AgeYrsdMilm_tmp[Ind_synthetic_collapse[,J],,drop=FALSE]
    Goal="init_agerange"
    variables=variables
    N.samples=1
    
    
    # Getting collapse for mono_table, mono_tree and multi_tree
    cores.table=30
    source("Code/05.02.01.Init_mono_multi.R")
    
    
    # Matching with mono_table
    pre.cores.table = 20
    cores.table = 30
    split.mono.table=15000
    cores.mono.table=40
    source("Code/05.02.02.mono_table.R")
    
    
    # Fitting mono_tree
    split.mono.tree=10000
    cores.mono.tree=30
    N.cartdraw = 10000
    cores.cartdraw = 2
    source("Code/05.02.03.mono_tree.R")
    
    
    # End
    synthetic_init_agerange[Ind_synthetic_collapse[,J]] = synthetic_Goal
    
  }
}

print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("synthetic_init_agerange"))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(table(synthetic_init_agerange))
print(table(data_AgeYrsdMilm$init_agerange))

save(synthetic_init_agerange, file = "Synthetic_Files/synthetic_init_agerange.RData")


###### Determining init_yrsdegrng

data_synthetic_AgeYrsdMilm =  data.frame(data_synthetic_AgeYrsdMilm, init_agerange=synthetic_init_agerange)

variables = c("init_year", "last_year", "total_year", "mode_agency", 
              "init_agency", "number_agency", "M", "gaps", "gender", 
              "init_race", "init_educ", "init_agerange", "collapse")

synthetic_init_yrsdegrng = rep(0,nrow(data_synthetic_AgeYrsdMilm))

for(J in ncol(Synthetic_Collapse):1)
{
  if(sum( Ind_synthetic_collapse[,J]) > 0)
  {
    
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J, "init_yrsdegrng"))
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
    
    
    
    data_AgeYrsdMilm_tmp = data.frame(data_AgeYrsdMilm,collapse=Collapse[,J])
    data_synthetic_AgeYrsdMilm_tmp = data.frame(data_synthetic_AgeYrsdMilm, collapse=Synthetic_Collapse[,J])
    
    
    ## Start Algorithm
    data_general=data_AgeYrsdMilm_tmp[Ind_collapse[,J] ,,drop=FALSE]
    data_synthetic_general=data_synthetic_AgeYrsdMilm_tmp[Ind_synthetic_collapse[,J],,drop=FALSE]
    Goal="init_yrsdegrng"
    variables=variables
    N.samples=1
    
    
    # Getting collapse for mono_table, mono_tree and multi_tree
    cores.table=30
    source("Code/05.02.01.Init_mono_multi.R")
    
    
    # Matching with mono_table
    pre.cores.table = 20
    cores.table = 30
    split.mono.table=15000
    cores.mono.table=40
    source("Code/05.02.02.mono_table.R")
    
    
    # Fitting mono_tree
    split.mono.tree=10000
    cores.mono.tree=30
    N.cartdraw = 10000
    cores.cartdraw = 2
    source("Code/05.02.03.mono_tree.R")
    
    
    # End
    synthetic_init_yrsdegrng[Ind_synthetic_collapse[,J]] = synthetic_Goal
    
  }
}

print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("synthetic_init_yrsdegrng"))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(table(synthetic_init_yrsdegrng))
print(table(data_AgeYrsdMilm$init_yrsdegrng))

save(synthetic_init_yrsdegrng, file = "Synthetic_Files/synthetic_init_yrsdegrng.RData")




###### Determining milmonths

data_synthetic_AgeYrsdMilm =  data.frame(data_synthetic_AgeYrsdMilm, init_yrsdegrng=synthetic_init_yrsdegrng)


variables = c("init_year", "last_year", "total_year", "mode_agency", 
              "init_agency", "number_agency", "M", "gaps", "gender", 
              "init_race", "init_educ", "init_agerange", "init_yrsdegrng", 
              "collapse")

synthetic_milmonths = rep(0,nrow(data_synthetic_AgeYrsdMilm))


for(J in ncol(Synthetic_Collapse):1)
{
  if(sum( Ind_synthetic_collapse[,J]) > 0)
  {
    
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J, "milmonths"))
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
    
    
    
    data_AgeYrsdMilm_tmp = data.frame(data_AgeYrsdMilm,collapse=Collapse[,J])
    data_synthetic_AgeYrsdMilm_tmp = data.frame(data_synthetic_AgeYrsdMilm, collapse=Synthetic_Collapse[,J])
    
    
    ## Start Algorithm
    data_general=data_AgeYrsdMilm_tmp[Ind_collapse[,J] ,,drop=FALSE]
    data_synthetic_general=data_synthetic_AgeYrsdMilm_tmp[Ind_synthetic_collapse[,J],,drop=FALSE]
    Goal="milmonths"
    variables=variables
    N.samples=1
    
    
    # Getting collapse for mono_table, mono_tree and multi_tree
    cores.table=30
    source("Code/05.02.01.Init_mono_multi.R")
    
    
    # Matching with mono_table
    pre.cores.table = 20
    cores.table = 30
    split.mono.table=15000
    cores.mono.table=40
    source("Code/05.02.02.mono_table.R")
    
    
    # Fitting mono_tree
    split.mono.tree=10000
    cores.mono.tree=30
    N.cartdraw = 10000
    cores.cartdraw = 2
    source("Code/05.02.03.mono_tree.R")
    
    
    # End
    synthetic_milmonths[Ind_synthetic_collapse[,J]] = synthetic_Goal
    
  }
}

print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("synthetic_milmonths"))
print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
print(table(synthetic_milmonths))
print(table(data_AgeYrsdMilm$milmonths))

save(synthetic_milmonths, file = "Synthetic_Files/synthetic_milmonths.RData")





