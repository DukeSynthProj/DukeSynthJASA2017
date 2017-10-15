setwd("~/Path/SyntheticData")
rm(list=ls())
library(parallel)
library(tree)


load("Aux_Files/bs_retallow.RData")
load("Aux_Files/bs_svsr_diff.RData")
load("Aux_Files/bs_paybasic.RData")

for(j in 1:24)
{
  bs_retallow[is.na(bs_retallow[,j]),j] = "NA.retallow"
  bs_svsr_diff[is.na(bs_svsr_diff[,j]),j] = "NA.svsr_diff"
}


data.svsr_diff.retallow<- function(J1)
{
  load(paste("Aux_Files/data_wage_year_",J1,".RData",sep=""))
  IND1 = data_wage$agency != "0"
  data_wage = data_wage[IND1,]
  paybasic_0 = bs_paybasic[IND1,J1]
  paybasic_0[is.na(paybasic_0)] = "NA.paybasic"
  paybasic_0[paybasic_0 == "0"] = "zero"
  unique_wage = unique(data_wage$wage[data_wage$agency!="0"])
  f <- function(J)
  {
    Ind = unique_wage[J] == data_wage$wage
    Ind2 = paybasic_0[Ind] != "zero" &  paybasic_0[Ind] != "NA.paybasic"
    if(sum(Ind2)>0)
    {
      m=mean(as.numeric( paybasic_0[Ind][Ind2]))
      paybasic_0[Ind][paybasic_0[Ind] == "zero" |  paybasic_0[Ind] == "NA.paybasic"] = m
    }
    
    if(J%%1000==0)# & length(names_paybasic)<=10000)        
      print(paste(J,"of",length(unique_wage)))
    
    list(Ind=which(Ind), pay = paybasic_0[Ind])
  }
  tmp = mclapply(1:length(unique_wage),f,mc.cores=45)  
  f <- function(J){paybasic_0[tmp[[J]]$Ind]  <<-  tmp[[J]]$pay }
  aux1<-sapply(1:length(unique_wage),f)
  Ind = paybasic_0 != "zero" &  paybasic_0 != "NA.paybasic" & paybasic_0 != "0"
  MEAN = mean(as.numeric(paybasic_0[Ind]))
  paybasic_0[paybasic_0 == "zero" |  paybasic_0 == "NA.paybasic"] = MEAN
  paybasic_0 = as.numeric(paybasic_0)
  print(paste(mean(paybasic_0[paybasic_0!="0"]),"paybasic in year",J1))

  tmp = rep(0,length(IND1))
  tmp[IND1] = paybasic_0    
  paybasic_0 = tmp
  
  load(paste("Aux_Files/data_wage_year_",J1,".RData",sep=""))
  
  cat_retallow = bs_retallow[,J1]
  cat_retallow[bs_retallow[,J1] != "NA.retallow" & bs_retallow[,J1] != "0"] = "Num_Value"
  cat_retallow[data_wage$agency != "0" & cat_retallow == "0"] = "zero"
  
  cat_svsr_diff = bs_svsr_diff[,J1]
  cat_svsr_diff[bs_svsr_diff[,J1] != "NA.svsr_diff" & bs_svsr_diff[,J1] != "0"] = "Num_Value"
  cat_svsr_diff[data_wage$agency != "0" & cat_svsr_diff == "0"] = "zero"
  
  data_retallow_svsr = data.frame(data_wage, paybasic_0=paybasic_0, retallow=bs_retallow[,J1], cat_retallow=cat_retallow, svsr_diff = bs_svsr_diff[,J1], cat_svsr_diff=cat_svsr_diff)
  
  save(data_retallow_svsr, file=paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))

print(paste("YEAR",J1,"Done"))
print(table(cat_svsr_diff))
print(table(cat_retallow))

}

sapply(1:24,data.svsr_diff.retallow)


#######################
#######################

setwd("~/Path/SyntheticData")
rm(list=ls())
library(parallel)
library(tree)


load("Synthetic_Files/bs_synthetic_workschd.RData")
load("Synthetic_Files/bs_synthetic_localpay.RData")
load("Synthetic_Files/bs_synthetic_pay_plan.RData")
load("Synthetic_Files/bs_synthetic_grade.RData")
load("Synthetic_Files/bs_synthetic_steprate.RData")
load("Synthetic_Files/bs_synthetic_paybasis.RData")
load("Synthetic_Files/bs_synthetic_payrated.RData")
load("Synthetic_Files/bs_synthetic_paybasic.RData")
load("Synthetic_Files/bs_synthetic_wage.RData")



data.svsr_diff.retallow<- function(J1)
{

  load(paste("Synthetic_Files/data_synthetic_wage_year_",J1,".RData",sep=""))

  IND1 = data_synthetic_wage$agency != "0"

  data_synthetic_wage = data_synthetic_wage[IND1,]

  synthetic_workschd = bs_synthetic_workschd[IND1,J1]
  synthetic_localpay = bs_synthetic_localpay[IND1,J1]
  synthetic_pay_plan = bs_synthetic_pay_plan[IND1,J1]
  synthetic_grade =    bs_synthetic_grade[IND1,J1]
  synthetic_steprate = bs_synthetic_steprate[IND1,J1]
  synthetic_paybasis = bs_synthetic_paybasis[IND1,J1]
  synthetic_payrated = bs_synthetic_payrated[IND1,J1]
  synthetic_paybasic = bs_synthetic_paybasic[IND1,J1]
  synthetic_wage = bs_synthetic_wage[IND1,J1]

   
  Synthetic_Wage = bs_synthetic_wage[IND1,J1]
  
  paybasic_0 = synthetic_paybasic
  paybasic_0[is.na(paybasic_0)] = "NA.paybasic"
  paybasic_0[synthetic_workschd != "0" & paybasic_0 == "0"] = "zero"
  unique_wage = unique(Synthetic_Wage[synthetic_workschd!="0"])
  f <- function(J)
  {
    Ind = unique_wage[J] == Synthetic_Wage
    Ind2 = paybasic_0[Ind] != "zero" &  paybasic_0[Ind] != "NA.paybasic"
    if(sum(Ind2)>0)
    {
      m=mean(as.numeric( paybasic_0[Ind][Ind2]))
      paybasic_0[Ind][paybasic_0[Ind] == "zero" |  paybasic_0[Ind] == "NA.paybasic"] = m
    }
    
    if(J%%1000==0)# & length(names_paybasic)<=10000)        
      print(paste(J,"of",length(unique_wage)))
    
    list(Ind=which(Ind), pay = paybasic_0[Ind])
  }
  tmp = mclapply(1:length(unique_wage),f,mc.cores=45)  
  f <- function(J){paybasic_0[tmp[[J]]$Ind]  <<-  tmp[[J]]$pay }
  aux1<-sapply(1:length(unique_wage),f)
  MEAN = mean(as.numeric(paybasic_0[paybasic_0 != "zero" &  paybasic_0 != "NA.paybasic"]))
  paybasic_0[paybasic_0 == "zero" |  paybasic_0 == "NA.paybasic"] = MEAN
  paybasic_0 = as.numeric(paybasic_0)
  print(paste(mean(paybasic_0[paybasic_0!="0"]),"sinthetic paybasic in year",J1-1))

  tmp = rep(0,length(IND1))
  tmp[IND1] = paybasic_0    
  paybasic_0 = tmp
 

  load(paste("Synthetic_Files/data_synthetic_wage_year_",J1,".RData",sep=""))

  synthetic_workschd = bs_synthetic_workschd[,J1]
  synthetic_localpay = bs_synthetic_localpay[,J1]
  synthetic_pay_plan = bs_synthetic_pay_plan[,J1]
  synthetic_grade =    bs_synthetic_grade[,J1]
  synthetic_steprate = bs_synthetic_steprate[,J1]
  synthetic_paybasis = bs_synthetic_paybasis[,J1]
  synthetic_payrated = bs_synthetic_payrated[,J1]
  synthetic_paybasic = bs_synthetic_paybasic[,J1]
  synthetic_wage = bs_synthetic_wage[,J1]

  data_synthetic_retallow_svsr = data.frame(data_synthetic_wage,
                                            grade    = synthetic_grade,     
                                            localpay =synthetic_localpay, 
                                            pay_plan =synthetic_pay_plan,  
                                            paybasic_0 = paybasic_0,  
                                            paybasic = synthetic_paybasic,  
                                            paybasis =synthetic_paybasis, 
                                            payrated =synthetic_payrated,  
                                            steprate =synthetic_steprate,  
                                            workschd =synthetic_workschd) 
  
  save(data_synthetic_retallow_svsr, file=paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1,".RData",sep=""))

print(paste("YEAR",J1,"Done"))
#print(table(cat_svsr_diff))
#print(table(cat_retallow))
gc()
}

sapply(1:24,data.svsr_diff.retallow)

#########################################
#########################################

setwd("~/Path/SyntheticData")
rm(list=ls())
library(parallel)
library(tree)

source("Code/99.02.cartdraw.R")
source("Code/99.03.cartdraw2.R")



J1 = 1

mcl.collapse <- function(collapse)
  simplify2array(mclapply(1:nrow(collapse),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

######################################## 1) YEAR 1 cat_retallow
############################
load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
agency_occ = mcl.collapse(cbind(
  as.vector(data_retallow_svsr$agency),
  as.vector(data_retallow_svsr$occ),
  as.vector(data_retallow_svsr$pay_plan),
  as.vector(data_retallow_svsr$grade),
  as.vector(data_retallow_svsr$localpay)))

load(paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1,".RData",sep=""))
synthetic_agency_occ = mcl.collapse(cbind(
  as.vector(data_synthetic_retallow_svsr$agency),
  as.vector(data_synthetic_retallow_svsr$occ),
  as.vector(data_synthetic_retallow_svsr$pay_plan),
  as.vector(data_synthetic_retallow_svsr$grade),
  as.vector(data_synthetic_retallow_svsr$localpay)))

cbind(head(agency_occ,20),head(synthetic_agency_occ,20))
mean(synthetic_agency_occ %in% agency_occ)


Collapse = cbind(
  as.vector(agency_occ),
  as.vector(agency_occ))

Synthetic_Collapse = cbind(
  as.vector(synthetic_agency_occ),
  as.vector(synthetic_agency_occ))


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

variables = c("agency","gender","race","educ_lvl","age","init_yrsdegrng",
              "milmonths","init_instrctpgm","total_year","number_agency",
              "M","occ","occ_cat","funcclas","flsa","appttype","polappttype",
              "worked_years","position","tenure","svsrstat",
              "grade","localpay","pay_plan","paybasic","paybasis","payrated",
              "steprate", "workschd", "collapse") 

synthetic_retallow_svsr_multi = matrix("0",nrow=nrow(Ind_synthetic_collapse),ncol=1) 

for(J in 2:1)
{
  if(sum( Ind_synthetic_collapse[,J]) > 0)
  {
    
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J))
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
    
    data_retallow_svsr_tmp = data.frame(data_retallow_svsr,collapse=Collapse[,J])
    data_synthetic_retallow_svsr_tmp = data.frame(data_synthetic_retallow_svsr, collapse=Synthetic_Collapse[,J])
    
    
    ## Start Algorithm
    data_general=data_retallow_svsr_tmp[Ind_collapse[,J],,drop=FALSE]
    data_synthetic_general=data_synthetic_retallow_svsr_tmp[Ind_synthetic_collapse[,J],,drop=FALSE]
    Goal="cat_retallow"
    variables=variables
    N.samples=1
    
    
    # Getting collapse for mono_table, mono_tree and multi_tree
    cores.table=30
    source("Code/10.00.01.Init_mono_multi.R")
    
    
    # Fitting multi_tree
    if(length(multi_tree)>0)
    {
      BIG = 1e5
      backup_multi_tree = multi_tree
      source("Code/10.00.02.Init_multi_tree.R")
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
        source("Code/10.00.03.Big_multi_tree.R")
      }
      # Small
      if(length(Small_multi_tree)>0)
      {
        multi_tree = Small_multi_tree
        split.multi.tree=5000
        cores.multi.tree=30
        N.cartdraw = 10000
        cores.cartdraw = 2
        source("Code/10.00.04.multi_tree.R")
      }
    }
    
    
    # Matching with mono_table
    pre.cores.table = 20
    cores.table = 30
    split.mono.table=15000
    cores.mono.table=40
    source("Code/10.00.05.mono_table.R")
    
    
    # Fitting mono_tree
    split.mono.tree=10000
    cores.mono.tree=30
    N.cartdraw = 10000
    cores.cartdraw = 2
    source("Code/10.00.06.mono_tree.R")
    
    
    # End
    tmp = matrix("0",nrow=length(synthetic_agency),ncol=N.samples)
    tmp[synthetic_agency!="0",] = synthetic_Goal
    synthetic_retallow_svsr_multi[Ind_synthetic_collapse[,J],] = tmp
    
  } 
}

synthetic_cat_retallow = synthetic_retallow_svsr_multi

save(synthetic_cat_retallow,file=paste("Synthetic_Files/synthetic_cat_retallow_year_",J1,".RData",sep=""))

print(" % ca_retallow")
print(sort(table(data_retallow_svsr$cat_retallow)));print(round(nrow(data_retallow_svsr)/nrow(data_synthetic_retallow_svsr)*sort(table(synthetic_cat_retallow))))


######################################## 1) YEAR 1 cat_svsr_diff

#### Collapsed variables -- used to split the data
## Collapsing agency + occ

load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
agency_occ = mcl.collapse(cbind(
  as.vector(data_retallow_svsr$agency),
  as.vector(data_retallow_svsr$occ),
  as.vector(data_retallow_svsr$pay_plan),
  as.vector(data_retallow_svsr$grade),
  as.vector(data_retallow_svsr$localpay)))

load(paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1,".RData",sep=""))
load(paste("Synthetic_Files/synthetic_cat_retallow_year_",J1,".RData",sep=""))
data_synthetic_retallow_svsr = data.frame(data_synthetic_retallow_svsr, cat_retallow=synthetic_cat_retallow)

synthetic_agency_occ = mcl.collapse(cbind(
  as.vector(data_synthetic_retallow_svsr$agency),
  as.vector(data_synthetic_retallow_svsr$occ),
  as.vector(data_synthetic_retallow_svsr$pay_plan),
  as.vector(data_synthetic_retallow_svsr$grade),
  as.vector(data_synthetic_retallow_svsr$localpay)))

cbind(head(agency_occ,20),head(synthetic_agency_occ,20))
mean(synthetic_agency_occ %in% agency_occ)


Collapse = cbind(
  as.vector(agency_occ),
  as.vector(agency_occ))

Synthetic_Collapse = cbind(
  as.vector(synthetic_agency_occ),
  as.vector(synthetic_agency_occ))


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

variables = c("agency","gender","race","educ_lvl","age","init_yrsdegrng",
              "milmonths","init_instrctpgm","total_year","number_agency",
              "M","occ","occ_cat","funcclas","flsa","appttype","polappttype",
              "worked_years","position","tenure","svsrstat",
              "grade","localpay","pay_plan","paybasic","paybasis","payrated",
              "steprate", "workschd", "cat_retallow", "collapse") 

synthetic_retallow_svsr_multi = matrix("0",nrow=nrow(Ind_synthetic_collapse),ncol=1) 


for(J in 2:1)
{
  if(sum( Ind_synthetic_collapse[,J]) > 0)
  {
    
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J))
    print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
    
    data_retallow_svsr_tmp = data.frame(data_retallow_svsr,collapse=Collapse[,J])
    data_synthetic_retallow_svsr_tmp = data.frame(data_synthetic_retallow_svsr, collapse=Synthetic_Collapse[,J])
    
    
    ## Start Algorithm
    data_general=data_retallow_svsr_tmp[Ind_collapse[,J],,drop=FALSE]
    data_synthetic_general=data_synthetic_retallow_svsr_tmp[Ind_synthetic_collapse[,J],,drop=FALSE]
    Goal="cat_svsr_diff"
    variables=variables
    N.samples=1
    
    
    # Getting collapse for mono_table, mono_tree and multi_tree
    cores.table=30
    source("Code/10.00.01.Init_mono_multi.R")
    
    
    # Fitting multi_tree
    if(length(multi_tree)>0)
    {
      BIG = 1e5
      backup_multi_tree = multi_tree
      source("Code/10.00.02.Init_multi_tree.R")
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
        source("Code/10.00.03.Big_multi_tree.R")
      }
      # Small
      if(length(Small_multi_tree)>0)
      {
        multi_tree = Small_multi_tree
        split.multi.tree=5000
        cores.multi.tree=30
        N.cartdraw = 10000
        cores.cartdraw = 2
        source("Code/10.00.04.multi_tree.R")
      }
    }
    
    
    # Matching with mono_table
    pre.cores.table = 20
    cores.table = 30
    split.mono.table=15000
    cores.mono.table=40
    source("Code/10.00.05.mono_table.R")
    
    
    # Fitting mono_tree
    split.mono.tree=10000
    cores.mono.tree=30
    N.cartdraw = 10000
    cores.cartdraw = 2
    source("Code/10.00.06.mono_tree.R")
    
    
    # End
    tmp = matrix("0",nrow=length(synthetic_agency),ncol=N.samples)
    tmp[synthetic_agency!="0",] = synthetic_Goal
    synthetic_retallow_svsr_multi[Ind_synthetic_collapse[,J],] = tmp
    
  } 
}

synthetic_cat_svsr_diff = synthetic_retallow_svsr_multi

save(synthetic_cat_svsr_diff,file=paste("Synthetic_Files/synthetic_cat_svsr_diff_year_",J1,".RData",sep=""))

print(" % ca_cat_svsr_diff")
print(sort(table(data_retallow_svsr$cat_svsr_diff)));print(round(nrow(data_retallow_svsr)/nrow(data_synthetic_retallow_svsr)*sort(table(synthetic_cat_svsr_diff))))



#############################################
#############################################


#####################
#####################

for(J1 in 2:24)
{
  
  ##########################
  ## Generating data
  ##########################
  #data.svsr_diff.retallow(J1)

  ######################################## 1) OTHER YEARS cat_retallow
  
  #### Collapsed variables -- used to split the data
  ## Collapsing agency + occ
  load(paste("Aux_Files/data_retallow_svsr_year_",J1-1,".RData",sep=""))
  cat_retallow_0 = as.vector(data_retallow_svsr$cat_retallow)
  load(paste("Synthetic_Files/synthetic_cat_retallow_year_",J1-1,".RData",sep=""))
  synthetic_cat_retallow_0 =  synthetic_cat_retallow
  
  load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
  agency_occ = mcl.collapse(cbind(
    as.vector(data_retallow_svsr$agency),
    as.vector(data_retallow_svsr$occ),
    as.vector(data_retallow_svsr$pay_plan),
    as.vector(data_retallow_svsr$grade),
    as.vector(data_retallow_svsr$localpay)))
  
  collapse = mcl.collapse(cbind(
    as.vector(agency_occ),
    as.vector(cat_retallow_0)))
  
  data_retallow_svsr = data.frame(data_retallow_svsr,cat_retallow_0=cat_retallow_0)
  
  
  load(paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1,".RData",sep=""))
  synthetic_agency_occ = mcl.collapse(cbind(
    as.vector(data_synthetic_retallow_svsr$agency),
    as.vector(data_synthetic_retallow_svsr$occ),
    as.vector(data_synthetic_retallow_svsr$pay_plan),
    as.vector(data_synthetic_retallow_svsr$grade),
    as.vector(data_synthetic_retallow_svsr$localpay)))
  
  synthetic_collapse = mcl.collapse(cbind(
    as.vector(synthetic_agency_occ),
    as.vector(synthetic_cat_retallow_0)))
  
  data_synthetic_retallow_svsr = data.frame(data_synthetic_retallow_svsr,cat_retallow_0=synthetic_cat_retallow_0)
  
  
  print(cbind(head(collapse,20)));print(cbind(head(synthetic_collapse,20)))
  print(mean(synthetic_collapse %in% collapse))
  
  print(cbind(head(agency_occ,20)));print(cbind(head(synthetic_agency_occ,20)))
  print(mean(synthetic_agency_occ %in% agency_occ))
  
  
  Collapse = cbind(
    as.vector(collapse),
    as.vector(agency_occ))
  
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse),
    as.vector(synthetic_agency_occ))
  
  
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
  
  variables = c("agency","gender","race","educ_lvl","age","init_yrsdegrng",
                "milmonths","init_instrctpgm","total_year","number_agency",
                "M","occ","occ_cat","funcclas","flsa","appttype","polappttype",
                "worked_years","position","tenure","svsrstat",
                "grade","localpay","pay_plan","paybasic","paybasis","payrated",
                "steprate", "workschd", "cat_retallow_0", "collapse") 
  
  synthetic_retallow_svsr_multi = matrix("0",nrow=nrow(Ind_synthetic_collapse),ncol=1) 
  
  
  for(J in 2:1)
  {
    if(sum( Ind_synthetic_collapse[,J]) > 0)
    {
      
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J))
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
      
      data_retallow_svsr_tmp = data.frame(data_retallow_svsr,collapse=Collapse[,J])
      data_synthetic_retallow_svsr_tmp = data.frame(data_synthetic_retallow_svsr, collapse=Synthetic_Collapse[,J])
      
      
      ## Start Algorithm
      data_general=data_retallow_svsr_tmp[Ind_collapse[,J],,drop=FALSE]
      data_synthetic_general=data_synthetic_retallow_svsr_tmp[Ind_synthetic_collapse[,J],,drop=FALSE]
      Goal="cat_retallow"
      variables=variables
      N.samples=1
      
      
      # Getting collapse for mono_table, mono_tree and multi_tree
      cores.table=30
      source("Code/10.00.01.Init_mono_multi.R")
      
      
      # Fitting multi_tree
      if(length(multi_tree)>0)
      {
        BIG = 1e5
        backup_multi_tree = multi_tree
        source("Code/10.00.02.Init_multi_tree.R")
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
          source("Code/10.00.03.Big_multi_tree.R")
        }
        # Small
        if(length(Small_multi_tree)>0)
        {
          multi_tree = Small_multi_tree
          split.multi.tree=5000
          cores.multi.tree=30
          N.cartdraw = 10000
          cores.cartdraw = 2
          source("Code/10.00.04.multi_tree.R")
        }
      }
      
      
      # Matching with mono_table
      pre.cores.table = 20
      cores.table = 30
      split.mono.table=15000
      cores.mono.table=40
      source("Code/10.00.05.mono_table.R")
      
      
      # Fitting mono_tree
      split.mono.tree=10000
      cores.mono.tree=30
      N.cartdraw = 10000
      cores.cartdraw = 2
      source("Code/10.00.06.mono_tree.R")
      
      
      # End
      tmp = matrix("0",nrow=length(synthetic_agency),ncol=N.samples)
      tmp[synthetic_agency!="0",] = synthetic_Goal
      synthetic_retallow_svsr_multi[Ind_synthetic_collapse[,J],] = tmp
      
    } 
  }
  
  synthetic_cat_retallow = synthetic_retallow_svsr_multi
  
  save(synthetic_cat_retallow,file=paste("Synthetic_Files/synthetic_cat_retallow_year_",J1,".RData",sep=""))
  
  print(" % ca_retallow")
  print(sort(table(data_retallow_svsr$cat_retallow)));print(round(nrow(data_retallow_svsr)/nrow(data_synthetic_retallow_svsr)*sort(table(synthetic_cat_retallow))))
  
  
  ######################################## 1) OTHER YEARS cat_svsr_diff
  
  #### Collapsed variables -- used to split the data
  ## Collapsing agency + occ
  load(paste("Aux_Files/data_retallow_svsr_year_",J1-1,".RData",sep=""))
  cat_svsr_diff_0 = as.vector(data_retallow_svsr$svsr_diff)
  load(paste("Synthetic_Files/synthetic_cat_svsr_diff_year_",J1-1,".RData",sep=""))
  synthetic_cat_svsr_diff_0 =  synthetic_cat_svsr_diff
  
  load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
  data_retallow_svsr = data.frame(data_retallow_svsr,cat_svsr_diff_0=cat_svsr_diff_0)
  
  agency_occ = mcl.collapse(cbind(
    as.vector(data_retallow_svsr$agency),
    as.vector(data_retallow_svsr$occ),
    as.vector(data_retallow_svsr$pay_plan),
    as.vector(data_retallow_svsr$grade),
    as.vector(data_retallow_svsr$localpay)))
  
  collapse = mcl.collapse(cbind(
    as.vector(agency_occ),
    as.vector(cat_svsr_diff_0)))
  
  load(paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1,".RData",sep=""))
  load(paste("Synthetic_Files/synthetic_cat_retallow_year_",J1,".RData",sep=""))
  data_synthetic_retallow_svsr = data.frame(data_synthetic_retallow_svsr, cat_retallow=synthetic_cat_retallow,  cat_svsr_diff_0=synthetic_cat_svsr_diff_0)
  
  synthetic_agency_occ = mcl.collapse(cbind(
    as.vector(data_synthetic_retallow_svsr$agency),
    as.vector(data_synthetic_retallow_svsr$occ),
    as.vector(data_synthetic_retallow_svsr$pay_plan),
    as.vector(data_synthetic_retallow_svsr$grade),
    as.vector(data_synthetic_retallow_svsr$localpay)))
  
  synthetic_collapse = mcl.collapse(cbind(
    as.vector(synthetic_agency_occ),
    as.vector(synthetic_cat_svsr_diff_0)))
  
  print(cbind(head(collapse,20)));print(cbind(head(synthetic_collapse,20)))
  print(mean(synthetic_collapse %in% collapse))
  
  print(cbind(head(agency_occ,20)));print(cbind(head(synthetic_agency_occ,20)))
  print(mean(synthetic_agency_occ %in% agency_occ))
  
  
  Collapse = cbind(
    as.vector(collapse),
    as.vector(agency_occ))
  
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse),
    as.vector(synthetic_agency_occ))
  
  
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
  
  variables = c("agency","gender","race","educ_lvl","age","init_yrsdegrng",
                "milmonths","init_instrctpgm","total_year","number_agency",
                "M","occ","occ_cat","funcclas","flsa","appttype","polappttype",
                "worked_years","position","tenure","svsrstat",
                "grade","localpay","pay_plan","paybasic","paybasis","payrated",
                "steprate", "workschd", "cat_retallow", "cat_svsr_diff_0", "collapse") 
  
  synthetic_retallow_svsr_multi = matrix("0",nrow=nrow(Ind_synthetic_collapse),ncol=1) 
  
  
  for(J in 2:1)
  {
    if(sum( Ind_synthetic_collapse[,J]) > 0)
    {
      
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("COLLAPSE",J))
      print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
      
      data_retallow_svsr_tmp = data.frame(data_retallow_svsr,collapse=Collapse[,J])
      data_synthetic_retallow_svsr_tmp = data.frame(data_synthetic_retallow_svsr, collapse=Synthetic_Collapse[,J])
      
      
      ## Start Algorithm
      data_general=data_retallow_svsr_tmp[Ind_collapse[,J],,drop=FALSE]
      data_synthetic_general=data_synthetic_retallow_svsr_tmp[Ind_synthetic_collapse[,J],,drop=FALSE]
      Goal="cat_svsr_diff"
      variables=variables
      N.samples=1
      
      
      # Getting collapse for mono_table, mono_tree and multi_tree
      cores.table=30
      source("Code/10.00.01.Init_mono_multi.R")
      
      
      # Fitting multi_tree
      if(length(multi_tree)>0)
      {
        BIG = 1e5
        backup_multi_tree = multi_tree
        source("Code/10.00.02.Init_multi_tree.R")
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
          source("Code/10.00.03.Big_multi_tree.R")
        }
        # Small
        if(length(Small_multi_tree)>0)
        {
          multi_tree = Small_multi_tree
          split.multi.tree=5000
          cores.multi.tree=30
          N.cartdraw = 10000
          cores.cartdraw = 2
          source("Code/10.00.04.multi_tree.R")
        }
      }
      
      
      # Matching with mono_table
      pre.cores.table = 20
      cores.table = 30
      split.mono.table=15000
      cores.mono.table=40
      source("Code/10.00.05.mono_table.R")
      
      
      # Fitting mono_tree
      split.mono.tree=10000
      cores.mono.tree=30
      N.cartdraw = 10000
      cores.cartdraw = 2
      source("Code/10.00.06.mono_tree.R")
      
      
      # End
      tmp = matrix("0",nrow=length(synthetic_agency),ncol=N.samples)
      tmp[synthetic_agency!="0",] = synthetic_Goal
      synthetic_retallow_svsr_multi[Ind_synthetic_collapse[,J],] = tmp
      
    } 
  }
  
  synthetic_cat_svsr_diff = synthetic_retallow_svsr_multi
  
  save(synthetic_cat_svsr_diff,file=paste("Synthetic_Files/synthetic_cat_svsr_diff_year_",J1,".RData",sep=""))
  
  print(" % ca_svsr_diff")
  print(sort(table(data_retallow_svsr$cat_svsr_diff)));print(round(nrow(data_retallow_svsr)/nrow(data_synthetic_retallow_svsr)*sort(table(synthetic_cat_svsr_diff))))
  
}


######################################## 1) YEAR 1,2,3 retallow and svsr_diff
for(J1 in 1:3)
{
  load(paste("Synthetic_Files/synthetic_cat_retallow_year_",J1,".RData",sep=""))
  synthetic_retallow = synthetic_cat_retallow
  save(synthetic_retallow,file=paste("Synthetic_Files/synthetic_retallow_year_",J1,".RData",sep=""))
  
  load(paste("Synthetic_Files/synthetic_cat_svsr_diff_year_",J1,".RData",sep=""))
  synthetic_svsr_diff = synthetic_cat_svsr_diff
  save(synthetic_svsr_diff,file=paste("Synthetic_Files/synthetic_svsr_diff_year_",J1,".RData",sep=""))
  
  print("##########################")
  print(paste("################## -- Year",J1))
  print("##########################")
  
  print(tail(sort(table(synthetic_retallow))))
  print(tail(sort(table(synthetic_cat_retallow))))
  
  print(tail(sort(table(synthetic_svsr_diff))))
  print(tail(sort(table(synthetic_cat_svsr_diff))))
  
}



J1 = 4
######################################## 1) YEAR 4 retallow and svsr_diff
load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
load(paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1,".RData",sep=""))

# retallow
load(paste("Synthetic_Files/synthetic_cat_retallow_year_",J1,".RData",sep=""))
synthetic_retallow = synthetic_cat_retallow

Ind = data_retallow_svsr$cat_retallow == "Num_Value"
perc_retallow = as.numeric(as.vector(data_retallow_svsr$retallow[Ind]))/as.numeric(as.vector(data_retallow_svsr$paybasic_0[Ind]))

print("##########################")
print(paste("################## -- Year",J1))
print("##########################")

print(paste("retallow",mean(as.numeric(as.vector(data_retallow_svsr$retallow[Ind])))))

Ind = synthetic_retallow == "Num_Value"
synthetic_retallow[Ind] = round(data_synthetic_retallow_svsr$paybasic_0[Ind]*sample(perc_retallow,sum(Ind),T))

print(paste("synthetic retallow",mean(as.numeric(as.vector(synthetic_retallow[Ind])))))

save(synthetic_retallow,file=paste("Synthetic_Files/synthetic_retallow_year_",J1,".RData",sep=""))

# svsr_diff
load(paste("Synthetic_Files/synthetic_cat_svsr_diff_year_",J1,".RData",sep=""))
synthetic_svsr_diff = synthetic_cat_svsr_diff

Ind = data_retallow_svsr$cat_svsr_diff == "Num_Value"
perc_svsr_diff = as.numeric(as.vector(data_retallow_svsr$svsr_diff[Ind]))/as.numeric(as.vector(data_retallow_svsr$paybasic_0[Ind]))

print(paste("svsr_diff",mean(as.numeric(as.vector(data_retallow_svsr$svsr_diff[Ind])))))

Ind = synthetic_svsr_diff == "Num_Value"
synthetic_svsr_diff[Ind] = round(data_synthetic_retallow_svsr$paybasic_0[Ind]*sample(perc_svsr_diff,sum(Ind),T))

print(paste("synthetic svsr_diff",mean(as.numeric(as.vector(synthetic_svsr_diff[Ind])))))

save(synthetic_svsr_diff,file=paste("Synthetic_Files/synthetic_svsr_diff_year_",J1,".RData",sep=""))


######################################## 1) OTHER YEARS retallow and svsr_diff
for(J1 in 5:24)
{
  
  print("##########################")
  print(paste("################## -- Year",J1))
  print("##########################")
  
  ### Checking
  load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
  Ind_A=data_retallow_svsr$cat_retallow == "Num_Value"
  load(paste("Synthetic_Files/synthetic_cat_retallow_year_",J1,".RData",sep=""))
  Ind_B = synthetic_cat_retallow == "Num_Value"
  if((sum(Ind_A)>0 & sum(Ind_B)>0)==FALSE)
  {
    load(paste("Synthetic_Files/synthetic_cat_retallow_year_",J1,".RData",sep=""))
    synthetic_retallow = synthetic_cat_retallow
    save(synthetic_retallow,file=paste("Synthetic_Files/synthetic_retallow_year_",J1,".RData",sep=""))
  }
  
  
  if(sum(Ind_A)>0 & sum(Ind_B)>0)
  {
    # retallow
    load(paste("Aux_Files/data_retallow_svsr_year_",J1-1,".RData",sep=""))
    Ind_0=data_retallow_svsr$cat_retallow == "Num_Value"
    load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
    Ind_1=data_retallow_svsr$cat_retallow == "Num_Value"
    
    Ind = Ind_0 == TRUE & Ind_1 == TRUE
    
    load(paste("Aux_Files/data_retallow_svsr_year_",J1-1,".RData",sep=""))
    retallow_0 = as.numeric(as.vector(data_retallow_svsr$retallow[Ind]))
    paybasic_0 = as.numeric(as.vector(data_retallow_svsr$paybasic_0[Ind]))
    perc_retallow_0 = retallow_0/paybasic_0
    
    load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
    retallow_1 = as.numeric(as.vector(data_retallow_svsr$retallow[Ind]))
    paybasic_1 = as.numeric(as.vector(data_retallow_svsr$paybasic_0[Ind]))
    perc_retallow_1 = retallow_1/paybasic_1
    
    prob =  mean(abs(perc_retallow_0  - perc_retallow_1)<0.02)
    
    load(paste("Synthetic_Files/synthetic_cat_retallow_year_",J1-1,".RData",sep=""))
    Ind_0 = synthetic_cat_retallow == "Num_Value"
    load(paste("Synthetic_Files/synthetic_cat_retallow_year_",J1,".RData",sep=""))
    synthetic_retallow_tmp = synthetic_cat_retallow
    Ind_1 = synthetic_cat_retallow == "Num_Value"
    
    Ind = Ind_0 == TRUE & Ind_1 == TRUE &  rbinom(length(Ind_0),1,prob) == 1
    Ind_syn = (Ind_1 - Ind)==1
    
    if(sum(Ind)>0)
    {
      load(paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1-1,".RData",sep=""))
      load(paste("Synthetic_Files/synthetic_retallow_year_",J1-1,".RData",sep=""))
      perc_retallow = as.numeric(as.vector(synthetic_retallow[Ind]))/data_synthetic_retallow_svsr$paybasic_0[Ind]
      load(paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1,".RData",sep=""))
      synthetic_retallow_tmp[Ind] = perc_retallow*data_synthetic_retallow_svsr$paybasic_0[Ind]
    }
    
    
    if(sum(Ind_syn)>0)
    {
      
      load(paste("Aux_Files/data_retallow_svsr_year_",J1-1,".RData",sep=""))
      Ind_0=data_retallow_svsr$cat_retallow == "Num_Value"
      load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
      Ind_1=data_retallow_svsr$cat_retallow == "Num_Value"
      
      load(paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1,".RData",sep=""))
      
      load(paste("Synthetic_Files/synthetic_cat_retallow_year_",J1,".RData",sep=""))
      
      Ind = Ind_0 == FALSE & Ind_1 == TRUE
      
      perc_retallow = as.numeric(as.vector(data_retallow_svsr$retallow[Ind]))/as.numeric(as.vector(data_retallow_svsr   $paybasic[Ind]))
      
      
      synthetic_retallow_tmp[Ind_syn] = data_synthetic_retallow_svsr$paybasic_0[Ind_syn]*sample(perc_retallow,sum(Ind_syn),T)
      
      print(paste("synthetic retallow",mean(as.numeric(as.vector(synthetic_retallow_tmp[Ind_syn])))))
    }
    synthetic_retallow = synthetic_retallow_tmp
    
    print(paste("retallow -- Year",J1,round(mean(as.numeric(as.vector(data_retallow_svsr$retallow[data_retallow_svsr$cat_retallow == "Num_Value"]))))))
    print(paste("synthetic retallow -- Year",J1,round(mean(as.numeric(as.vector(synthetic_retallow_tmp[synthetic_cat_retallow == "Num_Value"]))))))
    
    
    save(synthetic_retallow,file=paste("Synthetic_Files/synthetic_retallow_year_",J1,".RData",sep=""))
    
  }
  print(tail(sort(table(synthetic_retallow))))
  print(tail(sort(table(synthetic_cat_retallow))))
  
  ########################
  ########################
  
  ### Checking
  load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
  Ind_C=data_retallow_svsr$cat_svsr_diff == "Num_Value"
  load(paste("Synthetic_Files/synthetic_cat_svsr_diff_year_",J1,".RData",sep=""))
  Ind_D = synthetic_cat_svsr_diff == "Num_Value"
  
  if((sum(Ind_C)>0 & sum(Ind_D)>0)==FALSE)
  {
    load(paste("Synthetic_Files/synthetic_cat_svsr_diff_year_",J1,".RData",sep=""))
    synthetic_svsr_diff = synthetic_cat_svsr_diff
    save(synthetic_svsr_diff,file=paste("Synthetic_Files/synthetic_svsr_diff_year_",J1,".RData",sep=""))
  }
  
  
  if(sum(Ind_C)>0 & sum(Ind_D)>0)
  {
    # svsr_diff
    
    load(paste("Aux_Files/data_retallow_svsr_year_",J1-1,".RData",sep=""))
    Ind_0=data_retallow_svsr$cat_svsr_diff == "Num_Value"
    load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
    Ind_1=data_retallow_svsr$cat_svsr_diff == "Num_Value"
    
    Ind = Ind_0 == TRUE & Ind_1 == TRUE
    
    load(paste("Aux_Files/data_retallow_svsr_year_",J1-1,".RData",sep=""))
    svsr_diff_0 = as.numeric(as.vector(data_retallow_svsr$svsr_diff[Ind]))
    paybasic_0 = as.numeric(as.vector(data_retallow_svsr$paybasic_0[Ind]))
    perc_svsr_diff_0 = svsr_diff_0/paybasic_0
    
    load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
    svsr_diff_1 = as.numeric(as.vector(data_retallow_svsr$svsr_diff[Ind]))
    paybasic_1 = as.numeric(as.vector(data_retallow_svsr$paybasic_0[Ind]))
    perc_svsr_diff_1 = svsr_diff_1/paybasic_1
    
    prob =  mean(abs(perc_svsr_diff_0  - perc_svsr_diff_1)<0.02)
    
    
    
    load(paste("Synthetic_Files/synthetic_cat_svsr_diff_year_",J1-1,".RData",sep=""))
    Ind_0 = synthetic_cat_svsr_diff == "Num_Value"
    load(paste("Synthetic_Files/synthetic_cat_svsr_diff_year_",J1,".RData",sep=""))
    synthetic_svsr_diff_tmp = synthetic_cat_svsr_diff
    Ind_1 = synthetic_cat_svsr_diff == "Num_Value"
    
    Ind = Ind_0 == TRUE & Ind_0 == Ind_1 &  rbinom(length(Ind_0),1,prob) == 1
    Ind_syn = (Ind_1 - Ind)==1
    
    if(sum(Ind)>0)
    {
      load(paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1-1,".RData",sep=""))
      load(paste("Synthetic_Files/synthetic_svsr_diff_year_",J1-1,".RData",sep=""))
      perc_svsr_diff = as.numeric(as.vector(synthetic_svsr_diff[Ind]))/data_synthetic_retallow_svsr$paybasic_0[Ind]
      load(paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1,".RData",sep=""))
      synthetic_svsr_diff_tmp[Ind] = perc_svsr_diff*data_synthetic_retallow_svsr$paybasic_0[Ind]
    }
    
    
    if(sum(Ind_syn)>0)
    {
      
      load(paste("Aux_Files/data_retallow_svsr_year_",J1-1,".RData",sep=""))
      Ind_0=data_retallow_svsr$cat_svsr_diff == "Num_Value"
      load(paste("Aux_Files/data_retallow_svsr_year_",J1,".RData",sep=""))
      Ind_1=data_retallow_svsr$cat_svsr_diff == "Num_Value"
      
      load(paste("Synthetic_Files/data_synthetic_retallow_svsr_year_",J1,".RData",sep=""))
      load(paste("Synthetic_Files/synthetic_cat_svsr_diff_year_",J1,".RData",sep=""))
      
      Ind = Ind_0 == FALSE & Ind_1 == TRUE
      
      perc_svsr_diff = as.numeric(as.vector(data_retallow_svsr$svsr_diff[Ind]))/as.numeric(as.vector(data_retallow_svsr   $paybasic[Ind]))
      
      
      synthetic_svsr_diff_tmp[Ind_syn] = data_synthetic_retallow_svsr$paybasic_0[Ind_syn]*sample(perc_svsr_diff,sum(Ind_syn),T)
      
    }
    synthetic_svsr_diff = synthetic_svsr_diff_tmp
    
    print(paste("svsr_diff -- Year",J1,round(mean(as.numeric(as.vector(data_retallow_svsr$svsr_diff[data_retallow_svsr$cat_svsr_diff == "Num_Value"]))))))
    print(paste("synthetic svsr_diff -- Year",J1,round(mean(as.numeric(as.vector(synthetic_svsr_diff_tmp[synthetic_cat_svsr_diff == "Num_Value"]))))))
    
    save(synthetic_svsr_diff,file=paste("Synthetic_Files/synthetic_svsr_diff_year_",J1,".RData",sep=""))
    
  }
  
  print(tail(sort(table(synthetic_svsr_diff))))
  print(tail(sort(table(synthetic_cat_svsr_diff))))
}


#############################
#############################

load("Synthetic_Files/bs_synthetic_paybasic.RData")

bs_synthetic_retallow  = bs_synthetic_paybasic
bs_synthetic_svsr_diff = bs_synthetic_paybasic

for(J1 in 1:24)
{
  print(J1)
  load(paste("Synthetic_Files/synthetic_svsr_diff_year_",J1,".RData",sep=""))
  load(paste("Synthetic_Files/synthetic_retallow_year_",J1,".RData",sep=""))
  bs_synthetic_retallow[,J1]  = synthetic_retallow
  bs_synthetic_svsr_diff[,J1] = synthetic_svsr_diff
}

load("Synthetic_Files/bs_synthetic_agency.RData")

sum((bs_synthetic_retallow=="0") - (bs_synthetic_agency == "0"))
sum((bs_synthetic_svsr_diff=="0") - (bs_synthetic_agency == "0"))


save(bs_synthetic_retallow,file="Synthetic_Files/bs_synthetic_retallow.RData")
save(bs_synthetic_svsr_diff,file="Synthetic_Files/bs_synthetic_svsr_diff.RData")


