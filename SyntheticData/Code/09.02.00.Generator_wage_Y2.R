args = (commandArgs(TRUE))
J1.tmp=as.numeric(args)


setwd("~/Path/SyntheticData")
load("Synthetic_Files/J1.RData")

if(J1.tmp == (J1+1))
{
  J1 = J1.tmp
  
  library(parallel)
  library(tree)
  library(Matching)
  
  source("Code/99.02.cartdraw.R")
  source("Code/99.03.cartdraw2.R")
  
  loading.bs <- function(j)
  {
    if( j == 1){load("Aux_Files/bs_agency.RData"); bs = bs_agency}
    if( j == 2){load("Aux_Files/bs_NA_pay_plan.RData"); bs = bs_pay_plan}
    if( j == 3){load("Aux_Files/bs_NA_grade.RData"); bs = bs_grade}
    if( j == 4){load("Aux_Files/bs_NA_steprate.RData"); bs = bs_steprate}
    if( j == 5){load("Aux_Files/bs_NA_paybasis.RData"); bs = bs_paybasis}
    if( j == 6){load("Aux_Files/bs_NA_workschd.RData"); bs = bs_workschd}
    if( j == 7){load("Aux_Files/bs_NA_payrated.RData"); bs = bs_payrated}
    if( j == 8){load("Aux_Files/bs_NA_localpay.RData"); bs = bs_localpay}
    if( j == 9){load("Aux_Files/bs_NA_paybasic.RData"); bs = bs_paybasic}
    if( j == 10){load("Aux_Files/bs_NA_wage.RData"); bs = bs_wage}
    if( j == 11){load("Synthetic_Files/bs_synthetic_agency.RData"); bs = bs_synthetic_agency}
    if( j == 12){load("Synthetic_Files/bs_synthetic_pay_plan.RData"); bs = bs_synthetic_pay_plan}
    if( j == 13){load("Synthetic_Files/bs_synthetic_grade.RData"); bs = bs_synthetic_grade}
    if( j == 14){load("Synthetic_Files/bs_synthetic_steprate.RData"); bs = bs_synthetic_steprate}
    if( j == 15){load("Synthetic_Files/bs_synthetic_paybasis.RData"); bs = bs_synthetic_paybasis}
    if( j == 16){load("Synthetic_Files/bs_synthetic_workschd.RData"); bs = bs_synthetic_workschd}
    if( j == 17){load("Synthetic_Files/bs_synthetic_payrated.RData"); bs = bs_synthetic_payrated}
    if( j == 18){load("Synthetic_Files/bs_synthetic_localpay.RData"); bs = bs_synthetic_localpay}
    if( j == 19){load("Synthetic_Files/bs_synthetic_paybasic.RData"); bs = bs_synthetic_paybasic}
    if( j == 20){load("Synthetic_Files/bs_synthetic_wage.RData"); bs = bs_synthetic_wage}
    bs
  }
  
  bs.files = mclapply(1:20,loading.bs,mc.cores=20)
  
  bs_agency = bs.files[[1]]
  bs_pay_plan = bs.files[[2]]
  bs_grade = bs.files[[3]]
  bs_steprate = bs.files[[4]]
  bs_paybasis = bs.files[[5]]
  bs_workschd = bs.files[[6]]
  bs_payrated = bs.files[[7]]
  bs_localpay = bs.files[[8]]
  bs_paybasic = bs.files[[9]]
  bs_wage = bs.files[[10]]
  bs_synthetic_agency = bs.files[[11]]
  bs_synthetic_pay_plan = bs.files[[12]]
  bs_synthetic_grade = bs.files[[13]]
  bs_synthetic_steprate = bs.files[[14]]
  bs_synthetic_paybasis = bs.files[[15]]
  bs_synthetic_workschd = bs.files[[16]]
  bs_synthetic_payrated = bs.files[[17]]
  bs_synthetic_localpay = bs.files[[18]]
  bs_synthetic_paybasic = bs.files[[19]]
  bs_synthetic_wage = bs.files[[20]]
  
  rm(bs.files)
  
  gc()
  
  variables_pay_plan.0 = c("agency", "gender", "race", "educ_lvl", "age", "init_yrsdegrng", "milmonths",
                           "total_year", "number_agency",  "M" , "occ", "instrctpgm", "occ_cat", 
                           "funcclas", "flsa", "appttype", "polappttype", "position", "tenure", 
                           "svsrstat", "bargunit", "collapse")
  variables_grade.0     = c(variables_pay_plan.0, "pay_plan")
  variables_steprate.0     = c(variables_grade.0, "grade")
  variables_paybasis.0     = c(variables_steprate.0, "steprate")
  variables_workschd.0     = c(variables_paybasis.0, "paybasis")
  variables_payrated.0     = c(variables_workschd.0, "workschd")
  variables_localpay.0     = c(variables_payrated.0, "payrated")
  variables_paybasic.0     = c(variables_localpay.0, "localpay")
  
  
  last.variables = c("last.pay_plan", "last.grade", "last.steprate", "last.paybasis", "last.workschd", 
                     "last.payrated", "last.localpay", "last.paybasic")
  
  variables_pay_plan.1  = c(variables_pay_plan.0, last.variables)
  variables_grade.1  = c(variables_grade.0, last.variables)
  variables_steprate.1  = c(variables_steprate.0, last.variables)
  variables_paybasis.1  = c(variables_paybasis.0, last.variables)
  variables_workschd.1  = c(variables_workschd.0, last.variables)
  variables_payrated.1  = c(variables_payrated.0, last.variables)
  variables_localpay.1  = c(variables_localpay.0, last.variables)
  variables_paybasic.1  = c(variables_paybasic.0, last.variables)
  
  
  load(paste("Aux_Files/data_wage_year_",J1,".RData",sep=""))
  load(paste("Synthetic_Files/data_synthetic_wage_year_",J1,".RData",sep=""))
  
  Ind_synthetic_other_variables = data_synthetic_wage$agency!=0
  Ind_other_variables = data_wage$agency!=0
  
  data_synthetic_wage = data_synthetic_wage[Ind_synthetic_other_variables,]
  data_wage = data_wage[Ind_other_variables,]
  
  
  f<-function(j,bs,Lag)
  {
    tmp = "0"
    if(sum(bs[j,1:(J1-1)]!="0")>0)
      tmp = rev(tail(bs[j,1:(J1-1)][bs[j,1:(J1-1)]!="0"],5))[Lag]
    if(is.na(tmp))
      tmp = "Empty"
    tmp
  }
  
  
  bs = bs_pay_plan[Ind_other_variables,]
  last.pay_plan = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_wage = data.frame(data_wage, last.pay_plan = last.pay_plan)
  
  bs = bs_grade[Ind_other_variables,]
  last.grade = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_wage = data.frame(data_wage, last.grade = last.grade)
  
  bs = bs_steprate[Ind_other_variables,]
  last.steprate = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_wage = data.frame(data_wage, last.steprate = last.steprate)
  
  bs = bs_paybasis[Ind_other_variables,]
  last.paybasis = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_wage = data.frame(data_wage, last.paybasis = last.paybasis)
  
  bs = bs_workschd[Ind_other_variables,]
  last.workschd = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_wage = data.frame(data_wage, last.workschd = last.workschd)
  
  bs = bs_payrated[Ind_other_variables,]
  last.payrated = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_wage = data.frame(data_wage, last.payrated = last.payrated)
  
  bs = bs_localpay[Ind_other_variables,]
  last.localpay = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_wage = data.frame(data_wage, last.localpay = last.localpay)
  
  bs = bs_localpay[Ind_other_variables,]
  last.localpay = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_wage = data.frame(data_wage, last.localpay = last.localpay)
  
  bs = bs_agency[Ind_other_variables,]
  last.agency = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  bs = bs_wage[Ind_other_variables,]
  last.wage = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  bs = bs_paybasic[Ind_other_variables,]
  last.paybasic = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  
  paybasic_0 = last.paybasic
  unique_wage = unique(last.wage[last.agency!="0"])
  f <- function(J)
  {
    Ind = unique_wage[J] == last.wage
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
  tmp = mclapply(1:length(unique_wage),f,mc.cores=15)  
  f <- function(J){paybasic_0[tmp[[J]]$Ind]  <<-  tmp[[J]]$pay }
  aux1<-sapply(1:length(unique_wage),f)
  Ind = paybasic_0 != "zero" &  paybasic_0 != "NA.paybasic" & paybasic_0 != "0"
  MEAN = mean(as.numeric(paybasic_0[Ind]))
  paybasic_0[paybasic_0 == "zero" |  paybasic_0 == "NA.paybasic"] = MEAN
  paybasic_0 = as.numeric(paybasic_0)
  print(paste(mean(paybasic_0[paybasic_0!="0"]),"paybasic in year",J1-1))
  last.paybasic = as.factor(ceiling(paybasic_0/1000)*1000)
  
  data_wage = data.frame(data_wage, last.paybasic = last.paybasic)
 

   f<-function(j,bs,Lag)
  {
    tmp = "0"
    if(sum(bs[j,1:(J1-1)]!="0")>0)
      tmp = rev(tail(bs[j,1:(J1-1)][bs[j,1:(J1-1)]!="0"],5))[Lag]
    if(is.na(tmp))
      tmp = "Empty"
    tmp
  }
  
  
  bs = bs_synthetic_pay_plan[Ind_synthetic_other_variables,]
  last.pay_plan = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_wage = data.frame(data_synthetic_wage, last.pay_plan = last.pay_plan)
  
  bs = bs_synthetic_grade[Ind_synthetic_other_variables,]
  last.grade = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_wage = data.frame(data_synthetic_wage, last.grade = last.grade)
  
  bs = bs_synthetic_steprate[Ind_synthetic_other_variables,]
  last.steprate = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_wage = data.frame(data_synthetic_wage, last.steprate = last.steprate)
  
  bs = bs_synthetic_paybasis[Ind_synthetic_other_variables,]
  last.paybasis = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_wage = data.frame(data_synthetic_wage, last.paybasis = last.paybasis)
  

  bs = bs_synthetic_workschd[Ind_synthetic_other_variables,]
  last.workschd = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_wage = data.frame(data_synthetic_wage, last.workschd = last.workschd)
  
  bs = bs_synthetic_payrated[Ind_synthetic_other_variables,]
  last.payrated = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_wage = data.frame(data_synthetic_wage, last.payrated = last.payrated)
  
  bs = bs_synthetic_localpay[Ind_synthetic_other_variables,]
  last.localpay = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_wage = data.frame(data_synthetic_wage, last.localpay = last.localpay)
  
  bs = bs_synthetic_localpay[Ind_synthetic_other_variables,]
  last.localpay = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  data_synthetic_wage = data.frame(data_synthetic_wage, last.localpay = last.localpay)
  
  bs = bs_synthetic_agency[Ind_synthetic_other_variables,]
  last.agency = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  bs = bs_synthetic_wage[Ind_synthetic_other_variables,]
  last.wage = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  bs = bs_synthetic_paybasic[Ind_synthetic_other_variables,]
  last.paybasic = simplify2array(mclapply(1:nrow(bs),f,bs=bs,Lag=1,mc.cores=48))
  
  paybasic_0 = last.paybasic
  unique_wage = unique(last.wage[last.agency!="0"])
  f <- function(J)
  {
    Ind = unique_wage[J] == last.wage
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
  tmp = mclapply(1:length(unique_wage),f,mc.cores=15)  
  f <- function(J){paybasic_0[tmp[[J]]$Ind]  <<-  tmp[[J]]$pay }
  aux1<-sapply(1:length(unique_wage),f)
  Ind = paybasic_0 != "zero" &  paybasic_0 != "NA.paybasic" & paybasic_0 != "0"
  MEAN = mean(as.numeric(paybasic_0[Ind]))
  paybasic_0[paybasic_0 == "zero" |  paybasic_0 == "NA.paybasic"] = MEAN
  paybasic_0 = as.numeric(paybasic_0)
  print(paste(mean(paybasic_0[paybasic_0!="0"]),"paybasic in year",J1-1))
  last.paybasic = as.factor(ceiling(paybasic_0/1000)*1000)
  
  data_synthetic_wage = data.frame(data_synthetic_wage, last.paybasic = last.paybasic)


  ###### Collapsed function
  mcl.collapse <- function(collapse)
  simplify2array(mclapply(1:nrow(collapse),function(j){paste(collapse[j,],collapse="-")},mc.cores=48))

  
  ################################
  ## pay_plan
  ###############################
  collapse_agency_occ = mcl.collapse(cbind(
    as.vector(data_wage$agency),
    as.vector(data_wage$occ)))
  synthetic_collapse_agency_occ = mcl.collapse(cbind(
    as.vector(data_synthetic_wage$agency),
    as.vector(data_synthetic_wage$occ)))
  collapse_wage1 = mcl.collapse(cbind(
    as.vector(data_wage$agency),
    as.vector(data_wage$occ),
    as.vector(data_wage$last.payplan),
    as.vector(data_wage$last.grade),
    as.vector(data_wage$last.steprate)))
  synthetic_collapse_wage1 = mcl.collapse(cbind(
    as.vector(data_synthetic_wage$agency),
    as.vector(data_synthetic_wage$occ),
    as.vector(data_synthetic_wage$last.payplan),
    as.vector(data_synthetic_wage$last.grade),
    as.vector(data_synthetic_wage$last.steprate)))
  collapse_wage2 = mcl.collapse(cbind(
    as.vector(data_wage$agency),
    as.vector(data_wage$occ),
    as.vector(data_wage$last.payplan),
    as.vector(data_wage$last.grade)))
  synthetic_collapse_wage2 = mcl.collapse(cbind(
    as.vector(data_synthetic_wage$agency),
    as.vector(data_synthetic_wage$occ),
    as.vector(data_synthetic_wage$last.payplan),
    as.vector(data_synthetic_wage$last.grade)))
  Collapse = cbind(
    as.vector(collapse_wage1),
    as.vector(collapse_wage2),
    as.vector(collapse_agency_occ))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse_wage1),
    as.vector(synthetic_collapse_wage2),
    as.vector(synthetic_collapse_agency_occ))
  source("Code/09.02.07.collapsing.R")
  variables = variables_pay_plan.1
  synthetic_variable = rep(0,nrow(data_synthetic_wage))
  Goal="pay_plan"
  source("Code/09.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("pay_plan -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_wage$pay_plan))))
  bs_synthetic_pay_plan[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  if(sum((bs_synthetic_pay_plan[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in synthetic_pay_plan")
    print(which(((bs_synthetic_pay_plan[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  data_synthetic_wage = data.frame(data_synthetic_wage, pay_plan = synthetic_variable)
  ################################
  ## grade
  ###############################
  collapse.1 = mcl.collapse(cbind(
    as.vector(data_wage$pay_plan)))
  synthetic_collapse.1 = mcl.collapse(cbind(
    as.vector(data_synthetic_wage$pay_plan)))
  collapse.a = mcl.collapse(cbind(
    collapse_wage1,
    collapse.1))
  synthetic_collapse.a = mcl.collapse(cbind(
    synthetic_collapse_wage1,
    synthetic_collapse.1))
  collapse.b = mcl.collapse(cbind(
    collapse_wage2,
    collapse.1))
  synthetic_collapse.b = mcl.collapse(cbind(
    synthetic_collapse_wage2,
    synthetic_collapse.1))
  collapse.c = mcl.collapse(cbind(
    collapse_agency_occ,
    collapse.1))
  synthetic_collapse.c = mcl.collapse(cbind(
    synthetic_collapse_agency_occ,
    synthetic_collapse.1))
  Collapse = cbind(
    as.vector(collapse.a),
    as.vector(collapse.b),
    as.vector(collapse.c))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.a),
    as.vector(synthetic_collapse.b),
    as.vector(synthetic_collapse.c))
  source("Code/09.02.07.collapsing.R")
  variables = variables_grade.1
  synthetic_variable = rep(0,nrow(data_synthetic_wage))
  Goal="grade"
  source("Code/09.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("grade -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_wage$grade))))
  bs_synthetic_grade[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  if(sum((bs_synthetic_grade[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in synthetic_grade")
    print(which(((bs_synthetic_grade[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  data_synthetic_wage = data.frame(data_synthetic_wage, grade = synthetic_variable)
  ################################
  ## steprate
  ###############################
  collapse.1 = mcl.collapse(cbind(
    as.vector(data_wage$pay_plan),
    as.vector(data_wage$grade)))
  synthetic_collapse.1 = mcl.collapse(cbind(
    as.vector(data_synthetic_wage$pay_plan),
    as.vector(data_synthetic_wage$grade)))
  collapse.a = mcl.collapse(cbind(
    collapse_wage1,
    collapse.1))
  synthetic_collapse.a = mcl.collapse(cbind(
    synthetic_collapse_wage1,
    synthetic_collapse.1))
  collapse.b = mcl.collapse(cbind(
    collapse_wage2,
    collapse.1))
  synthetic_collapse.b = mcl.collapse(cbind(
    synthetic_collapse_wage2,
    synthetic_collapse.1))
  collapse.c = mcl.collapse(cbind(
    collapse_agency_occ,
    collapse.1))
  synthetic_collapse.c = mcl.collapse(cbind(
    synthetic_collapse_agency_occ,
    synthetic_collapse.1))
  Collapse = cbind(
    as.vector(collapse.a),
    as.vector(collapse.b),
    as.vector(collapse.c))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.a),
    as.vector(synthetic_collapse.b),
    as.vector(synthetic_collapse.c))
  source("Code/09.02.07.collapsing.R")
  variables = variables_steprate.1
  synthetic_variable = rep(0,nrow(data_synthetic_wage))
  Goal="steprate"
  source("Code/09.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("steprate -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_wage$steprate))))
  bs_synthetic_steprate[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  if(sum((bs_synthetic_steprate[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in synthetic_steprate")
    print(which(((bs_synthetic_steprate[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  data_synthetic_wage = data.frame(data_synthetic_wage, steprate = synthetic_variable)
  ################################
  ## paybasis
  ###############################
  collapse.1 = mcl.collapse(cbind(
    as.vector(data_wage$pay_plan),
    as.vector(data_wage$grade),
    as.vector(data_wage$steprate)))
  synthetic_collapse.1 = mcl.collapse(cbind(
    as.vector(data_synthetic_wage$pay_plan),
    as.vector(data_synthetic_wage$grade),
    as.vector(data_synthetic_wage$steprate)))
  collapse.a = mcl.collapse(cbind(
    collapse_wage1,
    collapse.1))
  synthetic_collapse.a = mcl.collapse(cbind(
    synthetic_collapse_wage1,
    synthetic_collapse.1))
  collapse.b = mcl.collapse(cbind(
    collapse_wage2,
    collapse.1))
  synthetic_collapse.b = mcl.collapse(cbind(
    synthetic_collapse_wage2,
    synthetic_collapse.1))
  collapse.c = mcl.collapse(cbind(
    collapse_agency_occ,
    collapse.1))
  synthetic_collapse.c = mcl.collapse(cbind(
    synthetic_collapse_agency_occ,
    synthetic_collapse.1))
  Collapse = cbind(
    as.vector(collapse.a),
    as.vector(collapse.b),
    as.vector(collapse.c))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.a),
    as.vector(synthetic_collapse.b),
    as.vector(synthetic_collapse.c))
  source("Code/09.02.07.collapsing.R")
  variables = variables_paybasis.1
  synthetic_variable = rep(0,nrow(data_synthetic_wage))
  Goal="paybasis"
  source("Code/09.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("paybasis -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_wage$paybasis))))
  bs_synthetic_paybasis[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  if(sum((bs_synthetic_paybasis[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in synthetic_paybasis")
    print(which(((bs_synthetic_paybasis[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  data_synthetic_wage = data.frame(data_synthetic_wage, paybasis = synthetic_variable)
  ################################
  ## workschd
  ###############################
  collapse.1 = mcl.collapse(cbind(
    as.vector(data_wage$pay_plan),
    as.vector(data_wage$grade),
    as.vector(data_wage$steprate),
    as.vector(data_wage$paybasis)))
  synthetic_collapse.1 = mcl.collapse(cbind(
    as.vector(data_synthetic_wage$pay_plan),
    as.vector(data_synthetic_wage$grade),
    as.vector(data_synthetic_wage$steprate),
    as.vector(data_synthetic_wage$paybasis)))
  collapse.a = mcl.collapse(cbind(
    collapse_wage1,
    collapse.1))
  synthetic_collapse.a = mcl.collapse(cbind(
    synthetic_collapse_wage1,
    synthetic_collapse.1))
  collapse.b = mcl.collapse(cbind(
    collapse_wage2,
    collapse.1))
  synthetic_collapse.b = mcl.collapse(cbind(
    synthetic_collapse_wage2,
    synthetic_collapse.1))
  collapse.c = mcl.collapse(cbind(
    collapse_agency_occ,
    collapse.1))
  synthetic_collapse.c = mcl.collapse(cbind(
    synthetic_collapse_agency_occ,
    synthetic_collapse.1))
  Collapse = cbind(
    as.vector(collapse.a),
    as.vector(collapse.b),
    as.vector(collapse.c))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.a),
    as.vector(synthetic_collapse.b),
    as.vector(synthetic_collapse.c))
  source("Code/09.02.07.collapsing.R")
  variables = variables_workschd.1
  synthetic_variable = rep(0,nrow(data_synthetic_wage))
  Goal="workschd"
  source("Code/09.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("workschd -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_wage$workschd))))
  bs_synthetic_workschd[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  if(sum((bs_synthetic_workschd[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in synthetic_workschd")
    print(which(((bs_synthetic_workschd[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  data_synthetic_wage = data.frame(data_synthetic_wage, workschd = synthetic_variable)
  ################################
  ## payrated
  ###############################
  collapse.1 = mcl.collapse(cbind(
    as.vector(data_wage$pay_plan),
    as.vector(data_wage$grade),
    as.vector(data_wage$steprate),
    as.vector(data_wage$paybasis),
    as.vector(data_wage$workschd)))
  synthetic_collapse.1 = mcl.collapse(cbind(
    as.vector(data_synthetic_wage$pay_plan),
    as.vector(data_synthetic_wage$grade),
    as.vector(data_synthetic_wage$steprate),
    as.vector(data_synthetic_wage$paybasis),
    as.vector(data_synthetic_wage$workschd)))
  collapse.a = mcl.collapse(cbind(
    collapse_wage1,
    collapse.1))
  synthetic_collapse.a = mcl.collapse(cbind(
    synthetic_collapse_wage1,
    synthetic_collapse.1))
  collapse.b = mcl.collapse(cbind(
    collapse_wage2,
    collapse.1))
  synthetic_collapse.b = mcl.collapse(cbind(
    synthetic_collapse_wage2,
    synthetic_collapse.1))
  collapse.c = mcl.collapse(cbind(
    collapse_agency_occ,
    collapse.1))
  synthetic_collapse.c = mcl.collapse(cbind(
    synthetic_collapse_agency_occ,
    synthetic_collapse.1))
  Collapse = cbind(
    as.vector(collapse.a),
    as.vector(collapse.b),
    as.vector(collapse.c))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.a),
    as.vector(synthetic_collapse.b),
    as.vector(synthetic_collapse.c))
  source("Code/09.02.07.collapsing.R")
  variables = variables_payrated.1
  synthetic_variable = rep(0,nrow(data_synthetic_wage))
  Goal="payrated"
  source("Code/09.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("payrated -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_wage$payrated))))
  bs_synthetic_payrated[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  if(sum((bs_synthetic_payrated[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in synthetic_payrated")
    print(which(((bs_synthetic_payrated[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  data_synthetic_wage = data.frame(data_synthetic_wage, payrated = synthetic_variable)
  ################################
  ## localpay
  ###############################
  collapse.1 = mcl.collapse(cbind(
    as.vector(data_wage$pay_plan),
    as.vector(data_wage$grade),
    as.vector(data_wage$steprate),
    as.vector(data_wage$paybasis),
    as.vector(data_wage$workschd),
    as.vector(data_wage$payrated)))
  synthetic_collapse.1 = mcl.collapse(cbind(
    as.vector(data_synthetic_wage$pay_plan),
    as.vector(data_synthetic_wage$grade),
    as.vector(data_synthetic_wage$steprate),
    as.vector(data_synthetic_wage$paybasis),
    as.vector(data_synthetic_wage$workschd),
    as.vector(data_synthetic_wage$payrated)))
  collapse.a = mcl.collapse(cbind(
    collapse_wage1,
    collapse.1))
  synthetic_collapse.a = mcl.collapse(cbind(
    synthetic_collapse_wage1,
    synthetic_collapse.1))
  collapse.b = mcl.collapse(cbind(
    collapse_wage2,
    collapse.1))
  synthetic_collapse.b = mcl.collapse(cbind(
    synthetic_collapse_wage2,
    synthetic_collapse.1))
  collapse.c = mcl.collapse(cbind(
    collapse_agency_occ,
    collapse.1))
  synthetic_collapse.c = mcl.collapse(cbind(
    synthetic_collapse_agency_occ,
    synthetic_collapse.1))
  Collapse = cbind(
    as.vector(collapse.a),
    as.vector(collapse.b),
    as.vector(collapse.c))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.a),
    as.vector(synthetic_collapse.b),
    as.vector(synthetic_collapse.c))
  source("Code/09.02.07.collapsing.R")
  variables = variables_localpay.1
  synthetic_variable = rep(0,nrow(data_synthetic_wage))
  Goal="localpay"
  source("Code/09.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("localpay -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_wage$localpay))))
  bs_synthetic_localpay[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  if(sum((bs_synthetic_localpay[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in synthetic_localpay")
    print(which(((bs_synthetic_localpay[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  data_synthetic_wage = data.frame(data_synthetic_wage, localpay = synthetic_variable)
  ################################
  ## wage
  ###############################
  synthetic_collapse.1 = mcl.collapse(cbind(
    as.vector(bs_synthetic_workschd[,J1]),
    as.vector(bs_synthetic_localpay[,J1]),
    as.vector(bs_synthetic_pay_plan[,J1]),
    as.vector(bs_synthetic_grade[,J1]),
    as.vector(bs_synthetic_steprate[,J1]),
    as.vector(bs_synthetic_paybasis[,J1]),
    as.vector(bs_synthetic_payrated[,J1])))
  
  bs_synthetic_wage[,J1] = synthetic_collapse.1 

  print("WAGE bs_synthetic_wage vs bs_wage")
  print(mean(bs_synthetic_wage[,J1] %in% bs_wage[,J1]))
  ################################
  ## paybasic
  ###############################
  collapse.1 = mcl.collapse(cbind(
    as.vector(data_wage$pay_plan),
    as.vector(data_wage$grade),
    as.vector(data_wage$steprate),
    as.vector(data_wage$paybasis),
    as.vector(data_wage$workschd),
    as.vector(data_wage$payrated),
    as.vector(data_wage$localpay)))
  synthetic_collapse.1 = mcl.collapse(cbind(
    as.vector(data_synthetic_wage$pay_plan),
    as.vector(data_synthetic_wage$grade),
    as.vector(data_synthetic_wage$steprate),
    as.vector(data_synthetic_wage$paybasis),
    as.vector(data_synthetic_wage$workschd),
    as.vector(data_synthetic_wage$payrated),
    as.vector(data_synthetic_wage$localpay)))
  collapse.a = mcl.collapse(cbind(
    collapse_wage1,
    collapse.1))
  synthetic_collapse.a = mcl.collapse(cbind(
    synthetic_collapse_wage1,
    synthetic_collapse.1))
  collapse.b = mcl.collapse(cbind(
    collapse_wage2,
    collapse.1))
  synthetic_collapse.b = mcl.collapse(cbind(
    synthetic_collapse_wage2,
    synthetic_collapse.1))
  collapse.c = mcl.collapse(cbind(
    collapse_agency_occ,
    collapse.1))
  synthetic_collapse.c = mcl.collapse(cbind(
    synthetic_collapse_agency_occ,
    synthetic_collapse.1))
  Collapse = cbind(
    as.vector(collapse.a),
    as.vector(collapse.b),
    as.vector(collapse.c))
  Synthetic_Collapse = cbind(
    as.vector(synthetic_collapse.a),
    as.vector(synthetic_collapse.b),
    as.vector(synthetic_collapse.c))
  source("Code/09.02.07.collapsing.R")
  variables = variables_paybasic.1
  synthetic_variable = rep("XX",nrow(data_synthetic_wage))
  Goal="paybasic"
  source("Code/09.02.08.compiled.R")
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX");print(paste("paybasic -- Year",J1))
  print("XXXXXXXXXXXXXXXX");print("XXXXXXXXXXXXXXXX")
  print(tail(sort(table(synthetic_variable))))
  print(tail(sort(table(data_wage$paybasic))))
  bs_synthetic_paybasic[bs_synthetic_agency[,J1] != "0",J1] = synthetic_variable
  if(sum((bs_synthetic_paybasic[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0)
  {
    print("Error: More zeros in synthetic_paybasic")
    print(which(((bs_synthetic_paybasic[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))>0))
    break()
  }
  data_synthetic_wage = data.frame(data_synthetic_wage, paybasic = synthetic_variable)
    
  
  
  save(bs_synthetic_pay_plan, file = "Synthetic_Files/bs_synthetic_pay_plan.RData")
  save(bs_synthetic_grade, file = "Synthetic_Files/bs_synthetic_grade.RData")
  save(bs_synthetic_steprate, file = "Synthetic_Files/bs_synthetic_steprate.RData")
  save(bs_synthetic_paybasis, file = "Synthetic_Files/bs_synthetic_paybasis.RData")
  save(bs_synthetic_workschd, file = "Synthetic_Files/bs_synthetic_workschd.RData")
  save(bs_synthetic_payrated, file = "Synthetic_Files/bs_synthetic_payrated.RData")
  save(bs_synthetic_localpay, file = "Synthetic_Files/bs_synthetic_localpay.RData")
  save(bs_synthetic_paybasic, file = "Synthetic_Files/bs_synthetic_paybasic.RData")
  save(bs_synthetic_wage, file = "Synthetic_Files/bs_synthetic_wage.RData")
  save(J1, file = "Synthetic_Files/J1.RData")
}
