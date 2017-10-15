setwd("~/Path/SyntheticData")

library(parallel)
library(tree)
library(Matching)

source("Code/99.02.cartdraw.R")
source("Code/99.03.cartdraw2.R")

load("Aux_Files/bs_agency.RData")
load("Aux_Files/bs_NA_pay_plan.RData")
load("Aux_Files/bs_NA_grade.RData")
load("Aux_Files/bs_NA_steprate.RData")
load("Aux_Files/bs_NA_paybasis.RData")
load("Aux_Files/bs_NA_workschd.RData")
load("Aux_Files/bs_NA_payrated.RData")
load("Aux_Files/bs_NA_localpay.RData")
load("Aux_Files/bs_NA_paybasic.RData")
load("Aux_Files/bs_NA_wage.RData")


load("Synthetic_Files/bs_synthetic_agency.RData")
bs_synthetic_pay_plan = bs_synthetic_agency
bs_synthetic_grade = bs_synthetic_agency
bs_synthetic_steprate = bs_synthetic_agency
bs_synthetic_paybasis = bs_synthetic_agency
bs_synthetic_workschd = bs_synthetic_agency
bs_synthetic_payrated = bs_synthetic_agency
bs_synthetic_localpay = bs_synthetic_agency
bs_synthetic_paybasic = bs_synthetic_agency
bs_synthetic_wage = bs_synthetic_agency


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


J1 = 1

load(paste("Aux_Files/data_wage_year_",J1,".RData",sep=""))
load(paste("Synthetic_Files/data_synthetic_wage_year_",J1,".RData",sep=""))


Ind_synthetic_other_variables = data_synthetic_wage$agency!=0
Ind_other_variables = data_wage$agency!=0

data_synthetic_wage = data_synthetic_wage[Ind_synthetic_other_variables,]
data_wage = data_wage[Ind_other_variables,]

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
Collapse = cbind(
  as.vector(collapse_agency_occ),
  as.vector(collapse_agency_occ))
Synthetic_Collapse = cbind(
  as.vector(synthetic_collapse_agency_occ),
  as.vector(synthetic_collapse_agency_occ))
source("Code/09.02.07.collapsing.R")
variables = variables_pay_plan.0
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
  as.vector(data_wage$agency),
  as.vector(data_wage$occ),
  as.vector(data_wage$pay_plan)))
synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(data_synthetic_wage$agency),
  as.vector(data_synthetic_wage$occ),
  as.vector(data_synthetic_wage$pay_plan)))
Collapse = cbind(
  as.vector(collapse.1),
  as.vector(collapse_agency_occ))
Synthetic_Collapse = cbind(
  as.vector(synthetic_collapse.1),
  as.vector(synthetic_collapse_agency_occ))
source("Code/09.02.07.collapsing.R")
variables = variables_grade.0
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
  as.vector(data_wage$agency),
  as.vector(data_wage$occ),
  as.vector(data_wage$pay_plan),
  as.vector(data_wage$grade)))
synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(data_synthetic_wage$agency),
  as.vector(data_synthetic_wage$occ),
  as.vector(data_synthetic_wage$pay_plan),
  as.vector(data_synthetic_wage$grade)))
Collapse = cbind(
  as.vector(collapse.1),
  as.vector(collapse_agency_occ))
Synthetic_Collapse = cbind(
  as.vector(synthetic_collapse.1),
  as.vector(synthetic_collapse_agency_occ))
source("Code/09.02.07.collapsing.R")
variables = variables_steprate.0
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
  as.vector(data_wage$agency),
  as.vector(data_wage$occ),
  as.vector(data_wage$pay_plan),
  as.vector(data_wage$grade),
  as.vector(data_wage$steprate)))
synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(data_synthetic_wage$agency),
  as.vector(data_synthetic_wage$occ),
  as.vector(data_synthetic_wage$pay_plan),
  as.vector(data_synthetic_wage$grade),
  as.vector(data_synthetic_wage$steprate)))
Collapse = cbind(
  as.vector(collapse.1),
  as.vector(collapse_agency_occ))
Synthetic_Collapse = cbind(
  as.vector(synthetic_collapse.1),
  as.vector(synthetic_collapse_agency_occ))
source("Code/09.02.07.collapsing.R")
variables = variables_paybasis.0
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
  as.vector(data_wage$agency),
  as.vector(data_wage$occ),
  as.vector(data_wage$pay_plan),
  as.vector(data_wage$grade),
  as.vector(data_wage$steprate),
  as.vector(data_wage$paybasis)))
synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(data_synthetic_wage$agency),
  as.vector(data_synthetic_wage$occ),
  as.vector(data_synthetic_wage$pay_plan),
  as.vector(data_synthetic_wage$grade),
  as.vector(data_synthetic_wage$steprate),
  as.vector(data_synthetic_wage$paybasis)))
Collapse = cbind(
  as.vector(collapse.1),
  as.vector(collapse_agency_occ))
Synthetic_Collapse = cbind(
  as.vector(synthetic_collapse.1),
  as.vector(synthetic_collapse_agency_occ))
source("Code/09.02.07.collapsing.R")
variables = variables_workschd.0
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
  as.vector(data_wage$agency),
  as.vector(data_wage$occ),
  as.vector(data_wage$pay_plan),
  as.vector(data_wage$grade),
  as.vector(data_wage$steprate),
  as.vector(data_wage$paybasis),
  as.vector(data_wage$workschd)))
synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(data_synthetic_wage$agency),
  as.vector(data_synthetic_wage$occ),
  as.vector(data_synthetic_wage$pay_plan),
  as.vector(data_synthetic_wage$grade),
  as.vector(data_synthetic_wage$steprate),
  as.vector(data_synthetic_wage$paybasis),
  as.vector(data_synthetic_wage$workschd)))
Collapse = cbind(
  as.vector(collapse.1),
  as.vector(collapse_agency_occ))
Synthetic_Collapse = cbind(
  as.vector(synthetic_collapse.1),
  as.vector(synthetic_collapse_agency_occ))
source("Code/09.02.07.collapsing.R")
variables = variables_payrated.0
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
  as.vector(data_wage$agency),
  as.vector(data_wage$occ),
  as.vector(data_wage$pay_plan),
  as.vector(data_wage$grade),
  as.vector(data_wage$steprate),
  as.vector(data_wage$paybasis),
  as.vector(data_wage$workschd),
  as.vector(data_wage$payrated)))
synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(data_synthetic_wage$agency),
  as.vector(data_synthetic_wage$occ),
  as.vector(data_synthetic_wage$pay_plan),
  as.vector(data_synthetic_wage$grade),
  as.vector(data_synthetic_wage$steprate),
  as.vector(data_synthetic_wage$paybasis),
  as.vector(data_synthetic_wage$workschd),
  as.vector(data_synthetic_wage$payrated)))
Collapse = cbind(
  as.vector(collapse.1),
  as.vector(collapse_agency_occ))
Synthetic_Collapse = cbind(
  as.vector(synthetic_collapse.1),
  as.vector(synthetic_collapse_agency_occ))
source("Code/09.02.07.collapsing.R")
variables = variables_localpay.0
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
## paybasic
###############################
collapse.1 = mcl.collapse(cbind(
  as.vector(data_wage$agency),
  as.vector(data_wage$occ),
  as.vector(data_wage$pay_plan),
  as.vector(data_wage$grade),
  as.vector(data_wage$steprate),
  as.vector(data_wage$paybasis),
  as.vector(data_wage$workschd),
  as.vector(data_wage$payrated),
  as.vector(data_wage$localpay)))
synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(data_synthetic_wage$agency),
  as.vector(data_synthetic_wage$occ),
  as.vector(data_synthetic_wage$pay_plan),
  as.vector(data_synthetic_wage$grade),
  as.vector(data_synthetic_wage$steprate),
  as.vector(data_synthetic_wage$paybasis),
  as.vector(data_synthetic_wage$workschd),
  as.vector(data_synthetic_wage$payrated),
  as.vector(data_synthetic_wage$localpay)))
Collapse = cbind(
  as.vector(collapse.1),
  as.vector(collapse_agency_occ))
Synthetic_Collapse = cbind(
  as.vector(synthetic_collapse.1),
  as.vector(synthetic_collapse_agency_occ))
source("Code/09.02.07.collapsing.R")
variables = variables_paybasic.0
synthetic_variable = rep(0,nrow(data_synthetic_wage))
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


## Wage

synthetic_collapse.1 = mcl.collapse(cbind(
  as.vector(bs_synthetic_workschd[,J1]),
  as.vector(bs_synthetic_localpay[,J1]),
  as.vector(bs_synthetic_pay_plan[,J1]),
  as.vector(bs_synthetic_grade[,J1]),
  as.vector(bs_synthetic_steprate[,J1]),
  as.vector(bs_synthetic_paybasis[,J1]),
  as.vector(bs_synthetic_payrated[,J1])))

bs_synthetic_wage[,J1] = synthetic_collapse.1 


# Checking zero in the synthetic data
sum((bs_synthetic_pay_plan[,J1]=="0") - (bs_synthetic_agency[,J1]=="0"))
sum((bs_synthetic_grade[,J1]=="0") - (bs_synthetic_agency[,J1]=="0"))
sum((bs_synthetic_steprate[,J1]=="0") - (bs_synthetic_agency[,J1]=="0"))
sum((bs_synthetic_paybasis[,J1]=="0") - (bs_synthetic_agency[,J1]=="0"))
sum((bs_synthetic_workschd[,J1]=="0") - (bs_synthetic_agency[,J1]=="0"))
sum((bs_synthetic_payrated[,J1]=="0") - (bs_synthetic_agency[,J1]=="0")) 
sum((bs_synthetic_localpay[,J1]=="0") - (bs_synthetic_agency[,J1]=="0"))
sum((bs_synthetic_paybasic[,J1]=="0") - (bs_synthetic_agency[,J1] == "0"))


save(bs_synthetic_pay_plan, file = "Synthetic_Files/bs_synthetic_pay_plan.RData")
save(bs_synthetic_grade, file = "Synthetic_Files/bs_synthetic_grade.RData")
save(bs_synthetic_steprate, file = "Synthetic_Files/bs_synthetic_steprate.RData")
save(bs_synthetic_paybasis, file = "Synthetic_Files/bs_synthetic_paybasis.RData")
save(bs_synthetic_workschd, file = "Synthetic_Files/bs_synthetic_workschd.RData")
save(bs_synthetic_payrated, file = "Synthetic_Files/bs_synthetic_payrated.RData")
save(bs_synthetic_localpay, file = "Synthetic_Files/bs_synthetic_localpay.RData")
save(bs_synthetic_paybasic, file = "Synthetic_Files/bs_synthetic_paybasic.RData")
save(bs_synthetic_wage, file = "Synthetic_Files/bs_synthetic_wage.RData")
save(J1,file = "Synthetic_Files/J1.RData")
