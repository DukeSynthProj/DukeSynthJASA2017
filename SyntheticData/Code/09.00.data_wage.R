setwd("~/Path/SyntheticData")
rm(list=ls())
library(parallel)

load("Aux_Files/bs_agency.RData")

load("Aux_Files/bs_workschd.RData")
load("Aux_Files/bs_localpay.RData")
load("Aux_Files/bs_pay_plan.RData")
load("Aux_Files/bs_grade.RData")
load("Aux_Files/bs_steprate.RData")
load("Aux_Files/bs_paybasis.RData")
load("Aux_Files/bs_payrated.RData")
load("Aux_Files/bs_paybasic.RData")

f<-function(Year)
{
  bs_tmp = cbind(
    bs_workschd[,Year],
    bs_localpay[,Year],
    bs_pay_plan[,Year],
    bs_grade[,Year],
    bs_steprate[,Year],
    bs_paybasis[,Year],
    bs_payrated[,Year])
  
  output<-apply(bs_tmp,1,function(x){paste(x,collapse="-")})
  print(Year)
  output
}
tmp = mclapply(1:24,f,mc.cores=24)
bs_wage = bs_agency
for(j in 1:24){bs_wage[,j] = tmp[[j]]}
save(bs_wage,file="Aux_Files/bs_wage.RData")


for(j in 1:24)
{
bs_workschd[is.na(bs_workschd[,j]),j]="NA.workschd"
bs_workschd[bs_workschd[,j]=="NA",j]="NA.workschd"

bs_localpay[is.na(bs_localpay[,j]),j]="NA.localpay"
bs_localpay[bs_localpay[,j]=="NA",j]="NA.localpay"

bs_pay_plan[is.na(bs_pay_plan[,j]),j]="NA.pay_plan"
bs_pay_plan[bs_pay_plan[,j]=="NA",j]="NA.pay_plan"

bs_grade[is.na(bs_grade[,j]),j]="NA.grade"
bs_grade[bs_grade[,j]=="NA",j]="NA.grade"
bs_grade[bs_agency[,j] != "0" & bs_grade[,j] == "0",j] = "zero"

bs_steprate[is.na(bs_steprate[,j]),j]="NA.steprate"
bs_steprate[bs_steprate[,j]=="NA",j]="NA.steprate"
bs_steprate[bs_agency[,j] != "0" & bs_steprate[,j] == "0",j] = "zero"

bs_paybasis[is.na(bs_paybasis[,j]),j]="NA.paybasis"
bs_paybasis[bs_paybasis[,j]=="NA",j]="NA.paybasis"

bs_payrated[is.na(bs_payrated[,j]),j]="NA.payrated"
bs_payrated[bs_payrated[,j]=="NA",j]="NA.payrated"
bs_payrated[bs_agency[,j] != "0" & bs_payrated[,j] == "0",j] = "zero"

bs_paybasic[is.na(bs_paybasic[,j]),j]="NA.paybasic"
bs_paybasic[bs_paybasic[,j]=="NA",j]="NA.paybasic"
bs_paybasic[bs_agency[,j] != "0" & bs_paybasic[,j] == "0",j] = "zero"
}

# Checking zero in the original data
sum((bs_pay_plan=="0") - (bs_agency == "0"))
sum((bs_grade=="0") - (bs_agency == "0"))
sum((bs_steprate=="0") - (bs_agency == "0"))
sum((bs_paybasis=="0") - (bs_agency == "0"))
sum((bs_workschd=="0") - (bs_agency == "0"))
sum((bs_payrated=="0") - (bs_agency=="0")) # Zero is a level in this variable
sum((bs_localpay=="0") - (bs_agency == "0"))
sum((bs_paybasic=="0") - (bs_agency == "0"))


f<-function(Year)
{
  bs_tmp = cbind(
    bs_workschd[,Year],
    bs_localpay[,Year],
    bs_pay_plan[,Year],
    bs_grade[,Year],
    bs_steprate[,Year],
    bs_paybasis[,Year],
    bs_payrated[,Year])
  
  output<-apply(bs_tmp,1,function(x){paste(x,collapse="-")})
  print(Year)
  output
}
tmp = mclapply(1:24,f,mc.cores=24)
bs_wage = bs_agency
for(j in 1:24){bs_wage[,j] = tmp[[j]]}

save(bs_pay_plan, file = "Aux_Files/bs_NA_pay_plan.RData")
save(bs_grade,    file = "Aux_Files/bs_NA_grade.RData")
save(bs_steprate, file = "Aux_Files/bs_NA_steprate.RData")
save(bs_paybasis, file = "Aux_Files/bs_NA_paybasis.RData")
save(bs_workschd, file = "Aux_Files/bs_NA_workschd.RData")
save(bs_payrated, file = "Aux_Files/bs_NA_payrated.RData")
save(bs_localpay, file = "Aux_Files/bs_NA_localpay.RData")
save(bs_paybasic, file = "Aux_Files/bs_NA_paybasic.RData")
save(bs_wage,     file = "Aux_Files/bs_NA_wage.RData")

f <- function(J1)
{
  # Data
  load(paste("Aux_Files/data_bargunit_year_",J1,".RData",sep=""))
  data_wage = data.frame(data_bargunit,wage=bs_wage[,J1],
                         workschd=bs_workschd[,J1],
                         localpay=bs_localpay[,J1],
#                         paytable=bs_paytable[,J1],
                         pay_plan=bs_pay_plan[,J1],
                         grade=bs_grade[,J1],
                         steprate=bs_steprate[,J1],
                         paybasis=bs_paybasis[,J1],
                         payrated=bs_payrated[,J1],
                         paybasic=bs_paybasic[,J1])
  
  save(data_wage,file=paste("Aux_Files/data_wage_year_",J1,".RData",sep=""))
}

mclapply(1:24,f,mc.cores=24)


