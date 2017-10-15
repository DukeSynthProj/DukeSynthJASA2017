#rm(list=ls())
setwd("~/Path/SyntheticData")
library(parallel)
library(OPMpackage)

### Number of moves
par(mfrow=c(2,4))
system.time(source("Code/01.03.001.Moves.R"))
Prob_Cond_Mov[[17]]=Prob_Cond_Mov[[16]]

### Transitions probabilities
system.time(source("Code/01.03.002.Prob_trans.R"))

### Probability distribution Agencies 24 Years
load("Aux_Files/Prob_Ag_24.RData")

#### Setting probs
Probs = list( theta = theta, Prob_M = Prob_M ,  Prob_Cond_Mov = Prob_Cond_Mov, Prob_trans = Prob_trans, Prob_Ag_24=Prob_Ag_24)

save(Probs,file = paste("Aux_Files/Probs_Theta_",theta,".RData",sep=""))

