setwd("~/Path/SyntheticData")
load("Synthetic_Files/data_synthetic_educ.RData")

variables<-c("gender","init_year","last_year","total_year","mode_agency","init_agency",
"number_agency","M","gaps","init_race")
data_synthetic_AgeYrsdMilm = data_synthetic_educ[,colnames(data_synthetic_educ) %in% variables]

# Edu
load("Synthetic_Files/bs_synthetic_new_educ_lvl.RData")


library(parallel)
# Initial educ
f<-function(j){bs_synthetic_new_educ_lvl[j,data_synthetic_educ$init_year[j]]}
init_educ <- simplify2array(mclapply(1:nrow(bs_synthetic_new_educ_lvl),f,mc.cores=48))
table(init_educ)

data_synthetic_AgeYrsdMilm = data.frame(data_synthetic_AgeYrsdMilm, init_educ = init_educ)
save(data_synthetic_AgeYrsdMilm,file="Synthetic_Files/data_synthetic_AgeYrsdMilm.RData")





