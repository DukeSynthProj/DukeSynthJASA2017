setwd("~/Path/SyntheticData")
load("Synthetic_Files/data_synthetic_gender.RData")
load("Synthetic_Files/synthetic_gender.RData")
gender = synthetic_gender

######
# data race
######
data_synthetic_race<-data.frame(data_synthetic_gender,gender = gender)
save(data_synthetic_race,file="Synthetic_Files/data_synthetic_race.RData")





