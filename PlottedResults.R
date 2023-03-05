# Script for Plotting results

load("~/Documents/GitHub/SoftSkillsUniversityPrograms/PreProcessing.RData")
rm(list=setdiff(ls(), "TODAS2"))


Network <- TODAS2[,c(1,2,7)]
Network <- Network[!duplicated(Network[c(1,2)]),]
library(dplyr)
NetworkPhD <- Network %>% filter(., Program.Level == "Doctorate")
