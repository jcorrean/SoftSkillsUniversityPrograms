load("~/Documents/GitHub/SoftSkillsUniversityPrograms/ResultsbyProgram.RData")
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/ResultsbyAccreditation.RData")
library(tidyverse)
Top10Specialization <- SoftSkillsCentrality %>% 
  filter(,SoftSkillsCentrality$Level == "Specialization") %>% 
  select(, SS, Eigen.vector) %>% 
  arrange(, desc(Eigen.vector))

Spec <- head(Top10Specialization, 10)

library(xtable)
spec <- xtable(Spec)
print(spec, include.rownames = TRUE, floating = FALSE, tabular.environment = "longtable")

Top10Master <- SoftSkillsCentrality %>% 
  filter(,SoftSkillsCentrality$Level == "Master") %>% 
  select(, SS, Eigen.vector) %>% 
  arrange(, desc(Eigen.vector))

MS <- head(Top10Master, 10)

master <- xtable(MS)
print(master, include.rownames = TRUE, floating = FALSE, tabular.environment = "longtable")


Top10PhD <- SoftSkillsCentrality %>% 
  filter(,SoftSkillsCentrality$Level == "Doctorate") %>% 
  select(, SS, Eigen.vector) %>% 
  arrange(, desc(Eigen.vector))

PhD <- head(Top10PhD, 10)

phd <- xtable(PhD)
print(phd, include.rownames = TRUE, floating = FALSE, tabular.environment = "longtable")

Top10HQS <- Centralities2 %>% 
  filter(,Accreditation == "High-Quality Certification") %>% 
  select(, SS, Eigen.vector) %>% 
  arrange(, desc(Eigen.vector))

HQS <- head(Top10HQS, 10)

hqs <- xtable(HQS)
print(hqs, include.rownames = TRUE, floating = FALSE, tabular.environment = "longtable")


Top10QC <- Centralities2 %>% 
  filter(,Accreditation == "Qualified Certification") %>% 
  select(, SS, Eigen.vector) %>% 
  arrange(, desc(Eigen.vector))

QC <- head(Top10QC, 10)

qc <- xtable(QC)
print(qc, include.rownames = TRUE, floating = FALSE, tabular.environment = "longtable")

