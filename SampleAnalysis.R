library(readr)
Muestra <- read_csv("~/Documents/GitHub/SoftSkillsUniversityPrograms/SampledPrograms.csv")
Muestra <- na.omit(Muestra)
table(Muestra$SECTOR)
library(tidyverse)
Muestra <- mutate(Muestra, Programa = ifelse(grepl("ESPECIALIZ", NOMBRE_DEL_PROGRAMA), "Especialización",
                                      ifelse(grepl("MAESTR", NOMBRE_DEL_PROGRAMA), "Maestría", "Doctorado")))
table(Muestra$Programa)
