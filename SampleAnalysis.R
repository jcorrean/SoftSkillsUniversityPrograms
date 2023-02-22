library(readr)
Muestra <- read_csv("~/Documents/GitHub/SoftSkillsUniversityPrograms/SampledPrograms.csv")
Muestra <- na.omit(Muestra)
table(Muestra$SECTOR)
library(tidyverse)
Muestra <- mutate(Muestra, Programa = ifelse(grepl("Especializac", NOMBRE_DEL_PROGRAMA), "Especialización",
                                      ifelse(grepl("Maestr", NOMBRE_DEL_PROGRAMA), "Maestría", "Doctorado")))
table(Muestra$Programa)
institution <- data.frame(table(Muestra$NOMBRE_INSTITUCIÓN))
Sector <- data.frame(table(Muestra$SECTOR))

Officials <- data.frame(subset(Muestra, Muestra$SECTOR == "Oficial"))
LevelsOfficials <- data.frame(table(Officials$Programa))
Private <- data.frame(subset(Muestra, Muestra$SECTOR == "Privado"))
LevelsPrivate <- data.frame(table(Private$Programa))

