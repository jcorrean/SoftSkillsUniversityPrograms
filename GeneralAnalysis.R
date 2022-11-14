library("ggplot2")
#setwd("Documents/PaperInnovacionPilar")
library(readr)
G21 <- read_csv("Graduados2021.csv")
GraduatePrograms <- filter(G21, G21$`NIVEL ACADÉMICO` == "POSGRADO")
GraduatePrograms <- GraduatePrograms[!duplicated(GraduatePrograms$`PROGRAMA ACADÉMICO`), ]
table(GraduatePrograms$`NIVEL DE FORMACIÓN`)

PHD <- filter(GraduatePrograms, grepl('Doctor|DOCTOR', GraduatePrograms$`PROGRAMA ACADÉMICO`))
MASTER <- filter(GraduatePrograms, grepl('Maestr|MAESTR', GraduatePrograms$`PROGRAMA ACADÉMICO`))
ESPEC <- filter(GraduatePrograms, grepl('ESPECIALIZAC', GraduatePrograms$`PROGRAMA ACADÉMICO`))

1793+1153+146



library(tidyverse)
setwd("Documents/PaperInnovacionPilar/Graduates")
G18 <- read_csv("Graduados2018.csv") # Columns 3, 6, 9, 10, 11, 14, 16, 18, 22, 24
G18 <- G18[,c(3,6,10,14,16,18,22,24,31)]
variable.names(G18)
G19 <- read_csv("Graduados2019.csv")
G19 <- G19[,c(3,6,10,14,16,18,22,24,37)]
variable.names(G19)
G20 <- read_csv("Graduados2020.csv")
G20 <- G20[,c(3,6,10,14,16,18,22,24,37)]
G21 <- read_csv("Graduados2021.csv")
G21 <- G21[,c(3,6,11,15,18,20,24,26,39)]


colnames(G18)[c(1,2,3,4,5,6,7,8,9)]  <- c("Institution", "Sector", "Department","Program","Level","Type","Discipline","Knowledge","Year") 
colnames(G19)[c(1,2,3,4,5,6,7,8,9)]  <- c("Institution", "Sector", "Department","Program","Level","Type","Discipline","Knowledge","Year") 
colnames(G20)[c(1,2,3,4,5,6,7,8,9)]  <- c("Institution", "Sector", "Department","Program","Level","Type","Discipline","Knowledge","Year") 
colnames(G21)[c(1,2,3,4,5,6,7,8,9)]  <- c("Institution", "Sector", "Department","Program","Level","Type","Discipline","Knowledge","Year") 

all <- list(G18, G19, G20, G21)
ALL <- do.call(rbind.data.frame, all)
rm(list = setdiff(ls(), "ALL"))

GraduatePrograms <- filter(ALL, Level == "POSGRADO")
Programs <- GraduatePrograms[!duplicated(GraduatePrograms$Program), ]

PHD <- filter(Programs, grepl('Doctor|DOCTOR', Type))
MASTER <- filter(Programs, grepl('Maestr', Type))
ESPEC <- filter(Programs, grepl('Especializa', Type))



table(PHD$Discipline)
table(MASTER$Discipline)
table(ESPEC$Discipline)
Universidades <- data.frame(table(GraduatePrograms$`INSTITUCIÓN DE EDUCACIÓN SUPERIOR (IES)`))


PHD <- filter(ProgramasUnicos, grepl('Doctor|DOCTOR', Name))
MASTER <- filter(ProgramasUnicos, grepl('Maestr', Type))
ESPEC <- filter(ProgramasUnicos, grepl('Especializa', Type))

table(PHD$Discipline)
table(MASTER$Discipline)
table(ESPEC$Discipline)

ProgramasDoctores <- data.frame(table(PHD$Name))

146+1154+1834
568/3134


ProgramsByDepartment <- data.frame(table(Programs$`DEPARTAMENTO DE DOMICILIO DE LA IES`))
#ProgramsByDep <- data.frame(table(Programs$`CÓDIGO DEL DEPARTAMENTO (IES)`))
library(tidyverse)
library(colmaps)
colmap(municipios, subset(Programs, Level == "POSGRADO"), var = "GRADUADOS")
