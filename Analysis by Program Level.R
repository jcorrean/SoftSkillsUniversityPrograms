# Analysis by Program Level

setwd("/home/jc/Documents/Paper Soft Skills Sampled Programs")
listado <- data.frame(dir())
library(readtext)
library(tm)
DirSource()
# Get the data directory from readtext
DATA_DIR <- system.file("extdata/", package = "readtext")
textos <- readtext(listado$dir..)
textos$doc_id <- gsub("[^0-9-]", "", textos$doc_id)


library(quanteda)
Textos <- corpus(textos)
#AllP <- dfm(Textos)
source("~/Documents/GitHub/SoftSkillsUniversityPrograms/SampleAnalysis.R")
docvars(Textos, "Programa") <- Muestra$NOMBRE_DEL_PROGRAMA
docvars(Textos, "Program.Level") <- Muestra$`Academic Level`
docvars(Textos, "Institution") <- Muestra$NOMBRE_INSTITUCIÓN
summary(Textos)
aja <- data.frame(summary(Textos, n = length(Textos)))

SPEC <- corpus_subset(Textos, Program.Level == "Specialization")
MS <- corpus_subset(Textos, Program.Level == "Masters")
PhD <- corpus_subset(Textos, Program.Level == "Doctorate")
