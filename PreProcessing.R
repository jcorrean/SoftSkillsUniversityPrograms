# Pre-Processing

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
AllPrograms <- corpus(textos)
source("~/Documents/GitHub/SoftSkillsUniversityPrograms/SampleAnalysis.R")
docvars(AllPrograms, "Programa") <- Muestra$NOMBRE_DEL_PROGRAMA
docvars(AllPrograms, "Program.Level") <- Muestra$`Academic Level`
docvars(AllPrograms, "Institution") <- Muestra$NOMBRE_INSTITUCIÓN

SPEC <- corpus_subset(AllPrograms, Program.Level == "Specialization")
MS <- corpus_subset(AllPrograms, Program.Level == "Masters")
PhD <- corpus_subset(AllPrograms, Program.Level == "Doctorate")


summary(AllPrograms)
aja <- data.frame(summary(AllPrograms, n = length(AllPrograms)))



Textos <- tokens(AllPrograms)
Textos <- tokens_tolower(Textos)
Textos

pc <- data.frame(kwic(Textos, pattern = phrase("pensamiento crítico")))
sp <- data.frame(kwic(Textos, pattern = phrase("solucionar problemas")))
comunicar <- data.frame(kwic(Textos, pattern = "comunicar"))
creatividad <- data.frame(kwic(Textos, pattern = "creatividad"))
paciencia <- data.frame(kwic(Textos, pattern = "paciencia"))
crear <- data.frame(kwic(Textos, pattern = "crear"))
liderar <- data.frame(kwic(Textos, pattern = "liderar"))
resolver <- data.frame(kwic(Textos, pattern = "resolver"))
comprometer <- data.frame(kwic(Textos, pattern = "comprometer"))
comprometerse <- data.frame(kwic(Textos, pattern = "comprometerse"))
gestionar <- data.frame(kwic(Textos, pattern = "gestionar"))
reflexionar <- data.frame(kwic(Textos, pattern = "reflexionar"))
controlar <- data.frame(kwic(Textos, pattern = "controlar"))
etico <- data.frame(kwic(Textos, pattern = "ético"))
tolerar <- data.frame(kwic(Textos, pattern = "tolerar"))
argumentar <- data.frame(kwic(Textos, pattern = "argumentar"))
conflicto <- data.frame(kwic(Textos, pattern = "conflictos"))
negociar <- data.frame(kwic(Textos, pattern = "negociar"))
comprender <- data.frame(kwic(Textos, pattern = "comprender"))
equipo <- data.frame(kwic(Textos, pattern = "equipos"))
planificar <- data.frame(kwic(Textos, pattern = "planificar"))
generar <- data.frame(kwic(Textos, pattern = "generar"))
empatia <- data.frame(kwic(Textos, pattern = "empatía"))
compartir <- data.frame(kwic(Textos, pattern = "compartir"))
analizar <- data.frame(kwic(Textos, pattern = "analizar"))
reconocer <- data.frame(kwic(Textos, pattern = "reconocer"))
orientar <- data.frame(kwic(Textos, pattern = "orientar"))
respetar <- data.frame(kwic(Textos, pattern = "respetar"))
motivar <- data.frame(kwic(Textos, pattern = "motivar"))
cooperar <- data.frame(kwic(Textos, pattern = "cooperar"))
fortalecer <- data.frame(kwic(Textos, pattern = "fortalecer"))
impulsar <- data.frame(kwic(Textos, pattern = "impulsar"))
acercar <- data.frame(kwic(Textos, pattern = "acercar"))
ayudar <- data.frame(kwic(Textos, pattern = "ayudar"))
cambiar <- data.frame(kwic(Textos, pattern = "cambiar"))
apreciar <- data.frame(kwic(Textos, pattern = "apreciar"))
dirigir <- data.frame(kwic(Textos, pattern = "dirigir"))
fomentar <- data.frame(kwic(Textos, pattern = "fomentar"))
interactuar <- data.frame(kwic(Textos, pattern = "interactuar"))
identificar <- data.frame(kwic(Textos, pattern = "identificar"))
competir <- data.frame(kwic(Textos, pattern = "competir"))
manifestar <- data.frame(kwic(Textos, pattern = "manifestar"))
responsable <- data.frame(kwic(Textos, pattern = "responsable"))
evaluar <- data.frame(kwic(Textos, pattern = "evaluar"))
innovar <- data.frame(kwic(Textos, pattern = "innovar"))
decidir <- data.frame(kwic(Textos, pattern = "decidir"))
td <- data.frame(kwic(Textos, pattern = phrase("tomar decisiones")))
flex <- data.frame(kwic(Textos, pattern = "flexibilidad"))


  
rm(institution, LevelsOfficials, LevelsPrivate, listado, Muestra, Officials, Private, Sector, textos, Textos, DATA_DIR)


TODAS <- rbind(decidir, sp, pc, creatividad, paciencia, crear, innovar, acercar, analizar, apreciar, argumentar, ayudar, cambiar, compartir, competir,
               comprender, comprometer, comprometerse, comunicar, conflicto, controlar, cooperar, dirigir,
               empatia, equipo, etico, evaluar, fomentar, fortalecer, generar, gestionar, identificar, impulsar,
               interactuar, liderar, manifestar, motivar, negociar, orientar, planificar, reconocer, reflexionar, 
               resolver, respetar, responsable, tolerar)
colnames(aja)[1]  <- "docname"  
library(dplyr)
TODAS2 <- TODAS %>% select(-from, -to, -pre, -post, -pattern) %>% left_join(aja, by = "docname") 
Spec <- TODAS2 %>% filter(., Program.Level == "Specialization")
MS <- TODAS2 %>% filter(., Program.Level == "Masters")
PhD <- TODAS2 %>% filter(., Program.Level == "Doctorate")
Spec <- Spec %>% select(., "docname", "keyword")
MS <- MS %>% select(., "docname", "keyword")
PhD <- PhD %>% select(., "docname", "keyword")
