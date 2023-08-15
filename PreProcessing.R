# Pre-Processing


library(readtext)
textos <- readtext("Paper Soft Skills Sampled Programs/")
textos$doc_id <- gsub("[^0-9-]", "", textos$doc_id)
library(dplyr)
textos <- mutate(textos, Program = ifelse(grepl("Especiali", text), "Specialization",
                                             ifelse(grepl("Maestr", text), "Master", "Doctorate")))

library(quanteda)
Textos <- corpus(textos)
head(summary(Textos), 10)
#AllP <- dfm(Textos)
source("SampleAnalysis.R")
docvars(Textos, "Institution") <- Muestra$NOMBRE_INSTITUCIÓN
docvars(Textos, "Accreditation") <- Muestra$Accreditation
summary(Textos)
aja <- data.frame(summary(Textos, n = length(Textos)))

SPEC <- corpus_subset(Textos, Program == "Specialization")
MS <- corpus_subset(Textos, Program == "Masters")
PhD <- corpus_subset(Textos, Program == "Doctorate")
QC <- corpus_subset(Textos, Accreditation == "Qualified Certification")
HQC <- corpus_subset(Textos, Accreditation == "High-Quality Certification")
phd <- data.frame(summary(PhD, n = length(PhD)))

#####################
#ave <- tokens(Textos, remove_numbers = TRUE, remove_punct = TRUE) %>%  dfm()
#ave
#ave2 <- tokens(PhD, remove_numbers = TRUE, remove_punct = TRUE) %>%  
#  dfm(remove = stopwords("spanish"))
#topfeatures(ave2, 50)

#ave3 <- dfm_remove(ave2, c("través", "áreas", "maestría", "mba", "i", "ii", "doctorado", "interinstitucional", "universidad", "pedagógica", "nacional"))

#ave4 <- as.matrix(ave3)
#ave5 <- dfm_keep(ave3, c("generar", "trabajo en equipo", "liderar", "ético"))
#ave6 <- as.matrix(ave5)
#library(bipartite)
#plotweb2(ave6)
#visweb(ave6)
#mod <- computeModules(ave6)
#plotModuleWeb(mod)
#row.names(ave6)[1:27] <- phd$Programa

#visweb(ave6)
#######################


# Keywords-in-context Search
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
persu <- data.frame(kwic(Textos, pattern = "persua*"))
conven <- data.frame(kwic(Textos, pattern = "convencer"))


  
rm(institution, LevelsOfficials, LevelsPrivate, listado, Muestra, Officials, Private, Sector, textos, Textos, DATA_DIR)


TODAS <- rbind(persu, conven, flex, td, decidir, sp, pc, creatividad, paciencia, crear, innovar, acercar, analizar, apreciar, argumentar, ayudar, cambiar, compartir, competir,
               comprender, comprometer, comprometerse, comunicar, conflicto, controlar, cooperar, dirigir,
               empatia, equipo, etico, evaluar, fomentar, fortalecer, generar, gestionar, identificar, impulsar,
               interactuar, liderar, manifestar, motivar, negociar, orientar, planificar, reconocer, reflexionar, 
               resolver, respetar, responsable, tolerar)
colnames(aja)[1]  <- "docname"  
library(dplyr)
TODAS2 <- TODAS %>% select(-from, -to, -pre, -post, -pattern) %>% left_join(aja, by = "docname") 
Spec <- TODAS2 %>% filter(., Program == "Specialization")
MS <- TODAS2 %>% filter(., Program == "Masters")
PhD <- TODAS2 %>% filter(., Program == "Doctorate")

