# Analysis by Program Level
#Spec <- Spec %>% select(., "docname", "keyword")
#MS <- MS %>% select(., "docname", "keyword")
#PhD <- PhD %>% select(., "docname", "keyword")
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/PreProcessing.RData")
library(quanteda)
source("~/Documents/GitHub/SoftSkillsUniversityPrograms/SampleAnalysis.R")
docvars(Textos, "Programa") <- Muestra$NOMBRE_DEL_PROGRAMA
docvars(Textos, "Program.Level") <- Muestra$`Academic Level`
docvars(Textos, "Institution") <- Muestra$NOMBRE_INSTITUCIÓN
docvars(Textos, "Accreditation") <- Muestra$Accreditation

summary(Textos)
aja <- data.frame(summary(Textos, n = length(Textos)))

SPEC <- corpus_subset(Textos, Program.Level == "Specialization")
MS <- corpus_subset(Textos, Program.Level == "Masters")
PhD <- corpus_subset(Textos, Program.Level == "Doctorate")
QC <- corpus_subset(Textos, Accreditation == "Qualified Certification")
HQC <- corpus_subset(Textos, Accreditation == "High-Quality Certification")

# Specialization Programs
TOKS <- tokens(SPEC, remove_numbers = TRUE, remove_punct = TRUE) %>% 
  tokens_remove(stopwords("es"))
DTM <- dfm(TOKS, tolower = TRUE)
#SoftSkills <- c("generar", "evaluar", "liderar", "equipos", "analizar", "gestionar", "fortalecer", "identificar", "crear", "comprender") #Eigenvector
#toks_inside <- tokens_keep(TOKS, pattern = SoftSkills, window = 0)
#DTM2 <- dfm(toks_inside)

DTMSpec <- as.matrix(DTM)
colnames(DTMSpec)
topfeatures(DTM, n = 20)

library(bipartite)
plotweb(DTM3, method = "normal", col.high = "lightgreen", col.low = "pink", col.interaction = "lightgrey")
# For reporting purposes, we changed the names of the columns
# as follows:
colnames(DTM3)[1:10] <- c("Understand", "Generate", "Identify", "Analytical", "Strength", "Leadership", "Teamwork", "Creativity", "Evaluate", "Management") 
plotweb(DTM3, method = "normal", col.high = "lightgreen", col.low = "pink", col.interaction = "lightgrey")
