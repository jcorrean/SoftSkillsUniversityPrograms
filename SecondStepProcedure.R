# Topic Modeling Solution

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

spanishstopwords <- c("egresado", "programa", "programas", "crédito", stopwords("spanish"))

textos$Sector <- Muestra$SECTOR
textos$Program.Level <- Muestra$`Academic Level`
textos$Accreditation <- Muestra$Accreditation

# General Classification
library(quanteda)
tokens <- textos$text %>%
  tokens(what = "word",
         remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(c("campo", "través", "maestría", "país", "áreas", "nivel", "calidad", "estudios", "universidad", "profesionales", "perfil", "profesional", "especialización", "nacional", "formación", "egresado", "programa", "programas", "crédito", stopwords("spanish")))

dfm <- dfm_trim(dfm(tokens), min_docfreq = 0.005, max_docfreq = 0.99, 
                docfreq_type = "prop", verbose = TRUE)
topfeatures(dfm, n = 40, scheme = "docfreq")

dfm <- dfm_remove(dfm, c("así", "estudiantes", "área", "así"))

library(quanteda.textstats)
Programs <- textstat_simil(dfm, margin = "documents", method = "jaccard")
ProgramsDF <- data.frame(as.matrix(Programs))
ProgramsDF <- data.frame(jaccard = ProgramsDF[lower.tri(ProgramsDF, diag = FALSE)])

# In fourth place, we applied
# a Gaussian finite mixture model fitted by EM algorithm
library(mclust)
fit <- Mclust(ProgramsDF)
summary(fit)
Classification <- data.frame(fit$classification)
names(Classification)[1] <- "classification"
clasificados$Category <- "Asian"