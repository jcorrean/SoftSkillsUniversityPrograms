# Step 1: Opening the sample of texts
# this local folder is a clone of the GitHub Repo
setwd("/home/jc/Documents/Paper Soft Skills Sampled Programs")
listado <- data.frame(dir())
library(readtext)
library(tm)
DirSource()
# Get the data directory from readtext
DATA_DIR <- system.file("extdata/", package = "readtext")
textos <- readtext(listado$dir..)
textos$doc_id <- gsub("[^0-9-]", "", textos$doc_id)


# Step 2: Creating a corpus from texts
library(quanteda)
Textos <- corpus(textos)


# Step 3: Tagging the texts according to
# their program type and accreditation
source("~/Documents/GitHub/SoftSkillsUniversityPrograms/SampleAnalysis.R")
docvars(Textos, "Programa") <- Muestra$NOMBRE_DEL_PROGRAMA
docvars(Textos, "Program.Level") <- Muestra$Programa
docvars(Textos, "Institution") <- Muestra$NOMBRE_INSTITUCIÓN
docvars(Textos, "Accreditation") <- Muestra$Accreditation
summary(Textos)
aja <- data.frame(summary(Textos, n = length(Textos)))
SPEC <- corpus_subset(Textos, Program.Level == "Specialization")
MS <- corpus_subset(Textos, Program.Level == "Masters")
PhD <- corpus_subset(Textos, Program.Level == "Doctorate")
QC <- corpus_subset(Textos, Accreditation == "Qualified Certification")
HQC <- corpus_subset(Textos, Accreditation == "High-Quality Certification")
phd <- data.frame(summary(PhD, n = length(PhD)))

# Step 4. Soft Skills theoretically driven identification
# Keywords-in-context Search
pc <- data.frame(kwic(Textos, pattern = phrase("pensamiento crı́tico")))
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
empatia <- data.frame(kwic(Textos, pattern = "empatı́a"))
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

rm(institution, LevelsOfficials,
   LevelsPrivate, listado, Muestra,
   Officials, Private, Sector, textos,
   Textos, DATA_DIR)
TODAS <- rbind(persu, conven, flex, td, decidir, sp,
               pc, creatividad, paciencia, crear,
               innovar, acercar, analizar, apreciar,
               argumentar, ayudar, cambiar, compartir,
               competir, comprender, comprometer,
               comprometerse, comunicar, conflicto,
               controlar, cooperar, dirigir, empatia,
               equipo, etico, evaluar, fomentar, fortalecer,
               generar, gestionar, identificar, impulsar,
               interactuar, liderar, manifestar, motivar,
               negociar, orientar, planificar, reconocer,
               reflexionar, resolver, respetar,
               responsable, tolerar)

colnames(aja)[1] <- "docname"
library(dplyr)
TODAS2 <- TODAS %>%
  select(-from, -to, -pre, -post, -pattern) %>%
  left_join(aja, by = "docname")
Spec <- TODAS2 %>% filter(., Program.Level == "Specialization")
MS <- TODAS2 %>% filter(., Program.Level == "Masters")
PhD <- TODAS2 %>% filter(., Program.Level == "Doctorate")

# Step 5. Plotting results
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/DataForFigure4.RData")
rm(list=setdiff(ls(), "TODAS"))
TODAS[TODAS=="acercar"] <- "S1"
TODAS[TODAS=="analizar"] <- "S2"
TODAS[TODAS=="argumentar"] <- "S3"
TODAS[TODAS=="ayudar"] <- "S4"
TODAS[TODAS=="cambiar"] <- "S5"
TODAS[TODAS=="compartir"] <- "S6"
TODAS[TODAS=="competir"] <- "S7"
TODAS[TODAS=="comprender"] <- "S8"
TODAS[TODAS=="comprometerse"] <- "S9"
TODAS[TODAS=="comunicar"] <- "S10"
TODAS[TODAS=="conflictos"] <- "S11"
TODAS[TODAS=="controlar"] <- "S12"
TODAS[TODAS=="crear"] <- "S13"
TODAS[TODAS=="creatividad"] <- "S14"
TODAS[TODAS=="decidir"] <- "S15"
TODAS[TODAS=="dirigir"] <- "S16"
TODAS[TODAS=="empatı́a"] <- "S17"
TODAS[TODAS=="equipos"] <- "S18"
TODAS[TODAS=="ético"] <- "S19"
TODAS[TODAS=="evaluar"] <- "S20"
TODAS[TODAS=="flexibilidad"] <- "S21"
TODAS[TODAS=="fomentar"] <- "S22"
TODAS[TODAS=="fortalecer"] <- "S23"
TODAS[TODAS=="generar"] <- "S24"
TODAS[TODAS=="gestionar"] <- "S25"
TODAS[TODAS=="identificar"] <- "S26"
TODAS[TODAS=="impulsar"] <- "S27"
TODAS[TODAS=="innovar"] <- "S28"
TODAS[TODAS=="interactuar"] <- "S29"
TODAS[TODAS=="liderar"] <- "S30"
TODAS[TODAS=="manifestar"] <- "S31"
TODAS[TODAS=="motivar"] <- "S32"
TODAS[TODAS=="orientar"] <- "S33"
TODAS[TODAS=="pensamiento crı́tico"] <- "S34"
TODAS[TODAS=="persuasión"] <- "S35"
TODAS[TODAS=="planificar"] <- "S36"
TODAS[TODAS=="reconocer"] <- "S37"
TODAS[TODAS=="reflexionar"] <- "S38"
TODAS[TODAS=="resolver"] <- "S39"
TODAS[TODAS=="respetar"] <- "S40"
TODAS[TODAS=="responsable"] <- "S41"
TODAS[TODAS=="solucionar problemas"] <- "S42"
TODAS[TODAS=="tomar decisiones"] <- "S43"

# Figure 4 Panel A
Network <- TODAS[,c(1,5)]
table(Network$keyword)
Network <- Network[!duplicated(Network[c(1,2)]),]
library(igraph)
bn2 <- graph.data.frame(Network,directed=FALSE)
bipartite.mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "red", "green")
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.5, 1)
V(bn2)$size <- sqrt(igraph::degree(bn2))
E(bn2)$color <- "lightgrey"
bn2.pr <- bipartite.projection(bn2)
Terms <- bn2.pr$proj2
centrality_scores <- degree(Terms)
CS <- centrality_scores
# Normalize the centrality scores to a range between 0 and 1, 
# as follows:
# centrality_scores - min(centrality_scores) (in the numerator)
# (max(centrality_scores) - min(centrality_scores) (in the denominator)


normalized_scores <- (CS - min(CS)) / (max(CS) - min(CS))

# Create a color palette with different colors
color_palette <- colorRampPalette(c("red", "pink", "lightgreen", "green"))(length(unique(normalized_scores)))

# Assign colors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(normalized_scores)]

# Plot the network with node colors based on centrality
plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_components, main = "")

# Figure 4 Panel B
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/DataForFigure4.RData")
rm(list=setdiff(ls(), "SkillsProgramsCentrality"))

library(psych)
pairs.panels(SkillsProgramsCentrality, 
             method = "spearman", 
             hist.col = "green",
             density = TRUE,  
             ellipses = TRUE,
             pch = 21,
             cex = 2.5,
             cex.axis = 1.8,
             cex.labels = 4.5,
             lwd = 2,
             rug = TRUE,
             stars = TRUE
)

# Figure 4 Panel C
load("/home/jc/Documents/GitHub/SoftSkillsUniversityPrograms")
rm(list=setdiff(ls(), "DTM3"))
# The DTM3 object is a matrix with 10 columns (with the soft skills 
# that proved to be more central and all programs as rows. In this
# matrix several programs don't have a connection with any of these
# central skills. Thus, we will discard these programs to decpict
# a bipartite Network for illustrative purposes.)

DTM4 <- apply(DTM3, 1, function(row) any(row != 0))
BN <- DTM3[DTM4, ]

library(bipartite)
plotweb(BN, method = "normal", 
        col.high = "lightgreen", 
        bor.col.high = "lightgreen",
        col.low = "pink", 
        bor.col.low = "pink",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        labsize = 2)







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

