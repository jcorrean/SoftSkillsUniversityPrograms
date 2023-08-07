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
veamos <- corpus(textos)
source("~/Documents/GitHub/SoftSkillsUniversityPrograms/SampleAnalysis.R")
docvars(veamos, "Programa") <- Muestra$NOMBRE_DEL_PROGRAMA
summary(veamos)
aja <- data.frame(summary(veamos, n = length(veamos)))
Textos <- tokens(veamos)
Textos <- tokens_tolower(Textos)
Textos

comunicar <- data.frame(kwic(Textos, pattern = "comunicar"))
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

rm(aja, institution, LevelsOfficials, LevelsPrivate, listado, Muestra, Officials, Private, Sector, textos, Textos, DATA_DIR, veamos)


TODAS <- rbind(crear, innovar, acercar, analizar, apreciar, argumentar, ayudar, cambiar, compartir, competir,
comprender, comprometer, comprometerse, comunicar, conflicto, controlar, cooperar, dirigir,
empatia, equipo, etico, evaluar, fomentar, fortalecer, generar, gestionar, identificar, impulsar,
interactuar, liderar, manifestar, motivar, negociar, orientar, planificar, reconocer, reflexionar, 
resolver, respetar, responsable, tolerar)

rm(list=setdiff(ls(), "TODAS"))

Network <- TODAS[,c(1,5)]
table(Network$keyword)
verbos <- data.frame(table(NetworkU$keyword))
#write.csv(verbos, file="verbos.csv")
#NU <- Network[!duplicated(Network[c(1,2)]),]

library(igraph)
bn2 <- graph.data.frame(Network,directed=FALSE)
bipartite.mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "red", "green")
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
#V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.8, 1)
V(bn2)$size <- 3.5
E(bn2)$color <- "grey"
plot(bn2, 
     vertex.label = NA, 
     layout = layout_components, 
     main = "")
summary(bn2)
V(bn2)$name[1:10]
table(V(bn2)$type)
V(bn2)$color[1:10]
V(bn2)$shape[1:10]

table(degree(bn2,v=V(bn2)[type==FALSE]))
mean(degree(bn2,v=V(bn2)[type==FALSE]))

V(bn2)$deg <- degree(bn2)
V(bn2)[type==FALSE & deg > 4]$name

RelevantPrograms <- data.frame(cbind(
  Program = V(bn2)[type == FALSE]$name,
  Skills = V(bn2)[type==FALSE & deg >= 4]$deg))

bn2.pr <- bipartite.projection(bn2)
Programs <- bn2.pr$proj1
Terms <- bn2.pr$proj2



graph.density(Programs)
graph.density(Terms)
get.adjacency(Programs)  
T2 <- as.matrix(get.adjacency(Terms)  )

summary(bn2)
hum <- data.frame(degree(bn2))
degree_distribution(bn2)
hist(degree_distribution(bn2))
ecount(bn2) #size of the graph (number of edges)
edge.betweenness(bn2)
hist(edge.betweenness(bn2))
EB <- data.frame(edge.betweenness(bn2))

edge.connectivity(bn2)
edge.connectivity(bn2)
SN <- as.matrix(get.adjacency(bn2))
SN1 <- as.data.frame(SN)
Skills <- as.data.frame(get.incidence(bn2))