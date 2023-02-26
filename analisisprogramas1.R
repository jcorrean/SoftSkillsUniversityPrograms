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

AllPrograms <- data.frame(cbind(
  Program = V(bn2)[type==FALSE & deg >= 1]$name,
  Skills = V(bn2)[type==FALSE & deg > 4]$deg))

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


library(igraph)
bn <- graph.incidence(Network)
bn

dict <- dictionary(list(Self_Awareness = c("emoci", "auto-percepción", "fortaleza", "necesidad", "valor", "autoeficacia", "espiritualidad"), 
                        Social_Awareness = c("perspectiva", "empatía", "diversidad", "respeto"),
                        Decision_Making = c("identificación", "situación", "análisis", "reflexi", "moral", "ético", "responsabilid"),
                        Self_Management = c("Impulso", "control", "auto-gestión", "auto-motivaci", "disciplina", "meta", "habilidad"),
                        Relationship_Management = c("lider", "comunicaci", "compromiso", "relaci", "cooperac", "negociaci", "conflict", "ayuda", "búsqued")))

pavet <- tokens(veamos)
class(pavet)
pave <- tokens(veamos) %>% tokens_lookup(dictionary = dict) %>% dfm()
pave
paved <- convert(pave, to = "data.frame")
paved$doc_id <- Muestra$NOMBRE_DEL_PROGRAMA
names(paved)[1] <- "Programa"

sum(paved$self_awareness)
sum(paved$social_awareness)
sum(paved$decision_making)
sum(paved$self_management)
sum(paved$relationship_management)

Focus <- data.frame(Soft_Skills=c("Self-Awareness", "Social Awareness", "Responsible Decision-Making", "Self Management", "Relationship Management"),
                 Frequency=c(sum(paved$self_awareness),
                             sum(paved$social_awareness),
                             sum(paved$decision_making),
                             sum(paved$self_management),
                             sum(paved$relationship_management)))
library(dplyr)
newcsv <- paved %>%
  group_by(Programa) %>%
  summarise(
    self_awareness = sum(self_awareness)
  )

newcsv2 <- paved %>%
  group_by(Programa) %>%
  summarise(
    social_awareness = sum(social_awareness)
  )

newcsv3 <- paved %>%
  group_by(Programa) %>%
  summarise(
    decision_making = sum(decision_making)
  )

newcsv4 <- paved %>%
  group_by(Programa) %>%
  summarise(
    self_management = sum(self_management)
  )

newcsv5 <- paved %>%
  group_by(Programa) %>%
  summarise(
    relationship_management = sum(relationship_management)
  )

library(ggplot2)
ggplot(data=Focus, aes(x=reorder(Soft_Skills, -Frequency), y=Frequency)) +
  geom_bar(stat="identity", fill = "red") + geom_text(aes(label=Frequency), vjust=1.6, color="white", size=3.5)+
  theme_bw()+ xlab("Socio-Emotional Skills")


summary(veamos)
veamos2 <- dfm(veamos, remove = stopwords("spanish"), remove_punct = TRUE, remove_numbers = TRUE)
class(veamos2)
topfeatures(veamos2, 20)
veamos3 <- dfm_remove(veamos2, c("formación", "investigación", 
                                 "gestión", "universidad", "proyectos",
                                 "maestría", "especialización", 
                                 "doctorado", "educación", "programa",
                                 "procesos", "profesionales", "salud",
                                 "profesional", "créditos", "estudios",
                                 "nacional", "nivel", "virtual", "perfil",
                                 "internacional", "i", "través", "egresado", "áreas"))
topfeatures(veamos3, 20)
red <- as.matrix(veamos2)
red_data <- data.frame(red)
palabras <- data.frame(variable.names(red_data))
 
library(quanteda.textstats)
freq_weight <- textstat_frequency(veamos3, n = 20)
freq_weight <- freq_weight %>% filter(!row_number() %in% c(1, 2, 4, 6, 7, 9, 12, 13, 15, 17, 18, 20))


ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Frecuencia")

library(tidyverse)
Terminos <- red_data %>% select(matches("emocio|percepc|fortalez|necesid|valo|eficacia|espiritualidad|perspectiva|empatía|diversidad|respeto|identificación|situación|análisis|reflexi|moral|ético|responsabilid|Impulso|control|auto-gestión|auto-motivaci|disciplina|meta|habilidad|comunicaci|compromiso|relaci|cooperac|negociaci|conflict|ayuda|búsqued"))
int <- as.matrix(Terminos)
skills <- as.matrix(paved)

library(igraph)
bn <- graph.incidence(skills)
summary(bn)
shapes <- c("circle","square")
colors <- c("lightgreen","red")
#Verdes son programas, Rojos son Enfoques
plot(bn,vertex.color=c("lightgreen","red")[V(bn)$type+1],
     vertex.shape=shapes[V(bn)$type+1],
     vertex.size=2,
     vertex.label.dist=1.2, vertex.label=NA)
word_degrees <- as.matrix(degree(bn, mode = "in"))
communities <- cluster_edge_betweenness(bn)

# Plot the bipartite graph with the communities colored
plot(bn, vertex.color = communities$membership, 
     vertex.size=4)

plot(bn, layout = layout_randomly,
     vertex.color=c("lightgreen","red")[V(bn)$type+1], 
     vertex.size=4,
     vertex.label=NA)

toks <- veamos %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = stopwords("spanish"), padding = FALSE)
fcmat <- fcm(toks, context = "window", tri = FALSE)
feat <- names(topfeatures(fcmat, 30))
feat[27] <- "NULL"

library(quanteda.textplots)
fcm_select(fcmat, pattern = feat) %>%
  textplot_network(min_freq = 0.5)


library(tidyverse)
redcualidades <- red_data %>% select(creatividad, liderazgo, conciencia)
palabras <- data.frame(variable.names(red_data))

dfmat_sotu <- dfm_trim(veamos2, min_termfreq = 5, min_docfreq = 3)
library(quanteda.textstats)
# hierarchical clustering - get distances on normalized dfm
tstat_dist <- textstat_dist(dfm_weight(dfmat_sotu, scheme = "prop"))
# hiarchical clustering the distance object
pres_cluster <- hclust(as.dist(tstat_dist))
# label with document names
pres_cluster$labels <- docnames(dfmat_sotu)
# plot as a dendrogram
plot(pres_cluster, xlab = "", sub = "",
     main = "Euclidean Distance on Normalized Token Frequency")
library(quanteda.textplots)
topgat_fcm <- fcm(veamos2, pattern = liderazgo)
textplot_network(topgat_fcm, min_freq = 1, edge_alpha = 0.8, edge_size = 5)
