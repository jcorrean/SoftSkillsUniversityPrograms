setwd("/home/jc/Documents/Paper Soft Skills Sampled Programs")
listado <- data.frame(dir())
muestra <- data.frame(Muestra$codigoprograma)
colnames(listado)[1]  <- "pave"
colnames(muestra)[1]  <- "pave"

library(arsenal)
comparedf(muestra, listado)
pavec <- summary(comparedf(muestra, listado))
pavee <- as.data.frame(pavec$diffs.table)
library(readtext)
library(tm)
DirSource()
# Get the data directory from readtext
DATA_DIR <- system.file("extdata/", package = "readtext")
textos <- readtext(listado$dir..)

library(quanteda)
veamos <- corpus(textos)
source("~/Documents/GitHub/SoftSkillsUniversityPrograms/SampleAnalysis.R")
docvars(veamos, "Programa") <- Muestra$NOMBRE_DEL_PROGRAMA
summary(veamos)

dict <- dictionary(list(Self_Awareness = c("emoci", "auto-perción", "fortaleza", "necesidad", "valor", "autoeficacia", "espiritualidad"), 
                        Social_Awareness = c("perspectiva", "empatía", "diversidad", "respeto"),
                        Decision_Making = c("identificación", "situación", "análisis", "reflexi", "moral", "ético", "responsabilid"),
                        Self_Management = c("Impulso", "control", "auto-gestión", "auto-motivaci", "disciplina", "meta", "habilidad"),
                        Relationship_Management = c("lider", "comunicaci", "compromiso", "relaci", "cooperac", "negociaci", "conflict", "ayuda", "búsqued")))

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
                 Frequency=c(127, 127, 315, 142, 32))
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

topfeatures(veamos2, 20)
red <- as.matrix(veamos2)
red_data <- data.frame(red)
palabras <- data.frame(variable.names(red_data))
 
library(quanteda.textstats)
freq_weight <- textstat_frequency(veamos2, n = 20)
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

library(igraph)

bn <- graph.incidence(int)
summary(bn)
shapes <- c("circle","square")
colors <- c("blue","red")
#Azules son programas, Rojos son Enfoques
plot(bn,vertex.color=c("lightgreen","red")[V(bn)$type+1],
     vertex.shape=shapes[V(bn)$type+1],
     vertex.size=2,
     vertex.label.dist=1.2, vertex.label=NA)


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
