setwd("/home/jc/Documents/PaperHabilidadesBlandasPilar")
listado <- data.frame(dir())
library(readtext)
library(tm)
DirSource()
# Get the data directory from readtext
DATA_DIR <- system.file("extdata/", package = "readtext")
textos <- readtext(listado$dir..)

library(quanteda)
veamos <- corpus(textos)
dict <- dictionary(list(Auto.Conciencia = c("emoci", "auto-perción", "fortaleza", "necesidad", "valor", "autoeficacia", "espiritualidad"), 
                        Conciencia.Social = c("perspectiva", "empatía", "diversidad", "respeto"),
                        Toma.de.Decisión = c("identificación", "situación", "análisis", "reflexi", "moral", "ético", "responsabilid"),
                        Auto.gestión = c("Impulso", "control", "auto-gestión", "auto-motivaci", "disciplina", "meta", "habilidad"),
                        Relacionamiento = c("comunicaci", "compromiso", "relaci", "cooperac", "negociaci", "conflict", "ayuda", "búsqued")))

pave <- tokens(veamos) %>% tokens_lookup(dictionary = dict) %>% dfm()
pave
paved <- data.frame(pave)
sum(paved$auto.conciencia)
sum(paved$conciencia.social)
sum(paved$toma.de.decisión)
sum(paved$auto.gestión)
sum(paved$relacionamiento)

Enfoque <- data.frame(Competencias=c("Auto-Conciencia", "Conciencia Social", "Toma de Decisión", "Auto-Gestión", "Relacionamiento"),
                 Frecuencias=c(113, 119, 289, 122, 29))
library(ggplot2)
ggplot(data=Enfoque, aes(x=reorder(Competencias, -Frecuencias), y=Frecuencias)) +
  geom_bar(stat="identity", fill = "red") + geom_text(aes(label=Frecuencias), vjust=1.6, color="white", size=3.5)+
  theme_bw()+ xlab("Competencias Socio-Emocionales")


summary(veamos)
veamos2 <- dfm(veamos, remove = stopwords("spanish"), remove_punct = TRUE, remove_numbers = TRUE)
topfeatures(veamos2, 20)
red <- as.matrix(veamos2)
red_data <- data.frame(red)
palabras <- data.frame(variable.names(red_data))
library(tidyverse)
Terminos <- red_data %>% select(matches("emocio|percepc|fortalez|necesid|valo|eficacia|espiritualidad|perspectiva|empatía|diversidad|respeto|identificación|situación|análisis|reflexi|moral|ético|responsabilid|Impulso|control|auto-gestión|auto-motivaci|disciplina|meta|habilidad|comunicaci|compromiso|relaci|cooperac|negociaci|conflict|ayuda|búsqued"))

library(igraph)
bn <- graph.incidence(Terminos)
shapes <- c("circle","square")
colors <- c("blue","red")
#Azules son programas, Rojos son Enfoques
plot(bn,vertex.color=colors[V(bn)$type+1],
     vertex.shape=shapes[V(bn)$type+1],
     vertex.size=2,
     vertex.label.dist=1.2, vertex.label=NA)


plot(bn, layout = layout_nicely,
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
