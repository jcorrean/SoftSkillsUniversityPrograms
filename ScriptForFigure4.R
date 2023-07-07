load("~/Documents/GitHub/SoftSkillsUniversityPrograms/DataForFigure4.RData")
rm(list=setdiff(ls(), "TODAS"))

# Figure Panel A
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

plot(Terms, vertex.label.color="black", vertex.label.cex=1.2, vertex.color="green", vertex.size=15, edge.width=2, edge.color="lightgray",  layout = layout_on_sphere, main = "")
pave <- as_data_frame(Terms)

pave[pave=="acercar"] <- "S1"
pave[pave=="analizar"] <- "S2"
pave[pave=="argumentar"] <- "S3"
pave[pave=="ayudar"] <- "S4"
pave[pave=="cambiar"] <- "S5"
pave[pave=="compartir"] <- "S6"
pave[pave=="competir"] <- "S7"
pave[pave=="comprender"] <- "S8"
pave[pave=="comprometerse"] <- "S9"
pave[pave=="comunicar"] <- "S10"
pave[pave=="conflictos"] <- "S11"
pave[pave=="controlar"] <- "S12"
pave[pave=="crear"] <- "S13"
pave[pave=="creatividad"] <- "S14"
pave[pave=="decidir"] <- "S15"
pave[pave=="dirigir"] <- "S16"
pave[pave=="empatía"] <- "S17"
pave[pave=="equipos"] <- "S18"
pave[pave=="ético"] <- "S19"
pave[pave=="evaluar"] <- "S20"
pave[pave=="flexibilidad"] <- "S21"
pave[pave=="fomentar"] <- "S22"
pave[pave=="fortalecer"] <- "S23"
pave[pave=="generar"] <- "S24"
pave[pave=="gestionar"] <- "S25"
pave[pave=="identificar"] <- "S26"
pave[pave=="impulsar"] <- "S27"
pave[pave=="innovar"] <- "S28"
pave[pave=="interactuar"] <- "S29"
pave[pave=="liderar"] <- "S30"
pave[pave=="manifestar"] <- "S31"
pave[pave=="motivar"] <- "S32"
pave[pave=="orientar"] <- "S33" 
pave[pave=="pensamiento crítico"] <- "S34"
pave[pave=="persuasión"] <- "S35"
pave[pave=="planificar"] <- "S36"
pave[pave=="reconocer"] <- "S37"
pave[pave=="reflexionar"] <- "S38"
pave[pave=="resolver"] <- "S39"
pave[pave=="respetar"] <- "S40"
pave[pave=="responsable"] <- "S41"
pave[pave=="solucionar problemas"] <- "S42"
pave[pave=="tomar decisiones"] <- "S43"

bn3 <- graph.data.frame(pave,directed=FALSE)
bipartite.mapping(bn3)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "red", "green")
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.5, 1)
V(bn2)$size <- sqrt(igraph::degree(bn2))
E(bn2)$color <- "lightgrey"

bn3.pr <- bipartite.projection(bn3)
Terms <- bn2.pr$proj2

plot(Terms, vertex.label.color="black", vertex.label.cex=1.2, vertex.color="green", vertex.size=15, edge.width=2, edge.color="lightgray",  layout = layout_on_sphere, main = "")





# Figure Panel B
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
