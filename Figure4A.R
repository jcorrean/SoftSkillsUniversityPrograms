load("DataForFigure4.RData")
rm(list=setdiff(ls(), "TODAS"))
TODAS$keyword <- tolower(TODAS$keyword)
TODAS$keyword[TODAS$keyword=="acercar"] <- "S1"
TODAS$keyword[TODAS$keyword=="analizar"] <- "S2"
TODAS$keyword[TODAS$keyword=="argumentar"] <- "S3"
TODAS$keyword[TODAS$keyword=="ayudar"] <- "S4"
TODAS$keyword[TODAS$keyword=="cambiar"] <- "S5"
TODAS$keyword[TODAS$keyword=="compartir"] <- "S6"
TODAS$keyword[TODAS$keyword=="competir"] <- "S7"
TODAS$keyword[TODAS$keyword=="comprender"] <- "S8"
TODAS$keyword[TODAS$keyword=="comprometerse"] <- "S9"
TODAS$keyword[TODAS$keyword=="comunicar"] <- "S10"
TODAS$keyword[TODAS$keyword=="conflictos"] <- "S11"
TODAS$keyword[TODAS$keyword=="controlar"] <- "S12"
TODAS$keyword[TODAS$keyword=="crear"] <- "S13"
TODAS$keyword[TODAS$keyword=="creatividad"] <- "S14"
TODAS$keyword[TODAS$keyword=="decidir"] <- "S15"
TODAS$keyword[TODAS$keyword=="dirigir"] <- "S16"
TODAS$keyword[TODAS$keyword=="empatía"] <- "S17"
TODAS$keyword[TODAS$keyword=="equipos"] <- "S18"
TODAS$keyword[TODAS$keyword=="ético"] <- "S19"
TODAS$keyword[TODAS$keyword=="evaluar"] <- "S20"
TODAS$keyword[TODAS$keyword=="flexibilidad"] <- "S21"
TODAS$keyword[TODAS$keyword=="fomentar"] <- "S22"
TODAS$keyword[TODAS$keyword=="fortalecer"] <- "S23"
TODAS$keyword[TODAS$keyword=="generar"] <- "S24"
TODAS$keyword[TODAS$keyword=="gestionar"] <- "S25"
TODAS$keyword[TODAS$keyword=="identificar"] <- "S26"
TODAS$keyword[TODAS$keyword=="impulsar"] <- "S27"
TODAS$keyword[TODAS$keyword=="innovar"] <- "S28"
TODAS$keyword[TODAS$keyword=="interactuar"] <- "S29"
TODAS$keyword[TODAS$keyword=="liderar"] <- "S30"
TODAS$keyword[TODAS$keyword=="manifestar"] <- "S31"
TODAS$keyword[TODAS$keyword=="motivar"] <- "S32"
TODAS$keyword[TODAS$keyword=="orientar"] <- "S33" 
TODAS$keyword[TODAS$keyword=="pensamiento crítico"] <- "S34"
TODAS$keyword[TODAS$keyword=="persuasión"] <- "S35"
TODAS$keyword[TODAS$keyword=="planificar"] <- "S36"
TODAS$keyword[TODAS$keyword=="reconocer"] <- "S37"
TODAS$keyword[TODAS$keyword=="reflexionar"] <- "S38"
TODAS$keyword[TODAS$keyword=="resolver"] <- "S39"
TODAS$keyword[TODAS$keyword=="respetar"] <- "S40"
TODAS$keyword[TODAS$keyword=="responsable"] <- "S41"
TODAS$keyword[TODAS$keyword=="solucionar problemas"] <- "S42"
TODAS$keyword[TODAS$keyword=="tomar decisiones"] <- "S43"


# Figure Panel A
Network <- TODAS[c(1,5)]
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

# Normalize the centrality scores to a range between 0 and 1
normalized_scores <- (centrality_scores - min(centrality_scores)) / (max(centrality_scores) - min(centrality_scores))

# Create a color palette with different colors
color_palette <- colorRampPalette(c("red", "pink", "lightgreen", "green"))(length(unique(normalized_scores)))

# Assign colors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(normalized_scores)]

# Plot the network with node colors based on centrality
plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_components, main = "")
