load("DataForFigure4.RData")
rm(list=setdiff(ls(), "TODAS"))
TODAS$keyword <- tolower(TODAS$keyword)
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
TODAS[TODAS=="empatía"] <- "S17"
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
TODAS[TODAS=="pensamiento crítico"] <- "S34"
TODAS[TODAS=="persuasión"] <- "S35"
TODAS[TODAS=="planificar"] <- "S36"
TODAS[TODAS=="reconocer"] <- "S37"
TODAS[TODAS=="reflexionar"] <- "S38"
TODAS[TODAS=="resolver"] <- "S39"
TODAS[TODAS=="respetar"] <- "S40"
TODAS[TODAS=="responsable"] <- "S41"
TODAS[TODAS=="solucionar problemas"] <- "S42"
TODAS[TODAS=="tomar decisiones"] <- "S43"


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

centrality_scores <- degree(Terms)

# Normalize the centrality scores to a range between 0 and 1
normalized_scores <- (centrality_scores - min(centrality_scores)) / (max(centrality_scores) - min(centrality_scores))

# Create a color palette with different colors
color_palette <- colorRampPalette(c("red", "pink", "lightgreen", "green"))(length(unique(normalized_scores)))

# Assign colors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(normalized_scores)]

# Plot the network with node colors based on centrality
plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_components, main = "")

