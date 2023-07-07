load("~/Documents/GitHub/SoftSkillsUniversityPrograms/DataForFigure4.RData")
rm(list=setdiff(ls(), "TODAS"))
# Figure Panel A
TODAS$keyword <- tolower(TODAS$keyword)
TODAS$keyword
Network <- TODAS[,c(1,5)]
table(Network$keyword)
Network <- Network[!duplicated(Network[c(1,2)]),]
TODAS[TODAS==""] <- ""
TODAS[TODAS==""] <- ""
TODAS[TODAS==""] <- ""
TODAS[TODAS==""] <- ""
TODAS[TODAS==""] <- ""
TODAS[TODAS==""] <- ""
TODAS[TODAS==""] <- ""
TODAS[TODAS==""] <- ""


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
