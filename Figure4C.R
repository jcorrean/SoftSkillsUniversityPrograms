load("~/Documents/Paper Soft Skills Sampled Programs/DataforResultingBipartiteNetwork.RData")
rm(list=setdiff(ls(), "DTM3"))
# The DTM3 object is a matrix with 10 columns (with the soft skills 
# that proved to be more central and all programs as rows. In this
# matrix several programs don't have a connection with any of these
# central skills. Thus, we will discard these programs to decpict
# a bipartite Network for illustrative purposes.)

DTM4 <- apply(DTM3, 1, function(row) any(row != 0))
BN <- DTM3[DTM4, ]

library(bipartite)
plotweb(BN, method = "normal", col.high = "lightgreen", col.low = "pink", col.interaction = "lightgrey")

library(igraph)
bn <- graph.incidence(BN)
shapes <- c("circle","square")
colors <- c("lightgreen","red")

plot(bn,
     vertex.color = colors[V(bn)$type + 1],
     vertex.shape = shapes[V(bn)$type + 1],
     vertex.size = 5,
     vertex.label = ifelse(colors[V(bn)$type + 1] == "red", V(bn)$name, ""),
     vertex.label.degree = ifelse(colors[V(bn)$type + 1] == "red", 6, -pi/2),
     vertex.label.dist = ifelse(colors[V(bn)$type + 1] == "red", 0.5, 1),
     vertex.label.cex = ifelse(colors[V(bn)$type + 1] == "red", 0.8, 0.2),
     layout = function(graph) {
       # Get the default bipartite layout
       layout_bipartite <- layout.bipartite(graph)
       
       # Swap x and y coordinates and negate x to rotate 90 degrees to the right
       rotated_layout <- cbind(-layout_bipartite[, 2], layout_bipartite[, 1])
       
       return(rotated_layout)
     }
)


plot(bn,
     vertex.color = colors[V(bn)$type + 1],
     vertex.shape = shapes[V(bn)$type + 1],
     vertex.size = 5,
     vertex.label = ifelse(colors[V(bn)$type + 1] == "red", V(bn)$name, ""),
     vertex.label.degree = ifelse(colors[V(bn)$type + 1] == "red", 0, -pi/2),
     vertex.label.dist = ifelse(colors[V(bn)$type + 1] == "red", 5, 1),
     vertex.label.cex = ifelse(colors[V(bn)$type + 1] == "red", 0.8, 0.2),
     layout = function(graph) {
       # Get the default bipartite layout
       layout_bipartite <- layout.bipartite(graph)
       
       # Swap x and y coordinates and negate x to rotate 90 degrees to the right
       rotated_layout <- cbind(-layout_bipartite[, 2], layout_bipartite[, 1])
       
       return(rotated_layout)
     }
)


plot(bn,
     vertex.color = colors[V(bn)$type + 1],
     vertex.shape = shapes[V(bn)$type + 1],
     vertex.size = 5,
     vertex.label = ifelse(colors[V(bn)$type + 1] == "red", V(bn)$name, ""),
     vertex.label.degree = ifelse(colors[V(bn)$type + 1] == "red", 0, -pi/2),
     vertex.label.dist = ifelse(colors[V(bn)$type + 1] == "red", c(7, 3), 1),
     vertex.label.cex = ifelse(colors[V(bn)$type + 1] == "red", 0.8, 0.2),
     layout = function(graph) {
       # Get the default bipartite layout
       layout_bipartite <- layout.bipartite(graph)
       
       # Swap x and y coordinates and negate x to rotate 90 degrees to the right
       rotated_layout <- cbind(-layout_bipartite[, 2], layout_bipartite[, 1])
       
       return(rotated_layout)
     }
)

