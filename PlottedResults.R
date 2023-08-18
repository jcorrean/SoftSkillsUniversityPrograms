# Script for Plotting results


load("~/Documents/GitHub/SoftSkillsUniversityPrograms/PreProcessing.RData")
rm(list=setdiff(ls(), "TODAS2"))


Network <- TODAS2[,c(1,2,7)]
Network <- Network[!duplicated(Network[c(1,2)]),]
library(dplyr)
NetworkPhD <- Network %>% filter(., Program.Level == "Doctorate")
library(igraph)
bn2 <- graph.data.frame(NetworkPhD,directed=FALSE)
bipartite.mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "red", "green")
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
#V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.8, 1)
V(bn2)$size <- degree(bn2)
E(bn2)$color <- "grey"
  plot(bn2, 
       vertex.label = NA, 
       layout = layout_as_bipartite, 
       main = "")

pave <- as_adj(bn2)
library(bipartite)
data("motten1982")
plotweb2(T2)
visweb(pave)

mod <- computeModules(T2)
plotModuleWeb(mod)

