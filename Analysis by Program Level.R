load("PreProcessing.RData")
rm(list=setdiff(ls(), "TODAS2"))
TODAS2$keyword <- tolower(TODAS2$keyword)
library(dplyr)
SpecPrograms <- TODAS2 %>% filter(., Program=="Specialization") %>% 
  select(., docname, keyword)
MasterPrograms <- TODAS2 %>% filter(., Program=="Master") %>% 
  select(., docname, keyword)
DoctoratePrograms <- TODAS2 %>% filter(., Program == "Doctorate") %>% 
  select(., docname, keyword)
General <- TODAS2 %>% select(., docname, keyword)

library(igraph)
BNS <- graph.data.frame(SpecPrograms, directed = FALSE)
BNM <- graph.data.frame(MasterPrograms, directed = FALSE)
BND <- graph.data.frame(DoctoratePrograms, directed = FALSE)
BNA <- graph.data.frame(General, directed = FALSE)

hist(degree_distribution(BNS))
hist(degree_distribution(BNM))
hist(degree_distribution(BND))
hist(degree_distribution(BNA))

Spec <- data.frame(Degree = igraph::degree(BNS),
                                       Closeness = igraph::closeness(BNS),
                                       Betweennes = igraph::betweenness(BNS),
                                       Eigen = igraph::eigen_centrality(BNS))
Spec <- Spec[ -c(5:25) ]
rownames(Spec)
Spec$SS <- rownames(Spec)
Spec <- Spec[order(Spec$SS), ]
Spec <- Spec[101:140,]
Spec$Level <- "Specialization"

MS <- data.frame(Degree = igraph::degree(BNM),
                 Closeness = igraph::closeness(BNM),
                 Betweennes = igraph::betweenness(BNM),
                 Eigen = igraph::eigen_centrality(BNM))
MS <- MS[ -c(5:25) ]
rownames(MS)
MS$SS <- rownames(MS)
MS <- MS[order(MS$SS), ]
MS <- MS[74:105,]
MS$Level <- "Master"

Doc <- data.frame(Degree = igraph::degree(BND),
                 Closeness = igraph::closeness(BND),
                 Betweennes = igraph::betweenness(BND),
                 Eigen = igraph::eigen_centrality(BND))
Doc <- Doc[ -c(5:25) ]
rownames(Doc)
Doc$SS <- rownames(Doc)
Doc <- Doc[order(Doc$SS), ]
Doc <- Doc[31:60,]
Doc$Level <- "Doctorate"

Gen <- data.frame(Degree = igraph::degree(BNA),
                   Closeness = igraph::closeness(BNA),
                   Betweennes = igraph::betweenness(BNA),
                   Eigen = igraph::eigen_centrality(BNA))
Gen <- Gen[ -c(5:25) ]
rownames(Gen)
Gen$SS <- rownames(Gen)
Gen <- Gen[order(Gen$SS), ]
Gen <- Gen[204:246,]


Centralities <- list(Spec, MS, Doc)
Centralities <- as.data.frame(do.call(rbind, Centralities))

Resumen <- data.frame(table(Centralities$SS))

library(ggridges)
library(ggplot2)
ggplot(Centralities, aes(x = Eigen.vector, y = Level, fill = Level)) +
  geom_density_ridges(alpha = 0.3) +
  theme_ridges() + 
  theme(legend.position = "none") + 
  xlab("Eigenvector Centrality") + 
  ylab("Academic Program") + 
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=20)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))


IM <- as_incidence_matrix(BNA, names = TRUE, sparse = TRUE, types = bipartite_mapping(BNA)$type)
IM2 <- as.matrix(IM)

bipartite.mapping(BNA)
V(BNA)$type <- bipartite_mapping(BNA)$type
V(BNA)$color <- ifelse(V(BNA)$type, "red", "green")
V(BNA)$shape <- ifelse(V(BNA)$type, "circle", "square")
V(BNA)$label.cex <- ifelse(V(BNA)$type, 0.8, 1)
V(BNA)$size <- sqrt(igraph::degree(BNA))
E(BNA)$color <- "lightgrey"
  plot(BNA, 
       vertex.label = NA, 
       layout = layout_as_tree, 
       main = "")

library(bipartite)
plotweb(IM2, method = "normal", col.high = "lightgreen", col.low = "pink", col.interaction = "lightgrey")
bipartite::visweb(IM2)
save.image("DataForFigure4B.RData")
