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


library(igraph)
BNS <- graph.data.frame(SpecPrograms, directed = FALSE)
BNM <- graph.data.frame(MasterPrograms, directed = FALSE)
BND <- graph.data.frame(DoctoratePrograms, directed = FALSE)

hist(degree_distribution(BNS))
hist(degree_distribution(BNM))
hist(degree_distribution(BND))

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
Doc <- Doc[26:58,]
Doc$SS <- rownames(Doc)
Doc$Level <- "Doctorate"

Centralities <- list(Spec, MS, Doc)
Centralities <- as.data.frame(do.call(rbind, Centralities))

Resumen <- data.frame(table(Centralities$SS))

library(ggridges)
library(ggplot2)

# Diamonds dataset is provided by R natively
#head(diamonds)

# basic example
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


IM <- as_incidence_matrix(BNS, names = TRUE, sparse = TRUE, types = bipartite_mapping(BNS)$type)
IM2 <- as.matrix(IM)

bipartite.mapping(BNS)
V(BNS)$type <- bipartite_mapping(BNS)$type
V(BNS)$color <- ifelse(V(BNS)$type, "red", "green")
V(BNS)$shape <- ifelse(V(BNS)$type, "circle", "square")
V(BNS)$label.cex <- ifelse(V(BNS)$type, 0.8, 1)
V(BNS)$size <- sqrt(igraph::degree(BNS))
E(BNS)$color <- "lightgrey"
  plot(BNS, 
       vertex.label = NA, 
       layout = layout_in_circle, 
       main = "")

library(bipartite)
plotweb(IM2, method = "normal", col.high = "lightgreen", col.low = "pink", col.interaction = "lightgrey")
bipartite::visweb(IM2)
