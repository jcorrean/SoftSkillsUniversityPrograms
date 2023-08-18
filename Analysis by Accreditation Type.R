# Analysis by Accreditation Type
load("PreProcessing.RData")
rm(list=setdiff(ls(), "TODAS2"))
TODAS2$keyword <- tolower(TODAS2$keyword)
library(dplyr)
# Let's go first with programs with qualified certification
QC <- TODAS2 %>% filter(., Accreditation=="Qualified Certification") %>% 
  select(., docname, keyword)
# Then, let's go with programs with High-Quality Accreditation
HQA <- TODAS2 %>% filter(., Accreditation=="High-Quality Certification") %>% 
  select(., docname, keyword)
library(igraph)
BNQC <- graph.data.frame(QC, directed = FALSE)
BNHQA <- graph.data.frame(HQA, directed = FALSE)

hist(degree_distribution(BNQC))
hist(degree_distribution(BNHQA))


QC <- data.frame(Degree = igraph::degree(BNQC),
                   Closeness = igraph::closeness(BNQC),
                   Betweennes = igraph::betweenness(BNQC),
                   Eigen = igraph::eigen_centrality(BNQC))
QC <- QC[ -c(5:25) ]
rownames(QC)
QC <- QC[182:223,]
QC$SS <- rownames(QC)
QC$Accreditation <- "Qualified Certification"

HQA <- data.frame(Degree = igraph::degree(BNHQA),
                 Closeness = igraph::closeness(BNHQA),
                 Betweennes = igraph::betweenness(BNHQA),
                 Eigen = igraph::eigen_centrality(BNHQA))
HQA <- HQA[ -c(5:25) ]
rownames(HQA)
HQA <- HQA[23:54,]
HQA$SS <- rownames(HQA)
HQA$Accreditation <- "High-Quality Certification"

Centralities2 <- list(HQA, QC)
Centralities2 <- as.data.frame(do.call(rbind, Centralities2))

Resumen2 <- data.frame(table(Centralities2$SS))

library(ggridges)
library(ggplot2)

ggplot(Centralities2, aes(x = Eigen.vector, y = Accreditation, fill = Accreditation)) +
  geom_density_ridges(alpha = 0.3) +
  theme_ridges() + 
  theme(legend.position = "none") + 
  xlab("Eigenvector Centrality") + 
  ylab("Type of Accreditation")


IM <- as_incidence_matrix(BNHQA, names = TRUE, sparse = TRUE, types = bipartite_mapping(BNHQA)$type)
IM2 <- as.matrix(IM)

bipartite.mapping(BNHQA)
V(BNHQA)$type <- bipartite_mapping(BNHQA)$type
V(BNHQA)$color <- ifelse(V(BNHQA)$type, "red", "green")
V(BNHQA)$shape <- ifelse(V(BNHQA)$type, "circle", "square")
V(BNHQA)$label.cex <- ifelse(V(BNHQA)$type, 0.8, 1)
V(BNHQA)$size <- sqrt(igraph::degree(BNHQA))
E(BNHQA)$color <- "lightgrey"
  plot(BNHQA, 
       vertex.label = NA, 
       layout = layout_components, 
       main = "")
  
  
library(ggplot2)
  ggplot(Centralities2, aes(x = Eigen.vector, y = Accreditation, fill = Accreditation)) +
    geom_density_ridges(alpha = 0.3) +
    theme_ridges() + 
    theme(legend.position = "none") + 
    xlab("Eigenvector Centrality") + 
    ylab("Type of Accreditation") + 
    theme(axis.text.x=element_text(size=15)) +
    theme(axis.text.y=element_text(size=15)) +
    theme(axis.title.x=element_text(face="italic", colour="black", size=20)) +
    theme(axis.title.y=element_text(face="italic", colour="black", size=20))

  
library(bipartite)
plotweb(IM2, method = "normal", col.high = "lightgreen", col.low = "pink", col.interaction = "lightgrey")
bipartite::visweb(IM2)
mod <- computeModules(IM2)
plotModuleWeb(mod)
