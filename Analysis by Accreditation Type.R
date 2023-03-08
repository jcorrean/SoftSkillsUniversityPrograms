# Analysis by Accreditation Type
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/PreProcessing.RData")
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

Centralities <- list(HQA, QC)
Centralities <- as.data.frame(do.call(rbind, Centralities))

Resumen <- data.frame(table(Centralities$SS))
