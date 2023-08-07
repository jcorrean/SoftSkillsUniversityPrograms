# First Step Procedure
# Analysis of Skills centrality

load("~/Documents/GitHub/SoftSkillsUniversityPrograms/PreProcessing.RData")
rm(list=setdiff(ls(), "TODAS"))
TODAS$keyword <- tolower(TODAS$keyword)
Dictionary <- data.frame(table(TODAS$keyword))

Network <- TODAS[,c(1,5)]
table(Network$keyword)
verbos <- data.frame(table(Network$keyword))
#write.csv(verbos, file="verbos.csv")
Network <- Network[!duplicated(Network[c(1,2)]),]
verbos <- data.frame(table(Network$keyword))

library(igraph)
bn2 <- graph.data.frame(Network,directed=FALSE)
igraph::degree(bn2)
bipartite.mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "red", "green")
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
#V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.8, 1)
V(bn2)$size <- 3.5
E(bn2)$color <- "grey"
plot(bn2, 
     vertex.label = NA, 
     layout = layout_nicely, 
     main = "")
summary(bn2)
V(bn2)$name[1:10]
table(V(bn2)$type)
V(bn2)$color[1:10]
V(bn2)$shape[1:10]

# Useful syntaxes for plotting0
#pave <- as_adjacency_matrix(bn2)
#library(bipartite)
#plotweb2(pave)
#visweb(pave)


# Useful syntaxes for exporting
#library(rgexf)
#pave <- igraph.to.gexf(bn2)
#write.gexf(pave, output = "/home/jc/pave.gexf", replace = TRUE)

# Network node prominence measures
Degrees <- data.frame(igraph::degree(bn2))
Betweenness <- data.frame(igraph::betweenness(bn2))
Closeness <- data.frame(igraph::closeness(bn2))
Eigen <- data.frame(igraph::eigen_centrality(bn2))
#prominence4 <- page.rank(bn2)
#prominence4 <- data.frame(prominence4$vector) 
#prominence4$Program <- rownames(prominence4)

table(igraph::degree(bn2,v=V(bn2)[type==FALSE]))
mean(igraph::degree(bn2,v=V(bn2)[type==FALSE]))
var(igraph::degree(bn2,v=V(bn2)[type==FALSE]))
min(igraph::degree(bn2,v=V(bn2)[type==FALSE]))
max(igraph::degree(bn2,v=V(bn2)[type==FALSE]))
edge_density(bn2)


library(igraph)
bn2 <- graph.data.frame(Network,directed=FALSE)
bipartite.mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "red", "green")
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.8, 1)
V(bn2)$size <- sqrt(igraph::degree(bn2))
E(bn2)$color <- "lightgrey"
  plot(bn2, 
       vertex.label = NA, 
       layout = layout_as_bipartite, 
       main = "")
  summary(bn2)
#x <- tkplot(bn2)

bn2.pr <- bipartite.projection(bn2)
max(igraph::degree(Programs))
Programs <- bn2.pr$proj1

SkillsProgramsCentrality <- data.frame(Degree = igraph::degree(bn2),
                                       Closeness = igraph::closeness(bn2),
                                       Betweennes = igraph::betweenness(bn2),
                                       Eigen = igraph::eigen_centrality(bn2))
SkillsProgramsCentrality <- SkillsProgramsCentrality[ -c(5:25) ]
names(SkillsProgramsCentrality)[4] <- "Eigenvector"
names(SkillsProgramsCentrality)[3] <- "Betweenness"

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


SkillsProgramsCentrality$Skill <- row.names(SkillsProgramsCentrality)


Terms <- bn2.pr$proj2
SkillsCentrality <- data.frame(Degree = degree(Terms),
                               Closeness = closeness(Terms),
                               Betweennes = betweenness(Terms),
                               Eigen = eigen_centrality(Terms))

SkillsCentrality <- SkillsCentrality[ -c(5:25) ]


library(psych)
pairs.panels(SkillsCentrality, 
             method = "spearman", 
             hist.col = "green",
             density = TRUE,  
             ellipses = TRUE,
             pch = 21,
             cex = 2.5,
             cex.axis = 1.8,
             cex.labels = 2.5,
             lwd = 2,
             rug = TRUE,
             stars = TRUE
)

SkillsCentrality$Skill <- row.names(SkillsCentrality)
#c1 = cluster_fast_greedy(Terms)
#c2 = cluster_fast_greedy(Terms)

# modularity measure
#modularity(c1)
#B = modularity_matrix(Programs, membership(c1))
#round(B[1,],2)
#membership(c1)
#length(c1)
#sizes(c1)
#crossing(c1, Terms)
#plot(c1, Terms, layout=layout_with_dh(Terms))
clique.number(bn2.pr$proj1)
clique.number(bn2.pr$proj2)
clique.number(Terms)
Cliques <- largest_cliques(Terms)
Cliques[[1]]
Cliques[[2]]



library(intergraph)
graph.coreness(Terms)
coreness <- graph.coreness(Terms)
table(coreness)

V(Terms)$color <- coreness + 1
plot(Terms, vertex.label.color="black", vertex.label.cex=1.2, vertex.color="green", vertex.size=15, edge.width=2, edge.color="lightgray",  layout = layout_on_sphere, main = "")
plot(Programs,vertex.label.color="black", vertex.label.cex=1, vertex.color="pink", vertex.size=20, edge.width=2, edge.color="lightgray", layout = layout_components, main = "Programs Unipartite Network")

cluster_fast_greedy(bn2)
cluster_louvain(bn2)
cluster_spinglass(bn2)
oye <- cluster_leading_eigen(Terms)
oye2 <- cluster_leading_eigen(Programs)
modularity(oye)
modularity(oye2)
modularity(bn2)
plot_dendrogram(oye)
plot_dendrogram(oye2)


modularity(c2)
B2 = modularity_matrix(Terms, membership(c2))
round(B2[1,],2)
 membership(c2)
length(c2)
sizes(c2)
crossing(c2, Terms)

par(mfrow=c(1,2))
plot(c2, Terms, layout=layout_with_dh)
plot(Programs, vertex.color=membership(c2), layout=layout_with_dh)

graph.density(Programs)
CentralityPrograms <- data.frame(degree(Programs))
graph.density(Terms)
graph.density(bn2)
ADJ <- get.adjacency(Programs)  
T2 <- as.matrix(get.adjacency(Terms)  )

summary(bn2)
summary(Programs)
graph.density(bn2)
graph.density(Programs)
graph.density(Terms)
components(Programs)


hum <- data.frame(degree(bn2))
degree_distribution(bn2)
hist(degree_distribution(bn2))
ecount(Terms) #size of the graph (number of edges)
edge.betweenness(Terms)
hist(edge.betweenness(Terms))
EB <- data.frame(edge.betweenness(Terms))

edge.connectivity(bn2)
edge.connectivity(bn2)
SN <- as.matrix(get.adjacency(bn2))
SN1 <- as.data.frame(SN)
Skills <- as.data.frame(get.incidence(bn2))