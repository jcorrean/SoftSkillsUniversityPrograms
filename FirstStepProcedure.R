# First Step Procedure

load("~/Documents/GitHub/SoftSkillsUniversityPrograms/PreProcessing.RData")
rm(list=setdiff(ls(), "TODAS"))


Network <- TODAS[,c(1,5)]
table(Network$keyword)
verbos <- data.frame(table(Network$keyword))
#write.csv(verbos, file="verbos.csv")
#NU <- Network[!duplicated(Network[c(1,2)]),]

library(igraph)
bn2 <- graph.data.frame(Network,directed=FALSE)
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

# Network node prominence measures
prominence <- data.frame(betweenness(bn2))
prominence2 <- data.frame(degree(bn2))
prominence3 <- data.frame(bonpow(bn2))
prominence4 <- page.rank(bn2)
prominence4 <- data.frame(prominence4$vector) 


table(degree(bn2,v=V(bn2)[type==FALSE]))
mean(degree(bn2,v=V(bn2)[type==FALSE]))
var(degree(bn2,v=V(bn2)[type==FALSE]))
min(degree(bn2,v=V(bn2)[type==FALSE]))
max(degree(bn2,v=V(bn2)[type==FALSE]))

V(bn2)$deg <- degree(bn2)
V(bn2)[type==FALSE & deg > 4]$name

RelevantPrograms <- data.frame(cbind(
  Program = V(bn2)[type == FALSE]$name,
  Skills = V(bn2)[type==FALSE & deg >= 4]$deg))

RelevantPrograms$Skills <- as.numeric(RelevantPrograms$Skills)

hist(RelevantPrograms$Skills, xlab= "Skills per program", main = "")

bn2.pr <- bipartite.projection(bn2)
Programs <- bn2.pr$proj1
Terms <- bn2.pr$proj2



graph.density(Programs)
CentralityPrograms <- data.frame(degree(Programs))
graph.density(Terms)
get.adjacency(Programs)  
T2 <- as.matrix(get.adjacency(Terms)  )

summary(bn2)
summary(Programs)

hum <- data.frame(degree(bn2))
degree_distribution(bn2)
hist(degree_distribution(bn2))
ecount(bn2) #size of the graph (number of edges)
edge.betweenness(bn2)
hist(edge.betweenness(bn2))
EB <- data.frame(edge.betweenness(bn2))

edge.connectivity(bn2)
edge.connectivity(bn2)
SN <- as.matrix(get.adjacency(bn2))
SN1 <- as.data.frame(SN)
Skills <- as.data.frame(get.incidence(bn2))


