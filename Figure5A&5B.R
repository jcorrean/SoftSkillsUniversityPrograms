# Network Comparisons
load("ResultsbyProgram.RData")
load("ResultsbyAccreditation.RData")

library(ggplot2)
library(ggridges)
p1 <- ggplot(Centralities, aes(x = Eigen.vector, y = Level, fill = Level)) +
  geom_density_ridges(alpha = 0.3) +
  theme_ridges() + 
  theme(legend.position = "none") + 
  xlab("Eigenvector Centrality") + 
  ylab("Academic Program") + 
  theme(axis.text.x=element_text(size=35)) +
  theme(axis.text.y=element_text(size=35)) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=35)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=35))


p2 <- ggplot(Centralities2, aes(x = Eigen.vector, y = Accreditation, fill = Accreditation)) +
  geom_density_ridges(alpha = 0.3) +
  theme_ridges() + 
  theme(legend.position = "none") + 
  xlab("Eigenvector Centrality") + 
  ylab("Type of Accreditation") + 
  theme(axis.text.x=element_text(size=35)) +
  theme(axis.text.y=element_text(size=35)) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=35)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=35))


library(ggpubr)
ggarrange(p1, p2, labels = c("(A)", "(B)"), ncol = 1, nrow = 2)
