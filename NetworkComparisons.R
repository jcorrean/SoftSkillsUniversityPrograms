# Network Comparisons
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/ResultsbyProgram.RData")
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/ResultsbyAccreditation.RData")


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

library(psych)
ByProgram <- psych::describe.by(Centralities$Eigen.vector, group = Centralities$Level, mat = TRUE)
one.way <- aov(Eigen.vector ~ Level, data = Centralities)
summary(one.way)

ByAccreditation <- psych::describe.by(Centralities2$Eigen.vector, group = Centralities2$Accreditation, mat = TRUE)
one.way2 <- aov(Eigen.vector ~ Accreditation, data = Centralities2)
summary(one.way2)

