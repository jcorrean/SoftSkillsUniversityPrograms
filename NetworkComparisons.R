# Network Comparisons
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/ResultsbyProgram.RData")
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/ResultsbyAccreditation.RData")


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


ggplot(Centralities2, aes(x = Eigen.vector, y = Accreditation, fill = Accreditation)) +
  geom_density_ridges(alpha = 0.3) +
  theme_ridges() + 
  theme(legend.position = "none") + 
  xlab("Eigenvector Centrality") + 
  ylab("Type of Accreditation")

library(psych)
ByProgram <- psych::describe.by(Centralities$Eigen.vector, group = Centralities$Level, mat = TRUE)
one.way <- aov(Eigen.vector ~ Level, data = Centralities)
summary(one.way)

ByAccreditation <- psych::describe.by(Centralities2$Eigen.vector, group = Centralities2$Accreditation, mat = TRUE)
one.way2 <- aov(Eigen.vector ~ Accreditation, data = Centralities2)
summary(one.way2)
