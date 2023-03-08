# Analysis of Soft Skills Centrality 
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/ResultsbyProgram.RData")
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/ResultsbyAccreditation.RData")
# Here we found that 31 soft skills are present all across sampled programs
table(Resumen$Freq)
SoftSkillsCentrality <- Centralities %>% filter(., grepl('analizar|ayudar|compartir|competir|comprender|comunicar|crear|creatividad|dirigir|equipos|ético|evaluar|flexibilidad|fomentar|fortalecer|generar|gestionar|identificar|impulsar|innovar|interactuar|liderar|orientar|pensamiento crítico|persuasión|planificar|reconocer|reflexionar|resolver|responsable|tomar decisiones', SS))

library(ggplot2)
ggplot(SoftSkillsCentrality, aes(x=reorder(SS, Eigen.vector), y=Eigen.vector)) +
  geom_point(size=5, aes(colour=Level), alpha=0.6) +
  # Use a larger dot
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed")) +
  coord_flip() + theme(legend.position="top")
