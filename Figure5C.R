# Analysis of Soft Skills Centrality 
load("ResultsbyProgram.RData")
load("ResultsbyAccreditation.RData")
# Here we found that 31 soft skills are present all across sampled programs
table(Resumen$Freq)
library(dplyr)
SoftSkillsCentrality <- Centralities %>% filter(., grepl('analizar|ayudar|compartir|competir|comprender|comunicar|crear|creatividad|dirigir|equipos|ético|evaluar|flexibilidad|fomentar|fortalecer|generar|gestionar|identificar|impulsar|innovar|interactuar|liderar|orientar|pensamiento crítico|persuasión|planificar|reconocer|reflexionar|resolver|responsable|tomar decisiones', SS))
library(ggplot2)
ggplot(SoftSkillsCentrality, aes(x=reorder(SS, Closeness), y=Closeness)) +
  scale_fill_discrete(name="Academic Program") + 
  geom_point(size=5, aes(colour=Level), alpha=0.6) +
  # Use a larger dot
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"),) +
  coord_flip() + theme(legend.position="top") +
  theme(axis.text.x=element_text(size=15, colour="black")) +
  theme(axis.text.y=element_text(size=15, colour="black")) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=20)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20)) +
  xlab("Soft Skills") +
  ylab("Closeness Centrality") +
  theme(legend.position=c(0.95,0.1), legend.justification=c(0.95,0.1)) 

SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'generar'] <- 'Generate'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'creatividad'] <- 'Creativity'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'crear'] <- 'Create'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'liderar'] <- 'Leadership'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'identificar'] <- 'Identify'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'analizar'] <- 'Analytical'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'resolver'] <- 'Solving'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'evaluar'] <- 'Evaluate'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'equipos'] <- 'Teamwork'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'gestionar'] <- 'Management'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'dirigir'] <- 'Addressing'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'tomar decisiones'] <- 'Decision Making'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'reconocer'] <- 'Acknowledge'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'innovar'] <- 'Innovate'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'responsable'] <- 'Accountability'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'pensamiento crítico'] <- 'Critical Thinking'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'comprender'] <- 'Understanding'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'ético'] <- 'Ethical Thinking'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'fortalecer'] <- 'Strength'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'orientar'] <- 'Guidance'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'compartir'] <- 'Sharing'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'fomentar'] <- 'Foment'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'interactuar'] <- 'Social Interaction'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'comunicar'] <- 'Communication'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'flexibilidad'] <- 'Flexibility'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'reflexionar'] <- 'Thoughtfulness'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'ayudar'] <- 'Helping others'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'persuasión'] <- 'Persuasiveness'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'impulsar'] <- 'Thrust'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'competir'] <- 'Competitiveness'
SoftSkillsCentrality$SS[SoftSkillsCentrality$SS == 'planificar'] <- 'Planning'

library(ggplot2)
p <- ggplot(SoftSkillsCentrality, aes(x=reorder(SS, Eigen.vector), y=Eigen.vector)) +
  scale_fill_discrete(name="Academic Program") + 
  geom_point(size=5, aes(colour=Level), alpha=0.6) +
  # Use a larger dot
  theme_bw() + 
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"),) +
  coord_flip() + theme(legend.position="top") +
  theme(axis.text.x=element_text(size=25, colour="black")) +
  theme(axis.text.y=element_text(size=25, colour="black")) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=25)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=25)) +
  xlab("Soft Skills") +
  ylab("Eigenvector Centrality") +
  theme(legend.title=element_text(size=20), legend.text = element_text(size = 20), legend.position=c(0.95,0.1), legend.justification=c(0.95,0.1)) 

p + labs(color = "Program Type")

