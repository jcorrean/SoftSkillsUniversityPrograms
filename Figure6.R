library(GGally)
load("ResultsbyProgram.RData")
load("ResultsbyAccreditation.RData")

table(Resumen$Freq)
library(dplyr)
SoftSkillsCentrality <- Centralities %>% filter(., grepl('analizar|ayudar|compartir|competir|comprender|comunicar|crear|creatividad|dirigir|equipos|ético|evaluar|flexibilidad|fomentar|fortalecer|generar|gestionar|identificar|impulsar|innovar|interactuar|liderar|orientar|pensamiento crítico|persuasión|planificar|reconocer|reflexionar|resolver|responsable|tomar decisiones', SS))

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


dat <- SoftSkillsCentrality[1:5]
options(scipen = 999)
dat <- SoftSkillsCentrality %>% filter(., Level == "Doctorate")

library(datawizard)
dat <- dat %>% mutate(., degree.rescaled = ifelse(Degree == 0, 0.00, rescale(dat$Degree, to = c(0,1))))
dat <- dat %>% mutate(., closeness.rescaled = ifelse(Closeness == 0, 0.00, rescale(dat$Closeness, to = c(0,1))))
dat <- dat %>% mutate(., betweennes.rescaled = ifelse(Betweennes == 0, 0.00, rescale(dat$Betweennes, to = c(0,1))))
dat <- dat %>% mutate(., eigenvector.rescaled = ifelse(Eigen.vector == 0, 0.00, rescale(dat$Eigen.vector, to = c(0,1))))

summary(dat$eigenvector.rescaled)
summary(dat$degree.rescaled)
summary(dat$betweennes.rescaled)
summary(dat$closeness.rescaled)
colnames(dat)


p1 <- ggplot(dat, aes(x = reorder(SS, degree.rescaled), y = degree.rescaled)) +
  geom_bar(stat = "identity", fill="lightgreen") + theme_bw() + 
  theme(axis.text.x=element_text(size=25, colour="black")) +
  theme(axis.text.y=element_text(size=25, colour="black")) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=25)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=25)) +
  coord_flip() + xlab("Soft Skills") + ylab("Degree Centrality (rescaled 0-1)")

p2 <- ggplot(dat, aes(x = reorder(SS, closeness.rescaled), y = closeness.rescaled)) +
  geom_bar(stat = "identity", fill="lightgreen") + theme_bw() + 
  theme(axis.text.x=element_text(size=25, colour="black")) +
  theme(axis.text.y=element_text(size=25, colour="black")) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=25)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=25)) +
  coord_flip() + xlab("Soft Skills") + ylab("Closeness Centrality (rescaled 0-1)")

p3 <- ggplot(dat, aes(x = reorder(SS, betweennes.rescaled), y = betweennes.rescaled)) +
  geom_bar(stat = "identity", fill="lightgreen") + theme_bw() +
  theme(axis.text.x=element_text(size=25, colour="black")) +
  theme(axis.text.y=element_text(size=25, colour="black")) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=25)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=25)) +
  coord_flip() + xlab("Soft Skills") + ylab("Betweenness Centrality (rescaled 0-1)")

p4 <- ggplot(dat, aes(x = reorder(SS, eigenvector.rescaled), y = eigenvector.rescaled)) +
  geom_bar(stat = "identity", fill="lightgreen") + theme_bw() + 
  theme(axis.text.x=element_text(size=25, colour="black")) +
  theme(axis.text.y=element_text(size=25, colour="black")) +
  theme(axis.title.x=element_text(face="italic", colour="black", size=25)) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=25)) +
  coord_flip() + xlab("Soft Skills") + ylab("Eigenvector Centrality (rescaled 0-1)")



library(ggpubr)
figure <- ggarrange(p1, p2, p3, p4, 
                    labels = c("(A)", "(B)", "(C)", "(D)"),
                    ncol = 2, nrow = 2)

figure
