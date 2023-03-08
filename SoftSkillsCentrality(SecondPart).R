library(GGally)
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/ResultsbyProgram.RData")
load("~/Documents/GitHub/SoftSkillsUniversityPrograms/ResultsbyAccreditation.RData")

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


library(networkD3)
library(dplyr)

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p



library(hrbrthemes)
library(GGally)
library(viridis)

# Data set is provided by R natively
data <- iris

# Plot
ggparcoord(data,
           columns = 1:4, groupColumn = 5, order = "anyClass",
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )




library(plyr)
# Summarize the ToothGrowth data
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
# Map supp to colour
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()

library(fmsb)

# Create data: note in High school for several students
set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
