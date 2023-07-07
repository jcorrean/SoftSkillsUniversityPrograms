load("~/Documents/GitHub/SoftSkillsUniversityPrograms/DataForFigure4.RData")
rm(list=setdiff(ls(), "SkillsProgramsCentrality"))

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
