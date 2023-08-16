load("DataForFigure4B.RData")
rm(list=setdiff(ls(), "Gen"))
Gen <- Gen[1:4]

library(psych)
pairs.panels(Gen, 
             method = "spearman", 
             hist.col = "green",
             density = TRUE,  
             ellipses = TRUE,
             pch = 15,
             cex = 1.5,
             cex.axis = 1.8,
             cex.labels = 2.5,
             lwd = 2,
             rug = TRUE,
             stars = TRUE
)

