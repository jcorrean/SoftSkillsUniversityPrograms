load("/home/jc/Documents/GitHub/SoftSkillsUniversityPrograms/DataforResultingBipartiteNetwork.RData")
rm(list=setdiff(ls(), "DTM3"))
# The DTM3 object is a matrix with 10 columns (with the soft skills 
# that proved to be more central and all programs as rows. In this
# matrix several programs don't have a connection with any of these
# central skills. Thus, we will discard these programs to decpict
# a bipartite Network for illustrative purposes.)

DTM4 <- apply(DTM3, 1, function(row) any(row != 0))
BN <- DTM3[DTM4, ]

library(bipartite)
plotweb(BN, method = "normal", 
        col.high = "lightgreen", 
        bor.col.high = "lightgreen",
        col.low = "pink", 
        bor.col.low = "pink",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        labsize = 2)

