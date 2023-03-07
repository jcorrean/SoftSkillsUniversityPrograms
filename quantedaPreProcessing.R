# DTM by program level from quanteda 
setwd("/home/jc/Documents/Paper Soft Skills Sampled Programs")
listado <- data.frame(dir())
library(readtext)
library(tm)
DirSource()
# Get the data directory from readtext
DATA_DIR <- system.file("extdata/", package = "readtext")
textos <- readtext(listado$dir..)
textos$doc_id <- gsub("[^0-9-]", "", textos$doc_id)

library(quanteda)
TOKS <- corpus(textos$text) %>% 
  tokens(remove_numbers = TRUE, remove_punct = TRUE) %>% 
  tokens_remove(stopwords("es"))
DTM <- dfm(TOKS, tolower = FALSE)
SoftSkills <- c("generar", "evaluar", "liderar", "equipos", "analizar", "gestionar", "fortalecer", "identificar", "crear", "comprender") #Eigenvector
toks_inside <- tokens_keep(TOKS, pattern = SoftSkills, window = 0)
DTM2 <- dfm(toks_inside)

DTM3 <- as.matrix(DTM2)
colnames(DTM3)

library(bipartite)
plotweb(DTM3, method = "normal", col.high = "lightgreen", col.low = "pink", col.interaction = "lightgrey")
# For reporting purposes, we changed the names of the columns
# as follows:
colnames(DTM3)[1:10] <- c("Understand", "Generate", "Identify", "Analytical", "Strength", "Leadership", "Teamwork", "Creativity", "Evaluate", "Management") 
plotweb(DTM3, method = "normal", col.high = "lightgreen", col.low = "pink", col.interaction = "lightgrey")

mod <- computeModules(t(DTM3))
plotModuleWeb(mod)
compart(DTM3)
H2fun(DTM3, H2_integer=TRUE)
networklevel(DTM3, index="ALLBUTDD", level="both", weighted=TRUE, 
             ISAmethod="Bluethgen",  SAmethod = "Bluethgen", extinctmethod = "r", 
             nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE, 
             logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE, 
             fcdist="euclidean", legacy=FALSE)
