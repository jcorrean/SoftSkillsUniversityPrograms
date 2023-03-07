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
