# Pre-Processing

setwd("/home/jc/Documents/Paper Soft Skills Sampled Programs")
listado <- data.frame(dir())
library(readtext)
library(tm)
DirSource()
# Get the data directory from readtext
DATA_DIR <- system.file("extdata/", package = "readtext")
textos <- readtext(listado$dir..)
textos$doc_id <- gsub("[^0-9-]", "", textos$doc_id)
Programs <- DataframeSource(textos)
Programs <- removeNumbers(Programs)
x <- Corpus(Programs)
P <- tm_map(x, stripWhitespace)
P <- tm_map(x, content_transformer(tolower))
P <- tm_map(x, removeWords, stopwords("spanish"))

inspect(x)


