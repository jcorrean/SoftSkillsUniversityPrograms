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
x <- Corpus(Programs)
P <- tm_map(x, content_transformer(tolower))
P <- tm_map(x, stripWhitespace)
P <- tm_map(x, removeWords, stopwords("spanish"))
uu<- content_transformer(function(x, ...) {gsub("\\(\\w+\\)", "", x)})
P <- tm_map(x, uu)
P <- tm_map(x, removeWords, c("académico", "académicos", "académica", "académicas", "con", "desde"))
P <- tm_map(x, removeWords, stopwords(kind = "es"))



dtm <- DocumentTermMatrix(P)
dtm
terminos <- data.frame(findFreqTerms(dtm))
dtm2 <- removeSparseTerms(dtm, 0.90)
dtm3 <- as.matrix(dtm2) # At least here we have a truly adjacency matrix
stopwords(kind = "es")



