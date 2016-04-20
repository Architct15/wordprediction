#
# This is the server logic of a Shiny web application 
# Application Name: Next Word Prediction Application
# Author: Simon Chan
# Date: 19 Apr 2016
# 

# Initialize
library(shiny)
require(data.table)
require(tm)
profanity_vector <- VectorSource(readLines("bad-words.txt"))

# Read all n-grams using fast fread
onegram <- fread("onegram.csv", header = TRUE, stringsAsFactors = FALSE)
bigram <- fread("bigram.csv", header = TRUE,stringsAsFactors = FALSE)
trigram <- fread("trigram.csv", header = TRUE, stringsAsFactors = FALSE)
fourgram <- fread("fourgram.csv", header = TRUE, stringsAsFactors = FALSE)
fivegram <- fread("fivegram.csv", header = TRUE, stringsAsFactors = FALSE)
defaultResult <- head(onegram[order(-freq)], 6)$word

# Create index for fast access
setkey(onegram,word)
setkeyv(bigram, c("word1"))
setkeyv(trigram, c("word1", "word2"))
setkeyv(fourgram, c("word1", "word2", "word3"))
setkeyv(fivegram, c("word1", "word2", "word3", "word4"))

# Clean up input string
cleaninput <- function(inputs) {
    inputs <- gsub("<.*?>", "", inputs)   
    inputs <- gsub("_*", "", inputs)
    inputs <- iconv(inputs, "latin1", "ASCII", sub="")
    inputs <- Corpus(VectorSource(inputs))
    inputs <- tm_map(inputs, removePunctuation)
    inputs <- tm_map(inputs, removeNumbers)
    inputs <- tm_map(inputs, tolower) 
    inputs <- tm_map(inputs, removeWords, profanity_vector$content) 
    inputs <- tm_map(inputs, stripWhitespace)
    inputs <- strsplit(inputs$content[[1]], split = " ")
    inputs <- inputs[[1]]
    return(rev(inputs))
}

# Search 5-gram
searchFivegram <- function(w1, w2, w3, w4) {
    result <- fivegram[.(w1, w2, w3, w4)]
    result <- result[order(-freq)]
    return(head(result,5)$word5)
}

# Search 4-gram
searchFourgram <- function(w1, w2, w3) {
    result <- fourgram[.(w1, w2, w3)]
    result <- result[order(-freq)]
    return(head(result,5)$word4)
}

# Search 3-gram
searchTrigram <- function(w1, w2) {
    result <- trigram[.(w1, w2)]
    result <- result[order(-freq)]
    return(head(result,5)$word3)
}

# Search 2-gram
searchBigram <- function(w1) {
    result <- bigram[.(w1)]
    result <- result[order(-freq)]
    return(head(result,5)$word2)
}

# Search 1-gram, return the default 5 most frequent uni-gram
searchOnegram <- function() {
    return(defaultResult)
}

# Prediction with Stupid back-off Algorithm 
predictword <- function (inwords){
    # break word and select last three words
    inwords <- cleaninput(inwords)
    wlist <- searchFivegram(inwords[4], inwords[3], inwords[2], inwords[1])
    wlist <- append(wlist, searchFourgram(inwords[3], inwords[2], inwords[1]))
    wlist <- append(wlist, searchTrigram(inwords[2], inwords[1]))
    wlist <- append(wlist, searchBigram(inwords[1]))
    wlist <- append(wlist, searchOnegram())
    wlist <- wlist[!is.na(wlist)]
    return(as.character(head(unique(wlist), 5)))
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$table <- renderTable(data.frame(Predictions = predictword(input$text)))
})
