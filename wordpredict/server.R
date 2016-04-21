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
require(ggplot2)
profanity_vector <- VectorSource(readLines("bad-words.txt"))

# Read all n-grams using fast fread
onegram <- fread("onegram.csv", header = TRUE, stringsAsFactors = FALSE)
bigram <- fread("bigram.csv", header = TRUE,stringsAsFactors = FALSE)
trigram <- fread("trigram.csv", header = TRUE, stringsAsFactors = FALSE)
fourgram <- fread("fourgram.csv", header = TRUE, stringsAsFactors = FALSE)
fivegram <- fread("fivegram.csv", header = TRUE, stringsAsFactors = FALSE)
defaultResult <- head(onegram[order(-freq)], 5)[,list(word, freq)]
defaultResult <- data.frame(rep("1-gram", nrow(defaultResult)),defaultResult$word, defaultResult$freq/sum(defaultResult$freq)*100)
colnames(defaultResult) <- c("Ngram", "word", "prob")


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
    result <- head(result,5)[,list(word5, freq)]
    result <- data.frame(rep("5-gram", nrow(result)),result$word5, result$freq/sum(result$freq)*100)
    colnames(result) <- c("Ngram", "word", "prob")
    return(result)
}

# Search 4-gram
searchFourgram <- function(w1, w2, w3) {
    result <- fourgram[.(w1, w2, w3)]
    result <- result[order(-freq)]
    result <- head(result,5)[,list(word4, freq)]
    result <- data.frame(rep("4-gram", nrow(result)),result$word4, result$freq/sum(result$freq)*100)
    colnames(result) <- c("Ngram", "word", "prob")
    return(result)
}

# Search 3-gram
searchTrigram <- function(w1, w2) {
    result <- trigram[.(w1, w2)]
    result <- result[order(-freq)]
    result <- head(result,5)[,list(word3, freq)]
    result <- data.frame(rep("3-gram", nrow(result)),result$word3, result$freq/sum(result$freq)*100)
    colnames(result) <- c("Ngram", "word", "prob")
    return(result)
}

# Search 2-gram
searchBigram <- function(w1) {
    result <- bigram[.(w1)]
    result <- result[order(-freq)]
    result <- head(result,5)[,list(word2, freq)]
    result <- data.frame(rep("2-gram", nrow(result)),result$word2, result$freq/sum(result$freq)*100)
    colnames(result) <- c("Ngram", "word", "prob")
    return(result)
}

# Search 1-gram, return the default 5 most frequent uni-gram
searchOnegram <- function() {
    return(defaultResult)
}

wbind <- function(l1, l2) {
  l1 <- l1[!is.na(l1$word),]
  l2 <- l2[!is.na(l2$word),]
  rbind(l1,l2)
}

# Prediction with Stupid back-off Algorithm 
predictword <- function (inwords){
    # break word and select last three words
    inwords <- cleaninput(inwords)
    wlist <- searchFivegram(inwords[4], inwords[3], inwords[2], inwords[1])
#    wtable <- data.frame(rep(paste(inwords[4], inwords[3], inwords[2], inwords[1], sep=" "), nrow(wlist)), wlist)
    wlist <- wbind(wlist, searchFourgram(inwords[3], inwords[2], inwords[1]))
    wlist <- wbind(wlist, searchTrigram(inwords[2], inwords[1]))
    wlist <- wbind(wlist, searchBigram(inwords[1]))
    wlist <- wbind(wlist, searchOnegram())
    wlist <- wlist[!is.na(wlist$word),]
    wlist$ymax = cumsum(wlist$prob)
    wlist$ymin = c(0, head(wlist$ymax, n=-1))
    return(wlist)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$table <- renderTable(data.frame(Prediction = head(unique(predictword(input$text)$word),5)))
    output$wplot <- renderPlot(
        ggplot(predictword(input$text)) + 
            geom_rect(aes(fill=word, ymin=ymin, ymax=ymax, xmax=4, xmin=3)) +
            geom_rect(aes(fill=Ngram, ymin=ymin, ymax=ymax, xmax=2.95, xmin=2)) +
            coord_polar(theta="y") +
            xlim(c(0, 4)) + 
            theme(aspect.ratio=1)+
            ggtitle("Top 5 Word Distribution in each of N-gram")
    )
})
