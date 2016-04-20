#
# Application Name: Next Word Prediction Application - Preparation Module
# Author: Simon Chan
# Date: 19 Apr 2016
# 

# Initialize
require(quanteda)
require(tm)
require(utils)
options(stringsAsFactors = FALSE)
options(mc.cores = 4)


# Step 1: read twitter, news and blogs from files
fnames <- DirSource("final//en_US//")
fnames <- fnames$filelist
profanity_list <- readLines(file("bad-words.txt",encoding = "UTF-8"),encoding = "UTF-8")

# Function to read one file
getfile <- function(fname) {
  doc.raw <- readLines(file(fname, encoding = "UTF-8"),encoding = "UTF-8")
  return(doc.raw)
}

# Function to reall all 3 files
getfiles <- function() {
  f1 <- getfile(fnames[1])
  f2 <- getfile(fnames[2])
  f3 <- getfile(fnames[3])
  return(c(f1, f2, f3))
}

# Read and store file contents 
f <- getfiles()

# Step 2: Clean Data and generate n-grams

# Function to remove HTML and unwanted characters
removehtml <- function(rawfile) {
  # To remove HTML and other non-word elements
  for(j in seq(rawfile)){   
    rawfile[[j]] <- gsub("<.*?>", "", rawfile[[j]])   
    rawfile[[j]] <- gsub("_*", "", rawfile[[j]])
    rawfile[[j]] <- iconv(rawfile[[j]], "latin1", "ASCII", sub="")
  }   
  return(rawfile)
}

# Function to generate a n-gram frequency table given a segment of the text corpus
genfreq <- function (n, lines) {
  # To generate an n-gram frequency table 
  doc.corpus <- corpus(lines)
  doc.dfm <- dfm(doc.corpus,
                 ngrams=n,
                 toLower=T,
                 removeNumbers=T,
                 removePunct=T,
                 removeSeparators = T,
                 removeTwitter = T,
                 ignoredFeatures=profanity_list,
                 concatenator = " ",
                 language = "english",
                 stem=F)
  doc.freq <- data.frame(word = colnames(doc.dfm), freq = colSums(doc.dfm))
  rownames(doc.freq) <- NULL
  return(doc.freq)
}

# Merge word frequencies. As RAM size cannot process the corpus in one go
# it is necessary to merge word frequencies from different segments of the corpus
mergefreq <- function(doc.freq, doc.freqi) {
  # merge two word-frequency table together
  doc.freq <- merge(doc.freq, doc.freqi, by="word", all=TRUE)
  doc.freq[is.na(doc.freq)]<-0
  doc.freqt <- data.frame(word=doc.freq$word, freq=rowSums(doc.freq[,-1]))
  return(doc.freqt)
}

# Function to iteratively divide the corpus and merge the word frequencies
gengram <- function(n, rawfile) {
  # to break the input file into smaller segments and perform word-frequency generation for each segment
  for(i in seq(1,length(rawfile), by=10000)) {
    doc.sample <- rawfile[i:(i+9999)]
    writeLines(sprintf("looping: i = %d", i))
    writeLines(sprintf("text length = %d", length(doc.sample)))
    doc.sample <- removehtml(doc.sample)
    doc.freqi <- genfreq(n, doc.sample)
    if (i == 1) {
      doc.freq <- doc.freqi
    } else {
      doc.freq <- mergefreq(doc.freq, doc.freqi)
    }
    writeLines(sprintf("wordlist size: %d overall size %d", nrow(doc.freqi), nrow(doc.freq)))
  } # end of for loop
  return(doc.freq)
}

# Function to split the words in n-grams for later indexing and fast access
splitwords <- function(grams) {
  # split the word list for n-grams into separate columns for each word for ease of lookup later
  grams <- data.frame(grams)
  grams <- within(grams, word<-data.frame(do.call('rbind', strsplit(as.character(word), ' ', fixed=TRUE))))
  return(grams)
}

# Generate one-gram and save to file
onegram <- gengram(1, f)
onegram <- onegram[order(onegram$word),]
write.csv(onegram, "onegram.csv", row.names = FALSE)

# Generate bi-gram and save to file
bigram <- gengram(2, f)
bigram <- bigram[order(bigram$word),]
bigram <- splitwords(bigram)
bigram <- data.frame(word1 = bigram$word$X1, word2 = bigram$word$X2, freq = bigram$freq, stringsAsFactors = FALSE)
write.csv(bigram, "bigram.csv", row.names = FALSE)

# Generate tri-gram and save to file
trigram <- gengram(3, f)
trigram <- trigram[order(trigram$word),]
trigram <- splitwords(trigram)
trigram <- data.frame(word1 = trigram$word$X1, word2 = trigram$word$X2, word3 = trigram$word$X3, freq = trigram$freq, stringsAsFactors = FALSE)
write.csv(trigram, "trigram.csv", row.names = FALSE)

# Generate four-gram and save to file
sample <- sample(f, length(f)/10)
fourgram <- gengram(4, sample)
fourgram <- fourgram[order(fourgram$word),]
fourgram <- splitwords(fourgram)
fourgram <- data.frame(word1 = fourgram$word$X1, word2 = fourgram$word$X2, word3 = fourgram$word$X3, word4 = fourgram$word$X4, freq = fourgram$freq, stringsAsFactors = FALSE)
write.csv(fourgram, "fourgram.csv", row.names = FALSE)

# Generate five-gram and save to file
sample <- sample(f, length(f)/10)
fivegram <- gengram(5, sample)
fivegram <- fivegram[order(fivegram$word),]
fivegram <- splitwords(fivegram)
fivegram <- data.frame(word1 = fivegram$word$X1, word2 = fivegram$word$X2, word3 = fivegram$word$X3, word4 = fivegram$word$X4, word5 = fivegram$word$X5, freq = fivegram$freq, stringsAsFactors = FALSE)
write.csv(fivegram, "fivegram.csv", row.names = FALSE)



