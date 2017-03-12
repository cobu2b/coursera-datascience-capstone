##########################################################################################
# File: tranforming.R
#
# Description:
# Split n-gram word into an array and encode the values in the array with unigram hashing
# index. After the n-gram models are processed, they will be saved into RDS files. 
##########################################################################################

# Load required libraries 
library(tm)
library(slam)
library(hash)
library(data.table)

# Load n-gram models
unigram_tdm <- readRDS("./rds/unigram_tdm.rds")
bigram_tdm <- readRDS("./rds/bigram_tdm.rds")
trigram_tdm <- readRDS("./rds/trigram_tdm.rds")
fourgram_tdm <- readRDS("./rds/fourgram_tdm.rds")
fivegram_tdm <- readRDS("./rds/fivegram_tdm.rds")

# Calculate Frequency words
getFreqs <- function(tdm) {
  as.data.table(sort(row_sums(tdm), decreasing = T), keep.rownames=TRUE)
}

unigram <- getFreqs(unigram_tdm)
bigram <- getFreqs(bigram_tdm)
trigram <- getFreqs(trigram_tdm)
fourgram <- getFreqs(fourgram_tdm)
fivegram <- getFreqs(fivegram_tdm)

# N-gram pruning
# - Remove singlestons on bigram, trigram, and fourgram
colnames(bigram) <- c("rn", "count")
colnames(trigram) <- c("rn", "count")
colnames(fourgram) <- c("rn", "count")
colnames(fivegram) <- c("rn", "count")
bigram <- bigram[count > 1]
trigram <- trigram[count > 1]
fourgram <- fourgram[count > 1]
fivegram <- fivegram[count > 1]

# Create unigram hash
index <- 1
unigram_hash <- hash()
words <- unigram$V1
for (word in words) {
  unigram_hash[word] <- index
  index <- index + 1
}
saveRDS(words, "unigram_ranked_words.rds")
saveRDS(unigram_hash, "unigram_hash.rds")

# Indexing
# Unigram: Set row name based on unigram hash indexes
unigram$V1 <- NULL
colnames(unigram) <- "count"
saveRDS(unigram, "unigram.rds")

# Higher n-gram models: Split n words and assign an index of each word
# based on unigram hash indexes
indexing <- function(dt, ngram) {
  index <- 1
  words <- dt$rn
  for (word in words) {
    word_list <- strsplit(word, " ")[[1]]
    
    # Look up unigram hash
    dt[index, w1 := unigram_hash[[word_list[1]]]]
    dt[index, w2 := unigram_hash[[word_list[2]]]]
    
    if (ngram > 2) {
      dt[index, w3 := unigram_hash[[word_list[3]]]]
    }
    if (ngram > 3) {
      dt[index, w4 := unigram_hash[[word_list[4]]]]
    }
    if (ngram > 4) {
      dt[index, w5 := unigram_hash[[word_list[5]]]]
    }
    
    index <- index + 1
  }
}

# Bigram
bigram <- cbind(w1=0, w2=0, bigram)
indexing(bigram, 2)
bigram$rn <- NULL
saveRDS(bigram, "bigram.rds")

# Trigram
trigram <- cbind(w1=0, w2=0, w3=0, trigram)
indexing(trigram, 3)
trigram$rn <- NULL
saveRDS(trigram, "trigram.rds")

# Fourgram
fourgram <- cbind(w1=0, w2=0, w3 = 0, w4 = 0, fourgram)
indexing(fourgram, 4)
fourgram$rn <- NULL
saveRDS(fourgram, "fourgram.rds")

# Fivegram
fivegram <- cbind(w1=0, w2=0, w3 = 0, w4 = 0, w5 = 0, fivegram)
indexing(fivegram, 5)
fivegram$rn <- NULL
saveRDS(fivegram, "fivegram.rds")