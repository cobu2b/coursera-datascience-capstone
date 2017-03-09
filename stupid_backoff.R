##########################################################################################
# File: stupid_backoff.R
#
# Description:
# 
##########################################################################################

# Load required libraries 
library(hash)
library(data.table)

# Load n-gram models
unigram <- readRDS("./rds/unigram.rds")
bigram <- readRDS("./rds/bigram.rds")
trigram <- readRDS("./rds/trigram.rds")
fourgram <- readRDS("./rds/fourgram.rds")

# Calculate scores of each models
stupid_backoff_scoring <- function(model, ngram, ngram_dt) {
  rows <- nrow(model)
  
  for (row in 1:rows) {
    ngram_count <- 0
    if (ngram == 1) {
      ngram_count <- ngram_dt[w1 == model[row, w1]]$count
    }
    else if (ngram == 2) {
      ngram_count <- ngram_dt[w1 == model[row, w1] & w2 == model[row, w2]]$count
    }
    else if (ngram == 3) {
      ngram_count <- ngram_dt[w1 == model[row, w1] & w2 == model[row, w2] 
                              & w3 == model[row, w3]]$count
    }    
    
    model[row, backoff_score := model[row, backoff_score]/ngram_count]
  }
}

# Unigram
all_words <- colSums(unigram)
unigram$backoff_score <- unigram$count/all_words
unigram <- cbind(w1=0, unigram)
unigram$w1 <- row.names(unigram)
saveRDS(unigram, "unigram_backoff.rds")

# Bigram
bigram$backoff_score <- bigram$count
stupid_backoff_scoring(bigram, 1, unigram)
saveRDS(bigram, "bigram_backoff.rds")

# Trigram
stupid_backoff_scoring_trigram <- function(ng_w1, ng_w2, ng_count) {
  ngram_count <- bigram[w1 == ng_w1 & w2 == ng_w2]$count
  ng_count/ngram_count
}

trigram$backoff_score <- trigram$count
#stupid_backoff_scoring(trigram, 2, bigram)
trigram[, backoff_score := mapply(stupid_backoff_scoring_trigram, w1, w2, count)]
saveRDS(trigram, "trigram_backoff.rds")

# Fourgram
stupid_backoff_scoring_fourgram <- function(ng_w1, ng_w2, ng_w3, ng_count) {
  ngram_count <- trigram[w1 == ng_w1 & w2 == ng_w2 & w3 == ng_w3]$count
  ng_count/ngram_count
}

fourgram$backoff_score <- fourgram$count
#stupid_backoff_scoring(fourgram, 3, trigram)
fourgram[, backoff_score := mapply(stupid_backoff_scoring_fourgram, w1, w2, w3, count)]
saveRDS(fourgram, "fourgram_backoff.rds")