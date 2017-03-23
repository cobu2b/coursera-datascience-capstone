##########################################################################################
# File: predict.R
#
# Description: 
# Predict a next word based on previouos words using stupid backoff algorithm
#
# Note highest ngram of 5 is performed pooriler than highest ngram of 4 in q2_set
# (4/10 vs 3/10)
##########################################################################################

# Load required libraries
library(tm)
library(hash)
library(data.table)

# Configuration
RANK_SIZE <- 5
HIGHEST_NGRAM <- 4

# Load unigram hash and n-gram models with calculated stupid_backoff
unigram_hash <- readRDS("./rds/unigram_hash.rds")
unigram_words <- readRDS("./rds/unigram_ranked_words.rds")
unigram <- readRDS("./rds/unigram_backoff.rds")
bigram <- readRDS("./rds/bigram_backoff.rds")
trigram <- readRDS("./rds/trigram_backoff.rds")
fourgram <- readRDS("./rds/fourgram_backoff.rds")
fivegram <- readRDS("./rds/fivegram_backoff.rds")

# Create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

transform_string <- function(string) {
  vcorpus <- Corpus(VectorSource(string))
  
  # Remove twitter characters
  vcorpus <- tm_map(vcorpus, toSpace, "@")
  vcorpus <- tm_map(vcorpus, toSpace, "#")
  
  # Transform to lower case
  vcorpus <- tm_map(vcorpus,content_transformer(tolower))
  
  # Remove numbers from corpus
  vcorpus <- tm_map(vcorpus, removeNumbers)
  
  # Remove punctuation from corpus
  vcorpus <- tm_map(vcorpus, removePunctuation)
  
  # Remove white spaces from corpus
  vcorpus <- tm_map(vcorpus, stripWhitespace)
  
  word_list <- strsplit(vcorpus[[1]]$content, " ")[[1]]
  
  # Remove an empty string on the word list
  return(word_list[word_list != ""])
}

to_hash <- function(words) {
  indexes <- c()
  
  for (index in 1:length(words)) {
    indexes <- c(indexes, unigram_hash[[words[index]]])
  }
  
  return(indexes)
}

update_top_scores <- function(dt, word, backoff_score) {
  # New word must not exist in the top words
  if (sum(dt$word_index == word) > 0) {
    return()
  }
  
  if (backoff_score > dt[RANK_SIZE, score]) {
    dt[RANK_SIZE, word_index := as.character(word)]
    dt[RANK_SIZE, score := backoff_score]
    
    setorder(dt, -score)
  }
}

select_top_scores_from_unigram <- function(model, lambda, dt) {
  selected_words <- model[1:RANK_SIZE, ]
  rows <- nrow(selected_words)
  
  # Validate selected words
  if (rows == 0) {
    return()
  }
  
  # Update ranked top words  
  for (row in 1:rows) {
    word_index <- selected_words[row, w1]
    backoff_score <- selected_words[row, backoff_score] * lambda
    update_top_scores(dt, word_index, backoff_score)
  }
}

select_top_scores_from_bigram <- function(model, lambda, words, dt) {
  # Select top words matched given words
  selected_words <- model[w1 == words[1]]
  rows <- nrow(selected_words)
  
  # Validate selected words
  if (rows == 0) {
    return()
  } 
  else if (rows > RANK_SIZE) {
    rows <- RANK_SIZE;
  }
  
  # Update ranked top words  
  for (row in 1:rows) {
    word_index <- selected_words[row, w2]
    backoff_score <- selected_words[row, backoff_score] * lambda
    update_top_scores(dt, word_index, backoff_score)
  }
}

select_top_scores_from_trigram <- function(model, lambda, words, dt) {
  # Select top words matched given words
  selected_words <- model[w1 == words[1] & w2 == words[2]]
  rows <- nrow(selected_words)
  
  # Validate selected words
  if (rows == 0) {
    return()
  } 
  else if (rows > RANK_SIZE) {
    rows <- RANK_SIZE;
  }
  
  # Update ranked top words  
  for (row in 1:rows) {
    word_index <- selected_words[row, w3]
    backoff_score <- selected_words[row, backoff_score] * lambda
    update_top_scores(dt, word_index, backoff_score)
  }
}

select_top_scores_from_fourgram <- function(model, lambda, words, dt) {
  # Select top words matched given words
  selected_words <- model[w1 == words[1] & w2 == words[2] & w3 == words[3]]
  rows <- nrow(selected_words)
  
  # Validate selected words
  if (rows == 0) {
    return()
  } 
  else if (rows > RANK_SIZE) {
    rows <- RANK_SIZE;
  }

  # Update ranked top words  
  for (row in 1:rows) {
    word_index <- selected_words[row, w4]
    backoff_score <- selected_words[row, backoff_score] * lambda
    update_top_scores(dt, word_index, backoff_score)
  }
}

select_top_scores_from_fivegram <- function(model, lambda, words, dt) {
  # Select top words matched given words
  selected_words <- model[w1 == words[1] & w2 == words[2] & w3 == words[3] & w4 == words[4]]
  rows <- nrow(selected_words)
  
  # Validate selected words
  if (rows == 0) {
    return()
  } 
  else if (rows > RANK_SIZE) {
    rows <- RANK_SIZE;
  }
  
  # Update ranked top words  
  for (row in 1:rows) {
    word_index <- selected_words[row, w5]
    backoff_score <- selected_words[row, backoff_score] * lambda
    update_top_scores(dt, word_index, backoff_score)
  }
}

stupid_backoff_ranking <- function(words) {
  lambda <- 1
  ngram <- HIGHEST_NGRAM
  ranked_dt <- data.table(word_index = rep("", RANK_SIZE), 
                          score = rep(double(1), RANK_SIZE))
  
  # Select n-gram models
  if (length(words) < HIGHEST_NGRAM) {
    ngram <- length(words) + 1
  }
  
  for (ng in ngram:1) {
    input <- tail(words, ng - 1)
    
    if (ng == 5) {
      select_top_scores_from_fivegram(fivegram, lambda, input, ranked_dt)
    }
    else if (ng == 4) {
      select_top_scores_from_fourgram(fourgram, lambda, input, ranked_dt)
    }
    else if (ng == 3) {
      select_top_scores_from_trigram(trigram, lambda, input, ranked_dt)
    }
    else if (ng == 2) {
      select_top_scores_from_bigram(bigram, lambda, input, ranked_dt)
    }
    else {
      select_top_scores_from_unigram(unigram, lambda, ranked_dt)
    }
    
    lambda <- lambda * 0.4    
  }
  
  return(ranked_dt)
}

#q1_set <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
#            "You're the reason why I smile everyday. Can you follow me please? It would mean the",
#            "Hey sunshine, can you follow me and make me the",
#            "Very early observations on the Bills game: Offense still struggling but the",
#            "Go on a romantic date at the",
#            "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
#            "Ohhhhh #PointBreak is on tomorrow. Love that  lm and haven't seen it in quite some",
#            "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
#            "Be grateful for the good times and keep the faith during the",
#            "If this isn't the cutest thing you've ever seen, then you must be")

#q2_set <- c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
#            "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
#            "I'd give anything to see arctic monkeys this",
#            "Talking to your mom has the same effect as a hug and helps reduce your",
#            "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
#            "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
#            "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
#            "Every inch of you is perfect from the bottom to the",
#            "I’m thankful my childhood was filled with imagination and bruises from playing",
#            "I like how the same people are in almost all of Adam Sandler's")

#for (sentence in q2_set) {
#  print(sentence)
#  words <- transform_string(sentence)
#  print(words)
#  hash_words <- to_hash(words)
#  print(hash_words)
#  ranked_dt <- stupid_backoff_ranking(hash_words)
#  ranked_dt[, word := unigram_words[as.numeric(word_index)]]
#  print(ranked_dt)
#  print(ranked_dt)
#}