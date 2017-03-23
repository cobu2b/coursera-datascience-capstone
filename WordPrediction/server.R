# Load required libraries
library(shiny)
library(ggplot2)
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
bos <- readRDS("./rds/bos_backoff.rds")

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

stupid_backoff_ranking <- function(words, max_ngram) {
  lambda <- 1
  ngram <- max_ngram
  ranked_dt <- data.table(word_index = rep("", RANK_SIZE), 
                          score = rep(double(1), RANK_SIZE))
  
  # Select n-gram models
  if (length(words) < HIGHEST_NGRAM) {
    ngram <- length(words) + 1
  }
  
  for (ng in ngram:1) {
    input <- tail(words, ng - 1)
    
    if (ng == 4) {
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

#default_ranked_dt <- data.table(word = unigram_words[1:RANK_SIZE],
#                                score = unigram[1:RANK_SIZE]$backoff_score)
default_ranked_dt <- bos[1:RANK_SIZE]
default_ranked_dt$word <- factor(default_ranked_dt$word,
                                 levels = unique(default_ranked_dt$word))

shinyServer(function(input, output, session) {
  topwords <- reactive({
    ranked_dt <- default_ranked_dt
    
    if (input$textinput != "") {
      # Processed input text should have a least one word
      words <- transform_string(input$textinput)
      
      if (length(words) > 0 && sum(words == "") == 0) {
        hash_words <- to_hash(words)
        ranked_dt <- stupid_backoff_ranking(hash_words, HIGHEST_NGRAM)
        ranked_dt[, word := unigram_words[as.numeric(word_index)]]
        ranked_dt$word <- factor(ranked_dt$word, levels = unique(ranked_dt$word))
      }
    }
    
    return(ranked_dt)
  })
  
  output$plotTopwords <- renderPlot({
    g <- ggplot(topwords(), aes(x = word, y = score))
    g <- g + geom_bar(stat="identity")
    g <- g + labs(x = "Top words", y = "Score")
    print(g)
  })
  
  observeEvent(input$selectWord, {
    updateTextInput(session, "textinput", 
                    value = paste(input$textinput, input$selectWord))
  })
  
  observe({
    # Update the select input choices
    ranked_dt <- topwords()
    req(ranked_dt)
    
    updateRadioButtons(session, "selectWord",
                       choices = ranked_dt$word,
                       selected = character(0))
  })
})
