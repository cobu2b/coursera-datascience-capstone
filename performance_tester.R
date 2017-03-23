##########################################################################################
# File: performance_tester.R
#
# Description:
#
##########################################################################################

# Load required libraries 
options(java.parameters = "-Xmx12g")
options(mc.cores=1)
library(RWeka)
library(slam)
library(tm)

# Import predict model
source("predict.R")

# Load test set
path <- paste(getwd(), "/samples/en_US.blogs.txt.testset", sep = "")
blogs <- readLines(path, encoding = "UTF-8")

path <- paste(getwd(), "/samples/en_US.news.txt.testset", sep = "")
news <- readLines(path, encoding = "UTF-8")

path <- paste(getwd(), "/samples/en_US.twitter.txt.testset", sep = "")
twitter <- readLines(path, encoding = "UTF-8")

stupid_backoff_accuracy <- function(testset) {
  # Initial performance data
  failure_data_cleaning <- NULL
  failure_hashing_sentence <- NULL
  failure_mismatch_sentence <- NULL
  failure_mismatch_backoff_words <- NULL  
  count_first_match <- 0
  count_other_match <- 0
  count_error <- 0
  total_sentences <- length(testset)
  
  for(sentence in testset) {
    # Data cleaning
    words <- transform_string(sentence)
    
    if (length(words) == 0 || sum(words == "") > 0) {
      failure_data_cleaning <- c(failure_data_cleaning, sentence)
      
      count_error <- count_error + 1
      next
    }
    
    # Put words in unigram hashing table and split their last word
    hash_words <- to_hash(words)
    
    if (length(hash_words) < 2) {
      failure_hashing_sentence <- c(failure_hashing_sentence, sentence)
      
      count_error <- count_error + 1
      next    
    }
    
    # Split test sentence and its result
    test_hash_words <- head(hash_words, -1)
    test_hash_result <- tail(hash_words, 1)
    
    # Perform stupid backoff algorithm
    ranked_dt <- stupid_backoff_ranking(test_hash_words)
    
    # Match result
    if (test_hash_result == ranked_dt$word_index[1]) {
      count_first_match <- count_first_match + 1
    }
    else if (test_hash_result %in% ranked_dt$word_index[2:RANK_SIZE]) {
      count_other_match <- count_other_match + 1
    }
    else {
      ranked_dt[, word := unigram_words[as.numeric(word_index)]]
      
      failure_mismatch_sentence <- c(failure_mismatch_sentence, sentence)
      failure_mismatch_backoff_words <- c(failure_mismatch_backoff_words, paste(ranked_dt$word, collapse=', ' ))
      
      count_error <- count_error + 1
    }
  }
  
  # Write failures to files
  if (!is.null(failure_data_cleaning)) {
    connector <- file("failure_data_cleaning.txt", open = "a", encoding = "UTF-8")
    writeLines(failure_data_cleaning, connector)
    close(connector) 
  }
  
  if (!is.null(failure_hashing_sentence)) {
    connector <- file("failure_hashing_sentence.txt", open = "a", encoding = "UTF-8")
    writeLines(failure_hashing_sentence, connector)
    close(connector) 
  }
  
  if (!is.null(failure_mismatch_sentence)) {
    connector <- file("failure_mismatch_sentence.txt", open = "a", encoding = "UTF-8")
    writeLines(failure_mismatch_sentence, connector)
    close(connector) 
  }
  
  if (!is.null(failure_mismatch_backoff_words)) {
    connector <- file("failure_mismatch_backoff_words.txt", open = "a", encoding = "UTF-8")
    writeLines(failure_mismatch_backoff_words, connector)
    close(connector) 
  }
  
  return(c(deparse(substitute(testset)), count_first_match, count_other_match, count_error, total_sentences)) 
}

blogs_accuracy_result <- stupid_backoff_accuracy(blogs)
news_accuracy_result <- stupid_backoff_accuracy(news)
twitter_accuracy_result <- stupid_backoff_accuracy(twitter)

accuracy_result <- as.data.frame(rbind(blogs_accuracy_result,
                                       news_accuracy_result,
                                       twitter_accuracy_result),
                                 stringsAsFactors = FALSE)
names(accuracy_result) <- c("testset", "best_match", "other_match", "error", "total")
accuracy_result$best_match <- as.integer(accuracy_result$best_match)
accuracy_result$other_match <- as.integer(accuracy_result$other_match)
accuracy_result$error <- as.integer(accuracy_result$error)
accuracy_result$total <- as.integer(accuracy_result$total)