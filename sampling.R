##########################################################################################
# File: sampling.R
#
# Description:
# The samplingData function opens a given file, summarizes the orignal data, 
# and generates sampling data set using rbinom for selecting representive data. 
# Note that while reading the data, nuls will not be included to clean the data 
# and non-ASCII characters will not be included in the sample data.
#
# Data: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
##########################################################################################

set.seed(952)

wordCount <- function(str) {
  sapply(gregexpr("\\b\\W+\\b", str, perl=TRUE), function(x) sum(x>0) ) + 1 
}

samplingData <- function(name, path) {
  labels <- c("Name", "Path", "Sample path", "Number of lines", "Longest line", "Word count")
  summarized_data <- c(name, path)

  # Read entire give file
  connector <- file(path, "r")
  strings <- readLines(connector, skipNul = TRUE)
  close(connector)
  
  # Randomly select and save rows to get an accurate approximation of data set
  path <- paste(path, ".sample", sep = "")
  connector <- file(path, "w")
  for (index in seq(1:length(strings))) {
    if (sum(rbinom(10, 1, 0.5)) > 5) {
      # Remove the non-ASCII characters 
      writeLines(iconv(strings[index], "UTF-8", "ASCII", ""), connector)
    }
  }
  close(connector)
  
  # Generate summarized data
  strings_lens <- lapply(strings, nchar)
  strings_wcs <- lapply(strings, wordCount) 
  summarized_data <- c(summarized_data, path, length(strings),
                       nchar(strings[which.max(strings_lens)]),
                       Reduce("+", strings_wcs))  
  
  return (data.frame(label = labels,
                     value = summarized_data))
}

blogs_path <- paste(getwd(), "/en_US/en_US.blogs.txt", sep = "")
blogs <- samplingData("blogs", blogs_path)

news_path <- paste(getwd(), "/en_US/en_US.news.txt", sep = "")
news <- samplingData("news", news_path)

twitter_path <- paste(getwd(), "/en_US/en_US.twitter.txt", sep = "")
twitter <- samplingData("twitter", twitter_path)
