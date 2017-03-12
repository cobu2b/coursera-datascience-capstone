##########################################################################################
# File: ngram.R
#
# Description:
# Data are consisted of lower-case and upper-case letters, so changing them to 
# lower-case letters can help improve an analysis. Data cleaning also includes removing 
# twitter characters (@ and #), numbers, white spaces, and punctuations.
#
# After data are processed, unigram, bigram, trigram, fourgram, and fivegram models
# are generated and saved into RDS files. 
##########################################################################################

# Load required libraries 
options(java.parameters = "-Xmx12g")
options(mc.cores=1)
library(RWeka)
library(slam)
library(tm)

# Load Intermediate data
path <- paste(getwd(), "/samples", sep = "")
vcorpus <- Corpus(DirSource(path, pattern = "*.sample"),
                  readerControl = list(reader = readPlain,
                                      language = "en",
                                      load = TRUE))

# Create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

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

# Generate n-gram models
ngramGenerator <- function(corpus, ng) {
  ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ng, max = ng))
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngramTokenizer,
                                                   wordLengths = c(1, Inf)))
}

unigram_tdm <- ngramGenerator(vcorpus, 1)
saveRDS(unigram_tdm, "unigram_tdm.rds", ascii = TRUE)

bigram_tdm <- ngramGenerator(vcorpus, 2)
saveRDS(bigram_tdm, "bigram_tdm.rds", ascii = TRUE)

trigram_tdm <- ngramGenerator(vcorpus, 3)
saveRDS(trigram_tdm, "trigram_tdm.rds", ascii = TRUE)

fourgram_tdm <- ngramGenerator(vcorpus, 4)
saveRDS(fourgram_tdm, "fourgram_tdm.rds", ascii = TRUE)

fivegram_tdm <- ngramGenerator(vcorpus, 5)
saveRDS(fivegram_tdm, "fivegram_tdm.rds", ascii = TRUE)

