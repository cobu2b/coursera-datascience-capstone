##########################################################################################
# File: BOS.R
#
# Description:
# Extract the beginning of sentences (BOS) using cut -d' ' -f1 <sample name> from 
# sample files. The BOS will be performed data cleaning, calculated counts and their 
# stupid backoff score.
##########################################################################################

# Load required libraries 
options(java.parameters = "-Xmx12g")
options(mc.cores=1)
library(RWeka)
library(slam)
library(tm)
library(openNLP)
library(data.table)

# Load Intermediate data
path <- paste(getwd(), "/samples", sep = "")
vcorpus <- Corpus(DirSource(path, pattern = "*.sample.firstword"),
                  readerControl = list(reader = readPlain,
                                       language = "en",
                                       load = TRUE))

# Create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

# Remove twitter characters
vcorpus <- tm_map(vcorpus, toSpace, "@")
vcorpus <- tm_map(vcorpus, toSpace, "#")

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
                                                   tolower = FALSE,
                                                   wordLengths = c(1, Inf)))
}

unigram_tdm <- ngramGenerator(vcorpus, 1)
saveRDS(unigram_tdm, "bos_tdm.rds", ascii = TRUE)

# Calculate Frequency words
getFreqs <- function(tdm) {
  as.data.table(sort(row_sums(tdm), decreasing = T), keep.rownames=TRUE)
}
bos <- getFreqs(unigram_tdm)

# N-gram pruning: Remove singlestons
colnames(bos) <- c("word", "count")
bos <- bos[count > 1]

# Calculate backoff score
unigram <- readRDS("./rds/unigram.rds")
all_words <- colSums(unigram)
bos$score <- bos$count/all_words
saveRDS(bos, "bos_backoff.rds")