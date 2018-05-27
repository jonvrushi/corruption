##Tracking corruption discourse online - 27 May 2018

##Jon Vrushi - Transaprency International Research Team

## peparations -------------------

source("packages.r")

##

library(rtweet)
library(tm)

load("/Users/jonvrushi/rkeys.RDa")
consumer_key <- twitter_key
consumer_secret <- twitter_secret
create_token(app = "ImpactCPITweet", consumer_key, consumer_secret,
             set_renv = TRUE)

corruption_tweets <- search_tweets2(
  "Corruption OR corruption OR corrupción OR Corrupción OR korruptsiya OR Korruptsiya OR коррупция OR fasad OR فساد"
  , n = 600000, retryonratelimit = TRUE, include_rts = FALSE
)

##We prepare the data for content analysis

sample_27may <- corruption_tweets[sample(nrow(corruption_tweets), 10000), ]

#Create a function which will serve to clean unnecessary words from the tweets
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), stopwords("es")
                                          , "corruption", "corrupción", "فساد",
                                          "amp", "коррупция", "via", "corrupcion",
                                          "can", "new", "one", "get", "now",
                                          "corrupt", "you", "the", "see",
                                          "like", "this", "and", "que",
                                          "they", "will", "just", "dont"))
  return(corpus)
}

#Transform the data frame into term document matrices

#before CPI launch

streams_source_27may <- VectorSource(sample_27may$text)
streams_corpus_27may  <- VCorpus(streams_source_27may)

streams_corpus_27may <- clean_corpus(streams_corpus_27may)
streams_tdm_27may <- TermDocumentMatrix(streams_corpus_27may)
streas_m_27may <- as.matrix(streams_tdm_27may)

before_frequency <- rowSums(streas_m_27may)
before_frequency <- sort(before_frequency,
                         decreasing = TRUE) 

barplot(before_frequency[1:50],
        col = "#3695d8", las = 2, main = "Most frequent words in tweets about corruption (before CPI launch)")

