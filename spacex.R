# import required libraries for analysis
library("ggplot2")
library("plotly")
library("syuzhet")
library("tm")
library("twitteR")
library("wordcloud")

# store twitter api keys/tokens
consumer_key <- 'oLq7CtjpyRXoRpCA0yCBElUHl'
consumer_secret <- 'p9xL67GgsDe69vWtq24I76QTnkx5aVEqwmQeLON7X3pknAubLk'
access_token <- '3253219856-fe3DPLtc8QzW3k8L52iuzyA5dq9EeW1Ewe8FqJe'
access_secret <- 'Lwou90NHJ8b3SXno82sXzGMW3KiOrENCFgCdmWPmBjjCw'

# connect to Twitter's API using API tokens
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# search for #spacex & #starlink tweets
# we ran the entire script twice just tweaking this hashtag.
tweets <- searchTwitter("#spacex", n=5000, lang="en")

# strip out retweets
tweets <- strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE)

# confirm more than 800 records
length(tweets)

#######################################################################################################################################

# add tweet text to a vector
tweets.text <- sapply(tweets, function(x) x$getText())

# remove problematic text@Q
tweets.text <- sapply(tweets.text, function(row) iconv(row, "latin1", "ASCII", sub=""))

# place tweets in corpus
myCorpus <- Corpus(VectorSource(tweets.text))

# convert all text to lowercase
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove punctuation, numbers, and URLs
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))


# remove (common) stop words
myCorpus <- tm_map(myCorpus, function(x) removeWords(x, stopwords()))

# perform stemming
myCorpus <- tm_map(myCorpus, stemDocument)

#######################################################################################################################################

# generate term document matrix
tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))

# find the terms that are mentioned at least 25 times
freq.terms <- findFreqTerms(tdm, lowfreq = 50)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 50)


# convert into dataframe
df <- data.frame(term = names(term.freq), freq = term.freq)

# create plot, order based on frequency
ggplot(df, aes(x=reorder(term, freq), y=freq)) + 
  geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()

#######################################################################################################################################

# remove sparse words
new_tdm <- removeSparseTerms(tdm, sparse = .96)
mat <- as.matrix(new_tdm)

# cluster the terms
dist_mat <- dist(scale(mat))
fit <- hclust(dist_mat, method = "ward.D")

# plot the dendrogram and cut the tree into 6 clusters
plot(fit, main="#spacex Cluster Dendrogram", xlab="Clusters")
rect.hclust(fit, k=5)

#######################################################################################################################################

# store the tweets in dataframe
tweets.df <- dataframe <- data.frame(text=unlist(sapply(myCorpus, `[`)), stringsAsFactors=F)

# Emotions for each tweet using NRC dictionary
emotions <- get_nrc_sentiment(tweets.df$text)
emo_bar <- colSums(emotions)
emo_sum <- data.frame(count=emo_bar, emotion=names(emo_bar))

# at this point, we ran emo_sum to obtain counts. we then moved these 
# counts to excel to run the barcharts.
emo_sum

emo_sum$emotion <- factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Visualize the emotions from NRC sentiments
plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: #spacex")
