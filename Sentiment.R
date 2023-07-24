# Read The file
apple <- read.csv(file.choose(),header= T) 
str(apple)

# Build Corpus
library(tm)
corpus <- iconv(apple$text, to = "utf-8-mac")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Cleaning

# Lowercase
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

# No Punctuation
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

# No Numbers
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

# No common English Words
corpus <- tm_map(corpus, removeWords, stopwords('english'))
inspect(corpus[1:5])

# No URLs

removeUrl <- function(x) gsub('http[[:alnum:]]*', '', x)
corpus <- tm_map(corpus, content_transformer(removeUrl))
inspect(corpus[1:5])

# No Whitespace
# No common Word

cleanset <- corpus
cleanset <- tm_map(cleanset, removeWords, c('aapl','apple'))
cleanset <- tm_map(cleanset, gsub, pattern='stocks', replacement='stock')
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

#Term document matrix

tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot

w <- rowSums(tdm)
w
w <- subset(w,w>=25)

barplot(w,
        las=2,
        col=rainbow(50),
        )

#Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = FALSE,
          min.freq = 5,
          colors= brewer.pal(8,'Dark2'))

library(wordcloud2)

w <- data.frame(names(w), w)

colnames(w) <- c('word', 'freq')

wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

# Sentiment Analysis

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to='utf-8-mac')

s <- get_nrc_sentiment(tweets)
head(s)

tweets[4]
get_nrc_sentiment('ugly')

barplot(colSums(s),
        las=2,
        col=rainbow(10),
        ylab ='Count',
        main = 'Sentiment Scores for Apple Tweets After Earings')






