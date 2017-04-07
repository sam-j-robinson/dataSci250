#############################################
# DS250 - Introduction to Data Science
# Credit to:
# Graham.Williams@togaware.com
#############################################

# Install packages as required
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("wordcloud")

# Load libraries
library(tm)            # Framework for text mining.
library(SnowballC)
library(qdap)          # Quantitative discourse analysis of transcripts.
library(qdapDictionaries)
library(dplyr)         # Data wrangling, pipe operator %>%().
library(RColorBrewer)  # Generate palette of colours for plots.
library(ggplot2)       # Plot word frequencies.
library(scales)        # Include commas in numbers.


# Load sample text data
setwd("~/Desktop/schoolwork/dataSci250/week6/big.txt")
docs <- Corpus(DirSource(getwd()))

# Examine documents
summary(docs)
inspect(docs[1])

# Prepare text data
getTransformations()
stopwords("english")

# Run transformations
docs <- tm_map(docs, content_transformer(removeNumbers))
docs <- tm_map(docs, content_transformer(removePunctuation))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stemDocument, language = "english")
docs <- tm_map(docs, stripWhitespace) # Run this last...

# Review transformations
summary(docs)
inspect(docs[1])

# Create Document Term Matrix
doctm <-DocumentTermMatrix(docs) 
doctm <- removeSparseTerms(doctm, 0.75)

# Review Document Term Matrix
inspect(doctm[1, 1000:1005])
dim(doctm)

# Term Frequency Analysis
freq <- colSums(as.matrix(doctm))
head(table(freq), 15) #  9989 terms that occur just once.

# Removing Sparse Terms
dim(doctm)
## [1]     1 23199
doctm <- removeSparseTerms(doctm, 0.5)
dim(doctm)
inspect(doctm)

# Review Term Frequenct
freq <- sort(colSums(as.matrix(doctm)), decreasing=TRUE)
head(freq, 14)

# Plot Term Frequency
library(ggplot2)
wf <- data.frame(word=names(freq), freq=freq)
subset(wf, freq>1500) %>% ggplot(aes(word, freq)) + geom_bar(stat="identity")                                            +
theme(axis.text.x=element_text(angle=45, hjust=1))

# Display word cloud
library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=40)

# Wordcloud options
set.seed(142)
wordcloud(names(freq), freq, max.words=100)
wordcloud(names(freq), freq, min.freq=100)
wordcloud(names(freq), freq, min.freq=1000, colors=brewer.pal(6, "Dark2"))
wordcloud(names(freq), freq, min.freq=500, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
