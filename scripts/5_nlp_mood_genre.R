library(caret)
library(grid)
library(caTools)
library(dplyr)
library(splines)
# install.packages('rms')
library(rms)
setwd('/Users/Koya/projects/spotify_playlist_summary')
load('./data/users.RData')
load('./data/spotify.RData')

playlist.update.5 <- rbind(users, spotify)
playlist.update.5 <- tbl_df(playlist.update.5)

################################ Functions ############################


################################ Main #################################
# NLP on tokens
playlist.update.5$tokens <- gsub("\\[|\\]|,", "", playlist.update.5$tokens)
write.csv(playlist.update.5, file = './data/playlist.update.5.csv',
          row.names = F)

# Count word length
playlist.update.5$title_len <- sapply(gregexpr("[[:alpha:]]+", 
                                               playlist.update.5$tokens), 
                                      function(x) sum(x > 0))


max(nchar(emails$text))

which.min(nchar(emails$text))

corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('en'))
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)

sparse <- removeSparseTerms(dtm, 0.95)
words.email <- as.data.frame(as.matrix(sparse))
colnames(words.email) <- make.names(colnames(words.email))
which.max(colSums(words.email))

words.email$spam <- emails$spam


ham <- subset(words.email, spam == 0)
names(ham[, colSums(ham) >= 5000])

spam <- subset(words.email, spam == 1)
names(spam[, colSums(spam) >= 1000])

