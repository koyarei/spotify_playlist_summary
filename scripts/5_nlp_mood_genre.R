library(caret)
library(grid)
library(caTools)
library(dplyr)
library(splines)
# install.packages('rms')
library(rms)
library(tm)
setwd('/Users/Koya/projects/spotify_playlist_summary')
load('./data/users.RData')
load('./data/spotify.RData')

playlist.update.5 <- rbind(users, spotify)
playlist.update.5 <- tbl_df(playlist.update.5)

################################ Functions ############################


################################ Main #################################
# Feature extraction from tokens
playlist.update.5$tokens <- gsub("\\[|\\]|,", "", playlist.update.5$tokens)

# Count word length
playlist.update.5$title_len <- sapply(gregexpr("[[:alpha:]]+", 
                                               playlist.update.5$tokens), 
                                      function(x) sum(x > 0))
users <- subset(playlist.update.5, owner != 'spotify')
# Explore
ggplot(G(users), aes(x = title_len, y = log(kpi))) + 
    geom_point() + geom_smooth() # use cubic
ggplot(G(users), aes(x = title_len)) + 
    geom_density()

# Count total title length
playlist.update.5$nchar <- nchar(playlist.update.5$tokens)
# Explore
ggplot(G(users), aes(x = nchar, y = log(kpi))) + 
    geom_point() + geom_smooth() # use quadratic
ggplot(G(users), aes(x = title_len)) + 
    geom_density()

# NLP 
corpus <- Corpus(VectorSource(playlist.update.5$tokens))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('en'))
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)
# 60398 unique words after stemming and removing stopwords
# If a word appeared in more than 0.01% (403) playlists, keep it
sparse <- removeSparseTerms(dtm, 0.999)
words.tokens <- as.data.frame(as.matrix(sparse))
colnames(words.tokens) <- make.names(colnames(words.tokens))
# Remove top 
words.tokens <- tbl_df(words.tokens)
words.tokens$playlist_uri <- playlist.update.5$playlist_uri
# Merge with the original
playlist.6 <- merge(playlist.update.5, words.tokens)
playlist.6 <- tbl_df(playlist.6)
save(playlist.6, file = './data/playlist.6.RData')




