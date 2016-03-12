library(caret)
library(grid)
library(caTools)
library(dplyr)
library(splines)
# install.packages('rms')
library(rms)
setwd('/Users/Koya/projects/spotify_playlist_summary')
load('./data/playlist.update.RData')

# Extract more features and do exploration on them.
################################# Functions #############################
G <- function(df) {
    # Sample 1/10 of the observations to graph
    # for cleaner graph and faster computation
    sample.rows <- sample(1:nrow(df), 40000)
    sample <- df[sample.rows,]
    return(sample)
}

ChooseNSDegreeOfFreedom <- function(df, x, nums, seed=123) {
    # Use cross-validation to choose the best degree of freedom
    result <- data.frame()
    set.seed(seed)
    fitControl <- trainControl(method = "cv", 
                               number = 10)
    for (i in nums) {
        formula <- as.formula(paste0('log(kpi) ~ bs(', x, ',', i, ')'))
        fit <- train(formula, 
                     data = df, 
                     method = 'lm', 
                     metric = 'RMSE',
                     trControl = fitControl)
        baseline.result <- cbind(i, fit$results)
        result <- rbind(result, baseline.result)
        print(result)
    }
    
    return(result)
}

############################# Main ########################################

# Add if playlist is created by Spotify.
playlist.update$owner_spotify <- ifelse(playlist.update$owner == "spotify",
                                        1, 0)

ggplot(G(playlist.update), aes(x = owner_spotify, y = log(kpi))) + 
    geom_point() + geom_smooth(method = 'lm')

# Add if previous monthly active users were 0, meaning new playlist.
playlist.update$is_new <- ifelse(playlist.update$mau_previous_month == 0, 1, 0)
ggplot(G(playlist.update), aes(x = is_new, y = log(kpi))) + 
    geom_point() + geom_smooth(method = 'lm')

# Add how many playlists a given owner has created.
list.production <- summary(playlist.update$owner, maxsum = length(playlist.update$owner))
list.production.df <-  data.frame(owner = names(list.production),
                                  list_created = list.production)
playlist.update.4 <- merge(playlist.update, list.production.df, by = "owner")
playlist.update.4 <- tbl_df(playlist.update.4)
ggplot(G(playlist.update.4), aes(x = list_created, y = log(kpi))) + 
    geom_point() + geom_smooth(method = 'lm')

# Try subset
spotify <- subset(playlist.update.4, owner == 'spotify')
users <- subset(playlist.update.4, owner != 'spotify')
ggplot(G(users), aes(x = list_created, y = log(kpi))) + 
    geom_point() + geom_smooth(method = 'lm')

# Closer look at n_tracks
ggplot(G(users), aes(x = log(n_tracks), y = log(kpi))) + 
    geom_point() + geom_smooth(method = 'lm')
ggplot(spotify, aes(x = log(n_tracks), y = log(kpi))) + 
    geom_point() + geom_smooth(method = 'lm')

# Look at n_albums
ggplot(spotify, aes(x = log(n_albums), y = log(kpi))) + 
    geom_point() + geom_smooth()
ggplot(G(users), aes(x = log(n_albums), y = log(kpi))) + 
    geom_point() + geom_smooth(formula = y ~ splines::ns(x, 2)) +
    scale_x_continuous(breaks = round(seq(0, 8, by = 0.2),1))
# Decide the best ns() degree of freedom using cv
# Users:
albums.bs.result <- ChooseNSDegreeOfFreedom(users, 'log(n_albums)',
                                            c(2:15)) # Choose 6 degrees of freedom

artists.bs.result <- ChooseNSDegreeOfFreedom(users, 'log(n_artists)',
                                            c(2:15)) # Choose 7 degrees of freedom
tracks.bs.result <- ChooseNSDegreeOfFreedom(users, 'log(n_tracks)',
                                             c(2:15)) # Choose 11 degrees of freedom

# Density for n_albums and n_artists
ggplot(G(users), aes(x = log(n_albums))) + geom_density() +
    scale_x_continuous(breaks = round(seq(0, 8, by = 0.2),1))
ggplot(G(users), aes(x = log(n_artists))) + geom_density() +
    scale_x_continuous(breaks = round(seq(0, 8, by = 0.2),1))

########################## Save Progress ####################
save(users, file = './data/users.RData')
save(spotify, file = './data/spotify.RData')



