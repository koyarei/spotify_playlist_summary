library(caret)
library(grid)
library(caTools)
library(dplyr)
setwd('/Users/Koya/projects/spotify_playlist_summary')
load('./data/playlist.update.RData')

# Extract more features and do exploration on them.

# Add if playlist is created by Spotify.
playlist.update$owner_spotify <- ifelse(playlist.update$owner == "spotify",
                                        1, 0)


# Add if previous monthly active users were 0, meaning new playlist.
playlist.update$is_new <- ifelse(playlist.update$mau_previous_month == 0, 1, 0)

# Add how many playlists a given owner has created; if owner is spotify, set to 1.
list.production <- summary(playlist.update$owner, maxsum = length(playlist.update$owner))
list.production.df <-  data.frame(owner = names(list.production),
                                  user_list_created = list.production)
playlist.update.4 <- merge(playlist.update, list.production.df, by = "owner")
playlist.update.4 <- tbl_df(playlist.update.4)
# If owner is spotify, set user_list_created to 0.
playlist.update.4[playlist.update.4$owner_spotify == 1, 'user_list_created'] <- 1


# sample 5000 observations for graphing
sample.rows <- sample(1:nrow(playlist.update), 40000)
sample <- playlist.update.4[sample.rows,]
ggplot(sample, aes(x = owner_spotify, y = log(kpi))) + geom_point() + geom_smooth(method = 'lm')
ggplot(sample, aes(x = is_new, y = log(kpi))) + geom_point() + geom_smooth(method = 'lm')
ggplot(sample, aes(x = log(n_tracks), y = log(kpi))) + geom_point() + geom_smooth()
ggplot(sample, aes(x = log(n_albums), y = log(kpi))) + geom_point() + geom_smooth()
ggplot(sample, aes(x = user_list_created, y = log(kpi))) + geom_point() + geom_smooth(method = 'lm')

# Try subset
spotify <- subset(playlist.update.4, owner == 'spotify')
users <- subset(playlist.update.4, owner != 'spotify')
