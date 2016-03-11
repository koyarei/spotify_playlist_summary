# library(caret)
# library(grid)
# library(caTools)
load('./data/playlist.update.RData')

# Explore kpi distribution
ggplot(playlist.update) + geom_density(aes(x = kpi)) + theme_bw()

# Try log transformation
ggplot(playlist.update) + geom_density(aes(x = log(kpi))) + theme_bw()

# Similar exploration and transformation are done on n_tracks, n_artists, and n_albums.
ggplot(playlist.update) + geom_density(aes(x = (n_tracks))) + theme_bw()
ggplot(playlist.update) + geom_density(aes(x = (n_artists))) + theme_bw()
ggplot(playlist.update) + geom_density(aes(x = (n_albums))) + theme_bw()

ggplot(playlist.update) + geom_density(aes(x = log(n_tracks))) + theme_bw()
ggplot(playlist.update) + geom_density(aes(x = log(n_artists))) + theme_bw()
ggplot(playlist.update) + geom_density(aes(x = log(n_albums))) + theme_bw()


# Examine genre_1 and mood_1
ggplot(playlist.update, aes(x = genre_1, y = kpi)) + 
    geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(playlist.update, aes(x = mood_1, y = kpi)) + 
    geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



