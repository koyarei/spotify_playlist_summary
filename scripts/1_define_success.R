# install.packages("dplyr")
library(dplyr)
library(caret)

playlist <- read.delim('./data/playlist_summary_external.txt',
                       header = T)
playlist <- tbl_df(playlist)

# To answer "What makes a playlist successful", we first have to 
# define "success".
# Because the dataset has no datetime information, looking at data related
# to "today" is not representative.
# Also, volume of active users, growth, >30s streams are all important.
# The final definition of success KPI is defined as:
# "# of streams over 30s by followers last month, this month and next (projected)".


# Current month streams over 30s per follower:
# (# of streams over 30s this month - # of streams over 30s by playlist owner this month) /
# (# of Monthly Active Users - 1)


playlist.update <- playlist %>%
    mutate(monthly_stream30s_per_follower = (monthly_stream30s - monthly_owner_stream30s) /
                                            (mau - 1),
           next_m_au = mau,
           kpi = (mau_previous_month + mau + next_m_au) * monthly_stream30s_per_follower) %>%
    select(-owner_country)

explore <- select(playlist.update, mau, mau_previous_month, mau_both_months, monthly_stream30s, 
                  monthly_owner_stream30s, monthly_stream30s_per_follower, next_m_au, kpi)
prev.zero <- filter(playlist.update, mau_previous_month == 0)

ten.k.songs <- filter(playlist.update, n_tracks <= 10000)

hist(log(playlist.update$kpi), breaks = 150)

one.artist <- filter(playlist.update, n_artists == 1)

spotify <- filter(playlist.update, owner == "spotify")
non.spotify <- filter(playlist.update, owner != 'spotify')

ggplot(non.spotify, aes(x = genre_1, y = kpi)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(non.spotify, aes(x = mood_1, y = kpi)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 45, hjust = 1))