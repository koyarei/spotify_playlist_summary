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

save(playlist.update, file = './data/playlist.update.RData')
