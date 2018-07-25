library(nflscrapR)
library(tidyverse)

pbp_2009 <- season_play_by_play(2009)
pbp_2010 <- season_play_by_play(2010)
pbp_2011 <- season_play_by_play(2011)
pbp_2012 <- season_play_by_play(2012)
pbp_2013 <- season_play_by_play(2013)
pbp_2016 <- season_play_by_play(2016)

head(pbp_2009)

pbp_2009 %>%
  filter(posteam != "") %>%
  group_by(posteam, PassAttempt) %>%
  summarise(avg_EPA = mean(EPA, na.rm = TRUE)) %>%
  arrange(desc(avg_EPA))


pbp_2016 %>%
  filter(posteam != "") %>%
  group_by(posteam, PassAttempt) %>%
  summarise(avg_EPA = mean(EPA, na.rm = TRUE)) %>%
  arrange(avg_EPA)


pbp_2016 %>%
  filter(posteam != "") %>%
  group_by(posteam, PassAttempt) %>%
  summarise(avg_EPA = mean(EPA, na.rm = TRUE)) %>%
  arrange(avg_EPA)

pbp_2016 %>%
  filter(down == 1 & ydstogo == 10 & AbsScoreDiff < 4) %>%
  group_by(posteam) %>%
  summarise(plays = n(), pass_plays = sum(PassAttempt), perc_of_passes = sum(PassAttempt) / n()) %>%
  arrange(desc(perc_of_passes))

sort(unique(pbp_2009$HomeTeam))

WL_data = read_csv('season_wins.csv')

head(WL_data)
