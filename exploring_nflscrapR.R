library(nflscrapR)
library(tidyverse)

pbp_2009 <- season_play_by_play(2009)
pbp_2010 <- season_play_by_play(2010)
pbp_2011 <- season_play_by_play(2011)
pbp_2012 <- season_play_by_play(2012)
pbp_2013 <- season_play_by_play(2013)
pbp_2014 <- season_play_by_play(2014)
pbp_2015 <- season_play_by_play(2015)
pbp_2016 <- season_play_by_play(2016)
pbp_2017 <- season_play_by_play(2017)

full_pbp <- rbind(pbp_2009, pbp_2010, pbp_2011, pbp_2012, 
                  pbp_2013, pbp_2014, pbp_2015, pbp_2016, pbp_2017)

full_pbp <- read.csv('full_pbp.csv', stringsAsFactors = FALSE)
season_wins <- read.csv('season_wins.csv', stringsAsFactors = FALSE)


sort(unique(full_pbp$posteam))

fix_old_team_name <- function(x){
  gsub("JAC", "JAX", x)
}

data.frame(lapply(full_pbp$posteam, fix_old_team_name))

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

####### Big plays
full_pbp$big_plays <- ifelse(full_pbp$Yards.Gained > 30, 1, 0)
full_pbp$year <- substring(full_pbp$Date, 1, 4)

big_plays_df <- full_pbp %>%
  filter(posteam != "") %>%
  group_by(posteam, year) %>%
  summarise(big_plays = sum(big_plays))

big_play_df_merge <- merge(big_plays_df, season_wins, by.x = c("posteam", "year"), by.y=c("Tm","yr"))

with(big_play_df_merge, cor(big_plays, W))

plot(with(big_play_df_merge, big_plays, W))


with(subset(big_play_df_merge, next_season_wins != 'NA'), cor(big_plays, next_season_wins))

library(corrplot)
M <- cor(big_play_df_merge[,c('big_plays','W','SoS','SRS','OSRS','DSRS','PRE_OU')])
corrplot(M)

####### Interceptions
interceptions_by_year <- full_pbp %>%
  filter(posteam != "") %>%
  group_by(year) %>%
  summarise(interceptions = sum(InterceptionThrown))

interceptions_by_year
