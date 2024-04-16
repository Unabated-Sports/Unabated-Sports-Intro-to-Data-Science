##########################################################
### UNABATED INTRO TO DATA SCIENCE LESSONS - ARTICLE 1 ###
##########################################################

####################################
# Get Baseball Data With baseballR #
####################################

# load library
library(baseballr)

# Get pitch-by-pitch data from statcast 
# you can use the player search functions on baseballr to find players
shohei <- statcast_search(
    start_date = "2022-04-06", end_date = "2022-04-15",
    playerid = 660271, player_type = 'batter'
)

# write the data to CSV (for upload to excel or whatever)
write.csv(shohei, "Shohei_statcast_data.csv")



# Get game-by-game logs from fangraphs 
# this is the data we'll be using for the series
shohei <- fg_batter_game_logs(playerid = 19755, year = 2022)

# write data
write.csv(shohei, "Shohei_Fangraphs_game_logs.csv")





##########################
# NFL Data with nflfastR #
##########################

# load library
library(nflfastR)

# get play by play data for 2019 through last year
nfl <- load_pbp(seasons = 2019:2022)

# write data
write.csv(nfl, "NFL_play_by_play_2019_2022.csv")




##########################
# CFB Data with cfbfastR #
##########################

# load library
library(cfbfastR)

# get play by play data for 2022
cfb <- cfbd_plays(year = 2022)

# write data
write.csv(cfb, "CFB_play_by_play_2022.csv")




#########################
# WNBA Data with wehoop #
#########################

# load library
library(wehoop)

# get WNBA play by play data
wnba <- load_wnba_pbp(seasons = 2022)

# write data
write.csv(wnba, "WNBA_some_2022_play_by_play.csv")


