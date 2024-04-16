##########################################################
### UNABATED INTRO TO DATA SCIENCE LESSONS - ARTICLE 2 ###
##########################################################




###########################################################
### ARTICLE 2 - PROCESS / Quick EDA / CREATE FEATURE(?) ###
###########################################################

# required packages
library(baseballr)
library(data.table)
library(tictoc)
library(zoo)

######################################################################
#  The following code will pull from Fangraphs API                   #
#  you can skip this step if you just want to load the provided csv  #
######################################################################

# get all players last 5 years first to get player ids
all_bs <- data.table()
for(YR in 2019:2022) {
    tmp <- data.table(fg_batter_leaders(
        startseason = YR, endseason = YR, qual="n", ind=1)
    )
    
    all_bs <- rbind(all_bs, tmp, fill=T)
}


# get all game logs (box score info) for each player, each season:

tic()  # start timer
blogs <- data.table()  # empty data frame that we'll append every player info to

# nested for loop to grab all game logs for every player in every season
for(yr in unique(all_bs$Season)) {
    
    for(batter in all_bs[Season==yr]$playerid) {
        
        fgdat <- fg_batter_game_logs(year=yr, playerid = batter)  # get game logs
        blogs <- rbind(blogs, fgdat, fill=T)  # attach to our big data frame
    }
}
toc()  # end timer


######################
###   PROCESSING   ###
######################

# if you skipped step above, start here and load csv
# blogs <- readRDS("batter_logs_for_2019_2022.rds")
# write.csv(blogs, "batter_logs_for_2019_2022.csv", row.names = F)
blogs <- data.table(read.csv("batter_logs_for_2019_2022.csv", check.names = FALSE))

# turn csv text into a Date then sort by date (just for ease of viewing)
blogs$Date <- as.Date(blogs$Date)
blogs <- blogs[order(Date)]

# add home and away columns
blogs[, "away" := ifelse(grepl("@", Opp, T), Team, Opp)]
blogs[, "home" := ifelse(grepl("@", Opp, T), Opp, Team)]

# remove the @ for matching / joining later
blogs$home <- gsub("@", "", blogs$home)
blogs$Opp <- gsub("@", "", blogs$Opp)

# create game ID - we use G in case there was a doubleheader
blogs[, "game_id" := paste(Date, away, home, G, sep="-")]



# We'll train the model on just 2021 data, so let's filter our data
blogs <- blogs[season==2021]


# Out modeling data frame will be one row per game, so we can model total runs in a game
# so, we'll create data frame by game - with home team, away team, date, etc, and model vars
# two ways to do this - do it in big table then separate out and remove dupes or 
# separate out, remove dupes, then append stats
# we do this b/c we're looking at game totals, so we build data 1 row per game

gms <- blogs[, .(Date, home, away, G, game_id)]  # these are the columns that define our key
gms <- gms[!duplicated(gms)]  # remove duplicates to arrive at our modeling frame


# lets say you wanted to use current season stats, then to build a model, you need to 
# calculate features you're interested in as they would exist in your in-use model...
# so, you'll need to know the feature value for all games prior to every game in your train/test set


# code to calculate avg runs scored going into each game and add to table
Y <- blogs[, .(runs_scored = sum(R)), by=.(Date, Team)]  # calc runs scored in the game by summing across players
Y <- Y[order(Date)]  # order DF by date
Y <- Y[, game_num := 1:.N, by = .(Team)]  # calculate cumulative sum from game 1 to 162

Y <- Y[, run_pg := cumsum(runs_scored) / (game_num - 1), by=.(Team)]  # calculate runs per game going into each game
Y <- Y[, game_num := NULL]  # un-needed column

gms <- merge(x=gms, y=Y, by.x=c("Date", "home"), by.y=c("Date", "Team"), all.x=TRUE)  # merge new home team info to our modeling data
names(gms)[which(names(gms)=="runs_scored")] <- "home_score"  # rename for home team
names(gms)[which(names(gms)=="run_pg")] <- "home_rpg_before"  # rename for home team

gms <- merge(x=gms, y=Y, by.x=c("Date", "away"), by.y=c("Date", "Team"), all.x=TRUE)  # merge new away team info to our modeling data
names(gms)[which(names(gms)=="runs_scored")] <- "away_score"  # rename for away team
names(gms)[which(names(gms)=="run_pg")] <- "away_rpg_before"  # rename for away team


# code to calculate avg runs allowed going into each game and add to table
Y <- blogs[, .(runs_allowed = sum(R)), by=.(Date, Opp)]  # get runs scored in the game by summing OPP values across players
names(Y)[2] <- "Team"  # rename column (or we could have switched all the var references below)
Y <- Y[order(Date)]  # order DF by date
Y <- Y[, game_num := 1:.N, by = .(Team)]  # calculate cumulative sum from game 1 to 162

Y <- Y[, run_a_pg := cumsum(runs_allowed) / (game_num - 1), by=.(Team)]  # calculate runs allowed per game going into each game
Y <- Y[, game_num := NULL]  # un-needed column
Y <- Y[, runs_allowed := NULL]  # this is already in our data we're building

gms <- merge(x=gms, y=Y, by.x=c("Date", "home"), by.y=c("Date", "Team"), all.x=TRUE)  # merge new home team info to our modeling data
names(gms)[which(names(gms)=="run_a_pg")] <- "home_rapg_before"  # rename for home team

gms <- merge(x=gms, y=Y, by.x=c("Date", "away"), by.y=c("Date", "Team"), all.x=TRUE)  # merge new away team info to our modeling data
names(gms)[which(names(gms)=="run_a_pg")] <- "away_rapg_before"  # rename for away team



# add team slugging percentage going into game
Y <- blogs[, .(
    TBs = (sum(`1B`) + 2*sum(`2B`) + 3*sum(`3B`) + 4*sum(HR)),
    ABs = sum(AB)
), 
by=.(Date, Team)
]  # get SLG info for each game

Y <- Y[order(Date)]  # order DF by date
Y <- Y[, game_num := 1:.N, by = .(Team)]  # calculate cumulative sum from game 1 to 162

Y <- Y[, tb_pg := cumsum(TBs) / (game_num - 1), by=.(Team)]  # calculate TB per game going into each game
Y <- Y[, ab_pg := cumsum(ABs) / (game_num - 1), by=.(Team)]  # calculate AB per game going into each game
Y <- Y[, slg_pct_before := tb_pg / ab_pg]

Y <- Y[, .(Date, Team, slg_pct_before)]  # the only stuff we need to add to our data

# attach home info
gms <- merge(x=gms, y=Y, by.x=c("Date", "home"), by.y=c("Date", "Team"), all.x=TRUE)  # merge new home team info to our modeling data
names(gms)[which(names(gms)=="slg_pct_before")] <- "home_slg_pct_before"

gms <- merge(x=gms, y=Y, by.x=c("Date", "away"), by.y=c("Date", "Team"), all.x=TRUE)  # merge new away team info to our modeling data
names(gms)[which(names(gms)=="slg_pct_before")] <- "away_slg_pct_before"



# add strong / weak (you can do this for anything, barrells, LD%, Launch Angle, etc)
Y <- blogs[, .(
    Hards = sum(HardHit, na.rm = T),  # players with no ABs get NA on HardHit and BIPCount
    BIPs = sum(bipCount, na.rm = T)
), 
by=.(Date, Team)
]  # get Hard Hit rate info for each game

Y <- Y[order(Date)]  # order DF by date
Y <- Y[, game_num := 1:.N, by = .(Team)]  # calculate cumulative sum from game 1 to 162

Y <- Y[, hh_pg_before := cumsum(Hards) / (game_num - 1), by=.(Team)]  # calculate HH per game going into each game
Y <- Y[, bip_pg_before := cumsum(BIPs) / (game_num - 1), by=.(Team)]  # calculate BIP per game going into each game
Y <- Y[, hh_pct_before := hh_pg_before / bip_pg_before]

Y <- Y[, .(Date, Team, hh_pg_before, bip_pg_before, hh_pct_before)]  # the only stuff we need to add to our data

# attach home info
gms <- merge(x=gms, y=Y, by.x=c("Date", "home"), by.y=c("Date", "Team"), all.x=TRUE)  # merge new home team info to our modeling data
names(gms)[which(names(gms)=="hh_pct_before")] <- "home_hh_pct_before"  # rename for home team
names(gms)[which(names(gms)=="hh_pg_before")] <- "home_hh_pg_before"  # rename for home team
names(gms)[which(names(gms)=="bip_pg_before")] <- "home_bip_pg_before"  # rename for home team


gms <- merge(x=gms, y=Y, by.x=c("Date", "away"), by.y=c("Date", "Team"), all.x=TRUE)  # merge new away team info to our modeling data
names(gms)[which(names(gms)=="hh_pct_before")] <- "away_hh_pct_before"  # rename for away team
names(gms)[which(names(gms)=="hh_pg_before")] <- "away_hh_pg_before"  # rename for away team
names(gms)[which(names(gms)=="bip_pg_before")] <- "away_bip_pg_before"  # rename for away team


# add strong / weak last 10 games
Y <- blogs[, .(
    Hards = sum(HardHit, na.rm = T), # players with no ABs get NA on HardHit and BIPCount
    BIPs = sum(bipCount, na.rm = T)
), 
by=.(Date, Team)
]  # get Hard Hit rate info for each game

Y <- Y[order(Date)]  # order DF by date

Y <- Y[, 
       hh_last_10 := rollapply(
           zoo(Hards/BIPs), 
           width=11, 
           align="right", 
           fill=NA, 
           FUN = function(x) mean(x[-1])
       ), 
       by=.(Team)
] # use the rollmean function to get rolling mean in previous 10 games of Hard %

Y <- Y[, .(Date, Team, hh_last_10)]  # the only data we want to attach

# attach home info
gms <- merge(x=gms, y=Y, by.x=c("Date", "home"), by.y=c("Date", "Team"), all.x=TRUE)  # merge new home team info to our modeling data
names(gms)[which(names(gms)=="hh_last_10")] <- "home_hh_last_10"  # rename for home team

gms <- merge(x=gms, y=Y, by.x=c("Date", "away"), by.y=c("Date", "Team"), all.x=TRUE)  # merge new away team info to our modeling data
names(gms)[which(names(gms)=="hh_last_10")] <- "away_hh_last_10"  # rename for away team

# ensure the vars created are numeric
gms$home_hh_last_10 <- as.numeric(gms$home_hh_last_10)  # make sure these are numeric
gms$away_hh_last_10 <- as.numeric(gms$away_hh_last_10)  # make sure these are numeric


# Correlations and data exploration
library(DataExplorer)

# remove stuff with NA's etc
dat <- gms[complete.cases(gms)]

# rename things for a pretty chart
names(dat)[6:21] <- c(
    "Home Score", "Home Runs Scored PG", "Away Score", "Away Runs Scored PG",
    "Home RA PG", "Away RA PG", "Home SLG", "Away SLG", "Home Hard Hit PG", 
    "Home BIP PG", "Home HH Rate", "Away Hard Hit PG", "Away BIP PG", "Away HH Rate",
    "Home HH Last 10", "Away HH Last 10"
)

plot_correlation(dat)

# see this blog post to do some other very quick reports
# https://www.r-bloggers.com/2021/03/super-fast-eda-in-r-with-dataexplorer/
# saveRDS(dat, "data_after_article_2.rds")
# write.csv(dat, "data_after_article_2.csv", row.names=F)




