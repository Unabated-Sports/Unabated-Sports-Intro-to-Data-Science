# Set your own work directory to the appropriate path on your machine

# setwd("D:/Downloads")
setwd("C:/Users/jaket/Documents/Trochelman Consulting LLC/Unabated Sports/Article Material/")

# required packages
library(baseballr)
library(data.table)
library(tictoc)
library(zoo)


#############################################
####### Intro to Data Science Part 4 ########
#############################################


require(data.table)
require(zoo)
require(baseballr)

# load data from article 2
# dat <- readRDS("data_after_article_2.rds")
dat <- data.table(read.csv("data_after_article_2.csv"))

# clean date (damn you, excel)
dat$Date <- as.Date(dat$Date, format="%m/%d/%Y")


####################
# attach odds data #
####################

# instantiate odds data frame
odds <- data.table()

# Load and attach odds data 2019
od1 <- data.table(read.csv("mlb2019_odds.csv"))  # read odds file
od1 <- od1[, .(Date, VH, Team, Close, Run.Line, X, Close.OU, X.2)]  # collect and rename cols
names(od1) <- c(
    "date", "home", "team", "ml_price", "run_line", "rl_price", "tot", "tot_price"
)

od1 <- od1[home != "N"]  #  remove neutral sites
od1 <- od1[home == "V", home := "A"]  # change V to A for "Away" designation
od1$ou <- od1$home  # in this data, Away team always listed with Over prices
od1 <- od1[ou == "A", ou := "O"]  # Assign overs
od1 <- od1[ou == "H", ou := "U"]  # assign unders
od1$season <- 2019

# bind/stack 2019 odds to odds data frame
odds <- rbind(odds, od1)


# Load and attach odds data 2020
od2 <- data.table(read.csv("mlb2020_odds.csv"))  # read odds file
od2 <- od2[, .(Date, VH, Team, Close, RunLine, X, CloseOU, X.2)]  # different than above
names(od2) <- c(
    "date", "home", "team", "ml_price", "run_line", "rl_price", "tot", "tot_price"
)

od2 <- od2[home != "N"]  #  remove neutral sites
od2 <- od2[home == "V", home := "A"]
od2$ou <- od2$home
od2 <- od2[ou == "A", ou := "O"]
od2 <- od2[ou == "H", ou := "U"]
od2$season <- 2020

# bind/stack 2020 odds to odds data frame
odds <- rbind(odds, od2)


# Load and attach odds data 2021
od3 <- data.table(read.csv("mlb2021_odds.csv"))  # read 2021 odds
od3 <- od3[, .(Date, VH, Team, Close, RunLine, X, CloseOU, X.2)]  # different than above
names(od3) <- c(
    "date", "home", "team", "ml_price", "run_line", "rl_price", "tot", "tot_price"
)

od3 <- od3[home != "N"]
od3 <- od3[home == "V", home := "A"]
od3$ou <- od3$home
od3 <- od3[ou == "A", ou := "O"]
od3 <- od3[ou == "H", ou := "U"]
od3$season <- 2021

# bind/stack 2021 odds to odds data frame
odds <- rbind(odds, od3)

# extract odds function
# this function is written to retrieve the date from odds data in the correct format
# odds data is in form of 401 for April 1
# you could do this in one line by attaching season first then using: as.Date(, format = '%m%d%y')
extract_odds_date <- function(seas, dat_str) {
    
    if (nchar(dat_str) == 4) {
        odate <- paste0(
            seas, "-",
            substr(as.character(dat_str), 1, 2), "-",
            substr(as.character(dat_str), 3, 4)
        )
    } else {
        odate <- paste0(
            seas, "-",
            substr(as.character(dat_str), 1, 1), "-",
            substr(as.character(dat_str), 2, 3)
        )
    }
    
    return(odate)
    
}

# process date using function above
odds[, "new_date" := extract_odds_date(season, date), by=.(date, season, team)]
odds$date <- as.Date(odds$new_date)
odds[, "new_date" := NULL]
names(odds)[1] <- "Date"
dat$Date <- as.Date(dat$Date)
odds$Date <- as.Date(odds$Date)

# change team names
# odds data ABBVs do not match FG Data.
odds[team=="BRS", team:="BOS"]
odds[team=="CUB", team:="CHC"]
odds[team=="CWS", team:="CHW"]
odds[team=="KAN", team:="KCR"]
odds[team=="SDG", team:="SDP"]
odds[team=="SFO", team:="SFG"]
odds[team=="TAM", team:="TBR"]
odds[team=="WAS", team:="WSN"]


# Attach odds to FG data
# We're only using 2021 data for analysis right now, similar process to use all data
# attach 2021 Moneylines 
Y <- odds[, .(Date, team, ml_price)]  # create data frame to attach
Y <- Y[!duplicated(Y[ , .(Date, team)])]  # the odds data has double headers, our data only has first games, so remove the 2nd game lines

# attach away ML's
names(Y) <- c("Date", "away", "a_ml")
dat <- merge(x=dat, y=Y, by=c("Date", "away"), all.x=T)

# home ML's
names(Y) <- c("Date", "home", "h_ml")
dat <- merge(x=dat, y=Y, by=c("Date", "home"), all.x=T)


# attach totals
Y <- odds[, .(Date, team, tot)]
Y <- Y[!duplicated(Y[ , .(Date, team)])] # the odds data has double headers, our data only has first games, so remove the 2nd game lines

names(Y) <- c("Date", "away", "total")
dat <- merge(x=dat, y=Y, by=c("Date", "away"), all.x=T)

# attach total prices
Y <- odds[, .(Date, team, tot_price)]
Y <- Y[!duplicated(Y[ , .(Date, team)])] # the odds data has double headers, our data only has first games, so remove the 2nd game lines

# over prices
names(Y) <- c("Date", "away", "o_price")
dat <- merge(x=dat, y=Y, by=c("Date", "away"), all.x=T)

# under prices
names(Y) <- c("Date", "home", "u_price")
dat <- merge(x=dat, y=Y, by=c("Date", "home"), all.x=T)

# clean data (removing rows with missing odds info)
dat <- dat[!is.na(h_ml)]
dat <- dat[!is.na(a_ml)]
dat <- dat[!is.na(o_price)]
dat <- dat[!is.na(u_price)]
dat <- dat[!is.na(total)]


#################################################
# calculate odds info (probs / covers / no-vig) #
#################################################

# we'll define 3 functions for processing american odds
# the first function converts american odds to implied probability
conv_odds_to_prob <- function(odds_amer) {
    if (odds_amer >=0) {
        out = (100 / (odds_amer + 100))
    } else {
        out = (-odds_amer / (-odds_amer + 100))
    }
    
    return(out)
}

# function to convert probabilities to american odds
conv_prob_to_odds <- function(prob) {
    if (prob <= 0.5) {
        out = 100 * (1/prob - 1)
    } else {
        out = -100 / (1/prob - 1)
    }
    
    return(round(out,0))
    
}

# function to remove vig from two implied with-vig probabiliites
remove_vig <- function(p1, p2, ret_p1_or_p2 = "p1") {
    
    out1 <- p1/(p1+p2)
    out2 <- p2/(p1+p2)
    
    if(ret_p1_or_p2 == "p1") {return(out1)} else {return(out2)}
}


# Add more implied probabilities
dat[, h_imp_prob := sapply(h_ml, conv_odds_to_prob)]
dat[, a_imp_prob := sapply(a_ml, conv_odds_to_prob)]

dat[, h_nv_prob := mapply(remove_vig, h_imp_prob, a_imp_prob, "p1")]
dat[, a_nv_prob := mapply(remove_vig, h_imp_prob, a_imp_prob, "p2")]

dat[, o_imp_prob := sapply(o_price, conv_odds_to_prob)]
dat[, u_imp_prob := sapply(u_price, conv_odds_to_prob)]

dat[, o_nv_prob := mapply(remove_vig, o_imp_prob, u_imp_prob, "p1")]
dat[, u_nv_prob := mapply(remove_vig, o_imp_prob, u_imp_prob, "p2")]


# add binary vars if things covered
dat[, h_cover := as.integer(home_score > away_score)]  # did the home team cover?
dat[, actual_total := home_score + away_score]  # calc game total
dat[, tot_over := as.integer(actual_total > total)]  # did game total go over vegas total?


################################
# attach starting pitcher data #
################################

# Like we did in original article with batters, let's grab SP data to attach
# you can also skip ahead 20 lines or so if you don't want to wait 15 min

# get all players last 5 years first to get player ids
# all_ps <- data.table(fg_pitcher_leaders(2019, 2022, qual="n", ind=1))

# package change necessitates the change instead of above
all_ps <- data.table()
for(YR in 2019:2022) {
    tmp <- data.table(fg_pitcher_leaders(
        startseason = YR, endseason = YR, qual="n", ind=1)
    )
    
    all_ps <- rbind(all_ps, tmp, fill=T)
}



# get all game logs (box score info) for each player, each game, in each season:
tic()  # start timer (will take about 15 min)

plogs <- data.table()  # empty data frame that we'll append every player info to

# nested for loop to grab all game logs for every player in every season
for(yr in unique(all_ps$Season)) {
    
    for(ptchr in all_ps[Season==yr]$playerid) {
        
        fgdat <- fg_pitcher_game_logs(year=yr, playerid = ptchr)  # get game logs from FG
        plogs <- rbind(plogs, fgdat, fill=T)  # attach to our big data frame
    }
}
toc()  # end timer

# if you want to skip waiting, just load this data
# plogs <- data.table(read.csv("pitcher_game_logs_FG_2021.csv"))  # skip above and just load data
plogs <- data.table(plogs)  # make a data table

# change date 
plogs$Date <- as.Date(plogs$Date)
plogs <- plogs[order(Date)]

# add home and away columns
plogs[, "away" := ifelse(grepl("@", Opp, T), Team, Opp)]
plogs[, "home" := ifelse(grepl("@", Opp, T), Opp, Team)]

# remove the @ for matching / joining later
plogs$home <- gsub("@", "", plogs$home)
plogs$Opp <- gsub("@", "", plogs$Opp)

# add game id
plogs[, "game_id" := paste(Date, away, home, G, sep="-")]

# there are some games where the data lists two starters (due to double header)
# this issue also affects the original data set, so we'll just remove all double header games
dbh <- plogs[, sum(GS), by=.(game_id)]  # count number of starters for each game_id
names(dbh)[2] <- "num_of_starters"
games_to_remove <- unique(dbh[num_of_starters==4]$game_id)  # games with 4 starters are double headers

dat <- dat[! (game_id %in% games_to_remove)]  # remove dbh from our modeling data
plogs <- plogs[! (game_id %in% games_to_remove)]  # remove dbh from pitcher data


# add away starters player_id
astrts <- plogs[GS == 1 & Team == away, .(game_id, playerid)]  # all away starters
names(astrts) <- c("game_id", "away_sp_id")
dat <- merge(x=dat, y=astrts, by="game_id", all.x=T)

# add home starters
hstrts <- plogs[GS == 1 & Team == home, .(game_id, playerid)]  # all home starters
names(hstrts) <- c("game_id", "home_sp_id")
dat <- merge(x=dat, y=hstrts, by="game_id", all.x=T)


# add starters avg hh rate
plogs <- plogs[season == 2021]  # use only 2021 stats, since we're using 2021

Y <- plogs[, .(
    Hards = sum(HardHit, na.rm = T),  # players with no ABs get NA on HardHit and BIPCount
    BIPs = sum(bipCount, na.rm = T)
), 
by=.(Date, playerid)
]  # get Hard Hit rate info for each game

Y <- Y[order(Date)]  # order DF by date
Y <- Y[, game_num := 1:.N, by = .(playerid)]  # calculate cumulative sum for games appeared in

Y <- Y[, hh_pg_before := cumsum(Hards) / (game_num - 1), by=.(playerid)]  # calculate HH per game going into each game
Y <- Y[, bip_pg_before := cumsum(BIPs) / (game_num - 1), by=.(playerid)]  # calculate BIP per game going into each game
Y <- Y[, hh_pct_before := hh_pg_before / bip_pg_before]

Y <- Y[, .(Date, playerid, hh_pct_before)]  # the only stuff we need to add to our data

# attach away info
dat <- merge(x=dat, y=Y, by.x=c("Date", "away_sp_id"), by.y=c("Date", "playerid"), all.x=TRUE)  # merge away pitcher info
names(dat)[which(names(dat)=="hh_pct_before")] <- "away_sp_hh_pct_before"  # rename for home team

# attach home info
dat <- merge(x=dat, y=Y, by.x=c("Date", "home_sp_id"), by.y=c("Date", "playerid"), all.x=TRUE)  # merge new away team info to our modeling data
names(dat)[which(names(dat)=="hh_pct_before")] <- "home_sp_hh_pct_before"  # rename for away team


# add  hard hit info for SPs in last 3 appearances
Y <- plogs[, .(
    Hards = sum(HardHit, na.rm = T), # players with no ABs get NA on HardHit and BIPCount
    BIPs = sum(bipCount, na.rm = T)
), 
by=.(Date, playerid)
]  # get Hard Hit rate info for each game

Y <- Y[order(Date)]  # order DF by date

Y <- Y[, 
       hh_last_3 := rollapply(
           zoo(Hards/BIPs), 
           width=4, 
           align="right", 
           fill=NA, 
           FUN = function(x) mean(x[-1])
       ), 
       by=.(playerid)
] # use the rollmean function to get rolling mean in previous 5 games of Hard %

Y <- Y[, .(Date, playerid, hh_last_3)]  # the only data we want to attach

# attach away info
dat <- merge(x=dat, y=Y, by.x=c("Date", "away_sp_id"), by.y=c("Date", "playerid"), all.x=TRUE)  # merge new home team info to our modeling data
names(dat)[which(names(dat)=="hh_last_3")] <- "away_sp_hh_last_3"  # rename for home team

# attach home info
dat <- merge(x=dat, y=Y, by.x=c("Date", "home_sp_id"), by.y=c("Date", "playerid"), all.x=TRUE)  # merge new away team info to our modeling data
names(dat)[which(names(dat)=="hh_last_3")] <- "home_sp_hh_last_3"  # rename for away team

# ensure the vars created are numeric
dat$home_sp_hh_last_3 <- as.numeric(dat$home_sp_hh_last_3)  # make sure these are numeric
dat$away_sp_hh_last_3 <- as.numeric(dat$away_sp_hh_last_3)  # make sure these are numeric


# final data clean up
dat <- dat[complete.cases(dat)]  # remove stuff with NA's etc


#################################################
# check correlations with odds info and new var #
#################################################
library(DataExplorer)

# only cols you need
gf <- dat[, 8:43]

# rename things for a pretty chart
names(gf) <- c(
    "Home Score", "Home Runs Scored PG", "Away Score", "Away Runs Scored PG",
    "Home RA PG", "Away RA PG", "Home SLG", "Away SLG", "Home Hard Hit PG",
    "Home BIP PG", "Home HH Rate", "Away Hard Hit PG", "Away BIP PG", "Away HH Rate",
    "Home HH Last 10", "Away HH Last 10", "Away ML", "Home ML", "Total", "Ov Price",
    "Un Price", "H Imp Prob", "A Imp Prob", "H NV Prob", "A NV Prob", "Ov Imp Prob",
    "Un Imp Prob", "Ov NV Prob", "Un NV Prob", "Home Cover", "Actual Total", 
    "Total Over", "A SP HH Pct", "H SP HH Pct", "A SP HH Last 3", "H SP HH Last 3"
)

plot_correlation(gf)