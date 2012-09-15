#----------------------------------------------------------------------------------------------
# File: roster-stats-url-grabber.R
# Date: 09-01-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/web-scraping/skater-register-url-grabber.R
# Email: theRcast@gmail.com
# Purpose: Assemble and scrape valid URLs with team and roster statistics data from www.hockey-reference.com
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - code adapted from Ryan Elmore's analysis of baseball boxscore data:
#   - https://github.com/rtelmore/Pitch_Count
#   - http://www.slideshare.net/rtelmore/user-2012-talk
#
# To Do:
#
# load(file="~/hockey_workspaces/url.check.data.RData")
# team.valid <- data.tracker.team[data.tracker.team$skater.register,]
#
# 
#----------------------------------------------------------------------------------------------

# load required packages
library(XML)
library(stringr)
library(RMySQL)

# source script to estabilish database credentials
source("~/Dropbox/rpodcast_code/nhl_analysis/lib/mysql.login.R")

# connect to hockey database
mychannel <- dbConnect(MySQL(), user=login$user, password=login$password, dbname=login$dbname)

# define variables used in loops below

load(file="~/hockey_workspaces/url.check.data.RData")
rm(data.tracker.team)

team.year.valid <- data.tracker.team.year[data.tracker.team.year$team.stats & data.tracker.team.year$team.schedule,]

#team.year.valid.debug <- team.year.valid[c(950, 1300),]

base.url <- "http://www.hockey-reference.com/teams/"

# add names for table columns here

reg.season.schedule.column.names <- c("gp",
                                    "date",
                                    "home.away.ind",
                                    "opp",
                                    "outcome",
                                    "outcome.type",
                                    "goals.for",
                                    "goals.against",
                                    "win.number",
                                    "loss.number",
                                    "tie.number",
                                    "streak",
                                    "notes")

playoff.schedule.column.names <- c("gp",
                                 "date",
                                 "home.away.ind",
                                 "opp",
                                 "outcome",
                                 "outcome.type",
                                 "goals.for",
                                 "goals.against",
                                 "win.number",
                                 "loss.number",
                                 "streak",
                                 "notes")

team.stats.column.names <- c("team.full",
                             "games.played",
                             "wins",
                             "losses",
                             "ties",
                             "points",
                             "points.percentage",
                             "goals.for",
                             "goals.against",
                             "srs",
                             "sos",
                             "goals.per.game",
                             "pp.goals.for",
                             "n.pp.for",
                             "pp.percent",
                             "pp.goals.against",
                             "n.pp.against",
                             "pk.percent",
                             "goals.shorthanded.for",
                             "goals.shorthanded.against")

roster.column.names <- c("jersey.number",
                         "player",
                         "position",
                         "age",
                         "height",
                         "weight",
                         "shoots.catches",
                         "yrs",
                         "birth.date",
                         "summary")

reg.season.scoring.column.names <- c("rk",
                                     "player",
                                     "pos",
                                     "age",
                                     "gp",
                                     "goals",
                                     "assists",
                                     "points",
                                     "goals.created",
                                     "plus-minus",
                                     "pim",
                                     "goals.even",
                                     "goals.pp",
                                     "goals.sh",
                                     "goals.gw",
                                     "shots",
                                     "shooting.percent",
                                     "time.on.ice",
                                     "avg.time.on.ice",
                                     "ops",
                                     "dps",
                                     "ps")

reg.season.goalie.column.names <- c("rk",
                                    "player",
                                    "pos",
                                    "age",
                                    "gp",
                                    "w",
                                    "l",
                                    "t-o",
                                    "ga",
                                    "sa",
                                    "sv",
                                    "sv_percent",
                                    "gaa",
                                    "so",
                                    "min",
                                    "gps")

playoff.scoring.column.names <- c("rk",
                                   "player",
                                   "pos",
                                   "age",
                                   "gp",
                                   "goals",
                                   "assists",
                                   "points",
                                   "goals.created",
                                   "plus-minus",
                                   "pim",
                                   "goals.even",
                                   "goals.pp",
                                   "goals.sh",
                                   "goals.gw",
                                   "shots",
                                   "shooting.percent",
                                   "time.on.ice",
                                   "avg.time.on.ice")

playoff.goalie.column.names <- c("rk",
                                    "player",
                                    "pos",
                                    "age",
                                    "gp",
                                    "w",
                                    "l",
                                    "t-o",
                                    "ga",
                                    "sa",
                                    "sv",
                                    "sv_percent",
                                    "gaa",
                                    "so",
                                    "min")

# construct loops to import data from urls

for(i in 1:nrow(team.year.valid)) {
  team <- team.year.valid[i, "team"]
  year <- team.year.valid[i, "year"]
  
  out.string <- paste(Sys.time(), "--", team, year, sep = " ")
  print(out.string)
  cat(out.string, "\n", file="~/hockey_workspaces/roster.stat.url.log.txt", append=TRUE)
  
  full.roster.url <- paste(base.url, team, "/", year, ".html", sep="")
  full.schedule.url <- paste(base.url, team, "/", year, "_games.html", sep="")
  
  roster.table.stats <- readHTMLTable(full.roster.url, header=FALSE)
  schedule.table.stats <- readHTMLTable(full.schedule.url, header=FALSE)
  
  #----------------------------------------------------------------------
  # team statistics table
  #----------------------------------------------------------------------
  team.stats <- roster.table.stats$team_stats
  names(team.stats) <- team.stats.column.names
  
  team.stats$team <- team
  team.stats$year <- year
  
  team.stats <- team.stats[!team.stats$team.full=="League Average",]
  
  if(dbExistsTable(mychannel, "TEAM_STATS")) {
    dbWriteTable(mychannel, "TEAM_STATS", team.stats, append = T, row.names=FALSE)
  } else dbWriteTable(mychannel, "TEAM_STATS", team.stats, row.names=FALSE)  
  
  rm(team.stats)
  
  #------------------------------------------------------------------------
  # roster statistics table
  #------------------------------------------------------------------------
  roster.stats <- roster.table.stats$roster
  names(roster.stats) <- roster.column.names
  
  roster.stats$team <- team
  roster.stats$year <- year
  
  if(dbExistsTable(mychannel, "ROSTER_STATS")) {
    dbWriteTable(mychannel, "ROSTER_STATS", roster.stats, append = T, row.names=FALSE)
  } else dbWriteTable(mychannel, "ROSTER_STATS", roster.stats, row.names=FALSE) 
  
  rm(roster.stats)
  
  #---------------------------------------------------------------------------
  # regular season scoring statistics table
  #---------------------------------------------------------------------------
  reg.season.scoring.stats <- roster.table.stats$skaters
  names(reg.season.scoring.stats) <- reg.season.scoring.column.names
  
  reg.season.scoring.stats$team <- team
  reg.season.scoring.stats$year <- year
  
  reg.season.scoring.stats <- reg.season.scoring.stats[!reg.season.scoring.stats$player=="Team Total",]
  
  if(dbExistsTable(mychannel, "REG_SEASON_SCORING_STATS")) {
    dbWriteTable(mychannel, "REG_SEASON_SCORING_STATS", reg.season.scoring.stats, append = T, row.names=FALSE)
  } else dbWriteTable(mychannel, "REG_SEASON_SCORING_STATS", reg.season.scoring.stats, row.names=FALSE)
  
  rm(reg.season.scoring.stats)
  
  #--------------------------------------------------------------------------
  # regular season goalie statistics table
  #--------------------------------------------------------------------------
  reg.season.goalie.stats <- roster.table.stats$goalies
  names(reg.season.goalie.stats) <- reg.season.goalie.column.names
  
  reg.season.goalie.stats$team <- team
  reg.season.goalie.stats$year <- year
  
  reg.season.goalie.stats <- reg.season.goalie.stats[!reg.season.goalie.stats$player=="Team Total",]
  
  if(dbExistsTable(mychannel, "REG_SEASON_GOALIE_STATS")) {
    dbWriteTable(mychannel, "REG_SEASON_GOALIE_STATS", reg.season.goalie.stats, append = T, row.names=FALSE)
  } else dbWriteTable(mychannel, "REG_SEASON_GOALIE_STATS", reg.season.goalie.stats, row.names=FALSE)
  
  rm(reg.season.goalie.stats)
  
  #-------------------------------------------------------
  # check if playoff scoring and goalie tables exist
  #-------------------------------------------------------

  if("skaters_playoffs" %in% names(roster.table.stats)) {
    playoff.scoring.stats <- roster.table.stats$skaters_playoffs
    names(playoff.scoring.stats) <- playoff.scoring.column.names
    
    playoff.scoring.stats$team <- team
    playoff.scoring.stats$year <- year
    
    playoff.scoring.stats <- playoff.scoring.stats[!playoff.scoring.stats$player=="Team Total",]
    
    if(dbExistsTable(mychannel, "PLAYOFF_SCORING_STATS")) {
      dbWriteTable(mychannel, "PLAYOFF_SCORING_STATS", playoff.scoring.stats, append = T, row.names=FALSE)
    } else dbWriteTable(mychannel, "PLAYOFF_SCORING_STATS", playoff.scoring.stats, row.names=FALSE)
    
    rm(playoff.scoring.stats)
  }
  
  if("goalies_playoffs" %in% names(roster.table.stats)) {
    playoff.goalie.stats <- roster.table.stats$goalies_playoffs
    names(playoff.goalie.stats) <- playoff.goalie.column.names
    
    playoff.goalie.stats$team <- team
    playoff.goalie.stats$year <- year
    
    playoff.goalie.stats <- playoff.goalie.stats[!playoff.goalie.stats$player=="Team Total",]
    
    if(dbExistsTable(mychannel, "PLAYOFF_GOALIE_STATS")) {
      dbWriteTable(mychannel, "PLAYOFF_GOALIE_STATS", playoff.goalie.stats, append = T, row.names=FALSE)
    } else dbWriteTable(mychannel, "PLAYOFF_GOALIE_STATS", playoff.goalie.stats, row.names=FALSE)  
    
    rm(playoff.goalie.stats)
  }
  
  #-------------------------------------------------------------------------
  # regular season schedule
  #-------------------------------------------------------------------------
  
  reg.season.schedule.stats <- schedule.table.stats$games
  names(reg.season.schedule.stats) <- reg.season.schedule.column.names
  
  reg.season.schedule.stats$team <- team
  reg.season.schedule.stats$year <- year
  
  reg.season.schedule.stats <- reg.season.schedule.stats[!reg.season.schedule.stats$gp=="GP",]
  
  if(dbExistsTable(mychannel, "REG_SEASON_SCHEDULE_STATS")) {
    dbWriteTable(mychannel, "REG_SEASON_SCHEDULE_STATS", reg.season.schedule.stats, append = T, row.names=FALSE)
  } else dbWriteTable(mychannel, "REG_SEASON_SCHEDULE_STATS", reg.season.schedule.stats, row.names=FALSE)
  
  rm(reg.season.schedule.stats)
  
  # check if playoff schedule exists
  
  if("games_playoffs" %in% names(schedule.table.stats)) {
    playoff.schedule.stats <- schedule.table.stats$games_playoffs
    names(playoff.schedule.stats) <- playoff.schedule.column.names
    
    playoff.schedule.stats$team <- team
    playoff.schedule.stats$year <- year
    
    playoff.schedule.stats <- playoff.schedule.stats[!playoff.schedule.stats$gp=="GP",]
    
    if(dbExistsTable(mychannel, "PLAYOFF_SCHEDULE_STATS")) {
      dbWriteTable(mychannel, "PLAYOFF_SCHEDULE_STATS", playoff.schedule.stats, append = T, row.names=FALSE)
    } else dbWriteTable(mychannel, "PLAYOFF_SCHEDULE_STATS", playoff.schedule.stats, row.names=FALSE)
    
    rm(playoff.schedule.stats)
  }
  
}

# close database connection

dbDisconnect(mychannel)
