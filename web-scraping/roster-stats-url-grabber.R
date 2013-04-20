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
#
# 
#----------------------------------------------------------------------------------------------

# load required packages
library(XML, lib.loc="/usr/local/lib/R/site-library")
library(stringr)
library(RMySQL)

# define debug flag
debug.set <- FALSE

# define database table names for MySQL
team.stats.dbtable <- "TEAM_STATS"
roster.stats.dbtable <- "ROSTER_STATS"
reg.season.scoring.dbtable <- "REG_SEASON_SCORING_STATS"
playoff.scoring.dbtable <- "PLAYOFF_SCORING_STATS"
reg.season.schedule.dbtable <- "REG_SEASON_SCHEDULE_STATS"
playoff.schedule.dbtable <- "PLAYOFF_SCHEDULE_STATS"
reg.season.goalie.dbtable <- "REG_SEASON_GOALIE_STATS"
playoff.goalie.dbtable <- "PLAYOFF_GOALIE_STATS"

# connect to hockey database
# assumes username and password are specified in ~/.my.cnf
mychannel <- dbConnect(MySQL(), dbname = "hockey", group="client")

# define indicator for whether to overwrite existing log with new log
log.name <- "roster.stat.url.log.txt"
overwrite.existing <- TRUE

if(overwrite.existing) {
  if(file.exists(file.path("logs", log.name))) {
    file.remove(file.path("logs", log.name))
  }
}

# define variables used in loops below

load(file="web-scraping/url.check.data.RData")
rm(data.tracker.team)

team.year.valid <- data.tracker.team.year[data.tracker.team.year$team.stats & data.tracker.team.year$team.schedule,]

if(debug.set) {
  #team.year.valid <- team.year.valid[c(950, 1300),]
  log.name <- "roster.stat.url.log.debug.txt"
  team.year.valid <- subset(team.year.valid,
                            team %in% c("BUF", "DET") & year %in% c(2006, 2009))
  
  # define database table names for MySQL
  team.stats.dbtable <- "TEAM_STATS_TEST"
  roster.stats.dbtable <- "ROSTER_STATS_TEST"
  reg.season.scoring.dbtable <- "REG_SEASON_SCORING_STATS_TEST"
  playoff.scoring.dbtable <- "PLAYOFF_SCORING_STATS_TEST"
  reg.season.schedule.dbtable <- "REG_SEASON_SCHEDULE_STATS_TEST"
  playoff.schedule.dbtable <- "PLAYOFF_SCHEDULE_STATS_TEST"
  reg.season.goalie.dbtable <- "REG_SEASON_GOALIE_STATS_TEST"
  playoff.goalie.dbtable <- "PLAYOFF_GOALIE_STATS_TEST"
  
  # remove debug sets prior to running code if they exist
  if(dbExistsTable(mychannel, team.stats.dbtable)) {
    dbRemoveTable(conn=mychannel, name=team.stats.dbtable)
    dbRemoveTable(conn=mychannel, name=roster.stats.dbtable)
    dbRemoveTable(conn=mychannel, name=reg.season.scoring.dbtable)
    dbRemoveTable(conn=mychannel, name=playoff.scoring.dbtable)
    dbRemoveTable(conn=mychannel, name=reg.season.schedule.dbtable)
    dbRemoveTable(conn=mychannel, name=playoff.schedule.dbtable)
    dbRemoveTable(conn=mychannel, name=reg.season.goalie.dbtable)
    dbRemoveTable(conn=mychannel, name=playoff.goalie.dbtable)
  }
}

base.url <- "http://www.hockey-reference.com/teams/"

# define names for MySQL table names and column types

reg.season.schedule.column.names <- c("team",
                                      "season",
                                      "gp",
                                      "date",
                                      "home_away_ind",
                                      "opp",
                                      "outcome",
                                      "outcome_type",
                                      "goals_for",
                                      "goals_against",
                                      "win_number",
                                      "loss_number",
                                      "tie_number",
                                      "streak",
                                      "notes")

reg.season.schedule.classes <- list(team="CHAR(100)",
                                    season="CHAR(50)",
                                    gp="INT",
                                    date="CHAR(40)",
                                    home_away_ind="CHAR(5)",
                                    opp="CHAR(100)",
                                    outcome="CHAR(5)",
                                    outcome_type="CHAR(10)",
                                    goals_for="INT",
                                    goals_against="INT",
                                    win_number="INT",
                                    loss_number="INT",
                                    tie_number="INT",
                                    streak="CHAR(10)",
                                    notes="CHAR(100)")

playoff.schedule.column.names <- c("team",
                                   "season", 
                                   "gp",
                                   "date",
                                   "home_away_ind",
                                   "opp",
                                   "outcome",
                                   "outcome_type",
                                   "goals_for",
                                   "goals_against",
                                   "win_number",
                                   "loss_number",
                                   #"tie_number",
                                   "streak",
                                   "notes")

playoff.schedule.classes <- list(team="CHAR(100)",
                                 season="CHAR(50)", 
                                 gp="INT",
                                 date="CHAR(40)",
                                 home_away_ind="CHAR(5)",
                                 opp="CHAR(100)",
                                 outcome="CHAR(5)",
                                 outcome_type="CHAR(10)",
                                 goals_for="INT",
                                 goals_against="INT",
                                 win_number="INT",
                                 loss_number="INT",
                                 #tie_number="INT",
                                 streak="CHAR(10)",
                                 notes="CHAR(100)")

team.stats.column.names <- c("team",
                             "season",
                             "team_full",
                             "games_played",
                             "wins",
                             "losses",
                             "ties",
                             "points",
                             "points_percentage",
                             "goals_for",
                             "goals_against",
                             "srs",
                             "sos",
                             "goals_per_game",
                             "pp_goals_for",
                             "n_pp_for",
                             "pp_percent",
                             "pp_goals_against",
                             "n_pp_against",
                             "pk_percent",
                             "goals_shorthanded_for",
                             "goals_shorthanded_against")

team.stats.classes <- list(team="CHAR(100)",
                           season="CHAR(50)",
                           team_full="CHAR(100)",
                           games_played="INT",
                           wins="INT",
                           losses="INT",
                           ties="INT",
                           points="INT",
                           points_percentage="DOUBLE(6,3)",
                           goals_for="INT",
                           goals_against="INT",
                           srs="DOUBLE(6,3)",
                           sos="DOUBLE(6,3)",
                           goals_per_game="DOUBLE(6,3)",
                           pp_goals_for="INT",
                           n_pp_for="INT",
                           pp_percent="DOUBLE(6,3)",
                           pp_goals_against="INT",
                           n_pp_against="INT",
                           pk_percent="DOUBLE(6,3)",
                           goals_shorthanded_for="INT",
                           goals_shorthanded_against="INT")

roster.column.names <- c("team",
                         "season",
                         "jersey_number",
                         "player",
                         "position",
                         "age",
                         "height",
                         "weight",
                         "shoots_catches",
                         "yrs",
                         "birth_date",
                         "summary")

roster.classes <- list(team="CHAR(100)",
                       season="CHAR(50)",
                       jersey_number="CHAR(5)",
                       player="CHAR(100)",
                       position="CHAR(10)",
                       age="INT",
                       height="CHAR(10)",
                       weight="INT",
                       shoots_catches="CHAR(10)",
                       yrs="CHAR(5)",
                       birth_date="CHAR(40)",
                       summary="CHAR(50)")

reg.season.scoring.column.names <- c("team",
                                     "season",
                                     "rk",
                                     "player",
                                     "pos",
                                     "age",
                                     "gp",
                                     "goals",
                                     "assists",
                                     "points",
                                     "goals_created",
                                     "plus_minus",
                                     "pim",
                                     "goals_even",
                                     "goals_pp",
                                     "goals_sh",
                                     "goals_gw",
                                     "assists_even",
                                     "assists_sh",
                                     "assists_pp",
                                     "shots",
                                     "shooting_percent",
                                     "time_on_ice",
                                     "avg_time_on_ice",
                                     "ops",
                                     "dps",
                                     "ps")

reg.season.scoring.classes <- list(team="CHAR(100)",
                                   season="CHAR(50)",
                                   rk="INT",
                                   player="CHAR(100)",
                                   pos="CHAR(5)",
                                   age="INT",
                                   gp="INT",
                                   goals="INT",
                                   assists="INT",
                                   points="INT",
                                   goals_created="INT",
                                   plus_minus="INT",
                                   pim="INT",
                                   goals_even="INT",
                                   goals_pp="INT",
                                   goals_sh="INT",
                                   goals_gw="INT",
                                   assists_even="INT",
                                   assists_sh="INT",
                                   assists_pp="INT",
                                   shots="INT",
                                   shooting_percent="DOUBLE(6,3)",
                                   time_on_ice="INT",
                                   avg_time_on_ice="CHAR(10)",
                                   ops="DOUBLE(6,3)",
                                   dps="DOUBLE(6,3)",
                                   ps="DOUBLE(6,3)")

reg.season.goalie.column.names <- c("team",
                                    "season",
                                    "rk",
                                    "player",
                                    "pos",
                                    "age",
                                    "gp",
                                    "w",
                                    "l",
                                    "tot",
                                    "ga",
                                    "sa",
                                    "sv",
                                    "sv_percent",
                                    "gaa",
                                    "so",
                                    "min",
                                    "gps")

reg.season.goalie.classes <- list(team="CHAR(100)",
                                  season="CHAR(50)",
                                  rk="INT",
                                  player="CHAR(100)",
                                  pos="CHAR(5)",
                                  age="INT",
                                  gp="INT",
                                  w="INT",
                                  l="INT",
                                  tot="INT",
                                  ga="INT",
                                  sa="INT",
                                  sv="INT",
                                  sv_percent="DOUBLE(6,3)",
                                  gaa="DOUBLE(6,3)",
                                  so="INT",
                                  min="INT",
                                  gps="DOUBLE(6,3)")

playoff.scoring.column.names <- c("team",
                                  "season",
                                  "rk",
                                   "player",
                                   "pos",
                                   "age",
                                   "gp",
                                   "goals",
                                   "assists",
                                   "points",
                                   "goals_created",
                                   "plus_minus",
                                   "pim",
                                   "goals_even",
                                   "goals_pp",
                                   "goals_sh",
                                   "goals_gw",
                                  "assists_even",
                                  "assists_sh",
                                  "assists_pp",
                                   "shots",
                                   "shooting_percent",
                                   "time_on_ice",
                                   "avg_time_on_ice")

playoff.scoring.classes <- list(team="CHAR(100)",
                                season="CHAR(50)",
                                rk="INT",
                                player="CHAR(100)",
                                pos="CHAR(5)",
                                age="INT",
                                gp="INT",
                                goals="INT",
                                assists="INT",
                                points="INT",
                                goals_created="INT",
                                plus_minus="INT",
                                pim="INT",
                                goals_even="INT",
                                goals_pp="INT",
                                goals_sh="INT",
                                goals_gw="INT",
                                assists_even="INT",
                                assists_sh="INT",
                                assists_pp="INT",
                                shots="INT",
                                shooting_percent="DOUBLE(6,3)",
                                time_on_ice="INT",
                                avg_time_on_ice="CHAR(10)")

playoff.goalie.column.names <- c("team",
                                 "season",
                                 "rk",
                                 "player",
                                 "pos",
                                 "age",
                                 "gp",
                                 "w",
                                 "l",
                                 "tot",
                                 "ga",
                                 "sa",
                                 "sv",
                                 "sv_percent",
                                 "gaa",
                                 "so",
                                 "min")

playoff.goalie.classes <- list(team="CHAR(100)",
                               season="CHAR(50)",
                                rk="INT",
                                player="CHAR(100)",
                                pos="CHAR(5)",
                                age="INT",
                                gp="INT",
                                w="INT",
                                l="INT",
                                tot="INT",
                                ga="INT",
                                sa="INT",
                                sv="INT",
                                sv_percent="DOUBLE(6,3)",
                                gaa="DOUBLE(6,3)",
                                so="INT",
                                min="INT")

# construct loops to import data from urls

for(i in 1:nrow(team.year.valid)) {
  team <- team.year.valid[i, "team"]
  year <- team.year.valid[i, "year"]
  season <- paste(year-1, str_sub(year, start=3, end=4), sep="-")
  
  out.string <- paste(Sys.time(), "--", team, year, sep = " ")
  print(out.string)
  cat(out.string, "\n", file="logs/roster.stat.url.log.txt", append=TRUE)
  
  full.roster.url <- paste(base.url, team, "/", year, ".html", sep="")
  full.schedule.url <- paste(base.url, team, "/", year, "_games.html", sep="")
  
  roster.table.stats <- readHTMLTable(full.roster.url, 
                                      header=FALSE,
                                      stringsAsFactors=FALSE)
  schedule.table.stats <- readHTMLTable(full.schedule.url, 
                                        header=FALSE,
                                        stringsAsFactors=FALSE)
  
  #----------------------------------------------------------------------
  # team statistics table
  #----------------------------------------------------------------------
  team.stats <- roster.table.stats$team_stats
  team.stats <- cbind(team, season, team.stats)
  names(team.stats) <- team.stats.column.names
  
#   team.stats$team <- team
#   team.stats$year <- year
  
  team.stats <- team.stats[!team.stats$team_full=="League Average",]
  
  if(dbExistsTable(mychannel, team.stats.dbtable)) {
    dbWriteTable(mychannel,
                 team.stats.dbtable, 
                 team.stats, 
                 append = TRUE, 
                 row.names=FALSE,
                 field.types=team.stats.classes)
  } else dbWriteTable(mychannel,
                      team.stats.dbtable, 
                      team.stats, 
                      row.names=FALSE,
                      field.types=team.stats.classes)  
  
  rm(team.stats)
  
  #------------------------------------------------------------------------
  # roster statistics table
  #------------------------------------------------------------------------
  roster.stats <- roster.table.stats$roster
  roster.stats <- cbind(team, season, roster.stats)
  names(roster.stats) <- roster.column.names
  
#   roster.stats$team <- team
#   roster.stats$year <- year
  
  if(dbExistsTable(mychannel, roster.stats.dbtable)) {
    dbWriteTable(mychannel, 
                 roster.stats.dbtable, 
                 roster.stats, 
                 append = TRUE, 
                 row.names=FALSE,
                 field.types=roster.classes)
  } else dbWriteTable(mychannel, 
                      roster.stats.dbtable, 
                      roster.stats, 
                      row.names=FALSE,
                      field.types=roster.classes) 
  
  rm(roster.stats)
  
  #---------------------------------------------------------------------------
  # regular season scoring statistics table
  #---------------------------------------------------------------------------
  reg.season.scoring.stats <- roster.table.stats$skaters
  reg.season.scoring.stats <- cbind(team, season, reg.season.scoring.stats)
  names(reg.season.scoring.stats) <- reg.season.scoring.column.names
  
#   reg.season.scoring.stats$team <- team
#   reg.season.scoring.stats$year <- year
  
  reg.season.scoring.stats <- reg.season.scoring.stats[!reg.season.scoring.stats$player=="Team Total",]
  
  if(dbExistsTable(mychannel, reg.season.scoring.dbtable)) {
    dbWriteTable(mychannel,
                 reg.season.scoring.dbtable, 
                 reg.season.scoring.stats, 
                 append = TRUE, 
                 row.names=FALSE,
                 field.types=reg.season.scoring.classes)
  } else dbWriteTable(mychannel, 
                      reg.season.scoring.dbtable, 
                      reg.season.scoring.stats, 
                      row.names=FALSE,
                      field.types=reg.season.scoring.classes)
  
  rm(reg.season.scoring.stats)
  
  #--------------------------------------------------------------------------
  # regular season goalie statistics table
  #--------------------------------------------------------------------------
  reg.season.goalie.stats <- roster.table.stats$goalies
  reg.season.goalie.stats <- cbind(team, season, reg.season.goalie.stats)
  names(reg.season.goalie.stats) <- reg.season.goalie.column.names
  
#   reg.season.goalie.stats$team <- team
#   reg.season.goalie.stats$year <- year
  
  reg.season.goalie.stats <- reg.season.goalie.stats[!reg.season.goalie.stats$player=="Team Total",]
  
  if(dbExistsTable(mychannel, reg.season.goalie.dbtable)) {
    dbWriteTable(mychannel,
                 reg.season.goalie.dbtable,
                 reg.season.goalie.stats,
                 append = TRUE, 
                 row.names=FALSE,
                 field.types=reg.season.goalie.classes)
  } else dbWriteTable(mychannel, 
                      reg.season.goalie.dbtable, 
                      reg.season.goalie.stats, 
                      row.names=FALSE,
                      field.types=reg.season.goalie.classes)
  
  rm(reg.season.goalie.stats)
  
  #-------------------------------------------------------
  # check if playoff scoring and goalie tables exist
  #-------------------------------------------------------

  if("skaters_playoffs" %in% names(roster.table.stats)) {
    playoff.scoring.stats <- roster.table.stats$skaters_playoffs
    playoff.scoring.stats <- cbind(team, season, playoff.scoring.stats)
    names(playoff.scoring.stats) <- playoff.scoring.column.names
    
#     playoff.scoring.stats$team <- team
#     playoff.scoring.stats$year <- year
    
    playoff.scoring.stats <- playoff.scoring.stats[!playoff.scoring.stats$player=="Team Total",]
    
    if(dbExistsTable(mychannel, playoff.scoring.dbtable)) {
      dbWriteTable(mychannel, playoff.scoring.dbtable, 
                   playoff.scoring.stats, 
                   append = TRUE,
                   row.names=FALSE,
                   field.types=playoff.scoring.classes)
    } else dbWriteTable(mychannel,
                        playoff.scoring.dbtable,
                        playoff.scoring.stats,
                        row.names=FALSE,
                        field.types=playoff.scoring.classes)
    
    rm(playoff.scoring.stats)
  }
  
  if("goalies_playoffs" %in% names(roster.table.stats)) {
    playoff.goalie.stats <- roster.table.stats$goalies_playoffs
    playoff.goalie.stats <- cbind(team, season, playoff.goalie.stats)
    names(playoff.goalie.stats) <- playoff.goalie.column.names
    
#     playoff.goalie.stats$team <- team
#     playoff.goalie.stats$year <- year
    
    playoff.goalie.stats <- playoff.goalie.stats[!playoff.goalie.stats$player=="Team Total",]
    
    if(dbExistsTable(mychannel, playoff.goalie.dbtable)) {
      dbWriteTable(mychannel,
                   playoff.goalie.dbtable, 
                   playoff.goalie.stats, 
                   append = TRUE, 
                   row.names=FALSE,
                   field.types=playoff.goalie.classes)
    } else dbWriteTable(mychannel, 
                        playoff.goalie.dbtable,
                        playoff.goalie.stats,
                        row.names=FALSE,
                        field.types=playoff.goalie.classes)  
    
    rm(playoff.goalie.stats)
  }
  
  #-------------------------------------------------------------------------
  # regular season and playoff schedule
  #-------------------------------------------------------------------------
  
  reg.season.schedule.stats <- schedule.table.stats$games
  reg.season.schedule.stats <- cbind(team, season, reg.season.schedule.stats)
  names(reg.season.schedule.stats) <- reg.season.schedule.column.names
  
  #reg.season.schedule.stats$team <- team
  #reg.season.schedule.stats$year <- year
  
  reg.season.schedule.stats <- reg.season.schedule.stats[!reg.season.schedule.stats$gp=="GP",]
  
  if(dbExistsTable(mychannel, reg.season.schedule.dbtable)) {
    dbWriteTable(mychannel, 
                 reg.season.schedule.dbtable, 
                 reg.season.schedule.stats, 
                 append = TRUE, 
                 row.names=FALSE,
                 field.types=reg.season.schedule.classes)
  } else dbWriteTable(mychannel, 
                      reg.season.schedule.dbtable, 
                      reg.season.schedule.stats, 
                      row.names=FALSE,
                      field.types=reg.season.schedule.classes)
  
  rm(reg.season.schedule.stats)
  
  # check if playoff schedule exists
  
  if("games_playoffs" %in% names(schedule.table.stats)) {
    playoff.schedule.stats <- schedule.table.stats$games_playoffs
    playoff.schedule.stats <- cbind(team, season, playoff.schedule.stats)
    names(playoff.schedule.stats) <- playoff.schedule.column.names
    
#     playoff.schedule.stats$team <- team
#     playoff.schedule.stats$year <- year
    
    playoff.schedule.stats <- playoff.schedule.stats[!playoff.schedule.stats$gp=="GP",]
    
    if(dbExistsTable(mychannel, playoff.schedule.dbtable)) {
      dbWriteTable(mychannel, 
                   playoff.schedule.dbtable, 
                   playoff.schedule.stats, 
                   append = TRUE, 
                   row.names=FALSE,
                   field.types=playoff.schedule.classes)
    } else dbWriteTable(mychannel, 
                        playoff.schedule.dbtable, 
                        playoff.schedule.stats, 
                        row.names=FALSE,
                        field.types=playoff.schedule.classes)
    
    rm(playoff.schedule.stats)
  }
  
}

# close database connection

dbDisconnect(mychannel)
