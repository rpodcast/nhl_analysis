#----------------------------------------------------------------------------------------------
# File: season-stats-url-grabber.R
# Date: 09-01-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/web-scraping/season-stats-url-grabber.R
# Email: theRcast@gmail.com
# Purpose: Assemble and scrape valid URLs with season statistics data from www.hockey-reference.com
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
#----------------------------------------------------------------------------------------------

# load required packages
library(XML)
library(stringr)
library(RMySQL)

# define debug flag
debug.set <- FALSE

# define database table names for MySQL
reg.season.game.dbtable <- "REG_SEASON_GAME_RESULTS"
playoff.game.dbtable <- "PLAYOFF_GAME_RESULTS"
season.game.dbtable <- "SEASON_GAME_RESULTS"

# connect to hockey database
# assumes username and password are specified in ~/.my.cnf
mychannel <- dbConnect(MySQL(), dbname = "hockey", group="client")

# define indicator for whether to overwrite existing log with new log
log.name <- "season.result.log.txt"
overwrite.existing <- TRUE

if(overwrite.existing) {
  if(file.exists(file.path("logs", log.name))) {
    file.remove(file.path("logs", log.name))
  }
}

# define variables used in loops below

years <- c(1918:2004, 2006:2012)
#years <- 2012

if(debug.set) {
  years <- 2012

  # define database table names for MySQL
  season.game.dbtable <- "SEASON_GAME_RESULTS_TEST"
  # remove debug sets prior to running code if they exist
  if(dbExistsTable(mychannel, season.game.dbtable)) {
    dbRemoveTable(conn=mychannel, name=season.game.dbtable)
  }
}



# define names for MySQL table names and column types

reg.season.game.column.names <- c("season",
                                  "reg_playoff_ind",
                                  "date",
                                  "team_away",
                                  "goals_away",
                                  "team_home",
                                  "goals_home",
                                  "outcome_type",
                                  "notes")

reg.season.game.classes <- list(season="CHAR(50)",
                                reg_playoff_ind="CHAR(20)",
                                date="CHAR(50)",
                                team_away="CHAR(100)",
                                goals_away="INTEGER",
                                team_home="CHAR(100)",
                                goals_home="INTEGER",
                                outcome_type="CHAR(10)",
                                notes="CHAR(100)")

playoff.game.column.names <- reg.season.game.column.names
playoff.game.classes <- reg.season.game.classes

season.game.column.names <- c("season",
                              "game_id",
                              "reg_playoff_ind",
                                  "date",
                                  "team_away",
                                  "goals_away",
                                  "team_home",
                                  "goals_home",
                                  "outcome_type",
                                  "notes")

season.game.classes <- list(season="CHAR(50)",
                            game_id="CHAR(50)",
                            reg_playoff_ind="CHAR(20)",
                                date="CHAR(50)",
                                team_away="CHAR(100)",
                                goals_away="INTEGER",
                                team_home="CHAR(100)",
                                goals_home="INTEGER",
                                outcome_type="CHAR(10)",
                                notes="CHAR(100)")



base.url <- "http://www.hockey-reference.com/leagues/"

for(year in years) {
  
  season <- paste(year-1, str_sub(year, start=3, end=4), sep="-")
  
  #full.standings.url <- str_c(base.url, "NHL_", year, "_standings.html")
  full.schedule.url <- str_c(base.url, "NHL_", year, "_games.html")
  #full.skater.url <- str_c(base.url, "NHL_", year, "_skaters.html")
  #full.goalie.url <- str_c(base.url, "NHL_", year, "_goalies.html")
  #full.rookie.url <- str_c(base.url, "NHL_", year, "_debut.html")
  
  out.string <- paste(Sys.time(), "--", year, sep = " ")
  print(out.string)
  cat(out.string, "\n", file=file.path("logs", log.name), append=TRUE)
  
  #------------------------------------------------------------------------------------------
  # misc code
  #standings.table.stats <- readHTMLTable(full.standings.url, header=FALSE)
  #skater.table.stats <- readHTMLTable(full.skater.url, header=FALSE)
  #goalie.table.stats <- readHTMLTable(full.goalie.url, header=FALSE)
  #rookie.table.stats <- readHTMLTable(full.rookie.url, header=FALSE)
  #cat("number of columns in standings table is ", ncol(standings.table.stats[[1]]), "\n")
  #------------------------------------------------------------------------------------------
  
  #---------------------------------------------------------------------------
  # regular season game results table
  #---------------------------------------------------------------------------
  
 
  schedule.table.stats <- readHTMLTable(full.schedule.url, 
                                        header=FALSE,
                                        stringsAsFactors=FALSE)

  
  reg.season.game <- schedule.table.stats$games
  reg.season.game <- cbind(season, "reg_season", reg.season.game)
  names(reg.season.game) <- reg.season.game.column.names
  
#   if(dbExistsTable(mychannel, reg.season.game.dbtable)) {
#     dbWriteTable(mychannel, 
#                  reg.season.game.dbtable, 
#                  reg.season.game, 
#                  append = TRUE, 
#                  row.names=FALSE,
#                  field.types=reg.season.game.classes)
#   } else dbWriteTable(mychannel, 
#                       reg.season.game.dbtable, 
#                       reg.season.game, 
#                       row.names=FALSE,
#                       field.types=reg.season.game.classes) 
  
  #---------------------------------------------------------------------------
  # playoff game results table
  # - some earlier seasons did not have playoffs, so need to check if 2 table
  #   are in the schedule results pull
  #---------------------------------------------------------------------------
  
  if(length(names(schedule.table.stats)) > 1) {
    playoff.game <- schedule.table.stats$games_playoffs
    playoff.game <- cbind(season, "playoff", playoff.game)
    names(playoff.game) <- playoff.game.column.names
    
#     if(dbExistsTable(mychannel, playoff.game.dbtable)) {
#       dbWriteTable(mychannel, 
#                    playoff.game.dbtable, 
#                    playoff.game, 
#                    append = TRUE, 
#                    row.names=FALSE,
#                    field.types=playoff.game.classes)
#     } else dbWriteTable(mychannel, 
#                         playoff.game.dbtable, 
#                         playoff.game, 
#                         row.names=FALSE,
#                         field.types=playoff.game.classes)
    
    season.game <- rbind(reg.season.game, playoff.game)
  }
  
  #--------------------------------------------------------------------------
  # create unique game ID for integrated reg.season and playoff.game tables
  #--------------------------------------------------------------------------
  
  game_id <- paste(season, 1:nrow(season.game), sep="-")
  
  start.col <- which(names(season.game) == "season")
  season.game <- cbind(season.game[1:start.col], game_id, season.game[(start.col+1):ncol(season.game)])
  
  if(dbExistsTable(mychannel, season.game.dbtable)) {
    dbWriteTable(mychannel, 
                 season.game.dbtable, 
                 season.game, 
                 append = TRUE, 
                 row.names=FALSE,
                 field.types=season.game.classes)
  } else dbWriteTable(mychannel, 
                      season.game.dbtable, 
                      season.game, 
                      row.names=FALSE,
                      field.types=season.game.classes) 
}

# close database connection

dbDisconnect(mychannel)