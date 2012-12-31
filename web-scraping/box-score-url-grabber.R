#----------------------------------------------------------------------------------------------
# File: box-score-url-grabber.R
# Date: 07-06-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/data/
# Email: theRcast@gmail.com
# Purpose: Assemble valid URLs with box score data from www.hockey-reference.com
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - url looks like following: http://www.hockey-reference.com/boxscores/201201050LAK.html
# - There is no boxscore data before 1987
# - code adapted from Ryan Elmore's analysis of baseball boxscore data:
#   - https://github.com/rtelmore/Pitch_Count
#   - http://www.slideshare.net/rtelmore/user-2012-talk
#
# TO DO:
#
# 1. Find way to import penalty summary (box scores staring with 2006)  while mapping correct 
#    period as an additional column
#
#    Possible solution: Count row indeces with populated penalty entries, since these will be the 
#    the blocks for each period.  Then use following function from StackOverflow to count 
#    adjacent runs of consecutive indeces and assign proper period to each row in the block:
#
#    lens <- rle(rows.to.extract - seq_along(rows.to.extract))$lengths 
#    block.results <- list(lengths = lens, values = unname(split(rows.to.extract, rep(seq_along(lens), lens))))

#----------------------------------------------------------------------------------------------

# load required packages
#library(XML)
library(XML, lib.loc="~/R-dev/")
library(stringr)
library(RMySQL)

# source script to estabilish database credentials
source("~/Dropbox/rpodcast_code/nhl_analysis/lib/mysql.login.R")

# connect to hockey database
mychannel <- dbConnect(MySQL(), user=login$user, password=login$password, dbname=login$dbname)

# rs <- dbSendQuery(mychannel, "select * from SKATER_REGISTER")
# 
# 
# tmp <- fetch(rs, n=10)
# 
# str(tmp)

# Coerces data.frame columns to the specified classes
colClasses <- function(d, colClasses) {
  colClasses <- rep(colClasses, len=length(d))
  d[] <- lapply(seq_along(d), function(i) switch(colClasses[i], 
                                                 numeric=as.numeric(d[[i]]), 
                                                 character=as.character(d[[i]]), 
                                                 Date=as.Date(d[[i]], origin='1970-01-01'), 
                                                 POSIXct=as.POSIXct(d[[i]], origin='1970-01-01'), 
                                                 factor=as.factor(d[[i]]),
                                                 as(d[[i]], colClasses[i]) ))
  d
}



# load box score url checker data frame
load(file="~/hockey_workspaces/boxscore.url.check.data.RData")

boxscore.valid <- boxscore.tracker[boxscore.tracker$boxscore.stats,]
boxscore.valid <- boxscore.valid[order(boxscore.valid$team, boxscore.valid$year, boxscore.valid$month, boxscore.valid$day),]

# system runs out of memory after a few teams are imported, so need to subset further
boxscore.valid <- boxscore.valid[!boxscore.valid$team %in% c("ANA", "ATL", "BOS", "BUF", "CAR", "CGY", "CHI", "COL", "DAL"),]

rm(boxscore.tracker)

base.url <- "http://www.hockey-reference.com/boxscores/"

player.table.column.names <- c("rk",
                               "player",
                               "goals",
                               "assists",
                               "points",
                               "plus.minus",
                               "pim",
                               "goals.even",
                               "goals.pp",
                               "goals.sh",
                               "shots",
                               "shooting.percent",
                               "shifts",
                               "time.on.ice",
                               "home.away.ind",
                               "team")

player.table.column.classes <- c("integer",
                                 "character",
                                 rep("numeric", 11),
                                 rep("character", 3))

goalie.table.column.names <- c("rk",
                               "player",
                               "decision",
                               "ga",
                               "sa",
                               "sv",
                               "sv.percent",
                               "so",
                               "pim",
                               "min",
                               "ev.ga",
                               "pp.ga",
                               "sh.ga",
                               "en.ga",
                               "home.away.ind",
                               "team")

goalie.table.column.classes <- c("integer",
                                 "character",
                                 "character",
                                 rep("numeric", 11),
                                 rep("character", 2))

# nrow(boxscore.valid)

for(i in 1:nrow(boxscore.valid)) {

  team <- as.character(boxscore.valid[i, "team"])
  year <- boxscore.valid[i, "year"]
  month <- boxscore.valid[i, "month"]
  
  month.url <- ifelse(str_length(month)==1,
                      paste(0, month, sep=""),
                      month)
  
  day <- boxscore.valid[i, "day"]
  
  day.url <- ifelse(str_length(day)==1,
                    paste(0, day, sep=""),
                    day)
  
  full.url <- paste(base.url, year, month.url, day.url, "0", team,".html", sep="")
  
  out.string <- paste(Sys.time(), "--", team, year, month, day, sep = " ")
  #print(out.string)
  cat(out.string, "\n", file="~/hockey_workspaces/box.score.grabber.log.txt", append=TRUE)
  table.stats <- try(readHTMLTable(full.url, header=FALSE), silent = TRUE)
  
  rm(out.string, full.url, day.url, month.url)
  
  if (!inherits(table.stats, "try-error")) {
    
    player.table.ind <- unlist(str_detect(names(table.stats), "\\_skaters"))
    goalie.table.ind <- unlist(str_detect(names(table.stats), "\\_goalies"))
    
    if (sum(player.table.ind, na.rm=TRUE) < 2 | sum(goalie.table.ind, na.rm=TRUE) < 2) next
    
    team.player.table.names <- names(table.stats)[player.table.ind]
    team.goalie.table.names <- names(table.stats)[goalie.table.ind]
    
    player.home.team.ind <- str_detect(team.player.table.names, team)
    goalie.home.team.ind <- str_detect(team.goalie.table.names, team)
    
    
    home.team.player.table.name <- team.player.table.names[player.home.team.ind]
    home.team.goalie.table.name <- team.goalie.table.names[goalie.home.team.ind]
    home.team.clean <- str_replace_all(team.player.table.names[player.home.team.ind], "\\_skaters", "")
    
    away.team.player.table.name <- team.player.table.names[!player.home.team.ind]
    away.team.goalie.table.name <- team.goalie.table.names[!goalie.home.team.ind]
    away.team.clean <- str_replace_all(team.player.table.names[!player.home.team.ind], "\\_skaters", "")
    
    home.player.table <- as.data.frame(table.stats[home.team.player.table.name])
    home.player.table$home.away.ind <- "H"
    home.player.table$team <- home.team.clean
    
    away.player.table <- as.data.frame(table.stats[away.team.player.table.name])
    away.player.table$home.away.ind <- "A"
    away.player.table$team <- away.team.clean
    
    names(home.player.table) <- player.table.column.names
    names(away.player.table) <- player.table.column.names
    
    home.goalie.table <- as.data.frame(table.stats[home.team.goalie.table.name])
    home.goalie.table$home.away.ind <- "H"
    home.goalie.table$team <- home.team.clean
    
    away.goalie.table <- as.data.frame(table.stats[away.team.goalie.table.name])
    away.goalie.table$home.away.ind <- "A"
    away.goalie.table$team <- away.team.clean         
    
    names(home.goalie.table) <- goalie.table.column.names
    names(away.goalie.table) <- goalie.table.column.names
    
    rm(table.stats)
    
    player.table <- rbind(home.player.table, away.player.table)
    
    player.table <- colClasses(player.table, player.table.column.classes)
    
    
    player.table$year <- year
    player.table$month <- month
    player.table$day <- day
    
    rm(home.player.table, away.player.table)
    
    #skater.dbtable <- str_c(team, "_SKATER_BOXSCORE")
    skater.dbtable <- "SKATER_BOXSCORE"
    
    if(dbExistsTable(mychannel, skater.dbtable)) {
      dbWriteTable(mychannel, skater.dbtable, player.table, append = T, row.names=FALSE)
    } else dbWriteTable(mychannel, skater.dbtable, player.table, row.names=FALSE) 
    
    rm(player.table, skater.dbtable)
    
    #all.player.table <- rbind(all.player.table, player.table)
    
    goalie.table <- rbind(home.goalie.table, away.goalie.table)
    
    goalie.table <- colClasses(goalie.table, goalie.table.column.classes)
    
    goalie.table$year <- year
    goalie.table$month <- month
    goalie.table$day <- day
    
    rm(home.goalie.table, away.goalie.table)
    
    
    #goalie.dbtable <- str_c(team, "_GOALIE_BOXSCORE")
    goalie.dbtable <- "GOALIE_BOXSCORE"
    
    if(dbExistsTable(mychannel, goalie.dbtable)) {
      dbWriteTable(mychannel, goalie.dbtable, goalie.table, append = T, row.names=FALSE)
    } else dbWriteTable(mychannel, goalie.dbtable, goalie.table, row.names=FALSE) 
    
    rm(goalie.table, goalie.dbtable)
    
    # remove other objects
    rm(player.home.team.ind, player.table.ind, goalie.home.team.ind, goalie.table.ind, 
       home.team.goalie.table.name, home.team.player.table.name, away.team.clean, home.team.clean,
       away.team.goalie.table.name, away.team.player.table.name, team, year, month, day, i,
       team.goalie.table.names, team.player.table.names)
    gc()
    
  } else {
    next
  }
}

dbDisconnect(mychannel)
