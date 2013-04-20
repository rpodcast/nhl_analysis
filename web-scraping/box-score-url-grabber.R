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
library(XML, lib.loc="/usr/local/lib/R/site-library")
library(stringr)
library(RMySQL)

# define debug flag
debug.set <- FALSE

# define database table names for MySQL
skater.dbtable <- "SKATER_BOXSCORE"
goalie.dbtable <- "GOALIE_BOXSCORE"

# connect to hockey database
# assumes username and password are specified in ~/.my.cnf
mychannel <- dbConnect(MySQL(), dbname = "hockey", group="client")

# define indicator for whether to overwrite existing log with new log
log.name <- "box.score.grabber.log.txt"
overwrite.existing <- TRUE

if(overwrite.existing) {
  if(file.exists(file.path("logs", log.name))) {
    file.remove(file.path("logs", log.name))
  }
}

# load box score url checker data frame
load(file="web-scraping/boxscore.url.check.data.RData")

boxscore.valid <- boxscore.tracker[boxscore.tracker$boxscore.stats, c(1:5)]
boxscore.valid <- boxscore.valid[order(boxscore.valid$team, boxscore.valid$year, boxscore.valid$month, boxscore.valid$day),]

if(debug.set) {
  boxscore.valid <- subset(boxscore.valid,
                           subset=(team %in% c("ATL", "DET") & year == "2002" & month %in% c(11, 12)))
  skater.dbtable <- "SKATER_BOXSCORE_TEST"
  goalie.dbtable <- "GOALIE_BOXSCORE_TEST"
  
  if(dbExistsTable(mychannel, skater.dbtable)) {
    dbRemoveTable(conn=mychannel, name=skater.dbtable)
    dbRemoveTable(conn=mychannel, name=goalie.dbtable)
  }
}

###################################################
######## CHECK TO SEE IF I STILL NEED TO DO THIS
# system runs out of memory after a few teams are imported, so need to subset further
# boxscore.valid <- boxscore.valid[!boxscore.valid$team %in% c("ANA", "ATL", "BOS", "BUF", "CAR", "CGY", "CHI", "COL", "DAL"),]
####################################################
rm(boxscore.tracker)

base.url <- "http://www.hockey-reference.com/boxscores/"

skater.table.column.names <- c("team",
                               "season",
                               "year",
                               "month",
                               "day",
                               "rk",
                               "player",
                               "goals",
                               "assists",
                               "points",
                               "plus_minus",
                               "pim",
                               "goals_even",
                               "goals_pp",
                               "goals_sh",
                               "shots",
                               "shooting_percent",
                               "shifts",
                               "time_on_ice",
                               "home_away_ind")

skater.table.classes <- list(team="CHAR(100)",
                             season="CHAR(20)",
                             year="INT",
                             month="INT",
                             day="INT",
                             rk="INT",
                             player="CHAR(100)",
                             goals="INT",
                             assists="INT",
                             points="INT",
                             plus_minus="INT",
                             pim="INT",
                             goals_even="INT",
                             goals_pp="INT",
                             goals_sh="INT",
                             shots="INT",
                             shooting_percent="DOUBLE(6,3)",
                             shifts="INT",
                             time_on_ice="CHAR(10)",
                             home_away_ind="CHAR(2)")

goalie.table.column.names <- c("team",
                               "season",
                               "year",
                               "month",
                               "day",
                               "rk",
                               "player",
                               "decision",
                               "ga",
                               "sa",
                               "sv",
                               "sv_percent",
                               "so",
                               "pim",
                               "min",
                               "ev_ga",
                               "pp_ga",
                               "sh_ga",
                               "en_ga",
                               "home_away_ind")

goalie.table.classes <- list(team="CHAR(100)",
                             season="CHAR(20)",
                             year="INT",
                             month="INT",
                             day="INT",
                             rk="INT",
                             player="CHAR(100)",
                             decision="CHAR(5)",
                             ga="INT",
                             sa="INT",
                             sv="INT",
                             sv_percent="DOUBLE(6,3)",
                             so="INT",
                             pim="INT",
                             min="CHAR(10)",
                             ev_ga="INT",
                             pp_ga="INT",
                             sh_ga="INT",
                             en_ga="INT",
                             home_away_ind="CHAR(2)")


for(i in 1:nrow(boxscore.valid)) {

  team <- as.character(boxscore.valid[i, "team"])
  year <- boxscore.valid[i, "year"]
  month <- boxscore.valid[i, "month"]
  season <- paste(year-1, str_sub(year, start=3, end=4), sep="-")
  
  month.url <- ifelse(str_length(month)==1,
                      paste(0, month, sep=""),
                      month)
  
  day <- boxscore.valid[i, "day"]
  
  day.url <- ifelse(str_length(day)==1,
                    paste(0, day, sep=""),
                    day)
  
  full.url <- paste(base.url, year, month.url, day.url, "0", team,".html", sep="")
  
  out.string <- paste(Sys.time(), "--", team, year, month, day, sep = " ")
  print(out.string)
  cat(out.string, "\n", file=file.path("logs", log.name), append=TRUE)
  table.stats <- try(readHTMLTable(full.url, header=FALSE), silent = TRUE)
  
  rm(out.string, full.url, day.url, month.url)
  
  if (!inherits(table.stats, "try-error")) {
    
    skater.table.ind <- unlist(str_detect(names(table.stats), "\\_skaters"))
    goalie.table.ind <- unlist(str_detect(names(table.stats), "\\_goalies"))
    
    if (sum(skater.table.ind, na.rm=TRUE) < 2 | sum(goalie.table.ind, na.rm=TRUE) < 2) next
    
    team.skater.table.names <- names(table.stats)[skater.table.ind]
    team.goalie.table.names <- names(table.stats)[goalie.table.ind]
    
    skater.home.team.ind <- str_detect(team.skater.table.names, team)
    goalie.home.team.ind <- str_detect(team.goalie.table.names, team)
    
    
    home.team.skater.table.name <- team.skater.table.names[skater.home.team.ind]
    home.team.goalie.table.name <- team.goalie.table.names[goalie.home.team.ind]
    home.team.clean <- str_replace_all(team.skater.table.names[skater.home.team.ind], "\\_skaters", "")
    
    away.team.skater.table.name <- team.skater.table.names[!skater.home.team.ind]
    away.team.goalie.table.name <- team.goalie.table.names[!goalie.home.team.ind]
    away.team.clean <- str_replace_all(team.skater.table.names[!skater.home.team.ind], "\\_skaters", "")
    
    home.skater.table <- as.data.frame(table.stats[home.team.skater.table.name])
    home.skater.table <- cbind(home.team.clean, season, year, month, day, home.skater.table)
    home.skater.table$home_away_ind <- "H"
    #home.skater.table$team <- home.team.clean
    
    away.skater.table <- as.data.frame(table.stats[away.team.skater.table.name])
    away.skater.table <- cbind(away.team.clean, season, year, month, day, away.skater.table)
    
    away.skater.table$home_away_ind <- "A"
    #away.skater.table$team <- away.team.clean
    
    names(home.skater.table) <- skater.table.column.names
    names(away.skater.table) <- skater.table.column.names
    
    home.goalie.table <- as.data.frame(table.stats[home.team.goalie.table.name])
    home.goalie.table <- cbind(home.team.clean, season, year, month, day, home.goalie.table)
    home.goalie.table$home_away_ind <- "H"
    #home.goalie.table$team <- home.team.clean
    
    away.goalie.table <- as.data.frame(table.stats[away.team.goalie.table.name])
    away.goalie.table <- cbind(away.team.clean, season, year, month, day, away.goalie.table)
    away.goalie.table$home_away_ind <- "A"
    #away.goalie.table$team <- away.team.clean         
    
    names(home.goalie.table) <- goalie.table.column.names
    names(away.goalie.table) <- goalie.table.column.names
    
    rm(table.stats)
    
    skater.table <- rbind(home.skater.table, away.skater.table)
  
    
    rm(home.skater.table, away.skater.table)
    
    if(dbExistsTable(mychannel, skater.dbtable)) {
      dbWriteTable(mychannel, 
                   skater.dbtable, 
                   skater.table, 
                   append = TRUE, 
                   row.names=FALSE,
                   field.types=skater.table.classes)
    } else dbWriteTable(mychannel, 
                        skater.dbtable, 
                        skater.table, 
                        row.names=FALSE,
                        field.types=skater.table.classes) 
    
    rm(skater.table)
    
    goalie.table <- rbind(home.goalie.table, away.goalie.table)
    
    rm(home.goalie.table, away.goalie.table)    
    
    if(dbExistsTable(mychannel, goalie.dbtable)) {
      dbWriteTable(mychannel, 
                   goalie.dbtable, 
                   goalie.table, 
                   append = TRUE, 
                   row.names=FALSE,
                   field.types=goalie.table.classes)
    } else dbWriteTable(mychannel, 
                        goalie.dbtable, 
                        goalie.table, 
                        row.names=FALSE,
                        field.types=goalie.table.classes) 
    
    rm(goalie.table)
    
    # remove other objects
    rm(skater.home.team.ind, skater.table.ind, goalie.home.team.ind, goalie.table.ind, 
       home.team.goalie.table.name, home.team.skater.table.name, away.team.clean, home.team.clean,
       away.team.goalie.table.name, away.team.skater.table.name, team, year, month, day, i,
       team.goalie.table.names, team.skater.table.names)
    gc()
    
  } else {
    next
  }
}

dbDisconnect(mychannel)
