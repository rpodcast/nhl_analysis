#----------------------------------------------------------------------------------------------
# File: franchise-stats-register-url-grabber.R
# Date: 04-19-2013
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/web-scraping/franchise-stats-url-grabber.R
# Email: theRcast@gmail.com
# Purpose: Assemble and scrape valid URLs with franchise season results 
#          from www.hockey-reference.com
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - code adapted from Ryan Elmore's analysis of baseball boxscore data:
#   - https://github.com/rtelmore/Pitch_Count
#   - http://www.slideshare.net/rtelmore/user-2012-talk
#
# To Do:
#
# ATLANTA (ATL) skaters table on web site does not have even strength assist, pp assits, sh assists
# http://www.hockey-reference.com/teams/ATL/skaters.html
# MTW
#----------------------------------------------------------------------------------------------

# load required packages
library(XML, lib.loc="/usr/local/lib/R/site-library")
library(stringr)
library(RMySQL)

# define debug flag
debug.set <- FALSE

# define database table names for MySQL
franchise.dbtable <- "FRANCHISE_SUMMARY"

# connect to hockey database
# assumes username and password are specified in ~/.my.cnf
mychannel <- dbConnect(MySQL(), dbname = "hockey", group="client")

# define indicator for whether to overwrite existing log with new log
log.name <- "franchise.log.txt"
overwrite.existing <- TRUE

if(overwrite.existing) {
  if(file.exists(file.path("logs", log.name))) {
    file.remove(file.path("logs", log.name))
  }
}

# define variables used in loops below

load(file="web-scraping/url.check.data.RData")

team.valid <- data.tracker.team[data.tracker.team$skater.register,]

teams <- team.valid$team

if(debug.set) {
  teams <- c("ANA")
  
  # define database table names for MySQL
  franchise.dbtable <- "FRANCHISE_SUMMARY_TEST"
  
  if(dbExistsTable(mychannel, franchise.dbtable)) {
    dbRemoveTable(conn=mychannel, name=franchise.dbtable)
  }
}

base.url <- "http://www.hockey-reference.com/teams/"

# define names for MySQL table names and column types

franchise.table.column.names <- c("season",
                               "league",
                               "team",
                                  "gp",
                                  "wins",
                                  "losses",
                                  "ties",
                                  "overtime_losses",
                                  "points",
                                  "points_percent",
                                  "srs",
                                  "sos",
                                  "finish",
                                  "playoff_notes",
                                  "coaches",
                                  "playoff_ind")

franchise.table.classes <- list(season="CHAR(100)",
                                league="CHAR(100)",
                                team="CHAR(100)",
                                gp="INT",
                                wins="INT",
                                losses="INT",
                                ties="INT",
                                overtime_losses="INT",
                                points="INT",
                                points_percent="DOUBLE(6,3)",
                                srs="DOUBLE(6,3)",
                                sos="DOUBLE(6,3)",
                                finish="CHAR(100)",
                                playoff_notes="CHAR(100)",
                                coaches="CHAR(100)",
                                playoff_ind="INT")

for (team in teams) { #team

    full.franchise.url <- paste(base.url, team, sep="")
    
    out.string <- paste(Sys.time(), "--", team, sep = " ")
    print(out.string)
    cat(out.string, "\n", file=file.path("logs", log.name), append=TRUE)
    
    table.franchise <- readHTMLTable(full.franchise.url, 
                                        #header=skater.table.column.names,
                                        header=NA,
                                        stringsAsFactors=FALSE)
    
    franchise.table <- table.franchise[[1]]
    
    franchise.table$playoff_ind <- str_detect(franchise.table$Team, "\\*")
    franchise.table$Team <- str_replace_all(franchise.table$Team, "\\*", "")
    
    if(dbExistsTable(mychannel, franchise.dbtable)) {
      dbWriteTable(mychannel, 
                   franchise.dbtable, 
                   franchise.table, 
                   append = TRUE, 
                   row.names=FALSE,
                   field.types=franchise.table.classes)
    } else dbWriteTable(mychannel, 
                        franchise.dbtable, 
                        franchise.table, 
                        row.names=FALSE,
                        field.types=franchise.table.classes)
}

# close database connection

dbDisconnect(mychannel)

