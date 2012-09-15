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

# source script to estabilish database credentials
source("~/Dropbox/rpodcast_code/nhl_analysis/lib/mysql.login.R")

# connect to hockey database
#mychannel <- dbConnect(MySQL(), user=login$user, password=login$password, dbname=login$dbname)

# define variables used in loops below

years <- c(1918:2004, 2006:2012)
#years <- 2012

base.url <- "http://www.hockey-reference.com/leagues/"

for(year in years) {
  
  full.standings.url <- str_c(base.url, "NHL_", year, "_standings.html")
  #full.schedule.url <- str_c(base.url, "NHL_", year, "_games.html")
  #full.skater.url <- str_c(base.url, "NHL_", year, "_skaters.html")
  #full.goalie.url <- str_c(base.url, "NHL_", year, "_goalies.html")
  #full.rookie.url <- str_c(base.url, "NHL_", year, "_debut.html")
  
  out.string <- paste(Sys.time(), "--", year, sep = " ")
  print(out.string)
  #cat(out.string, "\n", file="~/hockey_workspaces/season.grabber.log.txt", append=TRUE)
  
  standings.table.stats <- readHTMLTable(full.standings.url, header=FALSE)
  #schedule.table.stats <- readHTMLTable(full.schedule.url, header=FALSE)
  #skater.table.stats <- readHTMLTable(full.skater.url, header=FALSE)
  #goalie.table.stats <- readHTMLTable(full.goalie.url, header=FALSE)
  #rookie.table.stats <- readHTMLTable(full.rookie.url, header=FALSE)
  
  cat("number of columns in standings table is ", ncol(standings.table.stats[[1]]), "\n")
}