#----------------------------------------------------------------------------------------------
# File: hockey-reference-team-skaters-scratch.R
# Date: 07-06-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/web-scraping/hockey-reference-team-skaters-scratch.R
# Email: theRcast@gmail.com
# Purpose: Explore team schedule data from www.hockey-reference.com
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - url looks like following: http://www.hockey-reference.com/teams/DET/skaters.html
# - code adapted from Ryan Elmore's analysis of baseball boxscore data:
#   - https://github.com/rtelmore/Pitch_Count
#   - http://www.slideshare.net/rtelmore/user-2012-talk
#----------------------------------------------------------------------------------------------

# example url: http://www.hockey-reference.com/teams/DET/skaters.html
# - base url: http://www.hockey-reference.com/teams/
# - custom part: /ABC/
#   - ABC: Three-letter team abbreviation

library(XML)
library(stringr)

base.url <- "http://www.hockey-reference.com/teams/"
team <- "DET"

full.url <- paste(base.url, team, "/", "skaters.html", sep="")

table.stats <- readHTMLTable(full.url, header=TRUE)

names(table.stats)

skater.table <- table.stats[["skaters"]]

# remove records with Rk="Rk"

skater.table <- skater.table[!skater.table$Rk=="Rk",]


