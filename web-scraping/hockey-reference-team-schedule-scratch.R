#----------------------------------------------------------------------------------------------
# File: hockey-reference-team-schedule-scratch.R
# Date: 07-06-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/web-scraping/hockey-reference-team-schedule-scratch.R
# Email: theRcast@gmail.com
# Purpose: Explore team schedule data from www.hockey-reference.com
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - url looks like following: http://www.hockey-reference.com/teams/LAK/1968_games.html
# - code adapted from Ryan Elmore's analysis of baseball boxscore data:
#   - https://github.com/rtelmore/Pitch_Count
#   - http://www.slideshare.net/rtelmore/user-2012-talk
#----------------------------------------------------------------------------------------------

# example url: http://www.hockey-reference.com/teams/LAK/1968_games.html
# - base url: http://www.hockey-reference.com/teams/
# - custom part: ABC/yyyy.html
#   - yyyy: year (example: 1968 corresponds to 1967-1968 season)
#   - ABC: Three-letter team abbreviation

library(XML)
library(stringr)

base.url <- "http://www.hockey-reference.com/teams/"
year <- 2012
team <- "LAK"

full.url <- paste(base.url, team, "/", year, "_games.html", sep="")
bad.url <- paste(base.url, "WWW", "/", year, ".html", sep="")

reg.season.result.column.names <- c("gp",
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

playoff.result.column.names <- c("gp",
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

table.stats <- readHTMLTable(full.url, header=TRUE)

#names(table.stats)

games.reg.season <- table.stats[["games"]]

# remove records with GP="GP"

games.reg.season <- games.reg.season[!games.reg.season$GP=="GP",]
names(games.reg.season) <- reg.season.result.column.names
games.reg.season$season <- str_c(year-1, "-", year)

games.playoffs <- table.stats[["games_playoffs"]]

# remove records with GP="GP"

games.playoffs <- games.playoffs[!games.playoffs$GP=="GP",]

names(games.playoffs) <- playoff.result.column.names
games.playoffs$season <- str_c(year-1, "-", year)





