#----------------------------------------------------------------------------------------------
# File: hockey-reference-team-roster-stats-scratch.R
# Date: 07-06-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/web-scraping/hockey-reference-team-roster-stats-scratch.R
# Email: theRcast@gmail.com
# Purpose: Explore team roster statistics data from www.hockey-reference.com
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - url looks like following: http://www.hockey-reference.com/teams/LAK/1968.html
# - code adapted from Ryan Elmore's analysis of baseball boxscore data:
#   - https://github.com/rtelmore/Pitch_Count
#   - http://www.slideshare.net/rtelmore/user-2012-talk
#----------------------------------------------------------------------------------------------


# example url: http://www.hockey-reference.com/teams/LAK/1968.html
# - base url: http://www.hockey-reference.com/teams/
# - custom part: ABC/yyyy.html
#   - yyyy: year (example: 1968 corresponds to 1967-1968 season)
#   - ABC: Three-letter team abbreviation

library(XML)
library(stringr)

base.url <- "http://www.hockey-reference.com/teams/"
year <- 2012
team <- "LAK"

full.url <- paste(base.url, team, "/", year, ".html", sep="")
bad.url <- paste(base.url, "WWW", "/", year, ".html", sep="")

table.stats <- readHTMLTable(full.url, header=TRUE)

names(table.stats)

# team.stats.column.names <- c("team",
#                              "games.played",
#                              "wins",
#                              "losses",
#                              "ties",
#                              "points",
#                              "points.percentage",
#                              "goals.for",
#                              "goals.against",
#                              "srs",
#                              "sos",
#                              "goals.per.game",
#                              "pp.goals.for",
#                              "n.pp.for",
#                              "pp.percent",
#                              "pp.goals.against",
#                              "n.pp.against",
#                              "pk.percent",
#                              "goals.shorthanded.for",
#                              "goals.shorthanded.against")

team.stats <- table.stats[["team_stats"]]
team.roster <- table.stats[["roster"]]
team.skaters <- table.stats[["skaters"]]
team.goalies <- table.stats[["goalies"]]
team.skaters.playoffs <- table.stats[["skaters_playoffs"]]
team.goalies.playoffs <- table.stats[["goalies_playoffs"]]

