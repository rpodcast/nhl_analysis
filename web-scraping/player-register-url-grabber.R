#----------------------------------------------------------------------------------------------
# File: player-register-url-grabber.R
# Date: 07-06-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/web-scraping/skater-register-url-grabber.R
# Email: theRcast@gmail.com
# Purpose: Assemble and scrape valid URLs with skater and goalie register data 
#          from www.hockey-reference.com
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - code adapted from Ryan Elmore's analysis of baseball boxscore data:
#   - https://github.com/rtelmore/Pitch_Count
#   - http://www.slideshare.net/rtelmore/user-2012-talk
#
# To Do:
# - use data.tracker.team data frame and subset to just get valid urls
#
# load(file="~/hockey_workspaces/url.check.data.RData")
# team.valid <- data.tracker.team[data.tracker.team$skater.register,]
#
# 
#----------------------------------------------------------------------------------------------

# load required packages
library(XML)
library(stringr)

# define variables used in loops below

teams <- c("ANA", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", "CBJ", "DAL", "DET", "EDM", "FLA",
           "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PHX", "PIT", "SJS",
           "STL", "TBL", "TOR", "VAN", "WSH", "WPG")

base.url <- "http://www.hockey-reference.com/teams/"

# add names for goalie table columns here

skater.table.column.names <- c("rk",
                               "player",
                               "from",
                               "to",
                               "yrs",
                               "pos",
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

goalie.table.column.names <- c("rk",
                               "player",
                               "from",
                               "to",
                               "yrs",
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
                               "pim",
                               "min",
                               "g",
                               "a",
                               "pts")

coaches.table.column.names <- c("rk",
                                "coach",
                                "from",
                                "to",
                                "yrs",
                                "reg.gp",
                                "reg.w",
                                "reg.l",
                                "reg.t",
                                "reg.ol",
                                "reg.pts",
                                "reg.pts_percent",
                                "playoff.g",
                                "playoff.w",
                                "playoff.l",
                                "playoff.t",
                                "playoff.w-l_percent")

draft.table.column.names <- c("year",
                              "league",
                              "draft",
                              "round",
                              "overall",
                              "player",
                              "amateur.team",
                              "skater.gp",
                              "skater.goals",
                              "skater.assists",
                              "skater.points",
                              "skater.plus-minus",
                              "skater.pim",
                              "goalie.gp",
                              "goalie.wins",
                              "goalie.losses",
                              "goalie.t-o",
                              "goalie.save_percent",
                              "goalie.gaa")

captains.table.column.names <- c("rk",
                                 "season",
                                 "player",
                                 "scoring.gp",
                                 "scoring.goals",
                                 "scoring;assists",
                                 "scoring.points",
                                 "scoring.plus-minus",
                                 "scoring.pim",
                                 "goalie.gp",
                                 "goalie.wins",
                                 "goalie.losses",
                                 "goalie.t-o",
                                 "goalie.save_percent",
                                 "goalie.gaa")

h2h.table.column.names <- c("rk",
                            "franchise",
                            "gp",
                            "wins",
                            "losses",
                            "ties",
                            "overtime-losses",
                            "points",
                            "points_percent",
                            "goals.for",
                            "goals.against",
                            "goals.for.per.game",
                            "goals.against.per.game")

for (team in teams) { #team

    full.player.url <- paste(base.url, team, "/", "skaters.html", sep="")
    full.goalie.url <- paste(base.url, team, "/", "goalies.html", sep="")
    full.coach.url <- paste(base.url, team, "/", "coaches.html", sep="")
    full.draft.url <- paste(base.url, team, "/", "draft.html", sep="")
    full.captain.url <- paste(base.url, team, "/", "captains.html", sep="")
    full.h2h.url <- paste(base.url, team, "/", "head2head.html", sep="")
    
    out.string <- paste(Sys.time(), "--", team, sep = " ")
    print(out.string)
    cat(out.string, "\n", file="~/hockey_workspaces/register_data/register.log.txt", append=TRUE)
    
    table.player.stats <- try(readHTMLTable(full.player.url, header=TRUE, silent = TRUE))
    table.goalie.stats <- try(readHTMLTable(full.goalie.url, header=FALSE), silent = TRUE)
    table.coach.stats <- readHTMLTable(full.coach.url, header=FALSE)
    table.draft.stats <- readHTMLTable(full.draft.url, header=FALSE)
    table.captain.stats <- readHTMLTable(full.captain.url, header=FALSE)
    table.h2h.stats <- readHTMLTable(full.h2h.url, header=TRUE)
    
    if (!inherits(table.player.stats, "try-error") & !inherits(table.goalie.stats, "try-error")) {
    
      player.table.ind <- unlist(str_detect(names(table.player.stats), "skaters"))
      goalie.table.ind <- unlist(str_detect(names(table.goalie.stats), "goalies"))
      
      if (sum(player.table.ind, na.rm=TRUE) < 1 | sum(goalie.table.ind, na.rm=TRUE) < 1) next
      

      team.player.table <- table.player.stats[["skaters"]]
      team.player.table$team <- team
      
      team.goalie.table <- table.goalie.stats[["goalies"]]
      names(team.goalie.table) <- goalie.table.column.names
      team.goalie.table$team <- team
      
      # remove rows that are repeat of the header
      
      team.player.table <- team.player.table[!team.player.table$Rk=="Rk",]
      team.goalie.table <- team.goalie.table[!team.goalie.table$Rk=="Rk",] 
      
      #rm(table.player.stats, table.goalie.stats)
      
      } else {
      next
      }
     team.skater.register.data <- paste("skater.register.data.", team, sep="")
     team.goalie.register.data <- paste("goalie.register.data.", team, sep="")
     
     team.workspace.name <- str_c(team, ".player.goalie.register.RData")
     
     assign(team.skater.register.data, team.player.table)
     assign(team.goalie.register.data, team.goalie.table)
     
     save(list=c(team.skater.register.data, team.goalie.register.data),
          file=paste("~/hockey_workspaces/register_data/", team.workspace.name, sep=""))
     
     rm(list=c(team.skater.register.data, team.goalie.register.data))
     rm(team.player.table)
     rm(team.goalie.table)
}


