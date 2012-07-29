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
#----------------------------------------------------------------------------------------------

# load required packages
library(XML)
library(stringr)

# define variables used in loops below

teams <- c("ANA", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", "CBJ", "DAL", "DET", "EDM", "FLA",
           "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PHX", "PIT", "SJS",
           "STL", "TBL", "TOR", "VAN", "WSH", "WPG")
#teams.debug <- c("CHI", "DET", "NSH")
base.url <- "http://www.hockey-reference.com/boxscores/"
years <- 2005:2008
#years.debug <- years[1:2]
months <- c(9:12, 1:6)
#months.debug <- 11
days <- 1:31
#days.debug <- 9:13
box.score.urls <- c()

player.data.names <- str_c("player.boxscore.data.", years)
goalie.data.names <- str_c("goalie.boxscore.data.", years)

player.table.column.names <- c("index",
                               "player",
                               "goals",
                               "assists",
                               "points",
                               "plus.minus",
                               "pim",
                               "ev.goals",
                               "pp.goals",
                               "sh.goals",
                               "shots",
                               "shot.percent",
                               "shifts",
                               "ice.time",
                               "home.away.ind",
                               "team")

goalie.table.column.names <- c("index",
                               "player",
                               "decision",
                               "goals.against",
                               "shots.against",
                               "saves",
                               "save.percent",
                               "shutouts",
                               "pim",
                               "ice.time",
                               "ev.goals.against",
                               "pp.goals.against",
                               "sh.goals.against",
                               "en.goals.against",
                               "home.away.ind",
                               "team")


for (year in years) { #years
   all.player.table <- data.frame()
   all.goalie.table <- data.frame()
   
  for (month in months) { #month
    month.url <- ifelse(str_length(month)==1,
                        paste(0, month, sep=""),
                        month)
   
    for (day in days) { #day
      day.url <- ifelse(str_length(day)==1,
                        paste(0, day, sep=""),
                        day)

      for (team in teams) { #team

        full.url <- paste(base.url, year, month.url, day.url, "0", team,".html", sep="")
        out.string <- paste(Sys.time(), "--", team, year, month, day, sep = " ")
        print(out.string)
        cat(out.string, "\n", file="~/hockey_workspaces/log.txt", append=TRUE)
        table.stats <- try(readHTMLTable(full.url, header=FALSE), silent = TRUE)
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
          away.player.table$away.away.ind <- "A"
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
          player.table$year <- year
          player.table$month <- month
          player.table$day <- day
          
          all.player.table <- rbind(all.player.table, player.table)
          
          goalie.table <- rbind(home.goalie.table, away.goalie.table)
          goalie.table$year <- year
          goalie.table$month <- month
          goalie.table$day <- day
          
          all.goalie.table <- rbind(all.goalie.table, goalie.table)
          
          #box.score.urls <- c(box.score.urls, c(full.url))
        } else {
          next
        }
      }
    }
  }
   year.player.boxscore.data <- paste("player.boxscore.data.", year, sep="")
   year.goalie.boxscore.data <- paste("goalie.boxscore.data.", year, sep="")
   
   year.workspace.name <- str_c(year, ".boxscores.data.RData")
   
   assign(year.player.boxscore.data, all.player.table)
   assign(year.goalie.boxscore.data, all.goalie.table)
   
   save(list=c(year.player.boxscore.data, year.goalie.boxscore.data),
        file=paste("~/hockey_workspaces/", year.workspace.name, sep=""))
   
   rm(list=c(year.player.boxscore.data, year.goalie.boxscore.data))
   rm(all.player.table)
   rm(all.goalie.table)
}

# define name of workspace for these data files
#workspace.name <- str_c(min(years), ".", max(years), ".boxscores.data.RData")

# save(list=c(player.data.names, goalie.data.names),
#      file=paste("~/hockey_workspaces/", workspace.name, sep=""))
