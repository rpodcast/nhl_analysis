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

goalie.table.column.names <- c("Rk",
                               "Player",
                               "From",
                               "To",
                               "Yrs",
                               "GP",
                               "W",
                               "L",
                               "T-O",
                               "GA",
                               "SA",
                               "SV",
                               "SV_percent",
                               "GAA",
                               "SO",
                               "PIM",
                               "MIN",
                               "G",
                               "A",
                               "PTS")

for (team in teams) { #team

    full.player.url <- paste(base.url, team, "/", "skaters.html", sep="")
    full.goalie.url <- paste(base.url, team, "/", "goalies.html", sep="")
    
    out.string <- paste(Sys.time(), "--", team, sep = " ")
    print(out.string)
    cat(out.string, "\n", file="~/hockey_workspaces/register_data/register.log.txt", append=TRUE)
    
    table.player.stats <- try(readHTMLTable(full.player.url, header=TRUE, silent = TRUE))
    table.goalie.stats <- try(readHTMLTable(full.goalie.url, header=FALSE), silent = TRUE)
    
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


