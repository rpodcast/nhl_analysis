#----------------------------------------------------------------------------------------------
# File: team-stats-register-url-grabber.R
# Date: 08-09-2012
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
#
# load(file="~/hockey_workspaces/url.check.data.RData")
# team.valid <- data.tracker.team[data.tracker.team$skater.register,]
#
# 
#----------------------------------------------------------------------------------------------

# load required packages
library(XML)
library(stringr)
library(RMySQL)

# source script to estabilish database credentials
source("lib/mysql.login.R")

# connect to hockey database
mychannel <- dbConnect(MySQL(), user=login$user, password=login$password, dbname=login$dbname)

# define variables used in loops below

load(file="~/hockey_workspaces/url.check.data.RData")

team.valid <- data.tracker.team[data.tracker.team$skater.register,]

teams <- team.valid$team
#teams <- c("DET")

base.url <- "http://www.hockey-reference.com/teams/"

# add names for table columns here

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
                               "plus.minus",
                               "pim",
                               "goals.even",
                               "goals.pp",
                               "goals.sh",
                               "goals.gw",
                               "shots",
                               "shooting.percent",
                               "time.on.ice",
                               "avg.time.on.ice")

skater.colclasses <- c("integer",
                       "character", 
                       rep("integer", 3),
                       "character", 
                       rep("integer", 12), 
                       "numeric", 
                       "integer", 
                       "character")

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
                               "sv.percent",
                               "gaa",
                               "so",
                               "pim",
                               "min",
                               "g",
                               "a",
                               "pts")

goalie.colclasses <- c("integer", 
                       "character", 
                       rep("integer", 10),
                       rep("numeric", 2),
                       rep("integer", 6))

coach.table.column.names <- c("rk",
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
                                "reg.pts.percent",
                                "playoff.g",
                                "playoff.w",
                                "playoff.l",
                                "playoff.t",
                                "playoff.w.l.percent")

coach.colclasses <- c("integer", 
                      "character", 
                      rep("integer", 9), 
                      "numeric", 
                      rep("integer", 4), 
                      "numeric")

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
                              "skater.plus.minus",
                              "skater.pim",
                              "goalie.gp",
                              "goalie.wins",
                              "goalie.losses",
                              "goalie.t.o",
                              "goalie.save.percent",
                              "goalie.gaa")

draft.colclasses <- c("integer", 
                      rep("character", 2),
                      rep("integer", 2), 
                      rep("character", 2),
                      rep("integer", 10),
                      rep("numeric", 2))

captain.table.column.names <- c("rk",
                                 "season",
                                 "player",
                                 "scoring.gp",
                                 "scoring.goals",
                                 "scoring;assists",
                                 "scoring.points",
                                 "scoring.plus.minus",
                                 "scoring.pim",
                                 "goalie.gp",
                                 "goalie.wins",
                                 "goalie.losses",
                                 "goalie.t.o",
                                 "goalie.save.percent",
                                 "goalie.gaa")
captain.colclasses <- c("integer", 
                        rep("character", 2), 
                        rep("integer", 10),
                        rep("numeric", 2))

h2h.table.column.names <- c("rk",
                            "franchise",
                            "gp",
                            "wins",
                            "losses",
                            "ties",
                            "overtime.losses",
                            "points",
                            "points.percent",
                            "goals.for",
                            "goals.against",
                            "goals.for.per.game",
                            "goals.against.per.game")

h2h.colclasses <- c("integer", 
                    "character",
                    rep("integer", 6),
                    "numeric",
                    rep("integer", 2),
                    rep("numeric", 2))

for (team in teams) { #team

    full.skater.url <- paste(base.url, team, "/", "skaters.html", sep="")
    full.goalie.url <- paste(base.url, team, "/", "goalies.html", sep="")
    full.coach.url <- paste(base.url, team, "/", "coaches.html", sep="")
    full.draft.url <- paste(base.url, team, "/", "draft.html", sep="")
    full.captain.url <- paste(base.url, team, "/", "captains.html", sep="")
    full.h2h.url <- paste(base.url, team, "/", "head2head.html", sep="")
    
    out.string <- paste(Sys.time(), "--", team, sep = " ")
    print(out.string)
    cat(out.string, "\n", file="~/hockey_workspaces/register_data/register.log.txt", append=TRUE)
    
    table.skater.stats <- readHTMLTable(full.skater.url, 
                                        header=skater.table.column.names,
                                        colClasses=skater.colclasses,
                                        stringsAsFactors=FALSE)
    
    table.goalie.stats <- readHTMLTable(full.goalie.url,
                                        header=goalie.table.column.names,
                                        colClasses=goalie.colclasses,
                                        stringsAsFactors=FALSE)
    
    table.coach.stats <- readHTMLTable(full.coach.url, 
                                       header=coach.table.column.names,
                                       colClasses=coach.colclasses,
                                       stringsAsFactors=FALSE)
    
    table.draft.stats <- readHTMLTable(full.draft.url, 
                                       header=draft.table.column.names,
                                       colClasses=draft.colclasses,
                                       stringsAsFactors=FALSE)
    
    table.captain.stats <- readHTMLTable(full.captain.url, 
                                         header=captain.table.column.names,
                                         colClasses=captain.colclasses,
                                         stringsAsFactors=FALSE)
    
    table.h2h.stats <- readHTMLTable(full.h2h.url,
                                     header=h2h.table.column.names,
                                     colClasses=h2h.colclasses,
                                     stringsAsFactors=FALSE)
    
    
    #if (!inherits(table.skater.stats, "try-error") & !inherits(table.goalie.stats, "try-error")) {
    
      skater.table.ind <- unlist(str_detect(names(table.skater.stats), "skaters"))
      goalie.table.ind <- unlist(str_detect(names(table.goalie.stats), "goalies"))
      coach.table.ind <- unlist(str_detect(names(table.coach.stats), "coaches"))
      draft.table.ind <- unlist(str_detect(names(table.draft.stats), "stats"))
      captain.table.ind <- unlist(str_detect(names(table.captain.stats), "captains"))
      h2h.table.ind <- unlist(str_detect(names(table.h2h.stats), "head2head"))
      
      #if (sum(skater.table.ind, na.rm=TRUE) < 1 | sum(goalie.table.ind, na.rm=TRUE) < 1) next
      
      if (sum(skater.table.ind, na.rm=TRUE) == 1) {
        team.skater.table <- table.skater.stats[["skaters"]]
        names(team.skater.table) <- skater.table.column.names
        team.skater.table$team <- team
        team.skater.table <- team.skater.table[!team.skater.table$rk=="Rk",]
        team.skater.table <- team.skater.table[!is.na(team.skater.table$rk),]
        
        if(dbExistsTable(mychannel, "SKATER_REGISTER")) {
          dbWriteTable(mychannel, "SKATER_REGISTER", team.skater.table, append = T, row.names=FALSE)
        } else dbWriteTable(mychannel, "SKATER_REGISTER", team.skater.table, row.names=FALSE)  
      }
      else team.skater.table <- NULL
    
    
    if (sum(goalie.table.ind, na.rm=TRUE) == 1) {
      team.goalie.table <- table.goalie.stats[["goalies"]]
      names(team.goalie.table) <- goalie.table.column.names
      team.goalie.table$team <- team
      team.goalie.table <- team.goalie.table[!team.goalie.table$rk %in% c("Rk", ""),]
      team.goalie.table <- team.goalie.table[!is.na(team.goalie.table$rk),]
      
      if(dbExistsTable(mychannel, "GOALIE_REGISTER")) {
        dbWriteTable(mychannel, "GOALIE_REGISTER", team.goalie.table, append = T, row.names=FALSE)
      }  else dbWriteTable(mychannel, "GOALIE_REGISTER", team.goalie.table, row.names=FALSE)
    }
    else team.goalie.table <- NULL
    
    if (sum(coach.table.ind, na.rm=TRUE) == 1) {  
      team.coach.table <- table.coach.stats[["coaches"]]
      names(team.coach.table) <- coach.table.column.names
      team.coach.table$team <- team
      team.coach.table <- team.coach.table[!team.coach.table$rk=="Rk",]
      team.coach.table <- team.coach.table[!team.coach.table$coach=="Regular Season",]
      team.coach.table <- team.coach.table[!is.na(team.coach.table$rk),]
      
      if(dbExistsTable(mychannel, "COACH_REGISTER")) {
        dbWriteTable(mychannel, "COACH_REGISTER", team.coach.table, append = T, row.names=FALSE)
      } else dbWriteTable(mychannel, "COACH_REGISTER", team.coach.table, row.names=FALSE)
    }
    else team.coach.table <- NULL
    
    if (sum(draft.table.ind, na.rm=TRUE) == 1) { 
      team.draft.table <- table.draft.stats[["stats"]]
      names(team.draft.table) <- draft.table.column.names
      team.draft.table$team <- team
      team.draft.table <- team.draft.table[!team.draft.table$year=="Year",]
      team.draft.table <- team.draft.table[!is.na(team.draft.table$year),]
      
      if(dbExistsTable(mychannel, "DRAFT_REGISTER")) {
        dbWriteTable(mychannel, "DRAFT_REGISTER", team.draft.table, append = T, row.names=FALSE)
      } else dbWriteTable(mychannel, "DRAFT_REGISTER", team.draft.table, row.names=FALSE)
    }
    else team.draft.table <- NULL
    
    if (sum(captain.table.ind, na.rm=TRUE) == 1) { 
      team.captain.table <- table.captain.stats[["captains"]]
      names(team.captain.table) <- captain.table.column.names
      team.captain.table$team <- team
      
      if(dbExistsTable(mychannel, "CAPTAIN_REGISTER")) {
        dbWriteTable(mychannel, "CAPTAIN_REGISTER", team.captain.table, append = T, row.names=FALSE)
      } else dbWriteTable(mychannel, "CAPTAIN_REGISTER", team.captain.table, row.names=FALSE)
    }
    else team.captain.table <- NULL
    
    if (sum(h2h.table.ind, na.rm=TRUE) == 1) { 
      team.h2h.table <- table.h2h.stats[["head2head"]]
      names(team.h2h.table) <- h2h.table.column.names
      team.h2h.table$team <- team
      
      if(dbExistsTable(mychannel, "H2H_REGISTER")) {
        dbWriteTable(mychannel, "H2H_REGISTER", team.h2h.table, append = T, row.names=FALSE)
      } else dbWriteTable(mychannel, "H2H_REGISTER", team.h2h.table, row.names=FALSE)
    }
    else team.h2h.table <- NULL
    

     team.skater.register.data <- paste("skater.register.data.", team, sep="")
     team.goalie.register.data <- paste("goalie.register.data.", team, sep="")   
     team.coach.register.data <-  paste("coach.register.data.", team, sep="")
    team.draft.register.data <-  paste("draft.register.data.", team, sep="")
    team.captain.register.data <-  paste("captain.register.data.", team, sep="")
    team.h2h.register.data <-  paste("h2h.register.data.", team, sep="")
    
     
    team.workspace.name <- str_c(team, ".stats.register.RData")
     
    assign(team.skater.register.data, team.skater.table)
    assign(team.goalie.register.data, team.goalie.table)
    assign(team.coach.register.data, team.coach.table)
    assign(team.draft.register.data, team.draft.table)
    assign(team.captain.register.data, team.captain.table)
    assign(team.h2h.register.data, team.h2h.table)
     
     save(list=c(team.skater.register.data,
                 team.goalie.register.data,
                 team.coach.register.data,
                 team.draft.register.data,
                 team.captain.register.data,
                 team.h2h.register.data),
          file=paste("~/hockey_workspaces/register_data/", team.workspace.name, sep=""))
     
     rm(list=c(team.skater.register.data,
               team.goalie.register.data,
               team.coach.register.data,
               team.draft.register.data,
               team.captain.register.data,
               team.h2h.register.data))
     rm(team.skater.table)
     rm(team.goalie.table)
    rm(team.coach.table)
    rm(team.draft.table)
    rm(team.captain.table)
    rm(team.h2h.table)
}

# close database connection

dbDisconnect(mychannel)

