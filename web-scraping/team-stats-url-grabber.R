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
skater.dbtable <- "SKATER_REGISTER"
goalie.dbtable <- "GOALIE_REGISTER"
coach.dbtable <- "COACH_REGISTER"
draft.dbtable <- "DRAFT_REGISTER"
captain.dbtable <- "CAPTAIN_REGISTER"
h2h.dbtable <- "H2H_REGISTER"

# connect to hockey database
# assumes username and password are specified in ~/.my.cnf
mychannel <- dbConnect(MySQL(), dbname = "hockey", group="client")

# define indicator for whether to overwrite existing log with new log
log.name <- "register.log.txt"
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
  teams <- c("ANA", "ATL")
  
  # define database table names for MySQL
  skater.dbtable <- "SKATER_REGISTER_TEST"
  goalie.dbtable <- "GOALIE_REGISTER_TEST"
  coach.dbtable <- "COACH_REGISTER_TEST"
  draft.dbtable <- "DRAFT_REGISTER_TEST"
  captain.dbtable <- "CAPTAIN_REGISTER_TEST"
  h2h.dbtable <- "H2H_REGISTER_TEST"
  
  if(dbExistsTable(mychannel, skater.dbtable)) {
    dbRemoveTable(conn=mychannel, name=skater.dbtable)
    dbRemoveTable(conn=mychannel, name=goalie.dbtable)
    dbRemoveTable(conn=mychannel, name=coach.dbtable)
    dbRemoveTable(conn=mychannel, name=draft.dbtable)
    dbRemoveTable(conn=mychannel, name=captain.dbtable)
    dbRemoveTable(conn=mychannel, name=h2h.dbtable)
  }
}

base.url <- "http://www.hockey-reference.com/teams/"

# define names for MySQL table names and column types

skater.table.column.names <- c("team",
                               "rk",
                               "player",
                               "from",
                               "to",
                               "yrs",
                               "pos",
                               "gp",
                               "goals",
                               "assists",
                               "points",
                               "goals_created",
                               "plus_minus",
                               "pim",
                               "goals_even",
                               "goals_pp",
                               "goals_sh",
                               "goals_gw",
                               "assists_even",
                               "assists_sh",
                               "assists_pp",
                               "shots",
                               "shooting_percent",
                               "time_on_ice",
                               "avg_time_on_ice")

skater.table.classes <- list(team="CHAR(100)",
                             rk="INT",
                             player="CHAR(100)",
                             from="INT",
                             to="INT",
                             yrs="INT",
                             pos="CHAR(5)",
                             gp="INT",
                             goals="INT",
                             assists="INT",
                             points="INT",
                             goals_created="INT",
                             plus_minus="INT",
                             pim="INT",
                             goals_even="INT",
                             goals_pp="INT",
                             goals_sh="INT",
                             goals_gw="INT",
                             assists_even="INT",
                             assists_sh="INT",
                             assists_pp="INT",
                             shots="INT",
                             shooting_percent="DOUBLE(6,3)",
                             time_on_ice="INT",
                             avg_time_on_ice="CHAR(10)")


goalie.table.column.names <- c("team",
                               "rk",
                               "player",
                               "from",
                               "to",
                               "yrs",
                               "gp",
                               "w",
                               "l",
                               "tot",
                               "ga",
                               "sa",
                               "sv",
                               "sv_percent",
                               "gaa",
                               "so",
                               "min",
                               "goals",
                               "assists",
                               "points")

goalie.table.classes <- list(team="CHAR(100)",
                             rk="INT",
                             player="CHAR(100)",
                             from="INT",
                             to="INT",
                             yrs="INT",
                             gp="INT",
                             w="INT",
                             l="INT",
                             tot="INT",
                             ga="INT",
                             sa="INT",
                             sv="INT",
                             sv_percent="DOUBLE(6,3)",
                             gaa="DOUBLE(6,3)",
                             so="INT",
                             min="INT",
                             goals="INT",
                             assists="INT",
                             points="INT")


coach.table.column.names <- c("team",
                              "rk",
                                "coach",
                                "from",
                                "to",
                                "yrs",
                                "reg_gp",
                                "reg_w",
                                "reg_l",
                                "reg_t",
                                "reg_ol",
                                "reg_points",
                                "reg_points_percent",
                                "playoff_g",
                                "playoff_w",
                                "playoff_l",
                                "playoff_t",
                                "playoff_w_l_percent")

coach.table.classes <- list(team="CHAR(100)",
                            rk="INT",
                            coach="CHAR(100)",
                            from="INT",
                            to="INT",
                            yrs="INT",
                            reg_gp="INT",
                            reg_w="INT",
                            reg_l="INT",
                            reg_t="INT",
                            reg_ol="INT",
                            reg_points="INT",
                            reg_points_percent="DOUBLE(6,3)",
                            playoff_g="INT",
                            playoff_w="INT",
                            playoff_l="INT",
                            playoff_t="INT",
                            playoff_w_l_percent="DOUBLE(6,3)")


draft.table.column.names <- c("team",
                              "year",
                              "league",
                              "draft",
                              "round",
                              "overall",
                              "player",
                              "amateur_team",
                              "skater_gp",
                              "skater_goals",
                              "skater_assists",
                              "skater_points",
                              "skater_plus_minus",
                              "skater_pim",
                              "goalie_gp",
                              "goalie_wins",
                              "goalie_losses",
                              "goalie_tot",
                              "goalie_save_percent",
                              "goalie_gaa")

draft.table.classes <- list(team="CHAR(100)",
                            year="INT",
                            league="CHAR(20)",
                            draft="CHAR(20)",
                            round="INT",
                            overall="INT",
                            player="CHAR(100)",
                            amateur_team="CHAR(100)",
                            skater_gp="INT",
                            skater_goals="INT",
                            skater_assists="INT",
                            skater_points="INT",
                            skater_plus_minus="INT",
                            skater_pim="INT",
                            goalie_gp="INT",
                            goalie_wins="INT",
                            goalie_losses="INT",
                            goalie_tot="INT",
                            goalie_save_percent="DOUBLE(6,3)",
                            goalie_gaa="DOUBLE(6,3)")


captain.table.column.names <- c("team",
                                "rk",
                                 "season",
                                 "player",
                                 "skater_gp",
                                 "skater_goals",
                                 "skater_assists",
                                 "skater_points",
                                 "skater_plus_minus",
                                 "skater_pim",
                                 "goalie_gp",
                                 "goalie_wins",
                                 "goalie_losses",
                                 "goalie_tot",
                                 "goalie_save_percent",
                                 "goalie_gaa")

captain.table.classes <- list(team="CHAR(100)",
                              rk="INT",
                              season="CHAR(20)",
                              player="CHAR(100)",
                              scoring_gp="INT",
                              scoring_goals="INT",
                              scoring_assists="INT",
                              scoring_points="INT",
                              scoring_plus_minus="INT",
                              scoring_pim="INT",
                              goalie_gp="INT",
                              goalie_wins="INT",
                              goalie_losses="INT",
                              goalie_tot="INT",
                              goalie_save_percent="DOUBLE(6,3)",
                              goalie_gaa="DOUBLE(6,3)")


h2h.table.column.names <- c("team",
                            "rk",
                            "opponent",
                            "gp",
                            "wins",
                            "losses",
                            "ties",
                            "overtime_losses",
                            "points",
                            "points_percent",
                            "goals_for",
                            "goals_against",
                            "goals_for_per_game",
                            "goals_against_per_game")

h2h.table.classes <- list(team="CHAR(100)",
                          rk="INT",
                          opponent="CHAR(100)",
                          gp="INT",
                          wins="INT",
                          losses="INT",
                          ties="INT",
                          overtime_losses="INT",
                          points="INT",
                          points_percent="DOUBLE(6,3)",
                          goals_for="INT",
                          goals_against="INT",
                          goals_for_per_game="DOUBLE(6,3)",
                          goals_against_per_game="DOUBLE(6,3)")

for (team in teams) { #team

    full.skater.url <- paste(base.url, team, "/", "skaters.html", sep="")
    full.goalie.url <- paste(base.url, team, "/", "goalies.html", sep="")
    full.coach.url <- paste(base.url, team, "/", "coaches.html", sep="")
    full.draft.url <- paste(base.url, team, "/", "draft.html", sep="")
    full.captain.url <- paste(base.url, team, "/", "captains.html", sep="")
    full.h2h.url <- paste(base.url, team, "/", "head2head.html", sep="")
    
    out.string <- paste(Sys.time(), "--", team, sep = " ")
    print(out.string)
    cat(out.string, "\n", file=file.path("logs", log.name), append=TRUE)
    
    table.skater.stats <- readHTMLTable(full.skater.url, 
                                        #header=skater.table.column.names,
                                        header=NA,
                                        #colClasses=skater.colclasses,
                                        stringsAsFactors=FALSE)
    
    table.goalie.stats <- readHTMLTable(full.goalie.url,
                                        #header=goalie.table.column.names,
                                        header=NA,
                                        #colClasses=goalie.colclasses,
                                        stringsAsFactors=FALSE)
    
    table.coach.stats <- readHTMLTable(full.coach.url, 
                                       #header=coach.table.column.names,
                                       header=NA,
                                       #colClasses=coach.colclasses,
                                       stringsAsFactors=FALSE)
    
    table.draft.stats <- readHTMLTable(full.draft.url, 
                                       #header=draft.table.column.names,
                                       header=NA,
                                       #colClasses=draft.colclasses,
                                       stringsAsFactors=FALSE)
    
    table.captain.stats <- readHTMLTable(full.captain.url, 
                                         #header=captain.table.column.names,
                                         header=NA,
                                         #colClasses=captain.colclasses,
                                         stringsAsFactors=FALSE)
    
    table.h2h.stats <- readHTMLTable(full.h2h.url,
                                     #header=h2h.table.column.names,
                                     header=NA,
                                     #colClasses=h2h.colclasses,
                                     stringsAsFactors=FALSE)
    
    
    skater.table.ind <- unlist(str_detect(names(table.skater.stats), "skaters"))
    goalie.table.ind <- unlist(str_detect(names(table.goalie.stats), "goalies"))
    coach.table.ind <- unlist(str_detect(names(table.coach.stats), "coaches"))
    draft.table.ind <- unlist(str_detect(names(table.draft.stats), "stats"))
    captain.table.ind <- unlist(str_detect(names(table.captain.stats), "captains"))
    h2h.table.ind <- unlist(str_detect(names(table.h2h.stats), "head2head"))
      
      
    if (sum(skater.table.ind, na.rm=TRUE) == 1) {
      team.skater.table <- table.skater.stats[["skaters"]]
      
      # check if assist column names are present in the imported skater stat table
      # if not, then need to remove them from the column names
      
      if(names(team.skater.table)[18] != "EV" & names(team.skater.table)[19] != "SH" & names(team.skater.table)[20] != "PP") {       
        start.window <- which(names(team.skater.table) == "GW")         
        team.skater.table <- cbind(team, team.skater.table[1:start.window], NA, NA, NA, team.skater.table[(start.window+1):ncol(team.skater.table)])
      } else {
        team.skater.table <- cbind(team, team.skater.table)
      }
      
      names(team.skater.table) <- skater.table.column.names
      #team.skater.table$team <- team
      team.skater.table <- team.skater.table[!team.skater.table$rk=="Rk",]
      team.skater.table <- team.skater.table[!is.na(team.skater.table$rk),]
      
      if(dbExistsTable(mychannel, skater.dbtable)) {
        dbWriteTable(mychannel,
                     skater.dbtable,
                     team.skater.table,
                     append = TRUE, 
                     row.names=FALSE,
                     field.types=skater.table.classes)
      } else dbWriteTable(mychannel, 
                          skater.dbtable,
                          team.skater.table, 
                          row.names=FALSE,
                          field.types=skater.table.classes)  
    }
    else team.skater.table <- NULL
    
    
    if (sum(goalie.table.ind, na.rm=TRUE) == 1) {
      team.goalie.table <- table.goalie.stats[["goalies"]]
      team.goalie.table <- cbind(team, team.goalie.table)
      names(team.goalie.table) <- goalie.table.column.names
      #team.goalie.table$team <- team
      team.goalie.table <- team.goalie.table[!team.goalie.table$rk %in% c("Rk", ""),]
      team.goalie.table <- team.goalie.table[!is.na(team.goalie.table$rk),]
      
      if(dbExistsTable(mychannel, goalie.dbtable)) {
        dbWriteTable(mychannel, goalie.dbtable, 
                     team.goalie.table, 
                     append = TRUE, 
                     row.names=FALSE,
                     field.types=goalie.table.classes)
      }  else dbWriteTable(mychannel, 
                           goalie.dbtable, 
                           team.goalie.table, 
                           row.names=FALSE,
                           field.types=goalie.table.classes)
    }
    else team.goalie.table <- NULL
    
    if (sum(coach.table.ind, na.rm=TRUE) == 1) {  
      team.coach.table <- table.coach.stats[["coaches"]]
      team.coach.table <- cbind(team, team.coach.table)
      names(team.coach.table) <- coach.table.column.names
      #team.coach.table$team <- team
      team.coach.table <- team.coach.table[!team.coach.table$rk=="Rk",]
      team.coach.table <- team.coach.table[!team.coach.table$coach=="Regular Season",]
      team.coach.table <- team.coach.table[!is.na(team.coach.table$rk),]
      
      if(dbExistsTable(mychannel, coach.dbtable)) {
        dbWriteTable(mychannel, 
                     coach.dbtable, 
                     team.coach.table, 
                     append = TRUE,
                     row.names=FALSE,
                     field.types=coach.table.classes)
      } else dbWriteTable(mychannel, 
                          coach.dbtable,
                          team.coach.table,
                          row.names=FALSE,
                          field.types=coach.table.classes)
    }
    else team.coach.table <- NULL
    
    if (sum(draft.table.ind, na.rm=TRUE) == 1) { 
      team.draft.table <- table.draft.stats[["stats"]]
      team.draft.table <- cbind(team, team.draft.table)
      names(team.draft.table) <- draft.table.column.names
      #team.draft.table$team <- team
      team.draft.table <- team.draft.table[!team.draft.table$year=="Year",]
      team.draft.table <- team.draft.table[!is.na(team.draft.table$year),]
      
      if(dbExistsTable(mychannel, draft.dbtable )) {
        dbWriteTable(mychannel, 
                     draft.dbtable , 
                     team.draft.table, 
                     append = TRUE, 
                     row.names=FALSE,
                     field.types=draft.table.classes)
      } else dbWriteTable(mychannel, 
                          draft.dbtable , 
                          team.draft.table,
                          row.names=FALSE,
                          field.types=draft.table.classes)
    }
    else team.draft.table <- NULL
    
    if (sum(captain.table.ind, na.rm=TRUE) == 1) { 
      team.captain.table <- table.captain.stats[["captains"]]
      team.captain.table <- cbind(team, team.captain.table)
      names(team.captain.table) <- captain.table.column.names
      #team.captain.table$team <- team
      
      if(dbExistsTable(mychannel, captain.dbtable)) {
        dbWriteTable(mychannel, 
                     captain.dbtable, 
                     team.captain.table, 
                     append = TRUE, 
                     row.names=FALSE,
                     field.types=captain.table.classes)
      } else dbWriteTable(mychannel, 
                          captain.dbtable,
                          team.captain.table, 
                          row.names=FALSE,
                          field.types=captain.table.classes)
    }
    else team.captain.table <- NULL
    
    if (sum(h2h.table.ind, na.rm=TRUE) == 1) { 
      team.h2h.table <- table.h2h.stats[["head2head"]]
      team.h2h.table <- cbind(team, team.h2h.table)
      names(team.h2h.table) <- h2h.table.column.names
      #team.h2h.table$team <- team
      
      if(dbExistsTable(mychannel, h2h.dbtable)) {
        dbWriteTable(mychannel,
                     h2h.dbtable, 
                     team.h2h.table,
                     append = TRUE,
                     row.names=FALSE,
                     field.types=h2h.table.classes)
      } else dbWriteTable(mychannel, 
                          h2h.dbtable, 
                          team.h2h.table, 
                          row.names=FALSE,
                          field.types=h2h.table.classes)
    }
    else team.h2h.table <- NULL
    
    # clean up workspace
    
   rm(team.skater.table)
   rm(team.goalie.table)
    rm(team.coach.table)
    rm(team.draft.table)
    rm(team.captain.table)
    rm(team.h2h.table)
}

# close database connection

dbDisconnect(mychannel)

