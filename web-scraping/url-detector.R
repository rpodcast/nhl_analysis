#----------------------------------------------------------------------------------------------
# File: url-detector.R
# Date: 07-06-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/data/
# Email: theRcast@gmail.com
# Purpose: Create data frames with valid and invalid links to hockey reference data
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
#----------------------------------------------------------------------------------------------

# load required packages
library(RCurl)

# define indicator for whether to overwrite existing log with new log
log.name <- "valid.url.log.txt"
overwrite.existing <- TRUE

if(overwrite.existing) {
  if(file.exists(file.path("logs", log.name))) {
    file.remove(file.path("logs", log.name))
  }
}

# define utility function for checking if url is valid
url.checker <- function(url) {
  fn <- try(suppressWarnings(readLines(con <- url(url))), silent=TRUE)
  close(con)
  !inherits(fn, "try-error")
}

# define variables used in loops below

teams <- c("ANA", "ATF", "ATL", "BOS", "BRO", "BUF", "CGY", "CAR", "CBH", "CGS", "CHI", "CLE", "CLR", 
           "COL", "CBJ", "DAL", "DET", "DTC", "DTF", "EDM", "FLA", "HAM", "HAR", "KCS", "LAK", "MDA", 
           "MIN", "MNS", "MTL", "MTM", "MTW", "NSH", "NJD", "NYA", "NYI", "NYR", "OAK", "OTS", "OTT", 
           "PHI", "PHQ", "PHX", "PIT", "PTP", "QBC", "QUE", "SJS", "STE", "STL", "TBL", "TOR", "TRA", 
           "TRS", "VAN", "WIN", "WSH", "WPG")

#teams <- c("DET", "WAS")

years <- 1917:2012
#years <- 1998:1999

data.tracker.team.year <- expand.grid(teams, years)
names(data.tracker.team.year) <- c("team", "year")
data.tracker.team.year$team.stats <- NA
data.tracker.team.year$team.schedule <- NA

data.tracker.team <- data.frame(team=teams)
data.tracker.team$skater.register <- NA
data.tracker.team$goale.register <- NA
data.tracker.team$coach.register <- NA
data.tracker.team$draft.register <- NA
data.tracker.team$captain.register <- NA
data.tracker.team$h2h.results <- NA


base.url <- "http://www.hockey-reference.com/teams/"

for(team in teams) {
  
  out.string <- paste(Sys.time(), "--", team, sep = " ")
  print(out.string)
  
  cat(out.string, "\n", file=file.path("logs", log.name), append=TRUE)
  
  full.player.url <- paste(base.url, team, "/", "skaters.html", sep="")
  full.goalie.url <- paste(base.url, team, "/", "goalies.html", sep="")
  full.coach.url <- paste(base.url, team, "/", "coaches.html", sep="")
  full.draft.url <- paste(base.url, team, "/", "draft.html", sep="")
  full.captain.url <- paste(base.url, team, "/", "captains.html", sep="")
  full.h2h.url <- paste(base.url, team, "/", "head2head.html", sep="")
  
  full.team.urls <- c(full.player.url, full.goalie.url, full.coach.url,
                      full.draft.url, full.captain.url, full.h2h.url)
  
  #print(unlist(lapply(full.team.urls, url.checker)))
  result.vec <- unlist(lapply(full.team.urls, url.checker))
  
  data.tracker.team[data.tracker.team$team==team, 2:7] <- result.vec 
  
  rm(list=ls(pattern="full."))
  rm(result.vec)
  
  for(year in years) {
    
    out.string <- paste(Sys.time(), "--", year, sep = " ")
    print(out.string)
    cat(out.string, "\n", file=file.path("logs", log.name), append=TRUE)
    
    full.roster.url <- paste(base.url, team, "/", year, ".html", sep="")
    full.schedule.url <- paste(base.url, team, "/", year, "_games.html", sep="")
    
    full.year.urls <- c(full.roster.url, full.schedule.url)
    
    result.year.vec <- unlist(lapply(full.year.urls, url.checker))
    
    data.tracker.team.year[data.tracker.team.year$team==team & data.tracker.team.year$year==year, 3:4] <- result.year.vec 
    
    #-------------------------------------------------------------------------------
    # Issue: hockey-reference has duplicate urls for Winnipeg franchise
    # - correct version is team="WIN", incorrect version is team="WPG"
    # if team=="WPG" and year < 2012, set vector to false
    #-------------------------------------------------------------------------------
    
    if(team=="WPG" & year < 2012) {
      data.tracker.team.year[data.tracker.team.year$team==team & data.tracker.team.year$year==year, 3] <- FALSE 
      data.tracker.team.year[data.tracker.team.year$team==team & data.tracker.team.year$year==year, 4] <- FALSE 
    }
    

    
    rm(list=ls(pattern="full."))
    rm(result.year.vec)
    
  }
}

# save result into a workspace
save(data.tracker.team,
     data.tracker.team.year,
     file="web-scraping/url.check.data.RData")


