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
library(stringr)

url.checker <- function(url) {
  fn <- try(suppressWarnings(readLines(con <- url(url))), silent=TRUE)
  close(con)
  !inherits(fn, "try-error")
}

#url.result <- url.checker(full.url)

# define variables used in loops below

teams <- c("ANA", "ATF", "ATL", "BOS", "BRO", "BUF", "CGY", "CAR", "CBH", "CGS", "CHI", "CLE", "CLR", 
           "COL", "CBJ", "DAL", "DET", "DTC", "DTF", "EDM", "FLA", "HAM", "HAR", "KCS", "LAK", "MDA", 
           "MIN", "MNS", "MTL", "MTM", "MTW", "NSH", "NJD", "NYA", "NYI", "NYR", "OAK", "OTS", "OTT", 
           "PHI", "PHQ", "PHX", "PIT", "PTP", "QBC", "QUE", "SJS", "STE", "STL", "TBL", "TOR", "TRA", 
           "TRS", "VAN", "WIN", "WSH", "WPG")

#teams <- c("DET", "WAS")

years <- 1987:2012
#years <- 1998:1999

months <- c(9:12, 1:6)
days <- 1:31

boxscore.tracker <- expand.grid(teams, years, months, days)
names(boxscore.tracker) <- c("team", "year", "month", "day")

boxscore.tracker$boxscore.stats <- NA
base.url <- "http://www.hockey-reference.com/boxscores/"

for(team in teams) {
  
  for(year in years) {
    
    for(month in months) {
      
      month.url <- ifelse(str_length(month)==1,
                          paste(0, month, sep=""),
                          month)
      
      for (day in days) {
        
        day.url <- ifelse(str_length(day)==1,
                          paste(0, day, sep=""),
                          day)        
        
        full.url <- paste(base.url, year, month.url, day.url, "0", team,".html", sep="")
        out.string <- paste(Sys.time(), "--", team, year, month, day, sep = " ")
        print(out.string)
        cat(out.string, "\n", file="~/hockey_workspaces/boxscore.url.log.txt", append=TRUE)
        
        result.vec <- unlist(lapply(full.url, url.checker))
        
        boxscore.tracker[boxscore.tracker$team==team & boxscore.tracker$year==year & boxscore.tracker$month==month & boxscore.tracker$day==day, 5] <- result.vec 
        
        boxscore.tracker$skater.stats <- NA
        boxscore.tracker$goalie.stats <- NA
        
        rm(list=ls(pattern="full."))
        rm(result.vec)
        
      }
    }
    
  }
}

# save result into a workspace
save(boxscore.tracker,
     file="~/hockey_workspaces/boxscore.url.check.data.RData")


