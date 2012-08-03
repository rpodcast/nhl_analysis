#----------------------------------------------------------------------------------------------
# File: reg.stats.R
# Date: 07-04-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/data/reg.stats.R
# Email: theRcast@gmail.com
# Purpose: Import NHL regular season player statistics data files from Hockey Summary Project
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - Raw data files can be accessed by joining the Hockey Summary Project Yahoo group:
#   - http://sports.groups.yahoo.com/group/hockey_summary_project/
#----------------------------------------------------------------------------------------------

reg.data.dir <- ifelse(.Platform$OS.type=="unix",
                       '~/Dropbox/hockey_data_files/regular_season_files/',
                       'D:/PortableApps/Dropbox/hockey_data_files/regular_season_files/')


reg.stats.files <- list.files(path=reg.data.dir, pattern="^.*-estats\\.dat", full.names=TRUE, recursive=TRUE)

reg.stats.filedata <- file.info(reg.stats.files)
filedata.sub <- reg.stats.filedata[, 1:2]
filedata.sub$filename <- row.names(filedata.sub)
rownames(filedata.sub) <- NULL

files.to.remove <- filedata.sub[filedata.sub$size==0, c("filename")]

reg.stats.files <- setdiff(reg.stats.files, files.to.remove)


#-----------------------------------------------
# Stats File
# =========
#   Game ID
# Home/Visitor indictor ("H" or "V")
# Jersey Number
# Position
# Name
# Plus-Minus
# Time On Ice
# Number of Shifts
# Shots in period 1
# Shots in period 2
# Shots in period 3
# Shots in OT
# Total Shots
# Missed Shots
# Faceoff wins
# Faceoff losses
# Faceoff percent
# Number of penalties
# PIM
# Hits
# Takeaways
# Giveaways
# Blocked Shots
#-----------------------------------------------

col.names <- c("game.id",
               "home.away.ind",
               "jersey.number",
               "position",
               "player.name", 
               "plus.minus",
               "ice.time",
               "shifts",
               "shots.period1",
               "shots.period2",
               "shots.period3",
               "shots.ot",
               "shot.total",
               "shot.missed",
               "faceoff.wins",
               "faceoff.losses",
               "faceoff.pct",
               "penalties",
               "pim",
               "hits",
               "takeaways",
               "giveaways",
               "blocked.shots")

col.classes <- rep("character", 23)


library(plyr)

#------------------------------------------------------------------------------------------------
# files with issues:
# - many are size 0 (see file size code above): Indexes 12:22, 24:27, 29, 34, 39, 48, 51)
# - now they are removed from file list
#------------------------------------------------------------------------------------------------  

# [-c(12:22, 24:27, 29, 34, 39, 48, 51)]
tmp <- ldply(reg.stats.files,
             file.reader,
             col.classes=col.classes,
             col.names=col.names)

#str(tmp)

# get snapshot of unique values for each variable
#lapply(tmp, function(x) sort(unique(x))[1:30])

reg.stats <- tmp[,1:length(col.classes)]

names(reg.stats) <- col.names

# get snapshot of unique values for each variable
#lapply(reg.stats, function(x) sort(unique(x))[1:30])
