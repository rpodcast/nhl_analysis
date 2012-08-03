#----------------------------------------------------------------------------------------------
# File: reg.period.R
# Date: 07-04-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/data/reg.period.R
# Email: theRcast@gmail.com
# Purpose: Import NHL regular season period data files from Hockey Summary Project
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - Raw data files can be accessed by joining the Hockey Summary Project Yahoo group:
#   - http://sports.groups.yahoo.com/group/hockey_summary_project/
#----------------------------------------------------------------------------------------------

reg.data.dir <- ifelse(.Platform$OS.type=="unix",
                       '~/Dropbox/hockey_data_files/regular_season_files/',
                       'D:/PortableApps/Dropbox/hockey_data_files/regular_season_files/')


reg.period.files <- list.files(path=reg.data.dir, pattern="^.*-period\\.dat", full.names=TRUE, recursive=TRUE)

reg.period.filedata <- file.info(reg.period.files)
filedata.sub <- reg.period.filedata[, 1:2]
filedata.sub$filename <- row.names(filedata.sub)
rownames(filedata.sub) <- NULL

files.to.remove <- filedata.sub[filedata.sub$size==0, c("filename")]

reg.period.files <- setdiff(reg.period.files, files.to.remove)


#-----------------------------------------------
# Period File
# ============
#   Game ID
# Team
# Home/Visitor indictor ("H" or "V")
# Period
# Number of Goals
# Shots on Goal
# Number of Penalties
# Minutes in Penalties
# Powerplay Goals
# Powerplay Opportunities
# Offensive Zone Time
# Defensive Zone Time
# Neutral Ice Zone Time
#-----------------------------------------------

col.names <- c("game.id",
               "team",
               "home.away.ind",
               "period",
               "goals",
               "shots",
               "penalties",
               "pim",
               "pp.goals",
               "pp.opp",
               "off.time",
               "def.time",
               "neutral.time")

col.classes <- rep("character", length(col.names))


library(plyr)

#------------------------------------------------------------------------------------------------
# files with issues:
# - files that are size 0 (see file size code above): Indexes 1,4 (now they are removed from file list)
#------------------------------------------------------------------------------------------------  

tmp <- ldply(reg.period.files,
             file.reader,
             col.classes=col.classes,
             col.names=col.names)

#str(tmp)

# get snapshot of unique values for each variable
#lapply(tmp, function(x) sort(unique(x))[1:30])

reg.period <- tmp[,1:length(col.classes)]

names(reg.period) <- col.names


