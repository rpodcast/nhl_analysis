#----------------------------------------------------------------------------------------------
# File: reg.goal.R
# Date: 07-04-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/data/reg.goal.R
# Email: theRcast@gmail.com
# Purpose: Import NHL regular season goal data files from Hockey Summary Project
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - Raw data files can be accessed by joining the Hockey Summary Project Yahoo group:
#   - http://sports.groups.yahoo.com/group/hockey_summary_project/
#----------------------------------------------------------------------------------------------

reg.data.dir <- ifelse(.Platform$OS.type=="unix",
                       '~/Dropbox/hockey_data_files/regular_season_files/',
                       'D:/PortableApps/Dropbox/hockey_data_files/regular_season_files/')


reg.goal.files <- list.files(path=reg.data.dir, pattern="^.*-goal\\.dat", full.names=TRUE, recursive=TRUE)

reg.goal.filedata <- file.info(reg.goal.files)
filedata.sub <- reg.goal.filedata[, 1:2]
filedata.sub$filename <- row.names(filedata.sub)
rownames(filedata.sub) <- NULL

files.to.remove <- filedata.sub[filedata.sub$size==0, c("filename")]

reg.goal.files <- setdiff(reg.goal.files, files.to.remove)

#-----------------------------------------------
# Goal File
# =========
# Game ID
# Game Goal Number
# Period
# Time
# Team
# Player Name
# Scorer's Season Goal Number
# Goal Strength
#-----------------------------------------------

col.names <- c("game.id",
               "game.goal.number",
               "period",
               "time",
               "team",
               "player.name",
               "player.goal.number",
               "goal.strength")

col.classes <- rep("character", length(col.names))

############# files with issues
# 1919 (3) (missing | at end of each line)
# 1970 (29) (has n/a in the player goal number column)


library(plyr)

#reg.goal.files[-c(65,66)]
tmp <- ldply(reg.goal.files,
             file.reader,
             col.classes=col.classes,
             col.names=col.names)

#str(tmp)

# get snapshot of unique values for each variable
#lapply(tmp, function(x) sort(unique(x))[1:30])

reg.goal <- tmp[,1:length(col.classes)]

names(reg.goal) <- col.names


