#----------------------------------------------------------------------------------------------
# File: reg.assist.R
# Date: 07-04-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/data/reg.assist.R
# Email: theRcast@gmail.com
# Purpose: Import NHL regular season assist data files from Hockey Summary Project
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - Raw data files can be accessed by joining the Hockey Summary Project Yahoo group:
#   - http://sports.groups.yahoo.com/group/hockey_summary_project/
#----------------------------------------------------------------------------------------------

reg.data.dir <- ifelse(.Platform$OS.type=="unix",
                       '~/Dropbox/hockey_data_files/regular_season_files/',
                       'D:/PortableApps/Dropbox/hockey_data_files/regular_season_files/')

reg.assist.files <- list.files(path=reg.data.dir, pattern="^.*-assist\\.dat", full.names=TRUE, recursive=TRUE)

reg.assist.filedata <- file.info(reg.assist.files)
filedata.sub <- reg.assist.filedata[, 1:2]
filedata.sub$filename <- row.names(filedata.sub)
rownames(filedata.sub) <- NULL

files.to.remove <- filedata.sub[filedata.sub$size==0, c("filename")]

reg.assist.files <- setdiff(reg.assist.files, files.to.remove)

#-----------------------------------------------
#Assist File
#============
#  Game ID
#Game Goal Number
#Assist Number (1st or 2nd assist on the goal)
#Player Name
#Player's Season Assist Number
#-----------------------------------------------

col.names <- c("game.id", "game.goal.number", "assist.number", "player.name", "player.assist.number")

col.classes <- rep("character", length(col.names))

############# files with issues
# 1919 (3) (missing | at end of each line)
# 1970 (29) (has n/a in the player goal number column)


tmp <- ldply(reg.assist.files,
             file.reader,
             col.classes=col.classes,
             col.names=col.names)

# get snapshot of unique values for each variable
#lapply(tmp, function(x) sort(unique(x))[1:30])

reg.assist <- tmp[,1:length(col.classes)]

names(reg.assist) <- col.names
