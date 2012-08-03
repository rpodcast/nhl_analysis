#----------------------------------------------------------------------------------------------
# File: reg.game.R
# Date: 07-05-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/data/reg.game.R
# Email: theRcast@gmail.com
# Purpose: Import NHL regular season game data files from Hockey Summary Project
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - Raw data files can be accessed by joining the Hockey Summary Project Yahoo group:
#   - http://sports.groups.yahoo.com/group/hockey_summary_project/
#----------------------------------------------------------------------------------------------

reg.data.dir <- ifelse(.Platform$OS.type=="unix",
                       '~/Dropbox/hockey_data_files/regular_season_files/',
                       'D:/PortableApps/Dropbox/hockey_data_files/regular_season_files/')

reg.game.files <- list.files(path=reg.data.dir, pattern="^.*-game\\.dat", full.names=TRUE, recursive=TRUE)


reg.game.filedata <- file.info(reg.game.files)
filedata.sub <- reg.game.filedata[, 1:2]
filedata.sub$filename <- row.names(filedata.sub)
rownames(filedata.sub) <- NULL

files.to.remove <- filedata.sub[filedata.sub$size==0, c("filename")]

reg.game.files <- setdiff(reg.game.files, files.to.remove)

#-----------------------------------------------
# Game File
# ===========
#   Game ID
# Home Team
# Home Goals
# Away Team
# Away Goals
# Game Date
# Game Time
# Arena
# Attendance
# Home Powerplay Time
# Visitors Powerplay Time
# Overtime Indictor
#-----------------------------------------------


col.names <- c("game.id",
               "home.team",
               "home.goals",
               "away.team",
               "away.goals",
               "game.date",
               "game.time",
               "arena",
               "attendance",
               "home.pp.time",
               "away.pp.time",
               "overtime.ind")

col.classes <- rep("character", length(col.names))


############# files with issues
# 2007 (65) (read it in separately and then rbind it back to the rest)
# 2008 (66) (read it in separately and then rbind it back to the rest)

library(plyr)

tmp <- ldply(reg.game.files[-c(65,66)],
             file.reader,
             col.classes=col.classes,
             col.names=col.names)

# get snapshot of unique values for each variable
#lapply(tmp, function(x) sort(unique(x))[1:30])


reg.debug.2007 <- ldply(reg.game.files[65],
                 read.delim,
                 header=FALSE,
                 sep = "|",
                 #col.names=col.names,
                 #colClasses=col.classes,
                 stringsAsFactors=FALSE)

reg.debug.2007$V11 <- NA; reg.debug.2007$V12 <- NA

reg.debug.2008 <- ldply(reg.game.files[66],
                        read.delim,
                        header=FALSE,
                        sep = "|",
                        #col.names=col.names,
                        #colClasses=col.classes,
                        stringsAsFactors=FALSE)

#reg.debug.2008$V11 <- NULL; reg.debug.2008$V12 <- NULL; reg.debug.2008$V13 <- NULL
reg.debug.2008$V13 <- NULL

reg.game <- tmp[,1:length(col.classes)]
reg.game <- rbind(reg.game, reg.debug.2007, reg.debug.2008)
names(reg.game) <- col.names

# get snapshot of unique values for each variable
#lapply(reg.game, function(x) sort(unique(x))[1:30])
