#----------------------------------------------------------------------------------------------
# File: reg.goalieperiod.R
# Date: 07-04-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/data/reg.goalieperiod.R
# Email: theRcast@gmail.com
# Purpose: Import NHL regular season goalie period data files from Hockey Summary Project
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - Raw data files can be accessed by joining the Hockey Summary Project Yahoo group:
#   - http://sports.groups.yahoo.com/group/hockey_summary_project/
#----------------------------------------------------------------------------------------------

reg.data.dir <- ifelse(.Platform$OS.type=="unix",
                       '~/Dropbox/hockey_data_files/regular_season_files/',
                       'D:/PortableApps/Dropbox/hockey_data_files/regular_season_files/')


reg.goalieperiod.files <- list.files(path=reg.data.dir, pattern="^.*-goalieperiod\\.dat", full.names=TRUE, recursive=TRUE)

reg.goalieperiod.filedata <- file.info(reg.goalieperiod.files)
filedata.sub <- reg.goalieperiod.filedata[, 1:2]
filedata.sub$filename <- row.names(filedata.sub)
rownames(filedata.sub) <- NULL

files.to.remove <- filedata.sub[filedata.sub$size==0, c("filename")]

reg.goalieperiod.files <- setdiff(reg.goalieperiod.files, files.to.remove)


#-----------------------------------------------
# goalie File
# =================
# Game ID
# Team
# Goalie Name
# Period
# Shots Against
# Goals Against
#-----------------------------------------------

col.names <- c("game.id",
               "team",
               "goalie.name",
               "period",
               "shots.against",
               "goals.against")

col.classes <- rep("character", length(col.names))


library(plyr)
library(stringr)

file.reader <- function(x, header=FALSE, sep="|", stringsAsFactors=FALSE, col.classes, col.names) {
  #print(count.fields(x, sep=sep))
  n.fields <- max(count.fields(x, sep=sep))
  n.to.rep <- n.fields - length(col.classes)
  if (n.fields > length(col.classes)) {
    col.classes <- c(col.classes, rep(NA, n.to.rep))
  } else {
    col.classes <- col.classes[-c(seq(n.fields:length(col.classes)))]
  }
  #print(col.classes)
  read.delim(x,
             header=header,
             sep=sep,
             colClasses=col.classes,
             stringsAsFactors=FALSE)
}

file.messy <- function(x, messychar) {
  #print(count.fields(x, sep=sep))
  tmpfile <- readChar(x, file.info(x)$size)
  #cat("testing file ", x, "\n")
  messyfound <- str_detect(tmpfile, messychar)
  if(messyfound) cat("character ", messychar, " found in ", x, "\n")
}

#------------------------------------------------------------------------------------------------
# files with issues:
# 1995: Game 19950256 has two entries for Fuhr, last one has goals against of -20, only the first
#       record of his entry is accurate
#       - other records in 1995 file have goals against with negative numbers and shots against with 
#         ":" in front
# I manually removed those lines, file should be fine now
#------------------------------------------------------------------------------------------------  

blah <- ldply(reg.goalieperiod.files,
              file.messy,
              messychar="&#160;")
# 
# blah <- ldply(reg.goalieperiod.files,
#               file.messy,
#               messychar="<b>")
# 
# blah <- ldply(reg.goalieperiod.files,
#               file.messy,
#               messychar="<")

# [-c(23, 53, 63)]
# [-c(23, 53, 54, 55, 56, 63, 66, 67)]
tmp <- ldply(reg.goalieperiod.files,
             file.reader,
             col.classes=col.classes,
             col.names=col.names)


#do.call("ncol", tmp)


# get snapshot of unique values for each variable
#lapply(tmp, function(x) sort(unique(x))[1:30])

reg.goalieperiod <- tmp[,1:length(col.classes)]

names(reg.goalieperiod) <- col.names

# get snapshot of unique values for each variable
#lapply(reg.goalieperiod, function(x) sort(unique(x))[1:30])

