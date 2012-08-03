#----------------------------------------------------------------------------------------------
# File: reg.goalie.R
# Date: 07-04-2012
# Author: Eric Nantz
# URL: https://github.com/thercast/nhl_analysis/blob/master/data/reg.goalie.R
# Email: theRcast@gmail.com
# Purpose: Import NHL regular season goalie data files from Hockey Summary Project
# License: Creative Commons Attribution-ShareAlike 3.0 Unported License
#
# Notes:
# - Raw data files can be accessed by joining the Hockey Summary Project Yahoo group:
#   - http://sports.groups.yahoo.com/group/hockey_summary_project/
#----------------------------------------------------------------------------------------------

reg.data.dir <- ifelse(.Platform$OS.type=="unix",
                       '~/Dropbox/hockey_data_files/regular_season_files/',
                       'D:/PortableApps/Dropbox/hockey_data_files/regular_season_files/')


reg.goalie.files <- list.files(path=reg.data.dir, pattern="^.*-goalie\\.dat", full.names=TRUE, recursive=TRUE)

reg.goalie.filedata <- file.info(reg.goalie.files)
filedata.sub <- reg.goalie.filedata[, 1:2]
filedata.sub$filename <- row.names(filedata.sub)
rownames(filedata.sub) <- NULL

files.to.remove <- filedata.sub[filedata.sub$size==0, c("filename")]

reg.goalie.files <- setdiff(reg.goalie.files, files.to.remove)


#-----------------------------------------------
# goalie File
# =================
# Game ID
# Team
# Goalie Name
# Decision("W", "L" or "T" or blank)
# Minutes played
#-----------------------------------------------

col.names <- c("game.id",
               "team",
               "goalie.name",
               "decision",
               "minutes")

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
# - 1995 (index 54) is a complete mess, so is 1996 (index 55), 1997 (index 56), 2008 (index 66), 2009 (index 67)
# - files with semicolons: 1964 (index 23), 1994 (index 53)
# - files with "<b>": 2005 (index 63)
#------------------------------------------------------------------------------------------------  

# blah <- ldply(reg.goalie.files,
#               file.messy,
#               messychar=";")
# 
# blah <- ldply(reg.goalie.files,
#               file.messy,
#               messychar="<b>")
# 
# blah <- ldply(reg.goalie.files,
#               file.messy,
#               messychar="<")

# [-c(23, 53, 63)]
# [-c(23, 53, 54, 55, 56, 63, 66, 67)]
tmp <- ldply(reg.goalie.files,
             file.reader,
             col.classes=col.classes,
             col.names=col.names)


#do.call("ncol", tmp)


# get snapshot of unique values for each variable
#lapply(tmp, function(x) sort(unique(x))[1:30])

reg.goalie <- tmp[,1:length(col.classes)]

names(reg.goalie) <- col.names

# get snapshot of unique values for each variable
#lapply(reg.goalie, function(x) sort(unique(x))[1:30])

