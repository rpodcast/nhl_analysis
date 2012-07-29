#-------------------------------------------------------------------------------------------
# experiment with scraping hockey reference box score data
# - this can be an optimized version of player stats for each game
# see http://www.slideshare.net/rtelmore/user-2012-talk
# also see his github code: https://github.com/rtelmore/Pitch_Count/blob/master/src/do.R
#-------------------------------------------------------------------------------------------



# example url: http://www.hockey-reference.com/boxscores/199511210BOS.html
# - base url: http://www.hockey-reference.com/boxscores/
# - custom part: yyyymmdd0abc.html
#   - yyyy: year
#   - mmdd: month and day
#   - 0: Game ID (seems like all of them are 0)
#   - abc: Three-letter team abbreviation

library(XML)
library(stringr)

base.url <- "http://www.hockey-reference.com/boxscores/"
year <- 1995
team <- "BOS"
month <- 11
day <- 21

full.url <- "http://www.hockey-reference.com/boxscores/198805240BOS.html"

full.url <- paste(base.url, year, month, day, "0", team, ".html", sep="")
bad.url <- paste(base.url, year, month, day, "0", "WWW", ".html", sep="")


tmpdata <- geturl(full.url)

arq <- try(suppressWarnings(readHTMLTable(full.url, header=FALSE)), silent = TRUE)
arq <- try(suppressWarnings(readHTMLTable(bad.url, header=FALSE)), silent = TRUE)


table.stats <- readHTMLTable(full.url, header=FALSE)

unlist(str_detect(names(table.stats), "\\_skaters"))


blah <- names(table.stats)

unlist(str_extract_all(blah, "\\_skaters"))

player.table.ind <- unlist(str_detect(names(table.stats), "\\_skaters"))
goalie.table.ind <- unlist(str_detect(names(table.stats), "\\_goalies"))

sum(player.table.ind)
sum(goalie.table.ind)

sum(player.table.ind, na.rm=TRUE) < 2 | sum(goalie.table.ind, na.rm=TRUE) < 2


team.player.table.names <- names(table.stats)[player.table.ind]
team.goalie.table.names <- names(table.stats)[goalie.table.ind]

player.home.team.ind <- str_detect(team.player.table.names, team)
goalie.home.team.ind <- str_detect(team.goalie.table.names, team)


home.team.player.table.name <- team.player.table.names[player.home.team.ind]
home.team.goalie.table.name <- team.goalie.table.names[goalie.home.team.ind]
home.team.clean <- str_replace_all(team.player.table.names[player.home.team.ind], "\\_skaters", "")

away.team.player.table.name <- team.player.table.names[!player.home.team.ind]
away.team.goalie.table.name <- team.goalie.table.names[!goalie.home.team.ind]
away.team.clean <- str_replace_all(team.player.table.names[!player.home.team.ind], "\\_skaters", "")

table.stats[home.team.player.table.name]
table.stats[home.team.goalie.table.name]
table.stats[away.team.player.table.name]
table.stats[away.team.goalie.table.name]

player.table.column.names <- c("team",
                               "home.away.ind",
                               "index",
                               "player",
                               "goals",
                               "assists",
                               "points",
                               "plus.minus",
                               "pim",
                               "ev.goals",
                               "pp.goals",
                               "sh.goals",
                               "shots",
                               "shot.percent",
                               "shifts",
                               "ice.time")


home.player.table <- as.data.frame(table.stats[home.team.player.table.name])
home.player.table <- cbind(home.team.clean, "H", home.player.table)
names(home.player.table) <- player.table.column.names

away.player.table <- as.data.frame(table.stats[away.team.player.table.name])
away.player.table <- cbind(away.team.clean, "A", away.player.table)
names(away.player.table) <- player.table.column.names

goalie.table <- as.data.frame(table.stats[team.goalie.table.names[1]])
names(goalie.table) <- c("index",
                         "player",
                         "decision",
                         "goals.against",
                         "shots.against",
                         "saves",
                         "save.percent",
                         "shutouts",
                         "pim",
                         "ice.time",
                         "ev.goals.against",
                         "pp.goals.against",
                         "sh.goals.against",
                         "en.goals.against")
goalie.table


