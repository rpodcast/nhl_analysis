#--------------------------------------------
# do some more munging on reg.period
#--------------------------------------------

# backup copy before munging
reg.backup <- reg.period

#reg.period <- reg.backup


# convert appropriate variables to integer

integer.converter <- function(x) {
  x <- as.integer(x)
  x
}

cols.to.convert <- c("game.id", "goals", "shots", "penalties", "pim", "pp.goals", "pp.opp")

reg.period[,cols.to.convert] <- lapply(reg.period[,cols.to.convert], as.integer)


# delete row with home.team = "Eastern Conf" since I don't care about allstar game 
# X[ ! X$Variable1 %in% c(11,12), ]
reg.period <- reg.period[ ! reg.period$team %in% c("Eastern Conf", "Eas", "Wes", "Western Conf"),]

# delete rows with period="T" since I can do totals myself (also delete "0" since those records are useless)
reg.period <- reg.period[ ! reg.period$period %in% c("T", "0"),]

# remove records with missing game_id
reg.period <- reg.period[!is.na(reg.period$game.id),]

# remove any observations with away.team = missing
reg.period <- reg.period[reg.period$team != "",]

# get year and game number from game.id
reg.period$game.id2  <- str_trim(reg.period$game.id, side="both")

reg.period$year <- as.integer(substr(x=reg.period$game.id2, start=1, stop=4))
reg.period$game.number <- as.integer(substr(x=reg.period$game.id2, start=5, stop=8))
reg.period$game.id2 <- NULL

reg.period$era.ind <- ifelse(reg.period$year > 2003, 
                            "post-lockout",
                            "pre-lockout")


# use custom recodeteam function to recode messy team names
new.team.names <- recodeteam(reg.period$team)

reg.period$team.long <- new.team.names$team.long
reg.period$team.short <- new.team.names$team.short

# remove variables that aren't needed anymore
reg.period$team <- reg.period$game.id2 <- NULL


cache("reg.period")