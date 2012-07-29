#--------------------------------------------
# do some more munging on reg.stats
#
# To Do: recode messy position values?
#--------------------------------------------

# backup copy before munging
reg.backup <- reg.stats

#reg.stats <- reg.backup

# convert appropriate variables to integer

integer.converter <- function(x) {
  x <- as.integer(x)
  x
}

cols.to.convert <- c("game.id", "shifts", str_c("shots.period", c(1:3)), "shots.ot", "shot.total", "shot.missed",
                     "faceoff.wins", "faceoff.losses", "faceoff.pct", "penalties", "pim", "hits", "takeaways", "giveaways", 
                     "blocked.shots")

reg.stats[,cols.to.convert] <- lapply(reg.stats[,cols.to.convert], integer.converter)


# recode plus-minus "E" to 0 so I can get an integer from it
reg.stats$plus.minus <- recode(reg.stats$plus.minus, "c(' E', 'E', 'even', '-E')='0'")

# get rid of leading plus sign
reg.stats$plus.minus <- gsub("\\+", "",  reg.stats$plus.minus)

# convert plus/minus to integer
reg.stats$plus.minus <- as.integer(reg.stats$plus.minus)

# recode messy home.away.ind values
reg.stats$home.away.ind <- recode(reg.stats$home.away.ind, "'v'='V'")

# remove records with missing game_id
reg.stats <- reg.stats[!is.na(reg.stats$game.id),]

# get year and game number from game.id
library(stringr)
reg.stats$game.id2  <- str_trim(reg.stats$game.id, side="both")

reg.stats$year <- as.integer(substr(x=reg.stats$game.id2, start=1, stop=4))
reg.stats$game.number <- as.integer(substr(x=reg.stats$game.id2, start=5, stop=8))
reg.stats$game.id2 <- NULL

reg.stats$era.ind <- ifelse(reg.stats$year > 2003, 
                            "post-lockout",
                            "pre-lockout")


cache("reg.stats") # note: this is a large file (over 150 MB)