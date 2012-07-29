#--------------------------------------------
# do some more munging on reg.assist
#--------------------------------------------

# backup copy before munging
reg.backup <- reg.assist

#reg.assist <- reg.backup

# convert appropriate variables to numeric
reg.assist$game.id <- as.integer(reg.assist$game.id)
reg.assist$game.goal.number <- as.integer(reg.assist$game.goal.number)
reg.assist$assist.number <- as.integer(reg.assist$assist.number)
reg.assist$player.assist.number <- as.integer(reg.assist$player.assist.number)

# delete row with home.team = "Eastern Conf" since I don't care about allstar game 
# X[ ! X$Variable1 %in% c(11,12), ]


# remove records with missing game_id
reg.assist <- reg.assist[!is.na(reg.assist$game.id),]

# get year and game number from game.id
library(stringr)
reg.assist$game.id2  <- str_trim(reg.assist$game.id, side="both")

reg.assist$year <- as.integer(substr(x=reg.assist$game.id2, start=1, stop=4))
reg.assist$game.number <- as.integer(substr(x=reg.assist$game.id2, start=5, stop=8))
reg.assist$game.id2 <- NULL

reg.assist$era.ind <- ifelse(reg.assist$year > 2003, 
                            "post-lockout",
                            "pre-lockout")


cache("reg.assist")