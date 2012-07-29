#--------------------------------------------
# do some more munging on reg.goal
#--------------------------------------------

# backup copy before munging
reg.backup <- reg.goal

#reg.goal <- reg.backup

# convert appropriate variables to numeric
reg.goal$game.id <- as.integer(reg.goal$game.id)
reg.goal$game.goal.number <- as.integer(reg.goal$game.goal.number)
reg.goal$player.goal.number <- as.integer(reg.goal$player.goal.number)

# delete row with home.team = "Eastern Conf" since I don't care about allstar game 
# X[ ! X$Variable1 %in% c(11,12), ]
reg.goal <- reg.goal[ ! reg.goal$team=="Eastern Conf",]
reg.goal <- reg.goal[ ! reg.goal$team=="Western Conf",]
reg.goal <- reg.goal[ ! reg.goal$team=="First period -- Scoring:",]
reg.goal <- reg.goal[ ! reg.goal$team=="None.",]

# remove records with missing game_id
reg.goal <- reg.goal[!is.na(reg.goal$game.id),]

# remove any observations with away.team = missing
reg.goal <- reg.goal[reg.goal$team != "",]


# get year and game number from game.id
reg.goal$game.id2  <- str_trim(reg.goal$game.id, side="both")

reg.goal$year <- as.integer(substr(x=reg.goal$game.id2, start=1, stop=4))
reg.goal$game.number <- as.integer(substr(x=reg.goal$game.id2, start=5, stop=8))

reg.goal$era.ind <- ifelse(reg.goal$year > 2003, 
                            "post-lockout",
                            "pre-lockout")


#----------------------------------------------------------------------------------------------
# use car package to recode messy team names
# 
# notes:
# - California Seals, Oakland Seals, California Golden Seals, Cleveland Barons same franchise
#   (merged with Minnesota North Stars in 1978-1979)
#    - http://en.wikipedia.org/wiki/Cleveland_Barons_%28NHL%29
# - Kansas City Scouts, Colorado Rockies, and New Jersey Devils same franchise
#   - http://en.wikipedia.org/wiki/Colorado_Rockies_%28NHL%29
# - Quebec Nordiques and Colorado Avalanche same franchise
# - Minnesota North Stars and Dallas Stars same franchise ()
# 
#
# Cities with multiple franchises:
# - Minnesota
# - Colorado
# - Montreal
# - Atlanta
# - New York
# - Quebec
# - Pittsburgh
# - Philadelphia
# - St Louis
#---------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------
#
# 1. Montreal: unique values are Montreal, MON, MONW, MONM. MTL
#    - year <= 1938: only MON, MONW, MONM values
#    - year > 1938: only MON, Montreal, MTL values
#    - 1917: Either Canadiens or Wanderers (1917-1918) (MONW)
#    - 1924-1938: Maroons (MONM)
#
# 2. Minnesota: unique values are MIN, MINS
#    - year <= 1992: only MINS
#    - year > 1992: only MIN
#    - 1967-1992 (Minnesota North Stars)
#    - 2000-present (Minnesota Wild)
#
# 3. New York: unique values are NYI, NY Islanders, NYIslanders, NY Rangers, NYR, NYRangers, NYA
#    - year <= 1942: only NYA, NYR
#    - year > 1942: only NY Islanders NY Rangers   NYI          NYIslanders  NYR          NYRangers
#    - 1925-1942 (New York Americans)
#    - 1926-present (New York Rangers)
#    - 1972-present (New York Islanders)
#
# 4. Philadelphia: unique values are PHI, Philadelphia
#    - year <= 1931: NONE (must not have the Quakers summarized)
#    - year > 1931: only PHI, Philadelphia
#    - 1930-1931 (Philadelphia Quakers)
#    - 1967-present (Philadelphia Flyers)
#
# 5. Pittsburgh: unique values are 'PIT', 'PITP', 'Pittsburgh'
#    - year <= 1930: only PITP
#    - year > 1930: only PIT, Pittsburgh
#    - 1925-1930 (Pittsburgh Pirates)
#    - 1967-present (Pittsburgh Penguins)
#
# 6. Colorado: unique values are 'COL', 'COLR', 'Colorado'
#    - year <= 1982: only COLR
#    - year > 1982: only COL, Colorado
#    - 1976-1982 (Colorado Rockies)
#    - 1995-present (Colorado Avalanche)
#
# 7. Atlanta: unique values are 'ATL', 'ATLF'
#    - year <= 1980: only ATLF
#    - year > 1980: only ATL
#    - 1972-1980  (Atlanta Flames)
#    - 1999-2010 (Atlanta Thrashers)
#
# 8. St Louis: unique values are 'STL', 'St Louis', 'StLouis'
#    - year <= 1935: NONE
#    - year > 1935: STL, St Louis, StLouis
#    - 1934-1935 (St Louis Eagles)
#    - 1967-present (St Louis Blues)
#
# 9. Quebec: unique values are 'QUE', 'QUEB', 'Quebec'
#    - year <= 1920: only QUEB
#    - year > 1920: QUE, Quebec
#    - 1972-1995 (Quebec Nordiques)
#    - 1919-1920 (Quebec Athletic Club)
#------------------------------------------------------------------------------------------------------

# subsets to debug early years

mon.sub <- subset(reg.goal,
                  subset=(team %in% c("Montreal", "MON", "MONW", "MTL", "MONM") & year > 1938))

sort(unique(mon.sub$team))

min.sub <- subset(reg.goal,
                  subset=(team %in% c('MIN', 'MINS') & year > 1992))

sort(unique(min.sub$team))

ny.sub <- subset(reg.goal,
                 subset=(team %in% c('NYI', 'NY Islanders', 'NYIslanders', 'NY Rangers', 'NYR', 'NYRangers', 'NYA') & year > 1942))

sort(unique(ny.sub$team))

phi.sub <- subset(reg.goal,
                  subset=(team %in% c('PHI', 'Philadelphia') & year > 1932))

sort(unique(phi.sub$team))

pit.sub <- subset(reg.goal,
                  subset=(team %in% c('PIT', 'PITP', 'Pittsburgh') & year > 1930))

sort(unique(pit.sub$team))

col.sub <- subset(reg.goal,
                  subset=(team %in% c('COL', 'COLR', 'Colorado') & year > 1982))

sort(unique(col.sub$team))

atl.sub <- subset(reg.goal,
                  subset=(team %in% c('ATL', 'ATLF') & year > 1980))

sort(unique(atl.sub$team))

stl.sub <- subset(reg.goal,
                  subset=(team %in% c('STL', 'St Louis', 'StLouis') & year <= 1936))

sort(unique(stl.sub$team))

que.sub <- subset(reg.goal,
                  subset=(team %in% c('QUE', 'QUEB', 'Quebec') & year > 1920))

sort(unique(que.sub$team))

# First: Recode teams for cities that did not have multiple franchises
reg.goal$team <- recode(reg.goal$team,
                             "c('Anaheim', 'ANA')='Anaheim Ducks';
                             c('ATLF')='Atlanta Flames';
                             c('ATL')='Atlanta Thrashers';
                             c('BOS', 'Boston')='Boston Bruins';
                             c('BUF', 'Buffalo')='Buffalo Sabres';
                             c('CALG', 'CGY', 'Calgary') = 'Calgary Flames';
                             c('CALI', 'CAL')='California Golden Seals';
                             c('CAR', 'Carolina')='Carolina Hurricanes';
                             c('CHI', 'Chicago')='Chicago Blackhawks';
                             c('CBJ')='Columbus Blue Jackets';
                             c('CLE')='Cleveland Barons';
                             c('COLR')='Colorado Rockies';
                             c('COL', 'Colorado')='Colorado Avalanche';
                             c('DAL', 'Dallas')='Dallas Stars';
                             c('DET', 'Detroit')='Detroit Red Wings';
                             c('EDM', 'Edmonton')='Edmonton Oilers';
                             c('FLA', 'FLO', 'Florida')='Florida Panthers';
                             c('HAM')='Hamilton Tigers';
                             c('HAR', 'Hartford')='Hartford Whalers';
                             c('KC')='Kansas City Scouts';
                             c('L.A', 'LosAngeles', 'LAK', 'LAK Angeles', 'Los Angeles')='Los Angeles Kings';
                             c('MINS')='Minnesota North Stars';
                             c('MIN')='Minnesota Wild';
                             c('MON', 'Montreal', 'MTL')='Montreal Canadiens';
                             c('MONW')='Montreal Wanderers';
                             c('MONM')='Montreal Maroons';
                             c('NJ', 'N.J', 'New Jersey', 'NewJersey')='New Jersey Devils';
                             c('NYA')='New York Americans';
                             c('NYR', 'NYRangers', 'NY Rangers')='New York Rangers';
                             c('NYI', 'NY Islanders', 'NYIslanders')='New York Islanders';
                             c('NSH')='Nashville Predators';
                             c('OAK')='Oakland Seals';
                             c('OTT', 'Ottawa')='Ottawa Senators';
                             c('PHI', 'Philadelphia')='Philadelphia Flyers';
                             c('PHX', 'Phoenix')='Phoenix Coyotes';
                             c('PITP')='Pittsburgh Pirates';
                             c('PIT', 'Pittsburgh')='Pittsburgh Penguins';
                             c('QUEB')='Quebec Athletic Club';
                             c('QUE', 'Quebec')='Quebec Nordiques';
                             c('STL', 'St Louis', 'StLouis')='St Louis Blues';
                             c('S.J', 'San Jose', 'SJ', 'SJS Jose', 'SanJose')='San Jose Sharks';
                             c('T.B', 'TB', 'Tamba Bay')='Tampa Bay Lightning';
                             c('TORA', 'TORS', 'TOR', 'Toronto')='Toronto Maple Leafs';
                             c('VAN', 'Vancouver')='Vancouver Canucks';
                             c('WAS', 'WSH', 'Washington')='Washington Capitals';
                             c('WIN', 'WPG', 'Winnipeg')='Winnipeg Jets'",
                            as.factor.result=TRUE)

# use custom recodeteam function to recode messy team names
new.team.names <- recodeteam(reg.goal$team)

reg.goal$team.long <- new.team.names$team.long
reg.goal$team.short <- new.team.names$team.short

# remove variables that aren't needed anymore
reg.goal$team <- reg.goal$game.id2 <- NULL

cache("reg.goal")