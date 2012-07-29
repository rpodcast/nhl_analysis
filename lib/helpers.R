file.reader <- function(x, header=FALSE, sep="|", stringsAsFactors=FALSE, col.classes, col.names) {
  # Imports a hockey data file with varying number of columns
  #
  # Args:
  #   x: file name (full path included) of text data file as a character vector
  #   header: If TRUE, assumes header is present for data import. If FALSE, then 
  #           assumes no header is present.  Default is FALSE
  #   sep: character used for separating columns in data file.  Default is "|"
  #   stringsAsFactors: If TRUE, will convert all character variables to factors when importing 
  #                     data.  If FALSE, no conversion performed.  Default is FALSE
  #   col.classes: Character vector indicating the column type for each column in data file. 
  #                Must have same number of elements as desired number of columns to import
  #   col.names: Character vector of names to give each column
  #
  # Returns:
  #   Data frame for imported text file
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
  # Searches for presence of particular character sequence in text data file
  #
  # Args:
  #   x: file name (full path included) of text data file as a character vector
  #   messychar: Character sequence to search for in text data file
  #
  # Returns:
  #   Prints file name with messychar present.  No object is returned.
  #print(count.fields(x, sep=sep))
  tmpfile <- readChar(x, file.info(x)$size)
  #cat("testing file ", x, "\n")
  messyfound <- str_detect(tmpfile, messychar)
  if(messyfound) cat("character ", messychar, " found in ", x, "\n")
}

recodeteam <- function(x, as.factor.result=TRUE) {
  # Recode team labels to consistent team names using the recode function from car package
  #
  # Args:
  #   x: character vector with team names
  #   as.factor.result: If TRUE, new team name vector converted to factor. If FALSE,
  #                     stays as a character vector.  Default is TRUE.
  #
  # Returns: list with the following elements
  #   team.long: vector with recoded team names (long format)
  #   team.short: vector with recoded team names (short format)
  #
  require(car)
  team.long <- recode(x,
                          "c('Anaheim', 'ANA', 'Ana', 'Anaheim Mighty Ducks')='Anaheim Ducks';
                             c('ATLF')='Atlanta Flames';
                          c('ATL', 'Atlanta Thrasher')='Atlanta Thrashers';
                          c('BOS', 'Bos', 'Boston')='Boston Bruins';
                          c('BUF', 'Buf', 'Buffalo')='Buffalo Sabres';
                          c('CALG', 'CGY', 'Calgary', 'Cgy') = 'Calgary Flames';
                          c('CALI', 'CAL', 'Cal')='California Golden Seals';
                          c('CAR', 'Carolina', 'Car')='Carolina Hurricanes';
                          c('CHI', 'Chicago', 'Chi')='Chicago Blackhawks';
                          c('CBJ', 'CLM')='Columbus Blue Jackets';
                          c('CLE')='Cleveland Barons';
                          c('COLR')='Colorado Rockies';
                          c('COL', 'Colorado', 'Colorado Avalanch', 'Col')='Colorado Avalanche';
                          c('DAL', 'Dallas', 'Dal')='Dallas Stars';
                          c('DET', 'Detroit', 'Det')='Detroit Red Wings';
                          c('EDM', 'Edmonton', 'Edmonton Oiler', 'Edm')='Edmonton Oilers';
                          c('FLA', 'FLO', 'Florida', 'Flo')='Florida Panthers';
                          c('HAM')='Hamilton Tigers';
                          c('HAR', 'Hartford', 'Har', 'HFD')='Hartford Whalers';
                          c('KC')='Kansas City Scouts';
                          c('L.A', 'LosAngeles', 'LAK', 'LAK Angeles', 'Los Angeles', 'LA', 'Los', 'LOS', 'Los Angeles King', 'Los Angeles Kings<')='Los Angeles Kings';
                          c('MINS')='Minnesota North Stars';
                          c('MIN')='Minnesota Wild';
                          c('MON', 'Montreal', 'MTL', 'Canadiens De Montreal', 'Canadiens Montreal', 'Canadiens Montr\\xb3Al')='Montreal Canadiens';
                          c('MONW')='Montreal Wanderers';
                          c('MONM', 'MOMM')='Montreal Maroons';
                          c('NJ', 'N.J', 'New Jersey', 'NewJersey', 'Njd', 'NJD', 'New')='New Jersey Devils';
                          c('NYA')='New York Americans';
                          c('NYR', 'NYRangers', 'NY Rangers', 'NY')='New York Rangers';
                          c('NYI', 'NY Islanders', 'NYIslanders')='New York Islanders';
                          c('NSH', 'NAS')='Nashville Predators';
                          c('OAK')='Oakland Seals';
                          c('OTT', 'Ottawa', 'OTW', 'Ott')='Ottawa Senators';
                          c('PHI', 'Philadelphia', 'Phi')='Philadelphia Flyers';
                          c('PHX', 'Phoenix', 'PHO', 'Pho')='Phoenix Coyotes';
                          c('PITP')='Pittsburgh Pirates';
                          c('PIT', 'Pittsburgh', 'Pit')='Pittsburgh Penguins';
                          c('Quebec Bulldogs')='Quebec Bulldogs';
                          c('QUEB')='Quebec Athletic Club';
                          c('QUE', 'Quebec')='Quebec Nordiques';
                          c('STL', 'St Louis', 'StLouis', 'St.Louis Blues', 'St. Louis Blues', 'St')='St Louis Blues';
                          c('S.J', 'San Jose', 'SJ', 'SJS Jose', 'SanJose', 'SAN', 'San', 'SJS', 'S', 'San Jose Shark')='San Jose Sharks';
                          c('T.B', 'TB', 'Tamba Bay', 'Tampa Bay', 'Tam', 'TAM', 'TBL', 'TBY')='Tampa Bay Lightning';
                          c('TOR', 'Toronto', 'Tor')='Toronto Maple Leafs';
                          c('TORS', 'Toronto St. Patricks')='Toronto St Patricks';
                          c('TORA', 'Toronto Arenas')='Toronto Arenas';
                          c('VAN', 'Vancouver', 'Van')='Vancouver Canucks';
                          c('WAS', 'WSH', 'Washington', 'Was')='Washington Capitals';
                          c('WIN', 'WPG', 'Winnipeg', 'Win')='Winnipeg Jets'",
                          as.factor.result=as.factor.result)
  team.short <- recode(team.long,
                       "'Anaheim Ducks'='ANA';
                       'Atlanta Flames'='ATF';
                       'Atlanta Thrashers'='ATL';
                       'Boston Bruins'='BOS';
                       'Buffalo Sabres'='BUF';
                       'Calgary Flames'='CGY';
                       'California Golden Seals'='CGS';
                       'Carolina Hurricanes'='CAR';
                       'Chicago Blackhawks'='CHI';
                       'Cleveland Barons'='CLE';
                       'Colorado Avalanche'='COL';
                       'Colorado Rockies'='CLR';
                       'Columbus Blue Jackets'='CBJ';
                       'Dallas Stars'='DAL';
                       'Detroit Red Wings'='DET';
                       'Edmonton Oilers'='EDM';
                       'Florida Panthers'='FLA';
                       'Hamilton Tigers'='HAM';
                       'Hartford Whalers'='HAR';
                       'Kansas City Scouts'='KCS';
                       'Los Angeles Kings'='LAK';
                       'Minnesota North Stars'='MNS';
                       'Minnesota Wild'='MIN';
                       'Montreal Canadiens'='MTL';
                       'Montreal Maroons'='MTM';
                       'Montreal Wanderers'='MTW';
                       'Nashville Predators'='NSH';
                       'New Jersey Devils'='NJD';
                       'New York Americans'='NYA';
                       'New York Islanders'='NYI';
                       'New York Rangers'='NYR';
                       'Oakland Seals'='OAK';
                       'Ottawa Senators'='OTT';
                       'Philadelphia Flyers'='PHI';
                       'Phoenix Coyotes'='PHX';
                       'Pittsburgh Penguins'='PIT';
                       'Pittsburgh Pirates'='PTP';
                       'Quebec Athletic Club'='QBA';
                       'Quebec Bulldogs'='QBB';
                       'Quebec Nordiques'='QUE';
                       'San Jose Sharks'='SJS';
                       'St Louis Blues'='STL';
                       'Tampa Bay Lightning'='TBL';
                       'Toronto Arenas'='TRA';
                       'Toronto Maple Leafs'='TOR';
                       'Toronto St Patricks'='TRS';
                       'Vancouver Canucks'='VAN';
                       'Washington Capitals'='WSH';
                       'Winnipeg Jets'='WPG'",
                       as.factor.result=as.factor.result)
  return(list(team.long=team.long, team.short=team.short))
}
