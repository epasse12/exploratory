#---- Load Packages ----
if(!require("pacman")) install.packages("pacman") #Installs package loader
pacman::p_load(dplyr, ggplot2, rvest, RecordLinkage, flipTime)

#---- Import Player Data ----
# Create empty dataframe. Will use rbind over & over again
webData <- data.frame()

# As long as basketball-reference does not change their web structure, should be good here.
for(i in letters){
  location <- paste('https://www.basketball-reference.com/players/', i, '/', sep = '')
  
  tryCatch(
    webData <- webData %>% rbind(read_html(location) %>% html_table() %>% as.data.frame()),
    error = function(cond) 'skip'
  )
}

#-- Create some new columns, create a numeric height variable. Keep original format for displaying data
players <- webData %>%
  mutate(YrsPlayed = To - From,
         Player = gsub("[*]",'',Player),
         Height = as.numeric(gsub("-.*",'',Ht)) * 12 + as.numeric(gsub(".*-",'',Ht))) %>%
  mutate(birthYear = as.integer(substr(Birth.Date, nchar(Birth.Date) - 3, nchar(Birth.Date))))

# Write players file. Each player will have one record
write.csv(players, 'players.csv')

# Clean up environment
rm(i, location)

#---- Import Player Seasons Data (This is the good stuff) ----

# Create stat types & years to loop through
statTypes <- c('per_game', 'totals', 'per_minute', 'per_poss', 'advanced')
years <- c(1950:as.numeric(format(Sys.Date(), '%Y')))

# Loop through multiple types of statistics. Gets per game, totals, per minute, per possession, and other advanced statistics
for(s in statTypes){
  tempDF <- data.frame()
  for(y in years){
    currUrl <- paste('https://www.basketball-reference.com/leagues/NBA_', y, '_', s,'.html', sep = '')
    
    tryCatch(
      tempDF <- tempDF %>% rbind(read_html(currUrl) %>% html_table() %>% as.data.frame() %>%
                                   mutate(Year = y, Player = gsub('[*]', '', Player)) %>% select(-Rk)),
      error = function(cond) 'skip'
    )
    
    # Print so the user knows what's going on
    print(currUrl)
  }
  assign(paste(s, 'DF', sep = ''), tempDF)
  print(paste(s, nrow(tempDF), sep = ': '))
}

# Clean up environment
rm(statTypes, years, tempDF, currUrl, s, y)

#---- Data Transformation ----

# Merge the different state types together
nba <- totalsDF %>% filter(Player != 'Player') %>% distinct() %>%
  left_join(per_gameDF %>% filter(Player != 'Player') %>% select(-Age, -Pos, -G, -GS, -MP), by = c('Player', 'Tm', 'Year')) %>%
  left_join(per_minuteDF %>% filter(Player != 'Player') %>% select(-Age, -Pos, -G, -GS, -MP), by = c('Player', 'Tm', 'Year')) %>%
  left_join(per_possDF %>% filter(Player != 'Player') %>% select(-Age, -Pos, -G, -GS, -MP), by = c('Player', 'Tm', 'Year')) %>%
  left_join(advancedDF %>% filter(Player != 'Player') %>% select(-Age, -Pos, -G, -MP), by = c('Player', 'Tm', 'Year'))

# Create preferred naming of columns
colnames(nba) <- c('player', 'pos', 'age', 'tm', 'g', 'gs', 'mp', 'fgm', 'fga', 'fgp', 'fgm3', 'fga3', 'fgp3', 'fgm2', 'fga2', 'fgp2', 
                   'efg', 'ftm', 'fta', 'ftp', 'orb', 'drb', 'trb', 'ast', 'stl', 'blk', 'tov', 'pf', 'pts', 'year', 'fga_pg', 'fgm_pg', 
                   'rm01', 'fgm3_pg', 'fga3_pg', 'rm02', 'fgm2_pg', 'fga2_pg', 'rm03', 'rm04', 'ftm_pg', 'fta_pg', 'rm05', 'orb_pg', 
                   'drb_pg', 'trb_pg', 'ast_pg', 'stl_pg', 'blk_pg', 'tov_pg', 'pf_pg', 'pts_pg', 'fgm_36', 'fga_36', 'rm06', 'fgm3_36', 
                   'fga3_36',  'rm07', 'fgm2_36', 'fga2_36', 'rm08', 'ftm_36', 'fta_36', 'rm09', 'orb_36', 'drb_36', 'trb_36', 'ast_36', 
                   'stl_36', 'blk_36', 'tov_36', 'pf_36', 'pts_36', 'fgm_100', 'fga_100', 'rm10', 'fgm3_100', 'fga3_100', 'rm11', 
                   'fgm2_100', 'fga2_100', 'rm12', 'ftm_100', 'fta_100', 'rm13', 'orb_100', 'drb_100', 'trb_100', 'ast_100', 'stl_100', 
                   'blk_100', 'tov_100', 'pf_100', 'pts_100', 'rm14', 'ortg', 'drtg', 'per', 'ts', 'fg3r', 'ftr', 'orbr', 'drbr', 'trbr',
                   'astr', 'stlr', 'blkr', 'tovr', 'usg', 'rm15', 'ows', 'dws', 'ws', 'ws48', 'rm16', 'obpm', 'dbpm', 'bpm', 'vorp')

# Note: Much of the below code was written in my early R days, there's likely a better way to do most of it.

# Remove duplicate columns, renamed above as rm01, rm02... rm16
nba <- nba[,!(colnames(nba) %in% colnames(nba)[grepl('rm', colnames(nba))])]

# The following block is for creating a player ID for each player
nba <- nba %>% mutate(yob = as.numeric(year) - as.numeric(age)) #create a yob (year of birth) variable
nbaUnique <- nba %>% select(player, yob) %>% unique() #Create a dataframe with unique player-yob combinations
nbaUnique$Id <- 'P' %>% paste(as.vector(10000:(10000+nrow(nbaUnique) - 1)), sep = '') #Create a player ID for each seperate name & yob combination
nba <- merge(nba, nbaUnique %>% select(player, yob, Id), by = c('player', 'yob')) #merge the player ID back into the orgininal dataframe

# Create column for experience (years in NBA)
nba <- transform(nba, exp = ave(year, Id, FUN = function(x) rank(x, ties.method = "first")), check.names = FALSE)

#Reorder the dataframe, put the ID in front
nba <- nba[,c(length(nba), 1:length(nba)-1)]

#Add CT variable for number of records for each player-season
nba <- merge(nba, (nba %>% group_by(Id, year) %>% summarise(CT = n())), by = c('Id', 'year')) 

# Deal with years where player is traded. Keeps the total row, removes the 2 or more team rows
nbaTraded <- nba %>% filter(CT > 1 & tm != 'TOT')
nbaPlayerTeam <- nba %>% filter (tm != 'TOT') %>% select(-CT)
nba <- nba %>% filter(CT == 1 | tm == 'TOT') %>% select(-CT)

# Fix positions. Make it so a player is only given one position throughout his career.
nbaPositions <- nba %>% group_by(Id, player, pos) %>% summarise(CT = n()) #Create a count for how many seasons a player played which position
nbaPositions <- merge(nbaPositions, (nbaPositions %>% group_by(Id) %>% summarise(IdCT = n()))) #Create a count for how many positions each player played

# Create dataset where that player only has one position
nbaOnePos <- nbaPositions %>% filter(IdCT == 1)
# Create datasets with players for multiple positions
nbaMultPos <- nbaPositions %>% filter(IdCT > 1)

# Create a multiplier to later use top_n(1, CT) correctly to retrieve the best position for each player
positionWeights <- data.frame(pos = c('PG', 'SG', 'SF', 'PF', 'C'), weight = c(1.01, 1.02, 1.03, 1.04, 1.05)) #Create a dataframe for position weights

# References the created positionWeights dataframe. This allows us to remove duplicate positions
nbaMultPos$Mult <- as.numeric(lapply(nbaMultPos$pos, function(x) if(x %in% positionWeights$pos) positionWeights$weight[positionWeights$pos == x]
                                     else 1.0)) 

# Apply the multiplier to the CT variable
nbaMultPos$CT <- nbaMultPos$CT * nbaMultPos$Mult 
# Remove multiplier variable
nbaMultPos$Mult <- NULL 

# Take top position for each player. The 'top' position is the one which has occurred the most
nbaTopMultPos <- nbaMultPos %>% group_by(Id) %>% top_n(1, CT)
# Creates dataframe with unique Id - position pairs
nbaNewPositions <- bind_rows(nbaOnePos, nbaTopMultPos) %>% select(Id, pos) 
# Replaces all combo positions with the first position that shows up
nbaNewPositions$pos <- gsub("-.*", "", nbaNewPositions$pos) 

# Merge, get all new and cleaned positions for each player
nba <- nba %>% select(-pos) %>% merge(nbaNewPositions, by = 'Id') 
nbaPlayerTeam <- nbaPlayerTeam %>% select(-pos) %>% merge(nbaNewPositions, by = 'Id')

# Reorder the nba dataset
nba <- nba[c(1:5, length(nba), 7:length(nba) - 1)]
# Reorder nbaPlayerTeam
nbaPlayerTeam <- nbaPlayerTeam[c(1:5, length(nbaPlayerTeam), 7:length(nbaPlayerTeam) - 1)]

#---- Merge Player & Season Info ----
final <- players %>%
  mutate(Birth.Date = as.character(Birth.Date)) %>%
  mutate(birthYear = as.integer(substr(Birth.Date, nchar(Birth.Date) - 3, nchar(Birth.Date)))) %>%
  full_join(nba, by = c('Player' = 'player')) %>%
  rename(player = Player) %>%
  mutate(Jan = grepl('January', Birth.Date)) %>%
  mutate(yearDiff = ifelse(Jan == TRUE, birthYear - yob, birthYear + 1 - yob)) %>%
  filter(From <= year, To >= year,
         yearDiff == 0) %>%
  select(-yearDiff, -Jan, -X, -yob) %>%
  mutate(age = year - birthYear) %>%
  rename(yob = birthYear)

# Test for Incorrect Matching
final %>% mutate(matchTest = paste(player, age, tm, mp, pts, sep = ',')) %>%
  group_by(matchTest) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  arrange(matchTest, count) %>%
  nrow() #Should be no records found

#---- Write File ----
write.csv(final, 'playerseasons.csv', row.names = FALSE)

