#---- Setup & Reading files ----
# Use 'pacman' package for installing & loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, broom, tidyr, purrr, RecordLinkage, knitr, splitstackshape)

new_columns <- c('rk', 'player', 'class', 'season', 'pos', 'school', 'conf', 'g', 'mp', 'fgm', 'fga', 'fgm2', 'fga2', 'fgm3', 'fga3',
                 'ftm', 'fta', 'orb', 'drb', 'trb', 'ast', 'stl', 'blk', 'tov', 'pf', 'pts')

#-Select all files and add to a list of dataframes
college <- lapply(Sys.glob("college/original/cbb_standard*.csv"), read.csv, stringsAsFactors = FALSE, check.names = FALSE, skip = 2)
college <- lapply(college, setNames, new_columns)

numeric_columns <- c('rk', 'g', 'mp', 'fgm', 'fga', 'fgm2', 'fga2', 'fgm3', 'fga3', 'ftm', 'fta', 'orb', 'drb', 'trb', 'ast', 'stl',
                     'blk', 'tov', 'pf', 'pts')

#combine each dataframe in list (Each dataframe is a seperate csv)
college <- do.call('rbind', college)

#-- Assign new column names
colnames(college) <- new_columns

#-- remove additional title rows in dataframe
college <- college %>% filter(!(rk %in% c('','Rk','Player')),
                              !is.na(rk))

#-- Fix the year column. Set as the END of the basketball year.
college <- college %>% mutate(season = ifelse(season == '', NA,
                                              substr(season, 1, 4) %>% as.integer() + 1))

# Loop & make certain columns numeric for calculations later
for(i in numeric_columns){
    college[,i] <- as.numeric(college[,i])
}

#------------------------------------------------
#------Import NBA database and test for college names in NBA set. This is working under the assumption that the
                        # only players we are interested in are the ones that eventually play in the NBA

#--Import the nba dataframe
nba <- read.csv('playerseasons.csv', stringsAsFactors = FALSE, check.names = FALSE)

#-- Make some joins based on...
  # 1. Name 
  # 2. College Similarities.
    # College names do not match from nba to college data sets. Nba will also need to be split a few times.
  # 3. Years
derp <- nba %>% group_by(Id) %>% mutate(maxws = max(ws)) %>% ungroup() %>%
  select(Id, player, Colleges, From, To, Pos, Ht, Wt, Birth.Date, maxws) %>% distinct() %>%
  left_join(college, by = c('player' = 'player')) %>%
  mutate(Colleges = gsub('University of ', '', Colleges)) %>%
  mutate(Colleges = gsub('University', '', Colleges)) %>%
  mutate(Colleges = gsub('California, Los Angeles', 'UCLA', Colleges)) %>%
  mutate(Colleges = gsub('Virginia Polytechnic Institute and State', 'Virginia Tech', Colleges)) %>%
  cSplit('Colleges', sep = ',', type.convert = FALSE) %>%
  mutate(sscore1 = levenshteinSim(Colleges_1, school),
         sscore2 = levenshteinSim(Colleges_2, school),
         sscore3 = levenshteinSim(Colleges_3, school),
         sscore4 = levenshteinSim(Colleges_4, school)) %>%
  rowwise() %>%
  mutate(maxSimScore = max(sscore1, sscore2, sscore3, sscore4, na.rm = TRUE)) %>%
  filter(ifelse(is.na(season), 0, season) < To)

ggplot(data = derp) + geom_histogram(aes(x = maxSimScore), binwidth = .01)

#--- Limit the college dataframe to only those who have gone to the NBA
players <- aggregate(age ~ player, data = nba, FUN = max)
players <- merge(x = players, y = nba[,c('player', 'age', 'pos')], by = c('player', 'age'))

colnames(players) <- c('player', 'nba_pos')
write.csv(players, 'nba/nba_positions.csv', row.names = FALSE)
college_nba <- merge(x = players, y = college, by = 'player')
    
#---Seperate the dataframes based on whether or not 'mp' is NA or not.
college_mp <- college_nba[!is.na(college_nba$mp),]
college_nomp <- college_nba[is.na(college_nba$mp),]

#--- Remove the rank column
college_nba$rk <- NULL

#--Write the files to the combined_files folder
write.csv(college_mp, 'college/college_nba_mp.csv', row.names = FALSE)
write.csv(college_nomp, 'college/college_nba_nomp.csv', row.names = FALSE)

#--Import the no minutes dataframe with data manually added.
#college_nomp2 <- read.csv('college/college_nba_nomp_MANUALEDIT.csv', check.names = FALSE)

#-- Merge the two dataframes with minutes for the players.

#-- Edit the positions on a spreadsheet and merge back.

#---- Create more columns This should be after correcting the data so don't have to run it twice ----
#--Create a loop to add per_36 statistics to the dataframe.
fields <- c('fgm', 'fga', 'fgm2', 'fga2', 'fgm3', 'fga3', 'ftm', 'fta', 'orb', 'drb', 'trb', 'ast',
            'stl', 'blk', 'tov', 'pf', 'pts')

for(i in fields){
  college[,paste(c(i,'36'), collapse = '')] <- round(college[,i] / college$mp * 36, 2)
}

#----Compute 2p%, 3p%, ft%, efg%, ts%
college$fg2. <- college$fgm2 / college$fga2
college$fg3. <- college$fgm3 / college$fga3
college$ft. <- college$ftm / college$fta
college$efg. <- (1.5*(college$fgm3 / college$fga3) + (college$fgm2 / college$fga2)) / 2

college$ts. <- college$pts / (2*(college$fga3 + college$fga2) + (.88 * college$fta)) #-- this is incorrect



#---- Write Files ----

#--Write the final file to the combined_files folder
write.csv(college_nba, 'college/college.csv', row.names = FALSE)
write.csv(nba, 'college/player_by_season.csv', row.names = FALSE)