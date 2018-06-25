#---- Read Files & Import Packages ----
if(!require("pacman")) install.packages("pacman") #Installs package loader
pacman::p_load(dplyr, ggplot2, caret, boot)

# Set option to not display using scientific notation
options(scipen = 100, digits = 4)

# Read Files
college <- read.csv('collegeStats.csv', stringsAsFactors = FALSE)
nba <- read.csv('playerseasons.csv', stringsAsFactors = FALSE)
players <- read.csv('players.csv', stringsAsFactors = FALSE)

prompt <- 'No'

#---- Define Function Evaluate ----
Evaluate <- function(actual, pred){
  residual <- actual - pred
  
  ares <- residual %>% abs() %>% mean(na.rm = TRUE) %>% round(4)
  rmse <- sqrt((residual ^ 2 %>% sum(na.rm = TRUE)) / (residual %>% length())) %>% round(4)
  accDF <- data.frame(Method = c('ARES', 'RMSE'), Value = c(ares, rmse))
  print(accDF)
}

#---- Create month df for later joining in order to fix text birthdays ----
month <- data.frame(text = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'),
                    birthMonth = (1:12)) %>%
  mutate(text = as.character(text))

filter <- dplyr::filter
group_by <- dplyr::group_by
mutate <- dplyr::mutate

#---- Merge data, create the max_ws column, prepare for modeling ----
# Merge and eventually group by some demographic data. This will join, fix dates, calculate maximum win shares for each player, ages played in each year, etc...
grouped <- nba %>%
  group_by(player, From, To, Pos, Ht, Height, Wt, Birth.Date, Colleges) %>%
  summarise(max_ows = max(ows), max_dws = max(dws)) %>%
  ungroup() %>%
  inner_join(college, by = c('player', 'From' = 'from')) %>%
  group_by(player, From, To, Pos, Ht, Wt, Birth.Date, Colleges) %>%
  mutate(lastYear = max(year)) %>%
  ungroup() %>%
  filter(year == lastYear) %>%
  mutate(bmonth = gsub(" .*$", "", Birth.Date)) %>%
  left_join(month, by = c('bmonth' = 'text')) %>%
  mutate(birthDay = gsub(".* ", "", gsub(", .*$", '',Birth.Date)),
         birthYear = gsub(".*, ", "", Birth.Date)) %>%
  mutate(birthDate = as.Date(paste(birthYear, birthMonth, birthDay, sep = '-'))) %>%
  mutate(age = as.numeric(as.Date(paste(year, 1, 1, sep = '-')) - birthDate) / 365) %>%
  mutate(max_ws = max_ows + max_dws) %>%
  mutate(ast_tov_100 = ast_100 / tov_100) %>%
  mutate(Pos = ifelse(Pos == 'G-F', 'F', ifelse(Pos == 'F-G', 'F', ifelse(Pos == 'C-F', 'C', ifelse(Pos == 'F-C', 'C', Pos)))))

# These two are huge outliers that likely won't help prediction. Others could likely be added here, but none come to mind right now
modeling <- grouped %>% filter(!(player %in% c('Royce White', 'Greg Oden')))

# Prompt the user to see if we need to edit positions, birth dates, or heights
  # This could later expand to combine results, wingspan, etc...
# prompt <- readline("Do you need to add or change any positions/birth dates/heights for college players? Yes/No : ")
if(prompt %in% c('y', 'yes', 'Y', 'Yes')){
  positions <- modeling %>% select(player, Pos, birthDate, From, To) %>%
    distinct()
  
  write.csv(positions, 'playerpositions.csv', row.names = FALSE)
}

# Bring in new positions after editing them in the csv file using an editor of your choice
newPositions <- read.csv('playerpositions_modified.csv', stringsAsFactors = FALSE) %>%
  mutate(birthDate = as.Date(birthDate, '%m/%d/%Y'))



# Merge to get positions
modeling <- modeling %>%
  left_join(newPositions, by = c('player', 'Pos', 'birthDate', 'From', 'To'))


#---- Create Initial Model: Linear Regression with Cross vadliation ----

# Cross vadliation, n folds x times
controlConfig <- trainControl(method = 'cv', number = 10, repeats = 5)

# Train linear model. Further exploration could be done with variables (Feature selection, etc...)
lmModel <- train(max_ws ~ age * (stl_100 + blk_100 + pts_100 + ast_100 + trb_100 + tov_100 + dbpm) +
                   sos * (stl_100 + blk_100 + pts_100 + ast_100 + tov_100  + dbpm) +
                   newPos * (stl_100 + blk_100 + pts_100 + ast_100 + trb_100 + tov_100 + dbpm + ftp),
                 data = modeling %>% filter(year >= 2010, From <= 2018),
                 method = 'brnn',
                 trControl = controlConfig)

# New dataframe for testing
applied <- modeling %>% filter(From >= 2011, year >= 2010)

# Create prediction column
applied$lmPredict <- predict(lmModel, newdata = modeling)

applied %>% filter(From == 2018) %>% select(player, age, max_ws, lmPredict) %>% arrange(desc(lmPredict)) %>% View()



#---- Use Model(s) to apply to current draft ----


# Prompt again, same as above
# prompt <- readline("Do you need to write the file to add any birth dates, heights, or positions for the current draft year? Yes/No : ")
if(prompt %in% c('y', 'yes', 'Y', 'Yes')){
  players2019 <- college %>%
    filter(from == 2019) %>%
    select(player, from, school, conf) %>%
    distinct()
  
  # Write CSV file for current year. Will still need to find birth date & subsequent age
  write.csv(players2019, 'players2019.csv', row.names = FALSE)
  players2019 <- NULL
}

# Read in modified file
p2019 <- read.csv('players2019_modified.csv', stringsAsFactors = FALSE)

# Make some small data edits, including birth date. Calculate age, take most recent college year for prediction
p2019 <- p2019 %>%
  left_join(college, by = c('player', 'from', 'school', 'conf')) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(birthDate = as.Date(birthDate, '%m/%d/%Y')) %>%
  mutate(age = as.numeric(as.Date(paste(year, 1, 1, sep = '-')) - birthDate) / 365) %>%
  group_by(player, from, newPos) %>%
  mutate(lastYear = max(year), Ht = Height) %>%
  ungroup() %>%
  filter(year == lastYear) %>%
  mutate(Ht = gsub("'", "", Ht)) %>%
  mutate(Height = as.numeric(gsub("-.*",'',Ht)) * 12 + as.numeric(gsub(".*-",'',Ht)))

# Apply Model(s)
p2019$lmPredict <- predict(lmModel, newdata = p2019)

# Display order along with some info on players
p2019 %>% select(player, school, conf, newPos, age, Ht, lmPredict) %>% arrange(desc(lmPredict)) %>% View()


