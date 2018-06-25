#----- Read Files & Import Packages ----- 
if(!require("pacman")) install.packages("pacman") #Installs package loader
pacman::p_load(dplyr, ggplot2)

players <- read.csv('players.csv')
seasons <- read.csv('playerseasons.csv')
  
#----- Define Function: playerComps -----
playerComps <- function(bbdf = seasons, name = 'Ricky Rubio', year = NULL, exp = NULL, age = NULL,
                        yr_constraint = FALSE, age_constraint = FALSE, pos_constraint = FALSE, 
                        peak_df = player_peaks){
  
  display_list <- c('player', 'pos', 'tm', 'exp', 'age', 'year', 'ws48', 'ortg', 'drtg', 'pts_36', 'ast_36', 'stl_36', 'blk_36', 'trb_36', 'tov_36', 'usg', 'ts', 'sd')
  comp_list <- c('trb_36', 'ast_36', 'stl_36', 'blk_36', 'fga3_36', 'tov_36', 'pts_36', 'fta_36', 'dbpm', 'ws48', 'usg')
  comp_mult <- c(  1.3  ,    1.4  ,    1.3  ,    1.3  ,    1.3   ,     .8  ,    1.3  ,    .8   ,  1.1   ,  .7   ,   .7 )
  #-- comp_mult is for weights on values. Larger = more important
  sd_list <- unlist(lapply(comp_list, FUN = function(x) paste(c(x, 'sd'), collapse = "")))
  
  #--------Figures out which player season we are comparing
  if(!is.null(exp)){ # Select player by experience level
    comp_line <- bbdf[bbdf$player == name & bbdf$exp == exp,]
  }   else if(!is.null(year)){            # Else: select player by season
    comp_line <- bbdf[bbdf$player == name & bbdf$year == year,]
  }   else if(!is.null(age)){             # Else: select player by age
    comp_line <- bbdf[bbdf$player == name & bbdf$age == age,]
  }   else{                               # Else: select player by best total ws season
    playerdf <- bbdf[bbdf$player == name,]
    comp_line <- (playerdf[which.max(playerdf$ws),])
  }

  if(is.null(peak_df)){
    #--------filter our reference dataframe with a minutes floor
    df <- (efilter(bbdf, bbdf$mp, min = 400))
    #--------Year constraint (used mostly for comparing rookie seasons)
    if(yr_constraint == TRUE){
      df <- df[df$exp == comp_line[1,'exp'],]
    }
    #--------Age constraint
    if(age_constraint == TRUE){
      df <- df[df$age == comp_line[1,'age'],]
    }
  } else {
    df <- peak_df
  }

  #--------Position constraint
  if(pos_constraint == TRUE){
    df <- df[df$pos == comp_line[1,'pos'],]
  }
  #--------Calculate Standard Deviation for each column, calculate standard deviations away from target
  for(i in comp_list){
    p <- paste(c(i, 'sd'), collapse = '')
    df[,p] <- (df[,i] - comp_line[,i]) / sd(df[,i], na.rm = TRUE) * (1 + (1 - comp_mult[match(i, comp_list)]))
  }

  df$sd <- rowMeans(abs(df[,sd_list]))    # Takes average of all standard deviations previously calculated
  df <- df[df$player != name,]    # Removes name to not comp himself
  df <- df[order(df$sd, decreasing = FALSE),]     # Orders dataframe based on mean sd's
  
  #--Remove records that are far away from a combination of statistics.
  for(i in sd_list){
    df <- df[abs(df[,i]) < 2.5,]
  }

  #--------Return the top 10 comps & info from columns display_list
  comp_line$sd <- 0 # Allows a join of this row & comp table
  des_df <- comp_line[,display_list]  # Selects a specified list of columns to display
  comp_df <- df[1:10, display_list] # Creates the [Top 10] comp dataframe to then merge with the player
  comp_df <- merge(des_df, comp_df, all = TRUE)   
  return(comp_df[order(comp_df$sd, decreasing = FALSE),])
}

#----- Define Function: playerPeaks ----- 
#Create the dataframe, select zero rows
playerPeaks <- function(df = seasons){
  player_peaks <- head(df, 0)
  #-- Create a >250 minute limit
  limited <- efilter(df, df$mp, 300)
  
  #-- Limit the dataset for testing
  #limited <- limited[limited$player == 'Ricky Rubio',]
  
  for(i in unique(df$player)){
    temp_df <- df[df$player == i,]
    temp_df <- head(temp_df[order(temp_df$ws48, decreasing = T),], 3)
    output <- head(temp_df, 0)
    
    for(col in colnames(temp_df[,sapply(temp_df, is.numeric)])){
      output[1,col] <- mean(temp_df[,col])
    }
    
    output[1,'player'] <- i
    output$yr <- 'PEAK'
    output$tm <- 'PEAK'
    output$pos <- temp_df[1,'pos']
    
    player_peaks <- rbind(player_peaks, output)
  }
  return(player_peaks)
}
#----- Define Function: eFilter  ---- 
efilter <- function(df, col, min = NULL, max = NULL, sort_by = 'player', dec = TRUE){
  if(!is.null(min) & !is.null(max)){  # If there is a min & max
    df <- (df[col >= min & col <= max,])
  }
  else if(!is.null(min)){  # If there is only a min
    df <- (df[col >= min,])
  }
  else if(!is.null(max)){  # If there is only a max
    df <- (df[df$col <= max,])
  }
  return(df[order(df[,sort_by], decreasing = dec),])
}

#----- Define Function: normal -----
normal <- function(column){
  avgVal = mean(column, na.rm = TRUE)
  sdVal = sd(column, na.rm = TRUE)
  return((column - avgVal)/ sdVal)
}

#----- Utilize functions ----- 
# This will find player peaks for each player in the dataset
player_peaks <- playerPeaks()

playerComps(name = "Ricky Rubio", exp = NULL, year = 2017, age = NULL, yr_constraint = FALSE, age_constraint = FALSE, 
            pos_constraint = TRUE, peak_df = NULL)
