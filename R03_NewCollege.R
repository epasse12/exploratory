#----- Read Files & Import Packages ----- 
if(!require("pacman")) install.packages("pacman") #Installs package loader
pacman::p_load(dplyr, ggplot2)

college <- read.csv('collegeStats.csv', stringsAsFactors = FALSE)
college %>% summary()

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

#----- Define Function: collegeComps -----
collegeComps <- function(bbdf = college, name = 'Donte DiVincenzo', year = '2017-18', age = NULL,
                        age_constraint = FALSE, pos_constraint = FALSE){
  
  display_list <- c('player', 'school', 'season', 'conf', 'ws40', 'ortg', 'drtg', 'pts_36', 'ast_36', 'stl_36', 'blk_36', 'trb_36', 'tov_36', 'fg3p', 'usg', 'ts', 'sd')
  comp_list <- c('trb_36', 'ast_36', 'stl_36', 'blk_36', 'fga3_36', 'tov_36', 'pts_36', 'fta_36', 'ws40')
  comp_mult <- c(  1.3  ,    1.4  ,    1.3  ,    1.3  ,    1.3   ,     .8  ,    1.3  ,    .8    ,  .7   )
  #-- comp_mult is for weights on values. Larger = more important
  sd_list <- unlist(lapply(comp_list, FUN = function(x) paste(c(x, 'sd'), collapse = "")))

  #--------Figures out which player season we are comparing
  if(!is.null(year)){            # Else: select player by season
    comp_line <- bbdf[bbdf$player == name & bbdf$season == year,]
  }   else if(!is.null(age)){             # Else: select player by age
    comp_line <- bbdf[bbdf$player == name & bbdf$age == age,]
  }   else{                               # Else: select player by latest year
    playerdf <- bbdf[bbdf$player == name,]
    comp_line <- (playerdf[which.max(playerdf$year),])
  }
  
  #--------filter our reference dataframe with a minutes floor
  df <- (efilter(bbdf, bbdf$mp.1, min = 300))
  #--------Year constraint (used mostly for comparing rookie seasons)

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
  
  #-- Remove records that are far away from a combination of statistics.
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


#---- Run Stuff ----

collegeComps(name = 'Landry Shamet', year = NULL)

