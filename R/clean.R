# A function for cleaning the data
# be sure to load the packages from lab1.Rnw first!

library(tidyverse)
library(forcats)
library(lubridate)
library(stringr)
library(zoo)
library(chron)

cleanDatesData <- function(date_df) {
  # Arguments:
  #   date_df: a data.frame in the format of the output of the 
  #     loadDatesData() function
  # Returns:
  #   a data.frame similar to the input `dates` but with cleaned variables
  #     (number, day, date, time, datetime)
  
  # convert the dates variable to lubridate format
  date_df <- date_df %>% 
    # separate date variable into two variables: time and date
    # remove times
    mutate(date_sep = gsub("\\w+:\\w+:\\w+ ", "", date), 
           # remove day of the week
           date_sep = gsub("(Mon|Tue|Wed|Thu|Fri|Sat|Sun)", "", date_sep),
           # extract times
           time_sep = str_extract(as.character(date), " \\w+:\\w+:\\w+"),
           # combine date and time into single datetime variable
           datetime = mdy_hms(paste(date_sep, time_sep)),
           # convert day to a number
           day = as.numeric(as.character(day))) %>%
    # remove original date vairable and rename date_sep and time_sep
    select(-date, date = date_sep, time = time_sep)
  
  return(date_df)
}

cleanRedwoodDataType <- function(redwood_df) {
  # convert result_time to lubridate ymd_hms format
  redwood_df <- redwood_df %>% mutate(result_time = ymd_hms(redwood_df$result_time))
  redwood_df$Date <- sapply(strsplit(as.character(redwood_df$result_time), " "), "[", 1)
  redwood_df$Time <- sapply(strsplit(as.character(redwood_df$result_time), " "), "[", 2)
  redwood_df$Time <- as.hms(redwood_df$Time)
  
  
  redwood_df[[3]] <- as.factor(redwood_df[[3]])
  for(i in c(2, 4, 6)) {redwood_df[[i]] <- as.integer(redwood_df[[i]])}
  for(i in c(5, 7:11)) {redwood_df[[i]] <- as.numeric(redwood_df[[i]])}
  
  return(redwood_df)
}


cleanRedwoodData <- function(redwood_df) {
  
  #Removing rows outside the range of humidity
  redwood_df <- redwood_df %>% filter(humidity >= 16.4 & humidity <= 102)
  
  
  # Removing the Incident and Reflected PAR outside the range of Incident PAR and Reflected PAR
  redwood_df <- redwood_df %>% filter(hamatop <= 2154 & hamabot <= 180 & hamabot >= 0 & hamatop >= 0 )
  
  # Removing the Temperature readings outside the sensor range
  redwood_df <- redwood_df %>% filter(humid_temp >= 6.6 & humid_temp <= 32.6)
  
  # Removing the sensor with low voltage
  redwood_df <- subset(redwood_df, voltage >= 2.4)
  
  
  #Replacing NA with the mean of the column
  redwood_df <- redwood_df %>% drop_na()

  
  #we make a vector of all the noteIDs with the sensors at 0.1m distance from the trunk
  interior <- mote_location %>% filter(Dist == 0.1) %>% select(ID)
  interior_nodes <- interior[[1]]
  redwood_df <- redwood_df %>% mutate(Position = ifelse(nodeid %in% interior_nodes, "close_to_trunk", "far_from_trunk"))
  
  
  #we make a vector of the which tree for the dataframe
  tree_type <- mote_location %>% filter(Tree == "edge") %>% select(ID)
  edge_tree_nodes <- tree_type[[1]]
  redwood_df <- redwood_df %>% mutate(Tree = ifelse(nodeid %in% edge_tree_nodes, "edge", "interior"))

  
  #We make a list of heights of nodes to add a column to our data-frame
  sensor_height <- vector(mode="list", length=length(mote_location$ID))
  names(sensor_height) <- mote_location$ID
  for (i in c(1:length(mote_location$ID))) {
    sensor_height[[i]] <- mote_location$Height[[i]]
  }
  redwood_df <- redwood_df %>% mutate(Height = sensor_height[as.character(nodeid)])
  
  
  
  return(redwood_df)
}
