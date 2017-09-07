library(jsonlite)
library(listviewer)
library(stringr)
library(tidyverse)

# Setup
options(stringsAsFactors = FALSE)
cat(rep("\n", 40)) # Buffer console
remove(list = ls()) # Clear environment

# Read raw JSON data, create drives list
gameData <- jsonlite::fromJSON("data\\VT-duke_2016.json")
drives <- gameData[["drives"]][["previous"]][["plays"]]

# Create character vector of play text description
# (also determines how many plays in game, making other vectors more efficient 
text <-  vector(mode = "character", length = 0)
for (i in seq_along(drives)) {
  text <- c(text, unlist(drives[[i]]["text"], use.names = FALSE))
}
remove(i)
numPlays <- length(text)



type <- vector(mode = "integer", length = numPlays)
yardage <-  vector(mode = "integer",length = numPlays)
downAndDistBefore <- vector(mode = "character", length = numPlays)
downAndDistAfter <- vector(mode = "character", length = numPlays)
playsInDrive <- NULL
start <- 1
end <- 0
for (i in seq_along(drives)) {
  
  playsInDrive <-  length(unlist(drives[[i]]["statYardage"]))
  end <-  end + playsInDrive

  type[start:end] <- drives[[i]][["type"]][["id"]]
  yardage[start:end] <- drives[[i]][["statYardage"]]
  downAndDistBefore[start:end] <- as.character(drives[[i]][["start"]][["shortDownDistanceText"]])
  downAndDistAfter[start:end] <- drives[[i]][["end"]][["shortDownDistanceText"]]
  
  start <-  start + playsInDrive
}
remove(i, start, end, playsInDrive)


plays <- data.frame(type, text, yardage, downAndDistBefore, downAndDistAfter)

unique(plays$type)


# Add passer, receiver, rusher fields
plays$passer <- NA
plays$receiver <- NA
plays$rusher <- NA




