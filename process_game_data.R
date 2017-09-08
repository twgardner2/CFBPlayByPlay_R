source("01_setup.R")

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
typeText <- vector(mode = "character", length = numPlays)
yardage <-  vector(mode = "integer",length = numPlays)
downAndDistBefore <- vector(mode = "character", length = numPlays)
downAndDistAfter <- vector(mode = "character", length = numPlays)
playsInDrive <- NULL
start <- 1
end <- 0
for (i in seq_along(drives)) {
  
  playsInDrive <-  length(unlist(drives[[i]]["statYardage"]))
  end <-  end + playsInDrive

  type[start:end] <- as.integer(drives[[i]][["type"]][["id"]])
  typeText[start:end] <- drives[[i]][["type"]][["text"]]
  yardage[start:end] <- drives[[i]][["statYardage"]]
  downAndDistBefore[start:end] <- as.character(drives[[i]][["start"]][["shortDownDistanceText"]])
  downAndDistAfter[start:end] <- drives[[i]][["end"]][["shortDownDistanceText"]]
  
  start <-  start + playsInDrive
}
remove(i, start, end, playsInDrive)

# Construct data frame of plays
plays <- data.frame(type, typeText, text, yardage, downAndDistBefore, downAndDistAfter)

unique(plays$type)

uniquePlayTypes <- plays %>% group_by(type) %>% 
                              filter(row_number() == 1) %>% 
                              select(type, typeText, text) %>% 
                              ungroup() %>% 
                              mutate(type = as.integer(type)) %>% 
                              arrange(type)
uniquePlayTypes
                        



# Add passer, receiver, rusher fields
plays$passer <- NA
plays$receiver <- NA
plays$rusher <- NA

passingPlayIds <- c(3, 24)
rushingPlayIds <- c(5, 7, 68)

plays$test <- ifelse(plays$type %in% passingPlayIds, TRUE, FALSE)


