# Runs code to prepare environment, set global options, load packages, etc.
source("01_setup.R")

####
# Define functions ####
####
extract_play_data <- function(x) {
  
  plays <- data.frame(type = as.integer(x[["type"]][["id"]]), stringsAsFactors = FALSE)
  plays$typeText <- x[["type"]][["text"]]
  plays$text <- x[["text"]]
  plays$yardage <- x[["statYardage"]]
  plays$downAndDistBefore <- as.character(x[["start"]][["shortDownDistanceText"]])
  plays$downAndDistAfter <- x[["end"]][["shortDownDistanceText"]]
  plays$teamOffense <- x[["start"]][["team"]][["id"]]
  
  return(plays)
}
#####

# Load team info data
team_data <- readRDS("data\\team_info.rds")

# Read JSON game data, create drives list
gameData <- jsonlite::fromJSON('data\\400934497 - Virginia Tech vs West Virginia.json')
#gameData <- jsonlite::fromJSON("data\\VT-duke_2016.json")
drives <- gameData[["drives"]][["previous"]][["plays"]]

# Map extract_play_data function on game data to get desired play data
plays <- drives %>% map_dfr(extract_play_data)

# Print unique play types
(uniquePlayTypes <- plays %>% group_by(type) %>% 
    filter(row_number() == 1) %>% 
    select(type, typeText, text) %>% 
    ungroup() %>% 
    mutate(type = as.integer(type)) %>% 
    arrange(type))

# Add team abbreviations
plays <- left_join(plays, team_data, by = c("teamOffense" = "team_id"))

# Add passer, receiver, rusher columns
passingPlayIds <- c(3, 24)
rushingPlayIds <- c(5, 7, 68)

plays$passer <- ifelse(plays$type %in% passingPlayIds, str_extract(plays$text, "[a-zA-Z.']+\\s?[a-zA-Z.']+(?= pass)"), NA)
#plays$receiver <- ifelse(plays$type %in% passingPlayIds, str_extract(plays$text, "(?<=complete to )\\w+\\s?\\w+"), NA)
plays$receiver <- ifelse(plays$type %in% passingPlayIds, str_extract(plays$text, "(?<=complete to )[a-zA-Z.']+\\s?[a-zA-Z.]+"), NA)
plays$rusher <- ifelse(plays$type %in% rushingPlayIds, str_extract(plays$text, "[a-zA-Z.']+\\s?[a-zA-Z.']+((?= run)|(?= sacked))"), NA)

str_detect(".", "[a-zA-Z.]+")

plays %>% filter(type==7) %>% select(rusher)
# Calculate rushing totals
(rushingTotals <- plays %>% filter(type %in% rushingPlayIds) %>% 
                            group_by(abbreviation, rusher) %>% 
                            summarize(rushes = n(), 
                                      yards = sum(yardage),
                                      TD = sum(type == 68),
                                      yardsPerRush = yards/rushes,
                                      long = max(yardage)) %>% 
                            arrange(abbreviation, desc(yards))
)


# Calculate receiving totals
(receivingTotals <- plays %>% filter(type %in% passingPlayIds) %>% 
                             group_by(abbreviation, receiver) %>% 
                             summarize(targets = n(),
                                       receptions = sum(type == 24),
                                       yards = sum(yardage),
                                       TD = sum(type == 67)
                                       ) %>% 
                             arrange(abbreviation, desc(yards))

)

plays %>% filter(type %in% passingPlayIds, is.na(receiver))
plays %>% filter(type %in% passingPlayIds, receiver=="Ka")


jsonedit(gameData)
jsonedit(drives)









