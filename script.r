#!/usr/bin/env Rscript

# Load in data
raw_data <- readLines("data.txt")
raw_games <- split(raw_data[raw_data != ""], cumsum(raw_data == "")[raw_data != ""] )

# Process data
for (game in raw_games) {
  raw_date <- unlist(strsplit(game[1], "\t"))[2]
  date <- as.Date(raw_date)
  
  raw_players <- unlist(strsplit(game[2], "\t"))[2]
  players <- as.numeric(raw_players)
  
  winning_team <- unlist(strsplit(game[3], "\t"))[2]
  
  data <- data.frame(
    lapply(
      game[4:(4 +players - 1)],
      function (line) unlist(strsplit(line, "\t"))
    )
  )
  data <- t(unname(data))
  colnames(data) <- c("player", "role")
  
  # Add team column
  team <- lapply(
    data[,"role"],
    function (role) 
      if (role %in% c("merlin", "leffen", "resistance")) {
        "resistance"
      } else {
        "spies"
      }
  )
  data <- cbind(data, team)
  
  # Add result column
  result <- lapply(
    data[,"team"],
    function (role) 
      if (role == "resistance" & winning_team == "resistance") {
        "win"
      } else if (role == "spies" & winning_team == "spies") {
        "win"
      } else {
        "loss"
      }
  )
  data <- cbind(data, result)
}

# Wait for user to kill script (use this if running with Rscript)
#Sys.sleep(999999999999)
