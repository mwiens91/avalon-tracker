#!/usr/bin/env Rscript

library(plyr)

# Load in data
raw_data <- readLines("data.txt")
raw_games <- split(raw_data[raw_data != ""], cumsum(raw_data == "")[raw_data != ""] )

# Process data
games <- c()

for (game in raw_games) {
  raw_date <- unlist(strsplit(game[1], "\t"))[2]
  date <- as.Date(raw_date)

  raw_players <- unlist(strsplit(game[2], "\t"))[2]
  players <- as.numeric(raw_players)

  winning_team <- unlist(strsplit(game[3], "\t"))[2]

  # Make the game data frame
  game_df <- data.frame(
    lapply(
      game[4:(4 +players - 1)],
      function (line) unlist(strsplit(line, "\t"))
    )
  )
  game_df <- as.data.frame(t(unname(game_df)))
  colnames(game_df) <- c("player", "role")

  # Add in non-data frame attrs
  game_df.date <- date
  game_df.winning_team <- winning_team
  game_df.players <- players

  # Add team column
  team <- lapply(
    game_df[,"role"],
    function (role)
      if (role %in% c("merlin", "leffen", "resistance")) {
        "resistance"
      } else {
        "spies"
      }
  )
  game_df$team <- unlist(team)

  # Add result column
  result <- lapply(
    game_df[,"team"],
    function (role)
      if (role == "resistance" & winning_team == "resistance") {
        "win"
      } else if (role == "spies" & winning_team == "spies") {
        "win"
      } else {
        "loss"
      }
  )
  game_df$result <- unlist(result)

  games <- c(games, list(game_df))
}

# Count player win rates
big_df <- rbind.fill(games)

# Wait for user to kill script (use this if running with Rscript)
#Sys.sleep(999999999999)
