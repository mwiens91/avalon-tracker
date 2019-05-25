#!/usr/bin/env Rscript

library(plyr)
suppressMessages(library(dplyr))
library(ggplot2)
library(reshape2)
library(scales)

# Load in data
raw_data <- readLines("data.txt")
raw_games <- split(raw_data[raw_data != ""], cumsum(raw_data == "")[raw_data != ""])

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
    sapply(
      game[4:(4 + players - 1)],
      function(line) unlist(strsplit(line, "\t"))
    )
  )
  game_df <- as.data.frame(t(unname(game_df)))
  colnames(game_df) <- c("player", "role")

  # Add in non-data frame attrs
  game_df.date <- date
  game_df.winning_team <- winning_team
  game_df.players <- players

  # Add team column
  game_df$team <- sapply(
    game_df[, "role"],
    function(role)
      if (role %in% c("merlin", "leffen", "resistance")) {
        "resistance"
      } else {
        "spies"
      }
  )

  # Add result column
  game_df$result <- sapply(
    game_df[, "team"],
    function(role)
      if (role == "resistance" & winning_team == "resistance") {
        "win"
      } else if (role == "spies" & winning_team == "spies") {
        "win"
      } else {
        "loss"
      }
  )

  games <- c(games, list(game_df))
}

# Count player win rates
big_df <- rbind.fill(games)
molten_results <- melt(select(big_df, player, result), id.var = c("player"))
results <- dcast(molten_results, player ~ value, value.var = "player", length)

# Add in win %
results <- transform(results, win_percent = win / (win + loss))

# Plot results
results_freq <- as.data.frame(molten_results %>% group_by(player, value) %>% tally())
results_freq$n2 <- ifelse(results_freq$value == "loss", -1 * results_freq$n, results_freq$n)

p_wins_losses <- (
  ggplot(data = results_freq)
  + geom_bar(aes(x = reorder(player, -n2), y = n2, fill = value), stat = "identity", position = "identity")
    + labs(x = "player", y = "n")
    + scale_y_continuous(label = abs, breaks = pretty_breaks())
)
x11()
print(p_wins_losses)

# Plot win %
p_win_percent <- (
  ggplot(data = results, aes(x = reorder(player, -win_percent), y = win_percent))
  + geom_bar(stat = "identity", fill = "steelblue")
    + labs(x = "player", y = "win %")
    + scale_y_continuous(labels = scales::percent_format())
    + theme_minimal()
)
x11()
print(p_win_percent)

# Wait for user to kill script (uncomment this if running with Rscript)
# Sys.sleep(999999999999)
