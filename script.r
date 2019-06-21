#!/usr/bin/env Rscript

library(plyr)
suppressMessages(library(dplyr))
library(ggplot2)
library(reshape2)
library(scales)
library(yaml)

# Helper functions
get_molten_results <- function(df) {
  return(melt(select(df, player, result), id.var = c("player")))
}

get_formatted_results <- function(molten_results) {
  results <- dcast(molten_results, player ~ value, value.var = "player", length)
  results <- transform(results, win_delta = win - loss)
  results <- transform(results, total_games = win + loss)
  results <- transform(results, win_percent = win / (win + loss))

  return(results)
}

get_plot_wins_losses <- function(wide_results, molten_results, title) {
  results_freq <- as.data.frame(molten_results %>% group_by(player, value) %>% tally())
  results_freq$n2 <- ifelse(results_freq$value == "loss", -1 * results_freq$n, results_freq$n)
  results_freq$player <- factor(
    results_freq$player,
    levels = all_role_results[
      order(-all_role_results$win_delta, -all_role_results$total_games, all_role_results$player),
    ]$player
  )

  return(
    ggplot(data = results_freq)
    + geom_bar(aes(x = player, y = n2, fill = value), stat = "identity", position = "identity")
      + labs(title = title, x = "player", y = "n")
      + scale_y_continuous(minor_breaks = NULL, breaks = function(x) unique(floor(seq(-max(x) - 1, (max(x) + 1) * 1.1))))
      + theme(plot.title = element_text(hjust = 0.5))
  )
}

get_plots_win_percent <- function(results, title) {
  p_all_role_win_percent <- (
    ggplot(data = results, aes(x = reorder(player, -win_percent), y = win_percent))
    + geom_bar(stat = "identity", fill = "steelblue")
      + labs(title = title, x = "player", y = "win %")
      + scale_y_continuous(labels = scales::percent_format())
      + theme_minimal()
      + theme(plot.title = element_text(hjust = 0.5))
  )
}

# Load config file
config_options <- yaml.load_file("config.yaml")

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

# Put all games into a big dataframe
big_df <- rbind.fill(games)

# All roles
all_role_molten_results <- get_molten_results(big_df)
all_role_results <- get_formatted_results(all_role_molten_results)

if (config_options$print_all_role_wins_losses) {
  p_all_role_wins_losses <- get_plot_wins_losses(all_role_results, all_role_molten_results, title = "Player wins/losses for all roles")
  x11()
  print(p_all_role_wins_losses)
}

if (config_options$print_all_role_win_percent) {
  p_all_role_win_percent <- get_plots_win_percent(all_role_results, title = "Player win percentage for all roles")
  x11()
  print(p_all_role_win_percent)
}

# Resistance team
resistance_molten_results <- get_molten_results(big_df[big_df$team == "resistance", ])
resistance_results <- get_formatted_results(resistance_molten_results)

if (config_options$print_resistance_wins_losses) {
  p_resistance_wins_losses <- get_plot_wins_losses(resistance_results, resistance_molten_results, title = "Player wins/losses for resistance team")
  x11()
  print(p_resistance_wins_losses)
}

if (config_options$print_resistance_win_percent) {
  p_resistance_win_percent <- get_plots_win_percent(resistance_results, title = "Player win percentage for resistance team")
  x11()
  print(p_resistance_win_percent)
}

# Spies team
spies_molten_results <- get_molten_results(big_df[big_df$team == "spies", ])
spies_results <- get_formatted_results(spies_molten_results)

if (config_options$print_spies_wins_losses) {
  p_spies_wins_losses <- get_plot_wins_losses(spies_results, spies_molten_results, title = "Player wins/losses for spies team")
  x11()
  print(p_spies_wins_losses)
}

if (config_options$print_spies_win_percent) {
  p_spies_win_percent <- get_plots_win_percent(spies_results, title = "Player win percentage for spies team")
  x11()
  print(p_spies_win_percent)
}

# Merlin role
merlin_molten_results <- get_molten_results(big_df[big_df$role == "merlin", ])
merlin_results <- get_formatted_results(merlin_molten_results)

if (config_options$print_merlin_wins_losses) {
  p_merlin_wins_losses <- get_plot_wins_losses(merlin_results, merlin_molten_results, title = "Player wins/losses for Merlin role")
  x11()
  print(p_merlin_wins_losses)
}

if (config_options$print_all_role_win_percent) {
  p_merlin_win_percent <- get_plots_win_percent(merlin_results, title = "Player win percentage for Merlin role")
  x11()
  print(p_merlin_win_percent)
}

# Wait for user to kill script (uncomment this if running with Rscript)
if (config_options$sleep_at_end_of_execution) {
  Sys.sleep(999999999999)
}
