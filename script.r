#!/usr/bin/env Rscript

library(plyr)
suppressMessages(library(dplyr))
library(ggplot2)
library(reshape2)
library(scales)
library(yaml)

# Constants
RESISTANCE_ROLES <- c("resistance", "merlin", "percival")
SPIES_ROLES <- c("spy", "mordred", "morgana", "oberon", "morrigan", "golnar")

# Helper functions
get_molten_results <- function(df) {
  return(melt(select(df, player, result), id.var = c("player")))
}

get_formatted_results <- function(molten_results) {
  results <- dcast(molten_results, player ~ value, value.var = "player", length)

  if (!("win" %in% colnames(results))) {
    results <- cbind(results, win = 0)
  }

  if (!("loss" %in% colnames(results))) {
    results <- cbind(results, loss = 0)
  }

  results <- transform(
    results,
    win_delta = win - loss,
    total_games = win + loss,
    win_percent = win / (win + loss)
  )

  return(results)
}

get_plot_wins_losses <- function(wide_results, molten_results, title) {
  results_freq <- as.data.frame(molten_results %>% group_by(player, value) %>% tally())
  results_freq$n2 <- ifelse(results_freq$value == "loss", -1 * results_freq$n, results_freq$n)
  results_freq$player <- factor(
    results_freq$player,
    levels = wide_results[
      order(
        -wide_results$win_delta,
        -wide_results$total_games,
        as.character(wide_results$player)
      ),
    ]$player
  )
  results_freq$value <- factor(results_freq$value, levels = c("win", "loss"))

  return(
    ggplot(data = results_freq)
    + geom_bar(aes(x = player, y = n2, fill = value), stat = "identity", position = "identity")
      + labs(title = title, x = "player", y = "n")
      + scale_y_continuous(
        minor_breaks = NULL,
        breaks = function(x) unique(floor(seq(-max(x) - 1, (max(x) + 1) * 1.1)))
      )
      + scale_fill_manual(values = c("#00BFC4", "#F8766D"))
      + theme(plot.title = element_text(hjust = 0.5))
  )
}

get_plot_win_percent <- function(results, title) {
  results_win_per <- results
  results_win_per$player <- factor(
    results_win_per$player,
    levels = results_win_per[
      order(
        -results_win_per$win_percent,
        as.character(results_win_per$player)
      ),
    ]$player
  )

  return(
    ggplot(data = results_win_per, aes(x = player, y = win_percent))
    + geom_bar(stat = "identity", fill = "steelblue")
      + labs(title = title, x = "player", y = "win %")
      + scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1))
      + theme_minimal()
      + theme(plot.title = element_text(hjust = 0.5))
  )
}

print_results_plots <- function(
                                df,
                                print_wins_losses_plot = FALSE,
                                wins_losses_plot_title = "",
                                print_win_percent_plot = FALSE,
                                win_percent_plot_title = "") {
  # Get out if no rows in data frame
  if (dim(df)[1] == 0) {
    # TODO: find a way to not have this print NULL to the terminal
    return()
  }

  molten_results <- get_molten_results(df)
  results <- get_formatted_results(molten_results)

  if (print_wins_losses_plot) {
    p_wins_losses <- get_plot_wins_losses(
      results, molten_results, wins_losses_plot_title
    )
    x11()
    print(p_wins_losses)
  }

  if (print_win_percent_plot) {
    p_win_percent <- get_plot_win_percent(
      results, win_percent_plot_title
    )
    x11()
    print(p_win_percent)
  }
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
      if (role %in% RESISTANCE_ROLES) {
        "resistance"
      } else if (role %in% SPIES_ROLES) {
        "spies"
      } else {
        stop(sprintf("'%s' is not a recognized role!", role))
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

# Get out if no games
if (is.null(games)) {
  stop("No data!")
}

# Put all games into a big dataframe
big_df <- rbind.fill(games)

# All roles
print_results_plots(
  big_df,
  config_options$print_all_role_wins_losses,
  "Player wins/losses for all roles",
  config_options$print_all_role_win_percent,
  "Player win percentage for all roles"
)

# Resistance team
print_results_plots(
  big_df[big_df$team == "resistance", ],
  config_options$print_resistance_wins_losses,
  "Player wins/losses for resistance team",
  config_options$print_resistance_win_percent,
  "Player win percentage for resistance team"
)

# Spies team
print_results_plots(
  big_df[big_df$team == "spies", ],
  config_options$print_spies_wins_losses,
  "Player wins/losses for spies team",
  config_options$print_spies_win_percent,
  "Player win percentage for spies team"
)

# Vanilla resistance role
print_results_plots(
  big_df[big_df$role == "resistance", ],
  config_options$print_vanilla_resistance_wins_losses,
  "Player wins/losses for vanilla resistance role",
  config_options$print_vanilla_resistance_win_percent,
  "Player win percentage for vanilla resistance role"
)

# Vanilla spy role
print_results_plots(
  big_df[big_df$role == "spy", ],
  config_options$print_vanilla_spy_wins_losses,
  "Player wins/losses for vanilla spy role",
  config_options$print_vanilla_spy_win_percent,
  "Player win percentage for vanilla spy role"
)

# Mordred role
print_results_plots(
  big_df[big_df$role == "mordred", ],
  config_options$print_mordred_wins_losses,
  "Player wins/losses for Mordred role",
  config_options$print_mordred_win_percent,
  "Player win percentage for Mordred role"
)

# Morgana role
print_results_plots(
  big_df[big_df$role == "morgana", ],
  config_options$print_morgana_wins_losses,
  "Player wins/losses for Morgana role",
  config_options$print_morgana_win_percent,
  "Player win percentage for Morgana role"
)

# Percival role
print_results_plots(
  big_df[big_df$role == "percival", ],
  config_options$print_percival_wins_losses,
  "Player wins/losses for Percival role",
  config_options$print_percival_win_percent,
  "Player win percentage for Percival role"
)

# Merlin role
print_results_plots(
  big_df[big_df$role == "merlin", ],
  config_options$print_merlin_wins_losses,
  "Player wins/losses for Merlin role",
  config_options$print_merlin_win_percent,
  "Player win percentage for Merlin role"
)

# Wait for user to kill script (uncomment this if running with Rscript)
if (config_options$sleep_at_end_of_execution) {
  Sys.sleep(999999999999)
}
