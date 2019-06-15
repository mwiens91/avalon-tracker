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

# Put all games into a big dataframe
big_df <- rbind.fill(games)

# Count player win rates over all roles
all_role_molten_results <- melt(select(big_df, player, result), id.var = c("player"))
all_role_results <- dcast(all_role_molten_results, player ~ value, value.var = "player", length)
all_role_results <- transform(all_role_results, win_percent = win / (win + loss))

# Plot wins/losses for all roles
all_role_results_freq <- as.data.frame(all_role_molten_results %>% group_by(player, value) %>% tally())
all_role_results_freq$n2 <- ifelse(all_role_results_freq$value == "loss", -1 * all_role_results_freq$n, all_role_results_freq$n)

p_all_role_wins_losses <- (
  ggplot(data = all_role_results_freq)
  + geom_bar(aes(x = reorder(player, -n2), y = n2, fill = value), stat = "identity", position = "identity")
    + labs(title = "Player wins/losses for all roles", x = "player", y = "n")
    + scale_y_continuous(label = abs, breaks = pretty_breaks())
    + theme(plot.title = element_text(hjust = 0.5))
)
x11()
print(p_all_role_wins_losses)

# Plot win % for all roles
p_all_role_win_percent <- (
  ggplot(data = all_role_results, aes(x = reorder(player, -win_percent), y = win_percent))
  + geom_bar(stat = "identity", fill = "steelblue")
    + labs(title = "Player win percentage for all roles", x = "player", y = "win %")
    + scale_y_continuous(labels = scales::percent_format())
    + theme_minimal()
    + theme(plot.title = element_text(hjust = 0.5))
)
x11()
print(p_all_role_win_percent)

# Count player win rates for resistance team
resistance_big_df <- big_df[big_df$team == "resistance", ]
resistance_molten_results <- melt(select(resistance_big_df, player, result), id.var = c("player"))
resistance_results <- dcast(resistance_molten_results, player ~ value, value.var = "player", length)

# Plot wins/losses for resistance team
resistance_results_freq <- as.data.frame(resistance_molten_results %>% group_by(player, value) %>% tally())
resistance_results_freq$n2 <- ifelse(resistance_results_freq$value == "loss", -1 * resistance_results_freq$n, resistance_results_freq$n)
resistance_results <- transform(resistance_results, win_percent = win / (win + loss))

p_resistance_wins_losses <- (
  ggplot(data = resistance_results_freq)
  + geom_bar(aes(x = reorder(player, -n2), y = n2, fill = value), stat = "identity", position = "identity")
    + labs(title = "Player wins/losses for resistance team", x = "player", y = "n")
    + scale_y_continuous(label = abs, breaks = pretty_breaks())
    + theme(plot.title = element_text(hjust = 0.5))
)
x11()
print(p_resistance_wins_losses)

# Plot win % for all roles
p_resistance_win_percent <- (
  ggplot(data = resistance_results, aes(x = reorder(player, -win_percent), y = win_percent))
  + geom_bar(stat = "identity", fill = "steelblue")
    + labs(title = "Player win percentage for resistance team", x = "player", y = "win %")
    + scale_y_continuous(labels = scales::percent_format())
    + theme_minimal()
    + theme(plot.title = element_text(hjust = 0.5))
)
x11()
print(p_resistance_win_percent)


# Count player win rates for spies team
spies_big_df <- big_df[big_df$team == "spies", ]
spies_molten_results <- melt(select(spies_big_df, player, result), id.var = c("player"))
spies_results <- dcast(spies_molten_results, player ~ value, value.var = "player", length)

# Plot wins/losses for spies team
spies_results_freq <- as.data.frame(spies_molten_results %>% group_by(player, value) %>% tally())
spies_results_freq$n2 <- ifelse(spies_results_freq$value == "loss", -1 * spies_results_freq$n, spies_results_freq$n)
spies_results <- transform(spies_results, win_percent = win / (win + loss))

p_spies_wins_losses <- (
  ggplot(data = spies_results_freq)
  + geom_bar(aes(x = reorder(player, -n2), y = n2, fill = value), stat = "identity", position = "identity")
    + labs(title = "Player wins/losses for spies team", x = "player", y = "n")
    + scale_y_continuous(label = abs, breaks = pretty_breaks())
    + theme(plot.title = element_text(hjust = 0.5))
)
x11()
print(p_spies_wins_losses)

# Plot win % for all roles
p_spies_win_percent <- (
  ggplot(data = spies_results, aes(x = reorder(player, -win_percent), y = win_percent))
  + geom_bar(stat = "identity", fill = "steelblue")
    + labs(title = "Player win percentage for spies team", x = "player", y = "win %")
    + scale_y_continuous(labels = scales::percent_format())
    + theme_minimal()
    + theme(plot.title = element_text(hjust = 0.5))
)
x11()
print(p_spies_win_percent)


# Count player win rates for Merlin role
merlin_big_df <- big_df[big_df$role == "merlin", ]
merlin_molten_results <- melt(select(merlin_big_df, player, result), id.var = c("player"))
merlin_results <- dcast(merlin_molten_results, player ~ value, value.var = "player", length)

# Plot wins/losses for all roles
merlin_results_freq <- as.data.frame(merlin_molten_results %>% group_by(player, value) %>% tally())
merlin_results_freq$n2 <- ifelse(merlin_results_freq$value == "loss", -1 * merlin_results_freq$n, merlin_results_freq$n)
merlin_results <- transform(merlin_results, win_percent = win / (win + loss))

p_merlin_wins_losses <- (
  ggplot(data = merlin_results_freq)
  + geom_bar(aes(x = reorder(player, -n2), y = n2, fill = value), stat = "identity", position = "identity")
    + labs(title = "Player wins/losses for Merlin role", x = "player", y = "n")
    + scale_y_continuous(label = abs, breaks = pretty_breaks())
    + theme(plot.title = element_text(hjust = 0.5))
)
x11()
print(p_merlin_wins_losses)

# Plot win % for all roles
p_merlin_win_percent <- (
  ggplot(data = merlin_results, aes(x = reorder(player, -win_percent), y = win_percent))
  + geom_bar(stat = "identity", fill = "steelblue")
    + labs(title = "Player win percentage for Merlin role", x = "player", y = "win %")
    + scale_y_continuous(labels = scales::percent_format())
    + theme_minimal()
    + theme(plot.title = element_text(hjust = 0.5))
)
x11()
print(p_merlin_win_percent)

# Wait for user to kill script (uncomment this if running with Rscript)
# Sys.sleep(999999999999)