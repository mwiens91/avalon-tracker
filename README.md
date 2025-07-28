# Avalon Tracker

This is a simple (?) script that produces various win/losses and win%
plots from [The Resistance:
Avalon](https://boardgamegeek.com/boardgame/128882/resistance-avalon)
game history data.

## Game history data

Game history data should follow the format shown in [the data text
file](data.txt). For convenience, I've been pushing results for *my*
group's Avalon games to this repository, but you can overwrite these
results with your own.

## Setup

### Installing required R packages

In an R enviroment run

```R
install.packages(c(
  "plyr",
  "dplyr",
  "ggplot2",
  "reshape2",
  "scales",
  "yaml"
))
```

### Setting up the configuration file

Create a configuration from [the example configuration
file](config.yaml.example) with

```
cp config.yaml.example config.yaml
```

and edit the file as appropriate to enable/disable the plots you want to
generate. If you're running from a shell, you most likely want the
`sleep_at_end_of_execution` set to `true` so the plots persist after
executing the script; in an environment such as
[RStudio](https://www.rstudio.com/), just keep that setting as `false`.

## Running the script

To run the script in a shell, do

```
./script.r
```
