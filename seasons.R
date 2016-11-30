
# Load dependencies -------------------------------------------------------


library(rvest)
# library(xml2)
# library(tools)
#!!!!!
# Set the working directory here; important for reading-in files
setwd("~/R/misc/basketball")
#!!!!!


# Top level scraping of 'Seasons' -----------------------------------------


# Input - base url
base <- "http://www.basketball-reference.com"


# Extract ~/leagues table -------------------------------------------------

# Section - retrieve the non-parsed ~/leagues table
  # Input - url path to seasons data
  url <- file.path(base, "leagues")
  
  # Input - table identifier
  league_index_id <- "#all_stats"
  
  # Input - non-parsed ~/leagues table
  tbl.notparsed <- read_html(url) %>%
    html_node(league_index_id) %>%
    html_nodes("table")

# Data - NBA & ABA League Index table (~/leagues table)
tbl.parsed <- tbl.notparsed %>%
  html_table() %>%
  `[[`(1)


# Retrive season links from ~/leagues table -------------------------------


# Section - retrieve season links from the non-parsed ~/leagues table
  # Input - th's which may contain season links
  th <- tbl.notparsed %>%
    html_nodes("tr") %>%
    html_nodes("th")
  
  # Input - th identifier to scrape individual season links
  sub <- th %>%
    html_attr("data-stat")

  # Input - season links
  season.links <- th[grep("^season$", sub)] %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    paste0(base, .)


# Extract individual season data (~/leagues/<year>) -----------------------


# Input - div identifiers
div.ids <- list(
  conf_standings        = "#all_confs_standings_E",
  div_standings         = "#all_divs_standings_E",
  playoff_series        = "#all_all_playoffs",
  team_per_game_stats   = "#all_team-stats-per_game",
  opp_per_game_stats    = "#all_opponent-stats-per_game",
  team_stats            = "#all_team-stats-base",
  opp_stats             = "#all_opponent-stats-base",
  misc_stats            = "#all_misc_stats",
  team_shooting         = "#all_team_shooting",
  opp_shooting          = "#all_opponent_shooting",
  league_awards         = "#all_all_awards",
  players_of_week_month = "#all_players_of_the_week_and_month",
  league_leaders        = "#all_leaders",
  all_nba               = "#all_all-nba",
  all_defensive         = "#all_all-defensive",
  all_rookie            = "#all_all-rookie",
  all_star_rosters      = "#all_all_star_game_rosters"
)

# Input - links to Standings, Schedule and Results, and Leaders
season.misc.links <- page %>%
  html_nodes("#inner_nav") %>%
  html_nodes("ul") %>%
  html_nodes("li")





















