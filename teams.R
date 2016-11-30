
# Load dependencies -------------------------------------------------------


library(rvest)
# library(xml2)
# library(tools)
#!!!!!
# Set the working directory here; important for reading-in files
setwd("~/R/misc/basketball")
#!!!!!

# Top level scraping of 'Team' --------------------------------------------


# Input - base url
base <- "http://www.basketball-reference.com"


# Extract ~/teams/ tables -------------------------------------------------


# Input - ~/teams url
URL <- file.path(base, "teams")

# Input - table identifiers
active_id  <- "teams_active"
defunct_id <- "teams_defunct"

# Data - active and defunct team tables
tbls    <- read_html(URL) %>% html_nodes("table")
tbls.id <- html_attr(tbls, "id")
tbl.act <- tbls[tbls.id == active_id] %>% html_table() %>% `[[`(1)
tbl.def <- tbls[tbls.id == defunct_id] %>% html_table() %>% `[[`(1)

# File system - save active and defunct team tables
dir.create("teams")
write.csv(tbl.act, "teams/Active_Franchises.csv", row.names = FALSE)
write.csv(tbl.def, "teams/Defunct_Franchises.csv", row.names = FALSE)  


# Extract ~/teams/<shortName> table (Seasons.csv) -------------------------


# Input - team links
team.links <- html_nodes(tbls, "a") %>%
  html_attrs() %>% 
  sapply(function(x) gsub("^/", "", x)) %>%
  file.path(base, .)

# Input - team short names (from the non-parsed active and defunct HTML tables)
short.name <- basename(team.links)

# Function - scrape "Seasons" table for every team (~/teams/<shortName>)
team_recurse <- function(link) {
  short.name  <- basename(link)

  # Find table and table ids
  tbls        <- read_html(link) %>% html_nodes("table")
  tbls.id     <- html_attr(tbls, "id")
  tbl.seasons <- tbls[tbls.id == short.name] %>% html_table() %>% `[[`(1)

  # Create directory, dump data, and return the non-parse html table
  fpath <- file.path("teams", short.name)
  dir.create(fpath)
  fname <- paste0(file.path(fpath, "Seasons"), ".csv")
  write.csv(tbl.seasons, fname)
  return(tbls[tbls.id == short.name])
}

# Input - team links (from the "Seasons" table in ~/teams/<shortName>)
team.seasons <- lapply(team.links, team_recurse)
    # team.season <- Map(team_recurseNEW, team.links, short.name)
names(team.seasons) <- short.name


# Download individual team seasons (~/teams/<shortName>/<year> ------------


# teams <- list.dirs("basketball/teams", full.names = FALSE, recursive = FALSE)

# Function - extract links to all seasons for every team 
season_links <- function(tbl) {
  header <- tbl %>% 
    html_nodes("thead") %>% 
    html_nodes("th") %>% 
    html_text()
  pos    <- grep("Season", header)
  rows   <- tbl %>% html_nodes("tbody") %>% html_nodes("tr")
  hrefs  <- sapply(rows, function(x) {
    children <- xml2::xml_children(x)
    children[pos] %>% html_nodes("a") %>% html_attr("href")
  })
  return(paste0(base, hrefs))
}

# Input - links for all seasons for all teams (~/teams/<shorName> table)
season.links <- lapply(team.seasons, season_links)

  # Input - corresponding file names to generate for $season.links elements
  season.links.fnames <- lapply(season.links, function(x) {
    x %>% gsub(paste0(base, "/teams/"), "", .)
    })


# Input - all short names (team short name corresponding to a specific season)
short.name.season <- sapply(season.links, function(x) {
  basename(gsub("[0-9]*\\.html$", "", x))
})

  # Input - all unique short names for $short.name.season
  short.name.season.unique <- lapply(short.name.season, unique)

# Function - download all seasons for every team (wrapper function for loop)
download_season <- function(teams, dests) {
  
  # Function - download all seasons for every team (actual loop function)
  d_season <- function(seasons, teamname, fname) {
    fnames <- file.path("teams", teamname, fname)
    Map(f = download.file, seasons, fnames)
  }
  
  lapply(list(teams), function(teams.unlist) {
    Map(f = d_season, teams.unlist, names(teams.unlist), dests)
  })
}

# File System - create folder names for HTML files
dirs <- lapply(seq_along(short.name.season.unique), function(x) {
  lapply(short.name.season.unique[x], function(y) {
    Map(dir.create, file.path("teams", names(short.name.season.unique)[x], y))
  })
})

# Data - HTML page for all seasons for every team
team.seasons.html <- download_season(season.links, season.links.fnames)



# Scrape individual team season data (~/teams/<shortName>/<year> tables ----

# Input - directories where HTML files are located
dirs <- dir("teams") %>% .[tools::file_ext(.) == ""]
dirs <- lapply(seq_along(dirs), function(x) {
    ndir <- dir(file.path("teams", dirs[x]), recursive = TRUE) %>%
      .[tools::file_ext(.) == "html"]
    file.path("teams", dirs[x], ndir)
    })
names(dirs) <- sort(short.name)

# Input - CSS selectors for tables
season_table_ids <- list(
  roster_id            = "#all_roster",
  staff_id             = "#all_assistant_coaches",
  teamOP_id            = "#all_team_and_opponent",
  teamMisc_id          = "#all_team_misc",
  perGame_id           = "#all_per_game",
  total_id             = "#all_totals",
  per36Minutes_id      = "#all_per_minute",
  per100Poss_id        = "#all_per_poss",
  advanced_id          = "#all_advanced",
  shooting_id          = "#all_shooting",
  playoffsTotal_id     = "#all_playoffs_totals",
  playoffsPerGame_id   = "#all_playoffs_per_game",
  playoffsPerMinute_id = "#all_playoffs_per_minute",
  playoffsPerPoss_id   = "#all_playoffs_per_poss",
  playoffsAdvanced_id  = "#all_playoffs_advanced",
  playoffsShooting_id  = "#all_playoffs_shooting",
  salaries_id          = "#all_salaries"
)

# Section  - constructor functions
  # Function - is this an HTML node?
  is.node <- function(node) identical(attr(node, "class"), "xml_missing")

  # Function - get an HTML table that is commented out
  getTableFromComments <- function(html, id, parse = TRUE) {
    node <- html %>% html_nodes(id)
    if(!length(node)) return(NULL)
    
    tbl <- node %>% html_nodes("table")
    if (!length(tbl)) {
      tbl <- node %>%
        html_nodes(xpath = "comment()") %>%
        html_text() %>%
        read_html() %>%
        html_nodes("table")
      }
    if (parse) tbl <- tbl %>% html_table() %>% `[[`(1)
    # AND THEN WRITE
    return(tbl)
  }

  # Function - read every season link and get tables
  season_recurse <- function(link, ids) {
    page <- link %>% read_html()
    lapply(ids, function(x, y) getTableFromComments(y, x), y = page)
  }

# Function - read every team element
team_season_recurse <- function(list, ids) {
  team <- lapply(seq_along(list), function(x) season_recurse(list[x], ids))
  names(team) <- gsub("\\.html", "", basename(list))
  return(team)
}

# Data - all tables for all seasons for every team
team.seasons.stats <- lapply(dirs, function(x) {
  team_season_recurse(list = x, ids = season_table_ids)
  })

# There are some 'Team' fields where a (1) or similar footnote exists.
