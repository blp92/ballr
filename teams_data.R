teams_url = "http://stats.nba.com/stats/commonTeamYears?LeagueID=00"

request_headers = c(
  "accept-encoding" = "gzip, deflate, sdch",
  "accept-language" = "en-US,en;q=0.8",
  "cache-control" = "no-cache",
  "connection" = "keep-alive",
  "host" = "stats.nba.com",
  "pragma" = "no-cache",
  "upgrade-insecure-requests" = "1",
  "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9"
)

request = GET(teams_url, add_headers(request_headers))

teams_data = fromJSON(content(request, as = "text"))
teams = tbl_df(data.frame(teams_data$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
names(teams) = tolower(teams_data$resultSets$headers[[1]])

teams = mutate(teams,
  min_year = as.numeric(min_year),
  max_year = as.numeric(max_year),
  team_id = as.numeric(team_id)
)

if (Sys.Date() <= as.Date("2016-10-25")) {
  teams = mutate(teams, max_year = pmin(max_year, 2015))
}

#teams$name = sapply(team$display_last_comma_first, function(s) {
#  paste(rev(strsplit(s, ", ")[[1]]), collapse = " ")
#})

first_year_of_data = 1996
last_year_of_data = max(teams$max_year)
season_strings = paste(first_year_of_data:last_year_of_data,
                       substr(first_year_of_data:last_year_of_data + 1, 3, 4),
                       sep = "-")
names(season_strings) = first_year_of_data:last_year_of_data

available_teams = filter(teams, max_year >= first_year_of_data)

team_names_table = table(available_teams$abbreviation)
dupe_team_names = names(team_names_table[which(team_names_table > 1)])

#could comment this all out because team_names_table is already only the 30 unique current teams
available_teams$abbreviation[available_teams$abbreviation %in% dupe_team_names] = paste(
  available_teams$abbreviation[available_teams$abbreviation %in% dupe_team_names],
  available_teams$team_id[available_teams$abbreviation %in% dupe_team_names]
)

available_teams$lower_name = tolower(available_teams$abbreviation)
available_teams = arrange(available_teams, lower_name)

find_team_by_name = function(n) {
  filter(available_teams, lower_name == tolower(n))
}

find_teamr_id_by_name = function(n) {
  find_team_by_name(n)$team_id
}

default_team = find_team_by_name("MIN")
default_years = as.character(default_team$min_year:default_team$max_year)
default_seasons = as.character(season_strings[default_years])
default_season = rev(default_seasons)[1]

#--------Can't find the URL to get team logos easily
#team_photo_url = function(player_id) {
#  paste0("http://stats.nba.com/media/players/230x185/", player_id, ".png")
#}
