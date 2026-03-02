library(tidyverse)
library(worldfootballR)

# worldfootballR is no longer being maintained
# matches_eng <- worldfootballR::fb_match_results("ENG", "M", season_end_year = 2026)
# matches_esp <- worldfootballR::fb_match_results("ESP", "M", season_end_year = 2026)
# matches_ger <- worldfootballR::fb_match_results("GER", "M", season_end_year = 2026)
# matches_fra <- worldfootballR::fb_match_results("FRA", "M", season_end_year = 2026)
# matches_ita <- worldfootballR::fb_match_results("ITA", "M", season_end_year = 2026)


# Load data from football-data-uk like old times
matches_eng <- read_csv("https://www.football-data.co.uk/mmz4281/2526/E0.csv")
matches_esp <- read_csv("https://www.football-data.co.uk/mmz4281/2526/SP1.csv")
matches_ger <- read_csv("https://www.football-data.co.uk/mmz4281/2526/D1.csv")
matches_fra <- read_csv("https://www.football-data.co.uk/mmz4281/2526/F1.csv")
matches_ita <- read_csv("https://www.football-data.co.uk/mmz4281/2526/I1.csv")


# join all leagues
matches <- bind_rows(matches_eng, matches_esp, matches_ger, matches_fra, matches_ita)

# write to csv
write_csv(matches, "matches_all_leagues.csv")
