library(tidyverse)
library(worldfootballR)

matches_eng <- worldfootballR::fb_match_results("ENG", "M", season_end_year = 2024)
matches_esp <- worldfootballR::fb_match_results("ESP", "M", season_end_year = 2024)
matches_ger <- worldfootballR::fb_match_results("GER", "M", season_end_year = 2024)
matches_fra <- worldfootballR::fb_match_results("FRA", "M", season_end_year = 2024)
matches_ita <- worldfootballR::fb_match_results("ITA", "M", season_end_year = 2024)


# join all leagues
matches <- bind_rows(matches_eng, matches_esp, matches_ger, matches_fra, matches_ita)

# write to csv
write_csv(matches, "matches_all_leagues.csv")
