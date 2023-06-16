link <- "https://docs.google.com/spreadsheets/d/1jLzNA7SvNOxBIpGpVSk-XOK5UdoOxWzEZGDLvGLrryE/edit#gid=0"
scoreboard <- googlesheets4::read_sheet(ss = link, sheet = "espnImport")
rosters <- googlesheets4::read_sheet(ss = link, sheet = "Rosters")
scoringRules <- googlesheets4::read_sheet(ss = link, sheet = "scoringRules")
library(dplyr)

scoreboard2 <- left_join(scoreboard, rosters, by = c("PLAYER" = "Golfer")) %>% 
  select(PLAYER, SCORE, Team) %>% 
  filter(SCORE != "CUT" & SCORE != "WD") %>% 
  mutate(SCORE = case_when(SCORE == "E" ~ 0,
                           TRUE ~ as.numeric(gsub("+", "", SCORE)))) %>% 
  arrange(SCORE) %>% 
  mutate(order = row_number(),
         rank = rank(SCORE, ties.method = "min"))

scores <- left_join(scoreboard2, scoringRules, by = c("order" = "Rank")) %>% 
  mutate(Points = coalesce(Points, 0)) %>% 
  group_by(rank) %>% 
  mutate(Points2 = mean(Points, na.rm = TRUE)) %>% 
  ungroup()

playerPoints <- scores %>% 
  select(PLAYER, Points2)

googlesheets4::write_sheet(playerPoints, ss = link, sheet = "playerPoints")
