# Iteration 1: Points Rollup
library(dplyr)
link <- "https://docs.google.com/spreadsheets/d/1PUnl2D-CjsJtJs19QUsLCLRyOGn-cYtI_I-Rq9p0_Fc/edit#gid=0"
oldData <- googlesheets4::read_sheet(ss = link, sheet = "import2")

newData <- oldData %>%
  group_by(Rank) %>%
  mutate(newPoints = round(mean(RankOrderPoints), 3)) %>%
  ungroup() %>%
  select(PLAYER, POS, newPoints)

googlesheets4::write_sheet(data = newData, ss = link, sheet = "import3")
