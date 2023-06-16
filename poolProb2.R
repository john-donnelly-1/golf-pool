# Roll up pool Prob into overall scores for teams
# Still uses a naive estimate, predicts pool winner (team) instead of tournament winner (golfer)

library(dplyr)
library(lubridate)
# read in data
link <- "https://docs.google.com/spreadsheets/d/1PUnl2D-CjsJtJs19QUsLCLRyOGn-cYtI_I-Rq9p0_Fc/edit#gid=0"
googlesheets4::gs4_auth("john.donnelly@uconn.edu")
rosters <- googlesheets4::read_sheet(ss = link, sheet = "Rosters")
scoringRules <- googlesheets4::read_sheet(ss= link, sheet = "scoringRules") %>% 
  mutate(order = Rank, .keep = "unused")

rosters2 <- rosters %>% 
  filter(Score != "CUT" & Score != "WD") %>% 
  mutate(Score = case_when(Score == "E" ~ 0,
                           TRUE ~ as.numeric(gsub("+", "", Score))))


start <- now()
reps <- 1000
winnersVector <- character()
teamSim <- for(i in 1:reps) {
  simScores <- rosters2 %>% 
    mutate(simScore = round(Score + runif(nrow(rosters2), -5, 5)), 0) %>% 
    arrange(simScore) %>% 
    mutate(order = row_number(),
           rank = rank(simScore))
  
  simScores2 <- left_join(simScores, scoringRules, by = "order") %>% 
    group_by(rank) %>% 
    mutate(Points = mean(Points)) %>% 
    ungroup() %>% 
    group_by(Team) %>% 
    summarise(teamScore = sum(Points, na.rm=TRUE))
  
  winner <- simScores2 %>% 
    slice_max(teamScore) %>% 
    pull(Team)
  winnersVector <- c(winnersVector, winner)
}
table(winnersVector)
(duration <- now() - start)

