# Win probability take 1
# The most naive way for POC
# 1 parameter, current score, add uniform distribution U(-5,5)
# Whichever team has tournament winner, wins the pool - the way we used to do it
# The plan is to scale this up
library(dplyr)
library(lubridate)
# read in data
link <- "https://docs.google.com/spreadsheets/d/1PUnl2D-CjsJtJs19QUsLCLRyOGn-cYtI_I-Rq9p0_Fc/edit#gid=0"
rosters <- googlesheets4::read_sheet(ss = link, sheet = "Rosters")

# Format data
rosters2 <- rosters %>% 
  mutate(Score = case_when(Score == "E" ~ 0,
                           TRUE ~ as.numeric(gsub("+", "", Score))))


# I want this function to: 
# Take the current score for each golfer, add Unif(-5,5) to each of them, and return the winning golfer with their winning score
reps <- 10000
simSkel <- rosters2 %>% 
  mutate(simScore = Score) %>% 
  slice_min(simScore)
start <- now()  
winnerSim <- for(i in 1:reps) {
  simulatedScores <- rosters2 %>% 
    mutate(simScore = Score + runif(nrow(rosters), -5, 5)) %>% 
    slice_min(simScore)
  simSkel <- rbind(simSkel, simulatedScores)
}
end <- now()

(duration <- difftime(end, start, units = "secs"))

results <- simSkel %>% 
  group_by(Team) %>% 
  mutate(winPercent = n()/nrow(simSkel)*100) %>% 
  slice_head() %>% 
  select(Team, winPercent)
