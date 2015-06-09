library(readr)
library(dplyr)
library(gather)
library(ggplot2)
library(data.table)

dfReplays <- read_csv("data/replays.csv")

dfChars <- read_csv("data/chars.csv")

dfDivisions <- data.frame(name = c("Bronze", "Silver", "Gold", "Platinum", "Diamond", "Master"),
                          low = c(-Inf, 1630, 1960, 2260, 2730, 3370),
                          high = c(1630, 1960, 2260, 2730, 3370, Inf),
                          stringsAsFactors = FALSE)

dfTeamMMRs <- dfChars %>% 
  filter(!is.na(mmr_before)) %>% 
  group_by(replayid, is_winner) %>% 
  summarise(mean_team_mmr = round(mean(mmr_before)),
            sd_team_mmr = round(sd(mmr_before),1)) %>% 
  group_by(replayid, is_winner) %>% 
  mutate(division = dfDivisions$name[max(which(dfDivisions$low <= mean_team_mmr))]) %>% 
  ungroup()
  
dfTeamMMRsWinners <- dfTeamMMRs %>% 
  filter(is_winner == "True") %>%
  select(-is_winner) %>% 
  rename(mean_team_mmr_win = mean_team_mmr, 
         sd_team_mmr_win = sd_team_mmr,
         division_win = division)
dfTeamMMRsLosers <- dfTeamMMRs %>% 
  filter(is_winner == "False")  %>%
  select(-is_winner) %>% 
  rename(mean_team_mmr_lose = mean_team_mmr, 
         sd_team_mmr_lose = sd_team_mmr,
         division_lose = division)


dfTeamMMRsSpread <- dfTeamMMRsWinners %>% 
  inner_join(dfTeamMMRsLosers, by = "replayid") %>% 
  arrange(division_win)

dfWinPercent <- dfTeamMMRsSpread %>% 
  count(division_win, division_lose) %>% 
  ungroup() %>% 
  filter(division_win != division_lose) %>% 
  rowwise() %>% 
  mutate(h2h = paste(sort(c(division_win, division_lose)), collapse = "")) %>% 
  ungroup() %>% 
  arrange(h2h) %>% 
  group_by(h2h) %>% 
  mutate(prop = round(n / sum(n)*100,1)) %>% 
  ungroup()

div <- "Master"

dfPlotWinPercent <- dfWinPercent %>% 
  filter(division_win == div) %>% 
  select(-division_win, -h2h) %>% 
  mutate(outcome = "win") %>% 
  rename(opponent = division_lose) %>% 
  bind_rows(dfWinPercent %>% 
              filter(division_lose == div) %>% 
              select(-division_lose, -h2h) %>% 
              mutate(outcome = "loss") %>% 
              rename(opponent = division_win)) %>% 
  mutate(opponent = factor(opponent, levels = dfDivisions$name))
ggplot(dfPlotWinPercent, aes(opponent, prop, fill = outcome)) + geom_bar(stat = "identity")


