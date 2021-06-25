library(tidyverse)

rm(list = ls())

start.time <- Sys.time()

iterations = 100

team_ratings = read_csv("team_ratings.csv") %>%
  janitor::clean_names()

last_16_df <- read_csv("last_16_initial.csv") %>%
  janitor::clean_names()
  
  last_8_df <- read_csv("last_8.csv") %>%
  janitor::clean_names()
  
  last_4_df <- read_csv("last_4.csv") %>%
    janitor::clean_names()
  
  final_df <- read_csv("final.csv") %>%
    janitor::clean_names()

combined_results = data.frame()

for(i in 1:iterations){

  ### last 16
  
  last_16_matches <- last_16_df %>%
    rowwise() %>%
    mutate(match = paste(team_a, team_b, sep = " vs "),
           elo_diff = (min(team_a_points, team_b_points) - max(team_a_points, team_b_points)),
           team_a_pct = ifelse(team_a_points > team_b_points,
                               1 / (1 + 10^(elo_diff/400)),
                               1 - (1 / (1 + 10^(elo_diff/400))))) %>%
    ungroup()
  
  last_16_results = data.frame()
  
  for (i in 1:1){
    model <-   last_16_matches %>%
      mutate(rand = sample(100, size = nrow(last_16_matches), replace = TRUE)/100,
             winner = ifelse(rand > team_a_pct, team_b, team_a),
             loser = ifelse(rand < team_a_pct, team_b, team_a))
    
    df <- data.frame(model)
    last_16_results <- rbind(last_16_results,df)
  }
  
  last_16_losers <-
    last_16_results %>%
    group_by(match, loser) %>%
    tally() %>%
    group_by(match) %>%
    top_n(1, n) %>%
    ungroup() %>%
    select(loser) %>%
    mutate(result = "Last 16",
           points = 1) %>%
    rename(team = loser)
  
  last_16_results <-
  last_16_results %>%
    group_by(match, winner) %>%
    tally() %>%
    group_by(match) %>%
    top_n(1, n)
  
  ### last 8
  
  last_8_matches <- last_8_df %>%
    merge(last_16_results,
          by.x = "team",
          by.y = "winner") %>%
    select(team, match.x) %>%
    group_by(match.x) %>%
    mutate(rn = 1:n()) %>%
    ungroup() %>%
    spread(rn, team) %>%
    merge(team_ratings, by.x = "1", by.y = "team") %>%
    rename(team_a_points = "points") %>%
    merge(team_ratings, by.x = "2", by.y = "team") %>%
    rename("team_b" = 1,
           "team_a" = 2,
           "team_b_points" = 5) %>%
    select(-match.x) %>%
    rowwise() %>%
    mutate(match = paste(team_a, team_b, sep = " vs "),
           elo_diff =  (min(team_a_points, team_b_points) - max(team_a_points, team_b_points)),
           team_a_pct = ifelse(team_a_points > team_b_points,
                               1 / (1 + 10^(elo_diff/400)),
                               1 - (1 / (1 + 10^(elo_diff/400))))) %>%
    ungroup()
    
  last_8_results = data.frame()
  
  for (i in 1:1){
    model <- last_8_matches %>%
      mutate(rand = sample(100, size = nrow(last_8_matches), replace = TRUE)/100,
             winner = ifelse(rand > team_a_pct, team_b, team_a),
             loser = ifelse(rand < team_a_pct, team_b, team_a))
    
    df <- data.frame(model)
    last_8_results <- rbind(last_8_results,df)
  }
  
  last_8_losers <-
    last_8_results %>%
    group_by(match, loser) %>%
    tally() %>%
    group_by(match) %>%
    top_n(1, n) %>%
    ungroup() %>%
    select(loser) %>%
    mutate(result = "Quarter Finals",
           points = 2) %>%
    rename(team = loser)
  
  last_8_results <-
    last_8_results %>%
    group_by(match, winner) %>%
    tally() %>%
    group_by(match) %>%
    top_n(1, n)

  ### last 4
  
  last_4_matches <- last_4_df %>%
    merge(last_8_results,
          by.x = "team",
          by.y = "winner") %>%
    select(team, match.x) %>%
    group_by(match.x) %>%
    mutate(rn = 1:n()) %>%
    ungroup() %>%
    spread(rn, team) %>%
    merge(team_ratings, by.x = "1", by.y = "team") %>%
    rename(team_a_points = "points") %>%
    merge(team_ratings, by.x = "2", by.y = "team") %>%
    rename("team_b" = 1,
           "team_a" = 2,
           "team_b_points" = 5) %>%
    select(-match.x) %>%
    rowwise() %>%
    mutate(match = paste(team_a, team_b, sep = " vs "),
           elo_diff =  (min(team_a_points, team_b_points) - max(team_a_points, team_b_points)),
           team_a_pct = 1 / (1 + 10^(elo_diff/400))) %>%
    ungroup()
  
  last_4_results = data.frame()
  
  for (i in 1:1){
    model <- last_4_matches %>%
      mutate(rand = sample(100, size = nrow(last_4_matches), replace = TRUE)/100,
             winner = ifelse(rand > team_a_pct, team_b, team_a),
             loser = ifelse(rand < team_a_pct, team_b, team_a))
    
    df <- data.frame(model)
    last_4_results <- rbind(last_4_results,df)
  }
  
  last_4_losers <-
    last_4_results %>%
    group_by(match, loser) %>%
    tally() %>%
    group_by(match) %>%
    top_n(1, n) %>%
    ungroup() %>%
    select(loser) %>%
    mutate(result = "Semi Finals",
           points = 5) %>%
    rename(team = loser)
  
  last_4_results <-
    last_4_results %>%
    group_by(match, winner) %>%
    tally() %>%
    group_by(match) %>%
    top_n(1, n)

  ### final
  
  final <- final_df %>%
    merge(last_4_results,
          by.x = "team",
          by.y = "winner") %>%
    select(team) %>%
    t() %>%
    data.frame() %>%
    merge(team_ratings, by.x = "X1", by.y = "team") %>%
    rename(team_a_points = "points") %>%
    merge(team_ratings, by.x = "X2", by.y = "team") %>%
    rename("team_b" = 1,
           "team_a" = 2,
           "team_b_points" = 4) %>%
    rowwise() %>%
    mutate(match = paste(team_a, team_b, sep = " vs "),
           elo_diff =  (min(team_a_points, team_b_points) - max(team_a_points, team_b_points)),
           team_a_pct = 1 / (1 + 10^(elo_diff/400))) %>%
    ungroup()
  
  final_results = data.frame()
  
  for (i in 1:1){
    model <- final %>%
      mutate(rand = sample(100, size = nrow(final), replace = TRUE)/100,
             winner = ifelse(rand > team_a_pct, team_b, team_a),
             loser = ifelse(rand < team_a_pct, team_b, team_a))
    
    df <- data.frame(model)
    final_results <- rbind(final_results,df)
  }
  
  runner_up <-
    final_results %>%
    group_by(match, loser) %>%
    tally() %>%
    group_by(match) %>%
    top_n(1, n) %>%
    ungroup() %>%
    select(loser) %>%
    mutate(result = "Runner Up",
           points = 8) %>%
    rename(team = loser)
  
  winner <-
    final_results %>%
    group_by(match, winner) %>%
    tally() %>%
    group_by(match) %>%
    top_n(1, n) %>%
    ungroup() %>%
    select(winner) %>%
    mutate(result = "Winner",
           points = 10) %>%
    rename(team = winner)
  
  tournament_results <- rbind(last_16_losers, last_8_losers, last_4_losers, runner_up, winner)
  
  combined_results <- rbind(combined_results, tournament_results)

}

results <- combined_results %>%
  group_by(team, result) %>%
  mutate(result = factor(result, levels=c("Last 16", "Quarter Finals", "Semi Finals", "Runner Up", "Winner"))) %>%
  summarise(n = n(),
            points = sum(points)) %>%
  mutate(pct = round(n/iterations, 2)) %>%
  group_by(team) %>%
  mutate(total_points = sum(points))

ggplot(results, aes(x = result, y = reorder(team, total_points))) +
  geom_tile(aes(fill = pct)) +
  geom_text(aes(label = scales::percent(pct, 2))) +
  scale_fill_viridis_c(labels = scales::percent_format()) +
  theme_minimal()

end.time <- Sys.time()

time.taken <- end.time - start.time
time.taken
