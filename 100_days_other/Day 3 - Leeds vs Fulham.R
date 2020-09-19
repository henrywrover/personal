library(extrafont)
library(RColorBrewer)
library(tidyverse)
library(understatr)

stats <- get_match_stats(match_id = 14097)
shots <- get_match_shots(match_id = 14097) %>%
  mutate(team = case_when(
    h_a == "h" ~ "Leeds",
    h_a == "a" ~ "Fulham"
  ))

shots %>%
  group_by(team) %>%
  mutate(cumulative = cumsum(xG)) %>%
  ggplot(aes(x = minute, y = cumulative)) +
  geom_line(aes(colour = team), size = 2) +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  geom_col(data = shots %>% filter(result == "Goal"),
           aes(x = minute, y = xG, fill = team),
           width = 1,
           colour = "grey50") +
  geom_point(aes(x = minute, y = cumulative, size = xG), colour = "grey75") +
  labs(title = "Expected goals in Leeds vs Fulham 19-09-2020",
       subtitle = "Final Score: Leeds 4 Fulham 3 - Final xG: Leeds 1.46 Fulham 1.56 \nLine shows cumulative xG per team while points and bars show individual xG of shots and goals respectively",
       x = "Minute",
       y = "xG",
       fill = "Team",
       colour = "Team",
       caption = "@henrywrover2 \n github.com/henrywrover \n Source: https://understat.com/match/14097") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10),
        text = element_text(size = 12, family = "Gadugi"),
        plot.caption = element_text(size = 6)) +
  scale_x_continuous(breaks = c(0,15,30,45,60,75,90)) +
  ggsave(height = 6, width = 10, dpi = 120, type = "cairo-png", filename = "leeds_vs_fulham.png")
