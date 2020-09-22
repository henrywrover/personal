library(lubridate)
library(tidyverse)
theme_set(theme_minimal())

games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

games %>%
  filter(!is.na(owners)) %>%
  group_by(developer) %>%
  summarise(score = mean(metascore, na.rm = TRUE),
            playtime = mean(average_playtime, na.rm = TRUE),
            games = n()) %>%
  filter(!is.na(developer),
         !is.na(score),
         playtime > 0) %>%
  top_n(10, games) %>%
  ggplot(aes(x = reorder(developer, score), y = score, colour = playtime)) +
  geom_point(size = 4) +
  scale_y_continuous(limits = c(50,100)) +
  scale_colour_viridis_c() +
  coord_flip() +
  labs(title = "Top 10 Developers by Games published",
       subtitle = "How highly do their games score and how much are they played?",
       colour = "Average Playtime (Hours)",
       y = "Average Score on Metacritic",
       x = "Developer",
       caption = "@henrywrover2 \n github.com/henrywrover \n Source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-07-30") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggsave(filename = "developers.png", height = 6, width = 8, type = "cairo-png")

