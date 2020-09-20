library(tidyverse)
library(ggcorrplot)
theme_set(theme_minimal())
options(scipen = 999)

songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv') %>%
  rename(positivity = valence) %>%
  mutate("Duration" = (duration_ms / 1000) / 60)

songs %>%
  ggplot(aes(x = Duration, y = reorder(playlist_genre, Duration, mean))) +
  geom_jitter(aes(colour = playlist_genre), alpha = 0.2) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Which genre features the longest songs?",
       subtitle = "Sample of 32,833 songs from Spotify",
       x = "Duration (Minutes)",
       y = "Genre",
       caption = "@henrywrover2 \n github.com/henrywrover \n Source: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-21/readme.md") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggsave(filename = "song_genres.png",
         height = 5, width = 7, dpi = 120, type = "cairo-png")
