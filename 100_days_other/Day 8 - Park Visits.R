library(tidyverse)
library(geofacet)
theme_set(theme_bw())

options(scipen = 999)

park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")

merged <- merge(park_visits, state_pop, by = c("state", "year"), all.x = TRUE)

plot <- merged %>%
  filter(year != "Total") %>%
  group_by(state, year) %>%
  summarise(visits = sum(visitors),
            pop = mean(pop, na.rm = TRUE)) %>%
  filter(!is.na(pop)) %>%
  mutate(thou = pop/100000,
         visits_per_100k = round(visits / thou)) %>%
  ggplot(aes(x = as.numeric(year), y = visits_per_100k)) +
  geom_line(size = 1, colour = "firebrick", alpha = 0.8) +
  facet_geo(~ state, grid = "us_state_grid2", label = "name", scales = "free_y") +
  theme(axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        strip.text = element_text(size = 6)) +
  labs(title = "National Parks in America face lowest numbers since mid-1970s, although some states appear to be immune to change",
       subtitle = "Visits to national parks by state per 100,000 population, 1904 to 2016",
       x = "",
       y = "",
       caption = "Twitter @henrywrover2 | github.com/henrywrover \nSource: https://data.world/inform8n/us-national-parks-visitation-1904-2016-with-boundaries")

ggsave(plot, filename = "US_park2.png", height = 10, width = 15, dpi = 150)