library(extrafont)
library(tidyverse)
library(fuzzyjoin)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(plyr)
library(showtext)

### load data
  
places <- read_csv("places.csv") %>%
  janitor::clean_names()

suffix <- read_csv("suffixes.csv") %>%
  janitor::clean_names() %>%
  dplyr::rename("town" = term)

shapefile <- readOGR(dsn = "C:/Users/Henry/Documents/R/2021/Nov/UK Place Names",
                     layer="Boundary-line-ceremonial-counties_region")

mapdata <- tidy(shapefile, region = "NAME")

### match data

df <- fuzzy_join(places, suffix, match_fun = str_detect, by = "town", mode = "full") %>%
  dplyr::rename("town" = town.x,
         "suffix" = town.y) %>%
  filter(endsWith(town, suffix) == TRUE) %>%
  add_row(id = 44961, town = NA, county = "East Riding of Yorkshire", suffix = NA, origin = "OE", meaning = NA)

df %>%
  group_by(suffix, origin) %>%
  tally(sort = TRUE)

### analysis

# map of counties and % ON

df2 <- df %>%
  group_by(county, origin) %>%
  tally() %>%
  pivot_wider(names_from = origin,
              values_from = n) %>%
  mutate(ON = ifelse(is.na(ON), 0, ON),
         OE = as.integer(ifelse(is.na(OE), 0, OE)),
         pct_ON = ON / (ON + OE),
         pct_ON = as.integer(ifelse(is.na(pct_ON), 0, pct_ON))) %>%
  dplyr::rename(id = county) %>%
  right_join(mapdata %>%
               filter(id %in% df$county), on = "id")

p <- ggplot() +
  geom_polygon(data = df2, aes(x = long, y = lat, group = group, fill = pct_ON), alpha = 0.8, color = "#F8F5E6", size = 0.6) +
  coord_fixed(1) +
  scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1L)) +
  theme_void() +
  theme(plot.title = element_text(size = 32, hjust = 0.5, family = "Century Gothic"),
        plot.background = element_rect(fill = "#F8F5E6", colour = "#F8F5E6"),
        plot.subtitle = element_text(size = 16, hjust = 0.5, family = "Century Gothic"),
        legend.title = element_text(size = 16, hjust = 0.5, vjust = 0.7, family = "Century Gothic"),
        legend.text = element_text(size = 12, hjust = -0.1, family = "Century Gothic"),
        legend.position = "bottom",
        legend.key.size = unit(1.5, 'cm'),
        plot.margin=unit(c(1,1,1,1),"cm")) +
  labs(fill = "% of ON Town Names within County",
       title = "Town names in England with Old Norse Origins\n",
       subtitle = "Beginning in the 8th Century, England was frequently raided by Vikings, and by the late 9th Century
       they had begun to settle in the North and North-East regions. These settlements are marked today by the presence
       of suffixes with Old Norse origins in the towns and villages in the area. Suffixes such as '-by' (Grimsby, Derby)
       and '-thorpe' (Scunthorpe, Mablethorpe) can be seen frequently throughout these areas. Meanwhile in the South and
       West of England suffixes with Old English origins, such as '-ton' (Brighton, Southampton) or '-ham' (Lewisham, Horsham),
       are much more prominent")

ggsave(p, filename = "norse_plot.tiff", width = 15, height = 20)
