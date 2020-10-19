library(ggthemes)
library(tidyverse)
theme_set(theme_light())
options(scipen = 999)

data <- read_csv("leeds.csv") %>%
  rename(year = X1,
         "January" = "Jan",
         "February" = "Feb",
         "March" = "Mar",
         "August" = "Aug",
         "September" = "Sept",
         "October" = "Oct",
         "November"= "Nov",
         "December" = "Dec") %>%
  select(-Total) %>%
  filter(year != "Total") %>%
  pivot_longer(cols = January:December, names_to = "month", values_to = "visitors") %>%
  mutate(year = as.numeric(year),
         visitors = as.numeric(visitors),
         month = factor(month, levels = month.name))

data %>%
  group_by(year) %>%
  summarise(total = sum(visitors)) %>%
  ggplot(aes(x = year, y = total)) +
  geom_line(colour = "grey25", size = 1.5) +
  geom_point(colour = "firebrick", size = 2.5) +
  theme_calc() +
  theme(plot.title = element_text(size = 11),
        plot.caption = element_text(size = 6)) +
  scale_y_continuous(limits = c(0,600000)) +
  labs(title = "The effect of moving Leeds visitor centre away from the train station",
       caption = "Twitter: @henrywrover2 | Github: henrywrover\n 19th October 2020",
       x = "Year",
       y = "Footfall") +
  ggsave(filename = "leeds.png", dpi = 120, height = 6, width = 8, type = "cairo-png")
