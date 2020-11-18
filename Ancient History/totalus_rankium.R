library(tidyverse)
library(ggcorrplot)
library(GGally)

theme_set(theme_light())

### getting data and merging with extra dataset

df <- read_csv("totalus_rankium.csv") %>%
  janitor::clean_names() %>%
  select(-emperor_1) %>%
  rename(fightius_maximus = fights_maximus) %>%
  mutate(order = row_number())

rome <- df %>%
  filter(empire == "R") %>%
  select(-empire) %>%
  filter(order < 68)

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv") %>%
  filter(!name %in% c("Vetranio")) %>%
  mutate(order = row_number())

emperors <- emperors %>%
  separate(reign_start, 
           sep = "-", 
           into = c("reign_start_year", "reign_start_month", "reign_start_day"),
           remove = FALSE) %>%
  separate(reign_end, 
           sep = "-", 
           into = c("reign_end_year", "reign_end_month", "reign_end_day"),
           remove = FALSE)

emperors$reign_start_year  <- as.numeric(emperors$reign_start_year)
emperors$reign_start_month <- as.numeric(emperors$reign_start_month)
emperors$reign_start_day   <- as.numeric(emperors$reign_start_day)
emperors$reign_end_year    <- as.numeric(emperors$reign_end_year)
emperors$reign_end_month   <- as.numeric(emperors$reign_end_month)
emperors$reign_end_day     <- as.numeric(emperors$reign_end_day)

emperors$reign_start_year[1] <- -emperors$reign_start_year[1]
emperors$year <- emperors$reign_start_year
emperors <- merge(emperors, rome, by = "order")

years <- data.frame(year = rep(-100:395))

df <- merge(years, emperors, all.x = TRUE, all.y = TRUE, by= "year")

df <- df %>%
  fill(everything(), .direction = "down")

df = df[!duplicated(df$year),]

df <- df %>%
  mutate(century = ifelse(year < 0, "First Century BC",
                          ifelse(year < 100, "First Century",
                                 ifelse(year < 200, "Second Century",
                                        ifelse(year < 300, "Third Century",
                                               ifelse(year < 400, "Fourth Century"))))))
df <- df %>%
  group_by(century) %>%
  mutate(century_year = row_number()) %>%
  ungroup() %>%
  mutate(century = factor(century, levels = c("First Century BC",
                                              "First Century",
                                              "Second Century",
                                              "Third Century",
                                              "Fourth Century")),
         dynasty = factor(dynasty, levels = c("Julio-Claudian",
                                              "Flavian",
                                              "Nerva-Antonine",
                                              "Severan",
                                              "Gordian",
                                              "Constantinian",
                                              "Valentinian",
                                              "Theodosian")))

### stats over time

rome %>%
  pivot_longer(cols = fightius_maximus:total,
               values_to = "score",
               names_to = "category") %>%
  ggplot(aes(x = order, y = score)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(~category, scales = "free_y")


### correlation of stats

corr <- rome %>%
  select(fightius_maximus:tempo_completo) %>%
  cor()

ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           outline.col = "white",
           lab = TRUE,
           title = "Correlation Matrix of Roman Emperors",
           ggtheme = theme(legend.position = "none")) %>%
  ggsave(filename = "emperor_correlation.png",
         height = 5,
         width = 6,
         dpi = 180,
         type = "cairo-png")

ggplot(rome, aes(x = fightius_maximus, y = succesus_ultimus)) +
  geom_point(colour = "#66023c",
             size = 2) +
  geom_smooth(se = FALSE,
              method = "lm",
              colour = "#DFaF37",
              size = 1.2) +
  labs(x = "Fightius Maximus",
       y = "Succesus Ultimus",
       title = "Fightius Maximus and Succesus Ultimus") +
  ggsave(height = 5,
         width = 6,
         dpi = 180,
         type = "cairo-png",
         filename = "fightius_success.png")

ggplot(rome, aes(x = imago_facius, y = succesus_ultimus)) +
  geom_point(colour = "#66023c",
             size = 2) +
  geom_smooth(se = FALSE,
              method = "lm",
              colour = "#DFaF37",
              size = 1.2) +
  labs(x = "Imago Facius",
       y = "Succesus Ultimus",
       title = "Imago Facius and Succesus Ultimus") +
  ggsave(height = 5,
         width = 6,
         dpi = 180,
         type = "cairo-png",
         filename = "imago_success.png")

### using dates and dynasties

# scores by rise
emperors %>%
  group_by(rise) %>%
  summarise("Opprobrium Crazium" = mean(opprobrium_crazium),
            "Succesus Ultimus" = mean(succesus_ultimus),
            "Imago Facius" = mean(imago_facius),
            "Fightius Maximus" = mean(fightius_maximus),
            "Tempo Completo" = mean(tempo_completo),
            "total" = mean(total),
            n = n_distinct(index)) %>%
  filter(!rise %in% c("Election", "Purchase")) %>%
  pivot_longer(cols = "Opprobrium Crazium":total) %>%
  ggplot(aes(x = reorder(rise, n), y = value)) +
  geom_col(aes(fill = name)) +
  facet_wrap(~name, scales = "free_x") +
  coord_flip() +
  theme(legend.position = "none")

# scores by dynasty
emperors %>%
  group_by(dynasty) %>%
  summarise("Opprobrium Crazium" = mean(opprobrium_crazium),
            "Succesus Ultimus" = mean(succesus_ultimus),
            "Imago Facius" = mean(imago_facius),
            "Fightius Maximus" = mean(fightius_maximus),
            "Tempo Completo" = mean(tempo_completo),
            "total" = mean(total),
            order = mean(order)) %>%
  pivot_longer(cols = "Opprobrium Crazium":total) %>%
  ggplot(aes(x = reorder(dynasty, -order), y = value)) +
  geom_col(aes(fill = name)) +
  facet_wrap(~name, scales = "free_x") +
  coord_flip() +
  theme(legend.position = "none")

# scores by birth province
emperors %>%
  group_by(birth_prv) %>%
  summarise("Opprobrium Crazium" = mean(opprobrium_crazium),
            "Succesus Ultimus" = mean(succesus_ultimus),
            "Imago Facius" = mean(imago_facius),
            "Fightius Maximus" = mean(fightius_maximus),
            "Tempo Completo" = mean(tempo_completo),
            "total" = mean(total),
            n = n_distinct(order)) %>%
  pivot_longer(cols = "Opprobrium Crazium":total) %>%
  ggplot(aes(x = reorder(birth_prv, n), y = value)) +
  geom_col(aes(fill = name)) +
  facet_wrap(~name, scales = "free_x") +
  coord_flip() +
  theme(legend.position = "none")

# scores by cause
emperors %>%
  group_by(cause) %>%
  summarise("Opprobrium Crazium" = mean(opprobrium_crazium),
            "Succesus Ultimus" = mean(succesus_ultimus),
            "Imago Facius" = mean(imago_facius),
            "Fightius Maximus" = mean(fightius_maximus),
            "Tempo Completo" = mean(tempo_completo),
            "total" = mean(total),
            n = n_distinct(order)) %>%
  filter(!cause %in% c("Captivity", "Unknown")) %>%
  pivot_longer(cols = "Opprobrium Crazium":total) %>%
  ggplot(aes(x = reorder(cause, n), y = value)) +
  geom_col(aes(fill = name)) +
  facet_wrap(~name, scales = "free_x") +
  coord_flip() +
  theme(legend.position = "none")

# timeline of each score

df %>%
  filter(!is.na(order)) %>%
  pivot_longer(cols = fightius_maximus:total,
               names_to = "category") %>%
  ggplot(aes(x = reign_start_year, y = value)) +
  geom_smooth(aes(colour = category), se = FALSE) +
  facet_wrap(~category, scales = "free_y") +
  theme(legend.position = "none")

df %>%
  group_by(century) %>%
  filter(century != "First Century BC") %>%
  summarise("Opprobrium Crazium" = mean(opprobrium_crazium),
            "Succesus Ultimus" = mean(succesus_ultimus),
            "Imago Facius" = mean(imago_facius),
            "Fightius Maximus" = mean(fightius_maximus),
            "Tempo Completo" = mean(tempo_completo),
            "total" = mean(total),
            n = n_distinct(order)) %>%
  pivot_longer(cols = "Opprobrium Crazium":total) %>%
  ggplot(aes(x = century, y = value)) +
  geom_col(aes(fill = name)) +
  facet_wrap(~name, scales = "free_y")

# imago facio by dynasty

emperors %>%
  group_by(dynasty) %>%
  summarise(imago_facius = mean(imago_facius),
            order = min(order)) %>%
  ggplot(aes(x = reorder(dynasty, order), y = imago_facius)) +
  geom_col()

emperors %>%
  ggplot(aes(x = reorder(emperor, order), y = tempo_completo)) +
  geom_col(aes(fill = je_na_caesar)) +
  theme(axis.text.x = element_text(angle = 90))
