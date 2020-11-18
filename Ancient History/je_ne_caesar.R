### please load data from totalus rankium file first
library(png)
library(jpeg)
library(cowplot)
library(caret)
library(patchwork)
library(extrafont)
theme_set(theme_minimal())

emperors <- emperors %>%
  rename("Fightius Maximus" = "fightius_maximus",
         "Opprobrium Crazium" = "opprobrium_crazium",
         "Succesus Ultimus" = "succesus_ultimus",
         "Imago Facius" = "imago_facius",
         "Tempo Completo" = "tempo_completo")

rome <- rome %>%
  rename("Fightius Maximus" = "fightius_maximus",
         "Opprobrium Crazium" = "opprobrium_crazium",
         "Succesus Ultimus" = "succesus_ultimus",
         "Imago Facius" = "imago_facius",
         "Tempo Completo" = "tempo_completo")

### creating the theme

theme_rome <- function() {
  font <- "Times New Roman"
  theme_minimal() %+replace%
    
    theme(
      panel.grid = element_blank(),
      title = element_text(family = "Times New Roman",
                           size = 10,
                           hjust = 0.5),
      plot.subtitle= element_text(family = "Times New Roman",
                             size = 8,
                             hjust = 0.5),
      axis.ticks = element_blank(),
      axis.title = element_text(family = "Times New Roman",
                                size = 8,
                                face = "bold",
                                hjust = 0.5,
                                vjust = 0.5),
      axis.text = element_text(family = "Times New Roman",
                               size = 8),
      legend.position = "none",
      plot.caption = element_text(size = 6,
                                  family = "Times New Roman"),
      strip.text = element_text(size = 6, 
                                family = "Times New Roman"),
      panel.spacing = unit(.25, "cm")
    )
}

ml <- rome %>%
  mutate(je_ne_caesar = ifelse(je_na_caesar == "Yes", 1, 0)) %>%
  mutate(je_ne_caesar = as.factor(je_ne_caesar)) %>%
  select(-c("order", "emperor", "total", "je_na_caesar"))

### how to win je na caesar

# machine learning

scaled <- ml %>% mutate_at(c("Fightius Maximus", "Opprobrium Crazium", "Succesus Ultimus", "Imago Facius", "Tempo Completo"), ~(scale(.) %>% as.vector))

model.cv <- train(je_ne_caesar ~ .,
                  data = scaled,
                  preProcess = c('scale', 'center'),
                  na.action = na.omit)

ml_plot <- ggplot(varImp(model.cv)) +
                    geom_col(fill = "#DFaF37",
                             colour = "#66023c",
                             size = 2,
                             width = 0.8) +
  theme_rome() +
  labs(title = "What is the most important factor?",
       subtitle = "The big one at last. Using a lot of copy and paste and some machine learning methods\nI don't fully understand, here are the biggest factors in whether an Emperor will receive\nJe Ne Caesar. We can clearly see that how fighty an Emperor is has the\nbiggest impact on whether he received Je Ne Caesar.") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))

### what values do je na caesar winners have?

scatter <- rome %>%
  rename(je_ne_caesar = je_na_caesar) %>%
  pivot_longer(cols = "Fightius Maximus":"Tempo Completo",
               values_to = "Score") %>%
  ggplot(aes(x = total, y = Score)) +
  geom_point(aes(colour = je_ne_caesar)) +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = c("#DFaF37", "#66023c")) +
  theme(legend.position = "none") +
  theme_rome() +
  labs(x = "Total Score",
       title = "How many points do Je Ne Caesar winners get?",
       subtitle = "From the below plot we can see Je Ne Caesar winners (purple) often\nexcel in at least one area. We can also say that scoring low in one\nround does not count you out, as can be seen with Caligula and Otho.")

### birth province

born <- emperors %>%
  rename(je_ne_caesar = je_na_caesar) %>%
  group_by(je_ne_caesar, birth_prv) %>%
  tally() %>%
  pivot_wider(values_from = n,
              names_from = je_ne_caesar) %>%
  filter(!is.na(birth_prv)) %>%
  replace(is.na(.), 0) %>%
  group_by(birth_prv) %>%
  filter(sum(No, Yes) > 1) %>%
  mutate(pct_je_ne_caesar = Yes/sum(Yes,No, na.rm = TRUE),
         pct_not_je = 1 - pct_je_ne_caesar) %>%
  arrange(-pct_je_ne_caesar) %>%
  ungroup() %>%
  mutate(order = row_number()) %>%
  pivot_longer(cols = pct_je_ne_caesar:pct_not_je) %>%
  mutate(name = factor(name, levels = c("pct_not_je", "pct_je_ne_caesar"))) %>%
  ggplot(aes(x = reorder(birth_prv, -order), y = value, fill = name)) +
  geom_bar(stat = "identity", position = "fill", colour = "grey25") +
  coord_flip() +
  theme_rome() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("#DFaF37", "#66023c"),
                    labels = c("No", "Yes")) +
  labs(x = "",
       y = "",
       title = "What province was the Emperor born in?",
       subtitle = "Are the Totalus Rankium guys unassumingly biased? An interesting\nidea, but no. Many provinces have had emperors rise to power,\nbut there appears to be no bias (although Hispania has produced some remarkable Emperors).",
       caption = "Minimum 2 Emperors per Province")+
  theme(legend.position = "none")


### how were they chosen

chosen <- emperors %>%
  rename(je_ne_caesar = je_na_caesar) %>%
  group_by(je_ne_caesar, rise) %>%
  tally() %>%
  pivot_wider(values_from = n,
              names_from = je_ne_caesar) %>%
  filter(!is.na(rise)) %>%
  replace(is.na(.), 0) %>%
  group_by(rise) %>%
  mutate(pct_je_ne_caesar = Yes/sum(Yes,No, na.rm = TRUE),
         pct_not_je = 1 - pct_je_ne_caesar) %>%
  arrange(-pct_je_ne_caesar) %>%
  ungroup() %>%
  mutate(order = row_number()) %>%
  pivot_longer(cols = pct_je_ne_caesar:pct_not_je) %>%
  mutate(name = factor(name, levels = c("pct_not_je", "pct_je_ne_caesar"))) %>%
  ggplot(aes(x = reorder(rise, -order), y = value, fill = name)) +
  geom_bar(stat = "identity", position = "fill", colour = "grey25") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("#DFaF37", "#66023c"),
                    labels = c("No", "Yes")) +
  labs(x = "",
       y = "",
       title = "How did the Emperor come to power?",
       subtitle = "While most Emperors followed a rather standard route to power,\nothers were slightly more adventurous. Did their rise to power\ngive them a better chance at history's most coveted prize?") +
  theme(legend.position = "none") +
  theme_rome()


### final plot

scatter + (born + chosen) / (ml_plot) +
  plot_annotation(title = "Does he have a certain... Je Ne Caesar?",
                  subtitle = "Is it possible to determine whether an Emperor will receive Je Ne Caesar based on the information we have about him?",
                  caption = "Source | Totalus Rankium Podcast\nEmperor Ratings by Rob and Jamie\nVisulisation by Henry Wakefield\nTwitter | @henrywrover2",
                  theme= theme(plot.title = element_text(size = 24, family = "Times New Roman",
                                                  hjust = 0.5,
                                                  face = "bold"),
                               plot.subtitle = element_text(size = 16, family = "Times New Roman",
                                                         hjust = 0.5))) +
  plot_layout(widths = c(0.8, 2)) +
  ggsave(filename = "totalus_rankium_jnc.png", height = 8, width = 12, type = "cairo-png")
