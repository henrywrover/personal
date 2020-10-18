library(tidyverse)
library(tidytext)
theme_set(theme_minimal())
options(scipen = 999)

courses <- read_csv("coursera_courses.csv")
reviews <- read_csv("coursera_reviews.csv") %>%
  merge(courses, by = "course_id") %>%
  mutate(review_id = row_number())

summary(reviews)

courses %>%
  group_by(institution) %>%
  tally(sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(institution, n), y = n)) +
  geom_col(fill = "deepskyblue4", colour = "grey25") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.5) +
  labs(title = "Top 10 Institutions by number of courses offered")

split <- reviews %>%
  unnest_tokens(word, reviews) %>%
  anti_join(stop_words, by = "word")

split %>%
  group_by(word) %>%
  tally(sort = TRUE) %>%
  top_n(25, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "firebrick", colour = "grey25") +
  coord_flip() +
  labs(title = "Most common words in Coursera reviews")

### sentiment analysis

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

split %>%
  inner_join(bing) %>%
  group_by(sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_col(aes(fill = sentiment), colour = "grey25") +
  facet_wrap(~sentiment, scales = "free_x") +
  labs(title = "Most common negative and positive words within Coursera reviews")

split %>%
  inner_join(nrc) %>%
  group_by(sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(aes(fill = sentiment), colour = "grey25") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Most common words by emotion within Coursera reviews") +
  coord_flip()

split %>%
  inner_join(afinn) %>%
  group_by(review_id) %>%
  summarise(mean_score = mean(value), rating = mean(rating)) %>%
  sample_n(50000) %>%
  ggplot(aes(x = rating, y = mean_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

split %>%
  group_by(rating) %>%
  count(word, rating) %>%
  top_n(5, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(aes(fill = as.factor(rating))) +
  facet_wrap(~as.factor(rating), scales = "free") +
  coord_flip() +
  labs(title = "Most common words for each rating on Coursera reviews")

split %>%
  inner_join(afinn) %>%
  filter(value >= 4 |
         value == min(value)) %>%
  mutate(sentiment = ifelse(value >= 4, "Positive", "Negative")) %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(aes(fill = sentiment), alpha = 0.8) +
  facet_wrap(~sentiment, scales = "free") +
  scale_fill_manual(values = c("firebrick", "deepskyblue4")) +
  coord_flip() +
  labs(title = "Most Negative and Positive Words used on Coursera Reviews",
       x = "Word",
       y = "Number of Occurrences",
       caption = "Twitter: @henrywrover2 | Github: henrywrover\n18th October 2020") +
  theme(panel.border = element_rect(colour = "Grey50", fill = NA)) +
  ggsave(filename = "positive_negative.png", dpi = 300, height = 8, width = 12)
