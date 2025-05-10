# ───────────────────────────────────────────────────────────────────────────────
# 1. Load required packages (install if you haven’t already)
# ───────────────────────────────────────────────────────────────────────────────
library(readr)      # fast CSV reader
library(dplyr)      # data wrangling
library(lubridate)  # date handling
library(tidytext)   # text mining
library(ggplot2)    # plotting
library(wordcloud)  # word clouds
library(tidyr)      # unnesting
library(stringr)    # string ops
library(broom)      # tidy stats output
library(gridExtra)
# ───────────────────────────────────────────────────────────────────────────────
# 2. Read in the data, suppressing the column‐specification message
# ───────────────────────────────────────────────────────────────────────────────
tweets <- read_csv(
  "Twitter Jan Mar.csv",
  show_col_types = FALSE
)

# Quick structure check
glimpse(tweets)

# ───────────────────────────────────────────────────────────────────────────────
# 3. Basic preprocessing
# ───────────────────────────────────────────────────────────────────────────────
tweets_clean <- tweets %>%
  mutate(
    date = floor_date(date, unit = "day"),  
    content = str_to_lower(content)          
  ) %>%
  select(date, content, like_count, retweet_count)

tweets_plot_chart <- tweets_clean %>%
  count(date) %>%
  ggplot(aes(date, n)) +
  geom_col(fill = "steelblue") +
  labs(title = NULL, x = NULL, y = "Tweets per day") +
  theme_minimal()


ggsave(
  filename = "daily_tweet_volume.png",  # or .pdf, .svg, .jpg …
  plot     = tweets_plot_chart,                # the ggplot object
  width    = 8,                # width in inches
  height   = 6,                # height in inches
  dpi      = 300               # resolution for raster formats
)


# Load stopwords
data("stop_words")

# ───────────────────────────────────────────────────────────────────────────────
# 4. Sentiment classification
# ───────────────────────────────────────────────────────────────────────────────
# 4a. Using AFINN to get a numeric sentiment score per tweet
afinn_sent <- tweets_clean %>%
  unnest_tokens(word, content) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(date, row_number = row_number()) %>%
  summarise(afinn_score = sum(value), .groups = "drop")

# 4b. Using Bing for polarity counts
bing_sent <- tweets_clean %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"),
    by = "word",
    relationship = "many-to-many") %>%
  count(date, sentiment) %>%
  pivot_wider(names_from   = sentiment,
    values_from  = n,
    values_fill  = 0) %>%
  mutate(bing_net = positive - negative)


# 4c. Using NRC for emotion counts (optional: focus on “positive”/“negative”)
nrc_sent <- tweets_clean %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words, by="word") %>%
  inner_join(get_sentiments("nrc"),
    by = "word",
    relationship = "many-to-many") %>%
  filter(sentiment %in% c("positive","negative","anger","joy","fear","sadness")) %>%
  count(date, sentiment)


# ───────────────────────────────────────────────────────────────────────────────
# 5. Temporal sentiment trends
# ───────────────────────────────────────────────────────────────────────────────
# AFINN over time
afinn_trend <- afinn_sent %>%
  group_by(date) %>%
  summarise(mean_afinn = mean(afinn_score), .groups="drop")

afinn_plot <- ggplot(afinn_trend, aes(date, mean_afinn)) +
  geom_line() +
  labs(
    title = "Daily Mean AFINN Sentiment Score",
    x = "Date", y = "Mean AFINN Score"
  )

ggsave(
  filename = "afinn_trend.png",  # or .pdf, .svg, .jpg …
  plot     = afinn_plot,                # the ggplot object
  width    = 8,                # width in inches
  height   = 6,                # height in inches
  dpi      = 300               # resolution for raster formats
)

# Bing net sentiment over time
bing_trend <- bing_sent %>%
  group_by(date) %>%
  summarise(mean_bing = mean(bing_net), .groups="drop")

bing_plot <- ggplot(bing_trend, aes(date, mean_bing)) +
  geom_line(color="steelblue") +
  labs(
    title = "Daily Net Bing Sentiment",
    x = "Date", y = "Positive − Negative"
  )

ggsave(
  filename = "bing_trend.png",  # or .pdf, .svg, .jpg …
  plot     = bing_plot,                # the ggplot object
  width    = 8,                # width in inches
  height   = 6,                # height in inches
  dpi      = 300               # resolution for raster formats
)

# 1. Aggregate NRC counts per day × emotion
nrc_trend <- nrc_sent %>%
  group_by(date, sentiment) %>%
  summarise(count = sum(n), .groups = "drop")

# 2. Make a quick multi-line plot
nrc_plot <- ggplot(nrc_trend, aes(date, count, color = sentiment)) +
  geom_line() +
  labs(
    title = "Daily Emotion Counts (NRC)",
    x     = "Date",
    y     = "Word Count",
    color = "Emotion"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = "nrc_trend.png",
  plot     = nrc_plot,
  width    = 8, height = 6, dpi = 300
)

# Section 6: Word clouds side-by-side with thematic colors
# (updated to exclude 'intelligence')
# ---------------------------------------------------------------
# Build top-50 instead of top-100, excluding 'intelligence'
top_pos <- tweets_clean %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing") %>% filter(sentiment == "positive"), by = "word") %>%
  filter(word != "intelligence") %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 50)

top_neg <- tweets_clean %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing") %>% filter(sentiment == "negative"), by = "word") %>%
  filter(word != "intelligence") %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 50)

# Save side-by-side word clouds to a single image
png("wordclouds.png", width = 2400, height = 1200, res = 200, bg = "transparent")
par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))

# Positive
wordcloud(
  words        = top_pos$word,
  freq         = top_pos$n,
  max.words    = 50,
  scale        = c(3, 0.5),
  random.order = FALSE,
  rot.per      = 0.25,
  colors       = "darkgreen"
)
title("Top Positive Words", line = 1, cex.main = 2)

# Negative
wordcloud(
  words        = top_neg$word,
  freq         = top_neg$n,
  max.words    = 50,
  scale        = c(3, 0.5),
  random.order = FALSE,
  rot.per      = 0.25,
  colors       = "darkred"
)
title("Top Negative Words", line = 1, cex.main = 2)
dev.off()


# Section 7: Bigram sentiment pipeline and bar charts
# ---------------------------------------------------

# 7a. Compute bigrams and sentiment counts
custom_stop_bigrams <- c(
  "artificial intelligence",
  "intelligence ai",
  "breaking news",
  "critical thinking",
  "chatgpt rival",
  "chatgpt hype",
  "rival bard",
  "nick cave",
  "status haze",
  "haze 2023"
)
bigram_sent <- tweets_clean %>%
  unnest_tokens(bigram, content, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("w1","w2"), sep = " ") %>%
  filter(!w1 %in% stop_words$word, !w2 %in% stop_words$word) %>%
  mutate(bigram = paste(w1, w2, sep = " ")) %>%
  filter(!bigram %in% custom_stop_bigrams) %>%
  pivot_longer(c(w1, w2), names_to = "position", values_to = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(bigram, sentiment, sort = TRUE)
# Then select top 5 per sentiment and plot as horizontal bars


# 7b. Prepare top 5 bigrams per sentiment
top5_bigrams <- bigram_sent %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5) %>%
  ungroup()

# Split into positive and negative sets
pos_bigrams <- top5_bigrams %>% filter(sentiment == "positive")
neg_bigrams <- top5_bigrams %>% filter(sentiment == "negative")

# Create ggplot objects
pos_plot <- ggplot(pos_bigrams, aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "forestgreen", width = 0.6) +                # narrower bars
  coord_flip() +
  labs(
    title = "Top 5 Positive Phrases",
    x = NULL, y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(size = 18, face = "bold"),
    axis.text.y     = element_text(color = "black", size = 14),  # bigger labels
    axis.text.x     = element_text(size = 12),
    axis.title.x    = element_text(size = 14)
  )

# Negatives
neg_plot <- ggplot(neg_bigrams, aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "firebrick", width = 0.6) +
  coord_flip() +
  labs(
    title = "Top 5 Negative Phrases",
    x = NULL, y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(size = 18, face = "bold"),
    axis.text.y     = element_text(color = "black", size = 14),
    axis.text.x     = element_text(size = 12),
    axis.title.x    = element_text(size = 14)
  )

# Export smaller composite image
ggsave(
  filename = "bigram_bar_charts.png",
  plot     = grid.arrange(pos_plot, neg_plot, ncol = 2),
  width    = 10,    # inches
  height   = 5,     # inches
  dpi      = 300,
  bg       = "transparent"
)


# Section 8: Correlation of sentiment vs. engagement
# --------------------------------------------------
# 8a. Recompute per-tweet AFINN scores
tweet_scores <- tweets_clean %>%
  mutate(tweet_id = row_number()) %>%
  unnest_tokens(word, content) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(tweet_id) %>%
  summarise(
    afinn = sum(value),
    likes = first(like_count),
    rts   = first(retweet_count),
    .groups = "drop"
  )

# AFINN vs Likes
cor_likes <- cor.test(tweet_scores$afinn, tweet_scores$likes, method = "pearson")
r_likes   <- round(cor_likes$estimate, 4)
p_likes   <- round(cor_likes$p.value, 4)

likes_plot <- ggplot(tweet_scores, aes(x = afinn, y = likes)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  annotate(
    "text", x = Inf, y = Inf,
    label = paste0("r = ", r_likes, "\np = ", p_likes),
    hjust = 1.1, vjust = 1.1, color = "black"
  ) +
  labs(
    title = "AFINN Score vs Likes",
    x = "AFINN Sentiment Score",
    y = "Number of Likes"
  ) +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.title       = element_text(size = 18, face = "bold", color = "black"),
    axis.text.y      = element_text(color = "black", size = 14),
    axis.text.x      = element_text(color = "black", size = 12),
    axis.title.x     = element_text(color = "black", size = 14),
    axis.title.y     = element_text(color = "black", size = 14)
  )

ggsave(
  filename = "likes_correlation.png",
  plot     = likes_plot,
  width    = 8,
  height   = 6,
  dpi      = 300,
  bg       = "transparent"
)

# AFINN vs Retweets
cor_rts <- cor.test(tweet_scores$afinn, tweet_scores$rts, method = "pearson")
r_rts   <- round(cor_rts$estimate, 4)
p_rts   <- round(cor_rts$p.value, 4)

rts_plot <- ggplot(tweet_scores, aes(x = afinn, y = rts)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  annotate(
    "text", x = Inf, y = Inf,
    label = paste0("r = ", r_rts, "\np = ", p_rts),
    hjust = 1.1, vjust = 1.1, color = "black"
  ) +
  labs(
    title = "AFINN Score vs Retweets",
    x = "AFINN Sentiment Score",
    y = "Number of Retweets"
  ) +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.title       = element_text(size = 18, face = "bold", color = "black"),
    axis.text.y      = element_text(color = "black", size = 14),
    axis.text.x      = element_text(color = "black", size = 12),
    axis.title.x     = element_text(color = "black", size = 14),
    axis.title.y     = element_text(color = "black", size = 14)
  )

ggsave(
  filename = "retweets_correlation.png",
  plot     = rts_plot,
  width    = 8,
  height   = 6,
  dpi      = 300,
  bg       = "transparent"
)






