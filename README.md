# Twitter Sentiment Analysis

A comprehensive R-based sentiment analysis pipeline for Twitter data, analyzing emotional and semantic trends across tweets using multiple sentiment lexicons.

## Overview

This script performs multi-faceted sentiment analysis on Twitter data (January–March), using three different sentiment analysis approaches:

- **AFINN**: Numeric sentiment scoring (-5 to +5 per word)
- **Bing**: Binary polarity classification (positive/negative)
- **NRC**: Emotion classification (joy, anger, sadness, fear, positive, negative)

## Features

### 1. **Data Loading & Preprocessing**
- Fast CSV reading via `readr`
- Date normalization (floor to day level)
- Text lowercasing and cleaning
- Selection of key features (date, content, engagement metrics)

### 2. **Temporal Sentiment Analysis**
- Daily tweet volume tracking
- Mean sentiment score trends over time
- Multi-emotion tracking across the period
- Visualizations saved as high-resolution PNG files

### 3. **Text Mining & Visualization**
- **Word Clouds**: Side-by-side visualization of top 50 positive vs. negative words
- **Bigram Analysis**: Identification and scoring of two-word phrases with sentiment
- **Phrase Rankings**: Top 5 positive and negative phrases displayed in bar charts

### 4. **Engagement Correlation**
- Pearson correlation between sentiment scores and:
  - Number of likes (engagement)
  - Number of retweets (viral potential)
- Statistical significance testing with p-values
- Scatter plots with trend lines

## Requirements

### Dependencies

Install required packages using:
```r
install.packages(c(
  "readr", "dplyr", "lubridate", "tidytext", "ggplot2", 
  "wordcloud", "tidyr", "stringr", "broom", "gridExtra"
))
```

## Input Data

**File:** `Twitter Jan Mar.csv`

Expected columns:
- `date`: Tweet timestamp
- `content`: Tweet text
- `like_count`: Number of likes
- `retweet_count`: Number of retweets

## Output Files

All visualizations are saved as high-resolution PNG files (300 DPI):

| Output | Description |
|--------|-------------|
| `daily_tweet_volume.png` | Bar chart of tweets per day |
| `afinn_trend.png` | Line plot of daily mean AFINN sentiment |
| `bing_trend.png` | Line plot of daily net Bing sentiment |
| `nrc_trend.png` | Multi-line emotional trends (NRC) |
| `wordclouds.png` | Side-by-side positive/negative word clouds |
| `bigram_bar_charts.png` | Top 5 positive/negative phrases |
| `likes_correlation.png` | Scatter plot: AFINN vs. likes |
| `retweets_correlation.png` | Scatter plot: AFINN vs. retweets |

## Script Sections

1. **Package Loading** – Import required libraries
2. **Data Input** – Read and inspect Twitter CSV
3. **Preprocessing** – Clean and standardize data
4. **Sentiment Classification** – Apply AFINN, Bing, and NRC lexicons
5. **Temporal Trends** – Aggregate sentiment by date and create time-series plots
6. **Word Clouds** – Visualize most frequent positive/negative words
7. **Bigram Analysis** – Extract and score two-word phrases (filtered stop words)
8. **Correlation Analysis** – Test relationships between sentiment and engagement

## Key Parameters

- **Word cloud size**: Top 50 words per sentiment
- **Bigram filtering**: Custom stop phrases removed (e.g., "artificial intelligence", "breaking news")
- **Correlation method**: Pearson correlation with p-value testing
- **Visualization resolution**: 300 DPI for publication quality

## Notes

- The script excludes the word "intelligence" from word clouds (topic-specific filtering)
- Bigram analysis removes common stop words and custom filtered phrases
- All plots use minimal themes with transparent backgrounds for academic/professional use
- Sentiment lexicons are loaded via `tidytext::get_sentiments()`

## Author

Aafeef Ayman Ahmed

## License

Educational use

