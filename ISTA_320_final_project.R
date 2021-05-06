---
title: "ISTA 320 Final Project: Steam Review Data"
author: "Faye Bandet"
date: "Spring 2021"
output: "Data Visualizations Project"
---

```{r setup, include=FALSE}
# remember to add any other libraries you need here:
library(knitr)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(lubridate)
library(ggplot2)
opts_chunk$set(echo = TRUE)
```
For this project, I will be working with [Steam video game review data from Kaggle](www.kaggle.com/dataset/67b2c31f902db6fdcb46064951041df4d31aebe51e87bdca291669e282169c6e)

# Data Wrangling

Read the data in and do any changes deemed necessary.

```{r}
# Standard GitHub repository data read in: read_csv("Data/steam_reviews.csv")
# May need to modify the actual read-in line depending on local filepath (since my data is stored on an external drive)
# my kaggle: https://www.kaggle.com/fbandet/ista-320-final-project-steam-review-data

steam_data <- read_csv("F:/maxone/steam_reviews.csv") # local file
glimpse(steam_data)

steam_filtered_cols <- steam_data %>%
  select(`app_id`, `app_name`, `language`, `review`, `timestamp_created`, `recommended`) %>%
  filter(`language` == "english")
glimpse(steam_filtered_cols)

cleaned_data <- drop_na(steam_filtered_cols)
glimpse(cleaned_data) #removed NA values

# Timeseries wrangling: convert Unix timestamp (Epoch Date) to human-readable form
cleaned_time <- cleaned_data %>%
  mutate(`timestamp_created` = as.Date(as.POSIXct(`timestamp_created`, origin="1970-01-01")))
glimpse(cleaned_time)

# Find most commonly reviewed video games (>20000 reviews)
top_total_game_reviews <- cleaned_time %>%
  group_by(`app_id`) %>%
  count(cleaned_time$app_id) %>%
  filter(n > 105000)

unique(top_total_game_reviews$app_id)
#(620, 359550, 381210, 292030, 413150, 4000)

# hard coding since R is having issues handeling all the data

only620 <- cleaned_time %>%
  filter(`app_id` == 620)
only359550 <- cleaned_time %>%
  filter(`app_id` == 359550)
only381210 <- cleaned_time %>%
  filter(`app_id` == 381210)
only292030 <- cleaned_time %>%
  filter(`app_id` == 292030)
only413150 <- cleaned_time %>%
  filter(`app_id` == 413150)
only4000 <- cleaned_time %>%
  filter(`app_id` == 4000)

top_6 <- rbind(only620, only359550, only381210, only292030, only413150, only4000)
```

# Data Visualization

Questions: What are the most commonly reviewed Steam games? Was the daily increase or of aggregated reviews visible or relatively steady? Did the game gather a steady amount of reviews over time, or did reviews spike at point(s) in time?

```{r}
# Plot 1 : Bar Plot
top_6 %>% 
  group_by(`review`) %>%
  summarize(total = nrow(review)) %>%
  ggplot(aes(x = `app_name`,
             y = total, 
             fill = 'app_name')) +
  geom_bar() +
  scale_fill_manual(values = brewer.pal(5,"PuBuGn")) +
  theme(axis.text.y = element_text(angle = 30, vjust = 0)) + 
  labs(x = "Video Game Application", y = "Total Reviews (English)", title = "Top Six Recommended Video Games on Steam") +
  theme(legend.position = "bottom")
  # Conclusion: The top 5 recommended games are	Portal 2, Garry's Mod,  Tom Clancy's Rainbow Six Siege, Dead by Daylight, The Witcher 3: Wild Hunt, and Stardew Valley 

  
# Plot 2 : Line Graph
# line plot with aggregated daily review increases
top_6 %>% 
  group_by(`app_id`, timestamp_created`) %>%
  aggregate(`timestamp_created`) %>%
  summarize(total = nrow(review)) %>%
  ggplot(aes(x = `timestamp_created`,
             y = total, 
             color = 'app_name')) +
  geom_line() +
  scale_fill_manual(values = brewer.pal(5,"PuBuGn")) +
  theme(legend.position = "left", axis.text.y = element_text(angle = 30, vjust = 0)) + 
  labs(x = "Date", y = "Total Reviews (English)", title = "Aggregated Daily Reviews") 
# Conclusion: This shows that the reviews have accumulated over time.


# Plot 3: Timeseries Point Plot
top_6 %>%
  group_by(`app_name`, `timestamp_created`) %>%
  summarise(reviews = sum(`review`)) %>%
  ggplot(aes(x = `timestamp_created`,
             y = reviews, group = `app_name`, color = `app_name`)) +
  geom_point() +
  facet_wrap(~`app_name`, scales = "free") +
  labs(x = "Date", y = "Total Reviews (English)", title = "Full Timeseries Recommendation Submissions", caption = "Note that some video games were released later than others which influences their start date on the plot") +
  theme(legend.position = "bottom", strip.text = element_text(size = 15)) 
# Conclusion: As expected the total amount of reviews has increased over time. The visuals are also showing how some games were released earlier or later than others.
```
