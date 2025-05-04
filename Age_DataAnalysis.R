# Import Libraries 
library(tidyverse)
library(ggplot2)
library(dplyr)

data <- athlete_events

# Create a ggplot bar chart of Age frequency
ggplot(athlete_events, aes(x = Age)) +
  geom_bar(fill = "cornflowerblue", color = "white") +
  labs(title = "Frequency Distribution of Age",
       x = "Age",
       y = "Number of Athletes") +
  theme_minimal()


# Remove NA ages and limit age range (optional for readability)
clean_data <- athlete_events %>%
  filter(!is.na(Age))


# Count frequencies by Age and Year
age_year_freq <- athlete_events %>%
  group_by(Year, Age) %>%
  summarise(Freq = log(n(),10), .groups = 'drop')

# Plot heatmap

ggplot(age_year_freq, aes(x = Year, y = Age, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Heatmap of Athlete Ages Over Time",
       x = "Olympic Year",
       y = "Age",
       fill = "Frequency (log10)") +
  theme_minimal()

#ggplot(age_year_freq, aes(x = Year, y = Age, fill = Freq)) +
 # geom_tile(color = "white") +
  #scale_fill_viridis_c(option = "C") +
  #labs(title = "Heatmap of Athlete Ages Over Time",
   #    x = "Olympic Year",
    #   y = "Age",
     #  fill = "Frequency (log10)") +
  #theme_minimal()

# tile plot with linear scale
ggplot(age_year_freq, aes(x = Year, y = Age, fill = Freq)) +
  geom_tile(color = "grey90", width = 1, height = 1) +
  scale_fill_viridis_c( option = "D") +
  labs(title = "Age Frequency Tile Plot (Linear Scale)",
       x = "Olympic Year",
       y = "Athlete Age",
       fill = "Freq") +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank())


# Compute mean age per Olympic year
mean_age_by_year <- athlete_events %>%
  filter(!is.na(Age)) %>%
  group_by(Year) %>%
  summarise(Mean_Age = mean(Age), .groups = 'drop')

# Plot using ggplot2
ggplot(mean_age_by_year, aes(x = Year, y = Mean_Age)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "black") +
  labs(title = "Mean Athlete Age Over Olympic Years",
       x = "Year",
       y = "Mean Age") +
  theme_minimal(base_size = 14)


