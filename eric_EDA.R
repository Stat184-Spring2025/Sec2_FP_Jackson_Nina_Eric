library(tidyverse)
library(ggplot2)
library(scales)

url <- "https://huggingface.co/datasets/EFarrallpsu/STAT184_Eric_Jackson_Nina/resolve/main/athlete_events.csv"
olympic_raw <- read.csv(url)

#transform into dataframe with case as year, with columns for each country signifying appearances
year_case <- olympic_raw %>%
  pivot_wider(
    id_cols =  Year,
    names_from = NOC,
    values_from = NOC
  ) %>%
  arrange(Year)

# converts vector of strings signifying number of appearances to integer value
for (col in names(year_case)[-1]) {
  year_case[[col]] <- sapply(year_case[[col]], function(x) if (is.null(x)) NA else length(x))
}

# changes case to country-year instead of country
country_year <- year_case %>%
  pivot_longer(cols = !Year, names_to = "Country", values_to = "Appearances")


# identifies countries with most total appearances over the years
top_countries <- country_year %>%
  group_by(Country) %>%
  summarise(TotalAppearances = sum(Appearances, na.rm = TRUE)) %>%
  arrange(desc(TotalAppearances)) %>%
  slice_head(n = 8) %>%   # Get top 10
  pull(Country) 

top_countries <- c(top_countries, "URS", "RUS") #because of russia politics

#filters to only include contries with top appearances
top_country_year <- country_year %>%
  filter(Country %in% top_countries)
top_country_year$Country <- factor(top_country_year$Country, levels = rev(top_countries))

# Olympic years (skipping canceled Olympics)
olympic_years <- c(1896, 1900, 1904, 1908, 1912, 1920, 1924, 1928, 1932, 1936,
                   1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984,
                   1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)


#plot showing total appearances over time in top 10 countries
ggplot(
  data = top_country_year,
  mapping = aes(
    x = Year,
    y = Country,
    fill = Appearances,
  )) +
  geom_tile(color = "transparent") +
  scale_fill_viridis_c(option = "A", na.value = 'white') +
  scale_x_continuous(
    breaks = olympic_years
  ) +
  theme_minimal() +
  labs(title = "Olympic Participation Over Time (by athlete event pair)",
       x = "Year",
       y = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot with no empty space
ggplot(
  data = top_country_year,
  mapping = aes(
    x = factor(Year),
    y = Country,
    fill = Appearances,
  )) +
  geom_tile(color = "transparent") +
  scale_fill_viridis_c(option = "A", na.value = 'white') +
  theme_minimal() +
  labs(title = "Olympic Participation Over Time (by athlete event pair)",
       x = "Year",
       y = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Now showing by season. Using games instead of season column, as season does not differentiate for summers/winters across years
#transform into dataframe with case as season, with columns for each country signifying appearances

season_case <- olympic_raw %>%
  pivot_wider(
    id_cols =  Games,
    names_from = NOC,
    values_from = NOC
  )

season_case <- season_case %>%
  mutate(Year = as.numeric(str_extract(Games, "^\\d{4}"))) %>%  # Extract year
  arrange(Year)                                                 # Sort by year

View(season_case)
# converts vector of strings signifying number of appearances to integer value
for (col in names(season_case)[-1]) {
  season_case[[col]] <- sapply(season_case[[col]], function(x) if (is.null(x)) NA else length(x))
}


# changes case to country-games instead of country
country_season <- season_case %>%
  pivot_longer(cols = !Games, names_to = "Country", values_to = "Appearances")


country_summer <- country_season %>%
  filter(grepl("Summer", Games))

country_winter <- country_season %>%
  filter(grepl("Winter", Games))

# identifies countries with most total appearances over the years
top_countries_summer <- country_summer %>%
  group_by(Country) %>%
  summarise(TotalAppearances = sum(Appearances, na.rm = TRUE)) %>%
  arrange(desc(TotalAppearances)) %>%
  slice_head(n = 10) %>%   # Get top 10
  pull(Country) 

top_countries_winter <- country_winter %>%
  group_by(Country) %>%
  summarise(TotalAppearances = sum(Appearances, na.rm = TRUE)) %>%
  arrange(desc(TotalAppearances)) %>%
  slice_head(n = 10) %>%   # Get top 10
  pull(Country) 


#filters to only include countries with top appearances
country_summer <- country_summer %>%
  filter(Country %in% top_countries_summer)
country_summer$Country <- factor(country_summer$Country, levels = rev(top_countries_summer))

country_winter <- country_winter %>%
  filter(Country %in% top_countries_winter)
country_winter$Country <- factor(country_winter$Country, levels = rev(top_countries_winter))


#plot for summer games
ggplot(
  data = country_summer,
  mapping = aes(
    x = factor(Games),
    y = Country,
    fill = Appearances,
  )) +
  geom_tile(color = "transparent") +
  scale_fill_viridis_c(option = "A", na.value = 'white') +
  theme_minimal() +
  labs(title = "Summer Olympic Participation Over Time (by athlete event pair)",
       x = "Games",
       y = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot for winter games
ggplot(
  data = country_winter,
  mapping = aes(
    x = factor(Games),
    y = Country,
    fill = Appearances,
  )) +
  geom_tile(color = "transparent") +
  scale_fill_viridis_c(option = "A", na.value = "white") +
  theme_minimal() +
  labs(title = "Winter Olympic Participation Over Time (by athlete event pair)",
       x = "Games",
       y = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))