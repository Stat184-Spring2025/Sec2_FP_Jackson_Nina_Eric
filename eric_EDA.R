library(tidyverse)
library(ggplot2)
library(scales)

url <- "https://huggingface.co/datasets/EFarrallpsu/STAT184_Eric_Jackson_Nina/resolve/main/athlete_events.csv"
olympic_raw <- read.csv(url)

temp <- olympic_raw %>%
  pivot_wider(
    id_cols =  Year,
    names_from = NOC,
    values_from = NOC
  ) %>%
  arrange(Year)

for (col in names(temp)[-1]) {
  temp[[col]] <- sapply(temp[[col]], function(x) if (is.null(x)) 0 else length(x))
}


temp <- temp %>%
  pivot_longer(cols = !Year, names_to = "Country", values_to = "Appearances") %>%
  filter()

top_countries <- temp %>%
  group_by(Country) %>%
  summarise(TotalAppearances = sum(Appearances, na.rm = TRUE)) %>%
  arrange(desc(TotalAppearances)) %>%
  slice_head(n = 8) %>%   # Get top 10
  pull(Country) 

top_countries <- c(top_countries, "URS", "RUS") #because of russia politics

temp <- temp %>%
  filter(Country %in% top_countries)
temp$Country <- factor(temp$Country, levels = rev(top_countries))
View(temp)

# Olympic years (skipping canceled Olympics)
olympic_years <- c(1896, 1900, 1904, 1908, 1912, 1920, 1924, 1928, 1932, 1936,
                   1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984,
                   1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)

ggplot(
  data = temp,
  mapping = aes(
    x = Year,
    y = Country,
    fill = Appearances,
  )) +
  geom_tile(color = "transparent") +
  scale_fill_viridis_c(option = "H", trans = "log1p") +
  scale_x_continuous(
    breaks = olympic_years
  ) +
  theme_minimal() +
  labs(title = "Olympic Participation Over Time (by athlete event pair)",
       x = "Year",
       y = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

unique(temp$Year)