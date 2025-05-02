# Finding medal counts for countries across all sports

# Needs ----
library(tidyverse)
library(ggplot2)
library(knitr)
library(kableExtra)
library(stringr)

## Olympic Dataset
url <- "https://huggingface.co/datasets/EFarrallpsu/STAT184_Eric_Jackson_Nina
/resolve/main/athlete_events.csv"
athletes <- read.csv(url)

# Wrangling ----

## Unique events
events <- athletes %>%
  select(Sport, Event) %>%
  distinct(Sport,Event) %>%
  select(-c("Sport")) %>% arrange(Event)

## Team Event Indicators
teamEvents <- c(
  "Team","Volleyball","Bobsleigh","Doubles","Baseball",
  "Fours","Relay","Basketball","Tandem","Pairs",
  "Handball","Hockey","Lacrosse","Polo","Group",
  "17-man","6-man","Eights","Quadruple","Rugby",
  "Two Person","Three Person","Softball","Tug-Of-War", "Football"
)

athletes <- athletes %>%
  mutate(
    isTeamEvent = str_detect(Event, str_c(teamEvents, collapse = "|"))
  )
  
## Making team sports have one combined medal
teamMedals <- athletes %>%
  filter(isTeamEvent, !is.na(Medal)) %>%
  distinct(Team, Games, Event, Medal)

indivMedals <- athletes %>%
  filter(!isTeamEvent, !is.na(Medal)) %>%
  distinct(Team, Games, Event, Medal)

## Combining team medals and individual medals
all_medals = bind_rows(teamMedals, indivMedals)

## Medal counts
medalCountries <- all_medals %>%
  group_by(Team, Medal) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Medal,
    values_from = count,
    values_fill = 0
  ) %>%
  mutate(total = Bronze + Gold + Silver)

## Case is now Team and Medal type for Data Vis.
medalsPerTeam <- all_medals %>%
  group_by(Team) %>%
  summarise(
    goldWon = sum(`Medal` == "Gold", na.rm = TRUE),
    silverWon = sum(`Medal` == "Silver", na.rm = TRUE),
    bronzeWon = sum(`Medal` == "Bronze", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(goldWon, silverWon, bronzeWon),
               names_to = "Medal",
               values_to = "Count") %>%
  mutate(`Medal` = case_when(
    Medal == "goldWon" ~ "Gold", # Changing names of variables to medals
    Medal == "silverWon" ~ "Silver",
    Medal == "bronzeWon" ~ "Bronze"
  )) %>%
  mutate(Medal = factor(Medal, levels = c("Gold","Silver","Bronze"))) %>%
  group_by(Team) %>%
  mutate(total = sum(Count, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(total)) %>%
  slice_head(n=30)

## Sample 500 random male athletes size and medal count for data vis.
heightWeightSampleM <- athletes %>%
  filter(Sex == "M") %>%
  group_by(ID, Name) %>%
  summarise(
    avgHeight = mean(Height, na.rm = TRUE),
    avgWeight = mean(Weight, na.rm = TRUE),
    goldWon = sum(`Medal` == "Gold", na.rm = TRUE),
    silverWon = sum(`Medal` == "Silver", na.rm = TRUE),
    bronzeWon = sum(`Medal` == "Bronze", na.rm = TRUE),
    total = sum(!is.na(`Medal`)),
    .groups = "drop"
  ) %>%
  filter(!is.na(avgHeight)) %>% filter(!is.na(avgWeight)) %>%
  filter(total > 0) %>%
  slice_sample(n = 500) %>%
  mutate(avgHeight_IN = avgHeight/2.34,
         avgWeight_LB = avgWeight*2.2)

## Sample 500 females
heightWeightSampleF <- athletes %>%
  filter(Sex == "F") %>%
  group_by(ID, Name) %>%
  summarise(
    avgHeight = mean(Height, na.rm = TRUE),
    avgWeight = mean(Weight, na.rm = TRUE),
    goldWon = sum(`Medal` == "Gold", na.rm = TRUE),
    silverWon = sum(`Medal` == "Silver", na.rm = TRUE),
    bronzeWon = sum(`Medal` == "Bronze", na.rm = TRUE),
    total = sum(!is.na(`Medal`)),
    .groups = "drop"
  ) %>%
  filter(!is.na(avgHeight)) %>% filter(!is.na(avgWeight)) %>%
  filter(total > 0) %>%
  slice_sample(n = 500) %>%
  mutate(avgHeight_IN = avgHeight/2.34,
         avgWeight_LB = avgWeight*2.2)

# Data Visualizations ----

## Medal Vis.
ggplot(
  medalsPerTeam,
  mapping = aes(
    x = reorder(Team, -total),
    y = Count,
    fill = Medal
  )
) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Frequency of Medals Won by Country",
    x = "Country",
    y = "Medals Won"
  ) +
  scale_fill_manual(values = c("Gold" = "gold","Silver" = "gray",
                               "Bronze" = "brown")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 8, angle = 35),
    legend.title = element_text(face = "bold", size = 12, hjust = 0.5)
  )
  
## Show athlete size isn't why the US wins so much
ggplot(
  heightWeightSampleM,
  mapping = aes(
    x = avgHeight_IN,
    y = avgWeight_LB,
    size = total,
    color = total
  )
) + geom_point() +
  labs(
    title = "How Size Affects the Amount of Medals\na Male Athlete Wins",
    x = "Average Height per Athlete (in)",
    y = "Average Weight per Athlete (lbs)",
    size = "Number of Medals Won",
    color = "Number of Medals Won"
  ) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14,
                              color = "lightgray"),
    axis.title = element_text(face = "bold", size = 12, color = "lightgray"),
    axis.text.x = element_text(face = "bold", size = 8, angle = 35,
                               color="gray80"),
    axis.text.y = element_text(color = "gray80", face = "bold"),
    legend.title = element_text(face = "bold", size = 10, hjust = 0.5,
                                color = "lightgray"),
    legend.text = element_text(color = "gray80"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "gray30", color = NA),
    panel.background = element_rect(fill = "gray30", color = NA),
    legend.background = element_rect(fill = "gray30", color = NA),
    panel.grid.major = element_line(color = "gray50"),
    panel.grid.minor = element_line(color = "gray20"),
  ) +
  scale_color_gradient(low = "lightblue", high = "navy")

ggplot(
  heightWeightSampleF,
  mapping = aes(
    x = avgHeight_IN,
    y = avgWeight_LB,
    size = total,
    color = total
  )
) + geom_point() +
  labs(
    title = "How Size Affects the Amount of Medals\na Female Athlete Wins",
    x = "Average Height per Athlete (in)",
    y = "Average Weight per Athlete (lbs)",
    size = "Number of Medals Won",
    color = "Number of Medals Won"
  ) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14,
                              color = "lightgray"),
    axis.title = element_text(face = "bold", size = 12, color = "lightgray"),
    axis.text.x = element_text(face = "bold", size = 8, angle = 35,
                               color="gray80"),
    axis.text.y = element_text(color = "gray80", face = "bold"),
    legend.title = element_text(face = "bold", size = 10, hjust = 0.5,
                                color = "lightgray"),
    legend.text = element_text(color = "gray80"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "gray30", color = NA),
    panel.background = element_rect(fill = "gray30", color = NA),
    legend.background = element_rect(fill = "gray30", color = NA),
    panel.grid.major = element_line(color = "gray50"),
    panel.grid.minor = element_line(color = "gray20"),
  ) +
  scale_color_gradient(low = "lightblue", high = "navy")
