# Basketball Data Wrangling

# Needs ----
library(tidyverse)
library(ggplot2)
library(stringr)
library(knitr)
library(kableExtra)

## Olympic Dataset
athletes_url <- "https://huggingface.co/datasets/EFarrallpsu/STAT184_Eric_Jackson_Nina/resolve/main/athlete_events.csv"
# Use this URL to download Athletes file

athletes <- read.csv("~/Downloads/athlete_events.csv")

## NBA Players R Dataset
file_url <- "https://www.kaggle.com/datasets/drgilermo/nba-players-stats?resource=download"
# Download file from URL above
nba <- read.csv("~/Downloads/Stat184/Players.csv") # Update path if needed
# Outdated but not for Olympic data
# Only since 1950 but the gap between NBA players and 
# non-NBA Olympic athletes before 1950 is much smaller than it is today

# Wrangling ----

## All BB Athletes
basketball <- athletes %>%
  filter((`Sport` == "Basketball"))

## All Male BB Athletes
basketballMale <- athletes %>%
  filter((`Sport` == "Basketball")) %>%
  filter(`Sex` == "M")

## All Male BB Athletes who won medals
basketballMedalsM <- basketballMale %>%
  group_by(`ID`,`Name`) %>%
  summarise(
    avgHeight = mean(Height, na.rm = TRUE),
    avgWeight = mean(Weight, na.rm = TRUE),
    goldWon = sum(`Medal` == "Gold", na.rm = TRUE),
    silverWon = sum(`Medal` == "Silver", na.rm = TRUE),
    bronzeWon = sum(`Medal` == "Bronze", na.rm = TRUE),
    total = sum(!is.na(`Medal`)),
    .groups = "drop"
  )

## All Female BB Athletes
basketballFemale <- athletes %>%
  filter((`Sport` == "Basketball")) %>%
  filter(`Sex` == "F")

## All Female BB Athletes who won medals
basketballMedalsF <- basketballFemale %>%
  group_by(`ID`,`Name`) %>%
  summarise(
    avgHeight = mean(Height, na.rm = TRUE),
    avgWeight = mean(Weight, na.rm = TRUE),
    goldWon = sum(`Medal` == "Gold", na.rm = TRUE),
    silverWon = sum(`Medal` == "Silver", na.rm = TRUE),
    bronzeWon = sum(`Medal` == "Bronze", na.rm = TRUE),
    total = sum(!is.na(`Medal`)),
    .groups = "drop"
  )

## Countries who've played in basketball
basketballCountry <- basketball %>%
  distinct(`Team`,`Year`,`Medal`) %>%
  group_by(`Team`) %>%
  summarise(
    goldWon = sum(`Medal` == "Gold", na.rm = TRUE),
    silverWon = sum(`Medal` == "Silver", na.rm = TRUE),
    bronzeWon = sum(`Medal` == "Bronze", na.rm = TRUE),
    total = sum(!is.na(`Medal`)),
    .groups = "drop"
  ) %>%
  filter(!(`Team` == "Unified Team")) # Unified Team removed as it is not
# a real country but a combination of several

## Countries who've won a medal
countryMedalists <- basketballCountry %>%
  filter(!(`total` == 0)) 

## Case is now Team and Medal type for Data Vis.
medalsPerBBTeam <- countryMedalists %>%
  pivot_longer(cols = c(goldWon,silverWon,bronzeWon),
               names_to = "Medal",
               values_to = "Count") %>%
  mutate(`Medal` = case_when(
    Medal == "goldWon" ~ "Gold", # Changing names of variables to medals
    Medal == "silverWon" ~ "Silver",
    Medal == "bronzeWon" ~ "Bronze"
  )) %>%
  mutate(Medal = factor(Medal, levels = c("Gold","Silver","Bronze")))
  # Changing order of medal types for better graph visuals

## Removing all cases of NA in height and weight columns
## Converting to inches and pounds
maleBB_HandW <- basketballMedalsM %>%
  filter(!is.na(avgHeight)) %>%
  filter(!is.na(avgWeight)) %>%
  mutate(avgHeight = avgHeight/2.34,
         avgWeight = avgWeight*2.2)

femaleBB_HandW <- basketballMedalsF %>%
  filter(!is.na(avgHeight)) %>%
  filter(!is.na(avgWeight)) %>%
  mutate(avgHeight = avgHeight/2.34,
         avgWeight = avgWeight*2.2)

## NBA/Olympic merge

## Function to get only first and last names
getFirstLast <- function(x) {
  if (length(x) >= 2) {
    paste(x[1], x[length(x)]) 
  } else {paste(x[1])}
}

## Olympic Names
olympicBBNames <- basketballMale %>%
  select(Name) %>%
  mutate(Name = str_to_lower(str_squish(Name))) %>%
  mutate(first_last = sapply(str_split(Name, " "), getFirstLast)) %>%
# Make column without middle names to join two datasets
  rename("Player" = first_last)
  
## NBA Names
nbaNames <- nba %>%
  select(Player) %>%
  mutate(Player = str_to_lower(str_squish(Player))) %>%
  mutate(Player = str_replace_all(Player,"\\*$","")) # Remove asterisks

## Join
olympicNBA <- inner_join(olympicBBNames, nbaNames,
                         relationship = "many-to-many")

## Joining Countries
countryNameClean <- basketballMale %>%
  select(Name,Team) %>%
  mutate(Name = str_to_lower(str_squish(Name))) %>%
  mutate(first_last = sapply(str_split(Name, " "), getFirstLast)) %>%
  rename("Player" = first_last) %>%
  distinct(Player, .keep_all = TRUE)

olympicNBACountries <- right_join(olympicNBA, countryNameClean)

## Joining Medals
medalsNBACountries <- left_join(countryMedalists, olympicNBACountries,
                                by = "Team")

nbaPerCountry <- medalsNBACountries %>%
  distinct(Player, Team, .keep_all = TRUE) %>%
  group_by(Team) %>%
  summarise(
    Medals = first(total),
    numPlayers = n()
  ) %>%
  arrange(desc(Medals))

# Data Visualizations ----

## Medals for each country
ggplot(
  data = medalsPerBBTeam,
  mapping = aes(x = reorder(`Team`,-Count), y = `Count`, fill = `Medal`)
  # Order the bars by count
  ) +
  geom_bar(stat = "identity", position = "stack") + # Stacking diff. groups
  labs(
    title = "Frequency of Basketball Medals Won by Country",
    x = "Country",
    y = "Medals Won"
  ) +
  scale_fill_manual(values = c("Gold" = "gold","Silver" = "gray","Bronze" = "brown")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 8, angle = 35),
    legend.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title.x = element_text(vjust = 1, margin = margin(t = -20))
    # Closes gap between axis title and axis text
  ) +
  scale_y_continuous(limits = c(0,25))

## How size affects medals won for athletes
ggplot(
  data = maleBB_HandW,
  mapping = aes(
    x = avgHeight,
    y = avgWeight,
    size = total,
    color = total
    )
) + 
  geom_point() +
  labs(
    title = "How Size Affects the Amount of Medals\na Male Athlete Wins",
    x = "Average Height per Athlete (in)",
    y = "Average Weight per Athlete (lbs)",
    size = "Number of Medals Won",
    color = "Number of Medals Won"
  ) +
  scale_color_gradientn(colors = c("red","orange","yellow","green","blue")) +
  guides(size = guide_legend(), color = guide_legend()) + # Combine guides
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 8, angle = 35),
    legend.title = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position = "bottom"
  )

ggplot(
  data = femaleBB_HandW,
  mapping = aes(
    x = avgHeight,
    y = avgWeight,
    size = total,
    color = total
  )
) + 
  geom_point() +
  labs(
    title = "How Size Affects the Amount of Medals\na Female Athlete Wins",
    x = "Average Height per Athlete (in)",
    y = "Average Weight per Athlete (lbs)",
    size = "Number of Medals Won",
    color = "Number of Medals Won"
  ) +
  scale_color_gradientn(colors = c("red","orange","yellow","green","blue","purple")) +
  guides(size = guide_legend(), color = guide_legend()) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 8, angle = 35),
    legend.title = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position = "bottom"
  )

## NBA Players in each medaling country
nbaPerCountry %>%
  kable("html", caption = "<center>Olympic Basketball medalist countries and 
      how many NBA players have played for that country</center>",
      col.names = c("Country", "Number of Medals", "Number of NBA Players")) %>%
  footnote("Players who've played in multiple Olympics are counted once") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  row_spec(0, bold = TRUE, font_size = 15)


