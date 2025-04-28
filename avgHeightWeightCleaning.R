# Heights and Weights HTML Scraping

# Needs ----
library(tidyverse)
library(rvest)

## Turn URL into HTML
heightWeightHTML <- read_html(
  x = "https://www.worlddata.info/average-bodyheight.php") %>%
  html_table()

## Get Tables
maleAVG <- heightWeightHTML[[1]]

femaleAVG <- heightWeightHTML[[2]]

## Clean data

maleAvgClean <- maleAVG %>%
  rename(
    "avgHeight_inCM" = colnames(maleAVG)[2],
    "avgWeight_inKG" = colnames(maleAVG)[3],
    "BMI" = colnames(maleAVG)[4]
  ) %>%
  mutate(
    avgHeight_inCM = parse_number(avgHeight_inCM),
    avgWeight_inKG = parse_number(avgWeight_inKG)
  ) %>%
  mutate(
    avgHeight_inCM = avgHeight_inCM * 100
  )

femaleAvgClean <- femaleAVG %>%
  rename(
    "avgHeight_inCM" = colnames(femaleAVG)[2],
    "avgWeight_inKG" = colnames(femaleAVG)[3],
    "BMI" = colnames(femaleAVG)[4]
  ) %>%
  mutate(
    avgHeight_inCM = parse_number(avgHeight_inCM),
    avgWeight_inKG = parse_number(avgWeight_inKG)
  ) %>%
  mutate(
    avgHeight_inCM = avgHeight_inCM * 100
  )
