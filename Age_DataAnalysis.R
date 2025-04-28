# Import Libraries 
library(tidyverse)
library(ggplot2)

# create age frequency table ----
age_freq_table <- table(athlete_events$Age)

# Convert to data frame
age_freq_df <- as.data.frame(age_freq_table)
colnames(age_freq_df) <- c("Age", "Frequency")

# Print the data frame
print(age_freq_df)

# barplot for age in all data
barplot(age_freq_table,
        main = "Frequency of Ages",
        xlab = "Age",
        ylab = "Frequency",
        col = "skyblue",
        border = "white",
        las = 2,     # Rotate x-axis labels for better readability
        cex.names = 0.7)  # Shrink x-axis labels if too crowded

# create table/df for ages frequency by sport ----
age_sport_freq <- table(athlete_events$Sport, athlete_events$Age)

# View the table
print(age_sport_freq)

# Convert the table to a data frame
age_sport_freq_df <- as.data.frame(age_sport_freq) %>% 
  rename(
    Sport = Var1,
    Age = Var2
  )

