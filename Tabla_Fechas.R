# Given dates
dates <- c("2020-08-11", "2020-07-14", "2020-07-29", "2020-06-16", 
           "2020-06-02", "2020-06-30", "2020-05-15", "2020-09-01",
           "2021-04-14", "2021-08-10", "2021-08-24", "2021-07-13", 
           "2021-07-28", "2021-06-01", "2021-06-16", "2021-06-29",
           "2021-05-18", "2021-05-04", "2022-04-19", "2022-04-05", 
           "2022-08-23", "2022-08-09", "2022-07-12", "2022-07-26",
           "2022-06-14", "2022-06-28", "2022-05-17", "2022-05-03", 
           "2022-05-31", "2022-09-01")

# Convert dates to Date objects
dates <- as.Date(dates)

# Function to categorize seasons
categorize_season <- function(date) {
  month <- as.numeric(format(date, "%m"))
  if (month >= 3 && month <= 5) {
    return("Spring")
  } else if (month >= 6 && month <= 8) {
    return("Summer")
  } else {
    return(NA)
  }
}

# Extract year and season for each date
year <- as.numeric(format(dates, "%Y"))
season <- sapply(dates, categorize_season)

# Create a data frame with year, season, and date
dates_df <- data.frame(Year = year, Season = season, Date = dates)

# Spread the data frame to get a format similar to your requested table
library(tidyr)
library(dplyr)

# Arrange dates by year, then spread into a wide format
dates_df <- dates_df %>%
  filter(!is.na(Season)) %>%
  arrange(Year, Season, Date)

dates_table <- dates_df %>%
  group_by(Season, Year) %>%
  mutate(Row = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = Year, values_from = Date) %>%
  select(-Row)

# Display the result
print(dates_table)
