# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)


# List all CSV files in the directory
path <- "/Users/lio/Git/Grass-Cast_2023/data/2024"
csv_files <- list.files(path, pattern = "*.csv", full.names = TRUE)

###### Check Column names and Data #####

# Initialize an empty data frame to store the summary
summary_df <- data.frame(Year = character(), Date = character(), Col_Names = character())

# Iterate over each CSV file to extract Year, Date, and Column Names
for (file in csv_files) {
  # Extract the base name of the file (without the directory path)
  file_name <- basename(file)
  
  # Remove the file extension ".csv" to get just the base name
  file_name <- sub("\\.csv$", "", file_name)
  
  # Extract Year and Date from the file name
  year <- sub(".*_(\\d{4})_.*", "\\1", file_name)
  date <- sub(".*_\\d{4}_(.*)", "\\1", file_name)
  
  # Read the column names without loading the full file using read_delim
  col_names <- colnames(read_delim(file, delim = ",", n_max = 0))
  
  # Rename columns that match the pattern
  col_names <- gsub("NDVI_predict_\\d{4}_below", "NDVI_predict_below", col_names)
  col_names <- gsub("NDVI_predict_\\d{4}_avg", "NDVI_predict_avg", col_names)
  col_names <- gsub("NDVI_predict_\\d{4}_above", "NDVI_predict_above", col_names)
  
  # Concatenate all column names into a single string separated by commas
  col_names_str <- paste(col_names, collapse = ", ")
  
  # Create a new row for the summary data frame
  new_row <- data.frame(Year = year, Date = date, Col_Names = col_names_str)
  
  # Append the new row to the summary data frame
  summary_df <- rbind(summary_df, new_row)
}

# Display the summary data frame
print(summary_df)



###### Unite de Files #####

# Initialize an empty list to store processed dataframes
processed_dfs <- list()

# Iterate over each CSV file to process columns and add filename-based columns
for (file in csv_files) {
  # Extract the base name of the file (without the directory path)
  file_name <- basename(file)
  
  # Remove the file extension ".csv" to get just the base name
  file_name <- sub("\\.csv$", "", file_name)
  
  # Read the CSV file
  df <- read_csv(file)
  
  # Rename columns by removing numbers in patterns like "####"
  new_col_names <- gsub("_(\\d{4})_", "_", colnames(df))
  colnames(df) <- new_col_names
  
  # Extract year, month, and day from the filename
  year <- sub(".*_(\\d{4})_.*", "\\1", file_name)
  month <- sub(".*_(\\d{4})_([A-Za-z]+)_.*", "\\2", file_name)
  day <- sub(".*_(\\d{4})_[A-Za-z]+_(\\d+)", "\\2", file_name)
  
  # Add new columns for Year, Month, and Day
  df <- df %>% 
    mutate(Year = as.integer(year),
           Month = month,
           Day = as.integer(day),
           File_Name = file_name)
  
  # Create a Date column using Year, Month, and Day
  df <- df %>%
    mutate(Forecast = as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%B-%d"))
  
  # Append the processed dataframe to the list
  processed_dfs[[file_name]] <- df
}

# Unite all the processed dataframes into one
combined_df <- bind_rows(processed_dfs)

# Display the combined dataframe
print(combined_df)

hist(combined_df$NPP_predict_below)
hist(combined_df$NDVI_predict_below)
colnames(combined_df)
