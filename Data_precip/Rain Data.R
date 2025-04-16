library(dplyr)             # Load the dplyr package
library(tidyr)
library(purrr)
library(data.table)
library(geosphere) #nearest Neighbourt
library(stringdist) #cluster 
library(lubridate)
library(splusTimeDate)

# List all files in the folder
AZ_files <- list.files(path = "Data_precip/Rain_Data/Arizona", pattern = "\\.csv$", full.names = TRUE)
NM_files <- list.files(path = "Data_precip/Rain_Data/NewMexico", pattern = "\\.csv$", full.names = TRUE)

# Read and bind all CSV files
AZ_data <- AZ_files %>%
  map_dfr(read.csv)

NM_data <- NM_files %>%
  map_dfr(read.csv)

#combined and format date
SW_data <- rbind(AZ_data, NM_data)
SW_data$DATE <- as.Date(SW_data$DATE)

#REad ANPP Data
ANPP <- read.csv("data/ANPP_2020-2022.csv")
ANPP$Forecast <- as.Date(ANPP$Forecast)
ANPP <- split(ANPP, ANPP$Forecast)

Data <- function(N) {

#Set Dates
start_date <- as.Date(names(ANPP)[N])-lubridate::days(6)
end_date <- as.Date(names(ANPP)[N])

# Filter only rows within the specific date range
ANPP_Spring <- do.call(rbind, ANPP) %>%
  filter(Forecast >= start_date, Forecast <= end_date)

# Filter only rows within the specific date range
SW_spring <- SW_data %>%
  filter(DATE >= start_date, DATE <= end_date) %>%
  select(1:6, PRCP)

sort(unique(SW_spring$DATE), decreasing = TRUE)

# Create biweekly intervals using the cut function
SW_spring <- as.data.table(SW_spring)
SW_spring[, biweek_end_date := cut(DATE, breaks = "2 weeks")]

# Summarize precipitation values for each biweek using data.table
SW_spring_biweek <- SW_spring[, .(total_PRCP = sum(PRCP)),
                                by = .(STATION, biweek_end_date, NAME, LATITUDE, LONGITUDE, ELEVATION)] %>%
                                 mutate(week_number = week(biweek_end_date))

##### NEAREST NEIGHBOUR ####

# Function to find the nearest neighbor in df2 for each row in df1
find_nearest_neighbor <- function(lat, long, df) {
  distances <- distGeo(matrix(c(long, lat), ncol = 2), matrix(c(df$long, df$lat), ncol = 2))
  nearest_index <- which.min(distances)
  return(df[nearest_index, ])
}

# Create an empty list to store the results
result_list <- list()

# Iterate over each row in df1 and find the nearest neighbor in df2
for (i in 1:nrow(SW_spring_biweek)) {
  nearest_neighbor <- find_nearest_neighbor(SW_spring_biweek$LATITUDE[i], SW_spring_biweek$LONGITUDE[i], ANPP_Spring)
  result_list[[i]] <- cbind(SW_spring_biweek[i, ], nearest_neighbor[3:4],NPP_predict_avg = nearest_neighbor$NPP_predict_avg)
}

# Combine the results into a new data frame
merged_df <- do.call(rbind, result_list)

# Split names into two columns using the comma separator
split_names <- strsplit(merged_df$NAME, ",")

# Create new columns for First_Name and Last_Name
merged_df$County <- sapply(split_names, "[", 1)
merged_df$State <- sapply(split_names, "[", 2)

# Calculate the string distance matrix
dist_matrix <- stringdist::stringdistmatrix(merged_df$County, merged_df$County)

# Define a threshold for similarity
similarity_threshold <- 6 # You can adjust this value based on your data

# Perform hierarchical clustering
hc <- hclust(as.dist(dist_matrix))
clusters <- cutree(hc, h = similarity_threshold)

# Add the cluster information to the data frame
merged_df$cluster <- clusters

# Group by cluster and summarize the data
Cluster_summary <- merged_df %>%
  group_by(cluster) %>%
  summarise(
    Forecast = Forecast,
    mean_PRCP = mean(total_PRCP, na.rm = TRUE),
    avg_NPP_predict_avg = mean(NPP_predict_avg, na.rm = TRUE),
    LATITUDE = names(which.max(table(LATITUDE))),
    LONGITUDE = names(which.max(table(LONGITUDE))),
    County = names(which.max(table(County))),
    State = names(which.max(table(State))),
    week_number = names(which.max(table(week_number)))
  )

# Define the three latitude ranges
latitude_range1 <- c(38, 35)
latitude_range2 <- c(35, 33)
latitude_range3 <- c(33, 30)

# Filter data for each latitude range and group by State to calculate summary statistics
Lat_summary <- Cluster_summary %>%
  filter(
    (LATITUDE >= latitude_range1[2] & LATITUDE <= latitude_range1[1]) |
      (LATITUDE >= latitude_range2[2] & LATITUDE <= latitude_range2[1]) |
      (LATITUDE >= latitude_range3[2] & LATITUDE <= latitude_range3[1])
  ) %>%
  group_by(State, Latitude_Range = case_when(
    LATITUDE >= latitude_range1[2] & LATITUDE <= latitude_range1[1] ~ "35-33",
    LATITUDE >= latitude_range2[2] & LATITUDE <= latitude_range2[1] ~ "33-31",
    LATITUDE >= latitude_range3[2] & LATITUDE <= latitude_range3[1] ~ "31-29",
    TRUE ~ "Other"
  )) %>%
  summarise(
    Total_PRCP = max(mean_PRCP, na.rm = TRUE),
    Avg_NPP_predict_avg = mean(avg_NPP_predict_avg, na.rm = TRUE),
    Count = n()
  ) %>%
  mutate(Forecast = as.Date(names(ANPP)[N]))

return(Lat_summary)

}

# Initialize an empty data frame to store the results
Prep_Anpp <- data.frame()

# Loop to generate data frames and append to the combined data frame
for (i in 1:length(ANPP)) {
  result_df <- Data(i)
  Prep_Anpp <- rbind(Prep_Anpp, result_df)
}

##### CHECK #####
# Count the number of occurrences of "2 ABBOTT 1 SE, NM US" in the "NAME" column
sum(SW_spring$NAME == "ABBOTT 1 SE, NM US")
sum(SW_spring_biweek$NAME == "ABBOTT 1 SE, NM US")

# Replace "2 ABBOTT 1 SE, NM US" with your specific name
specific_name <- "ABBOTT 1 SE, NM US"

# Filter the data to keep only rows with the specific name
filtered_data <- SW_spring %>%
  filter(NAME == specific_name)

# Filter the data to keep only rows with the specific name
filtered_data2 <- SW_spring_biweek %>%
  filter(NAME == specific_name)

# Calculate the sum of "PRCP" for the filtered data
total_PRCP_specific_name <- sum(filtered_data$PRCP, na.rm = TRUE)
total_PRCP_specific_name2 <- sum(filtered_data2$total_PRCP, na.rm = TRUE)
