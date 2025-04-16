library(dplyr)

AllClimate <- read.csv("Data_precip/data/AllClimate_Data_2010.csv")

# select only columns that contain "normal" and the first 5 columns
cli_normal <- AllClimate %>%
  select(1:5, matches("normal")) %>%
  slice(which(rowSums(is.na(.)) == 0))

#Select Columns of Average rainfall, Latitute and longitude, name of station
mean_prec <- cli_normal %>% select(1:5,"ANN.PRCP.NORMAL")

#Write csv
write.csv(mean_prec, file="data/LongTermAvg_prec.csv")
