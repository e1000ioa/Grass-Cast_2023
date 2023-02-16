
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(anytime)

NPP_ANPP_20_21 <- read.csv(file = "data/grass_cast_20-21.csv", head = TRUE, sep=",")
NPP_ANPP_22 <- read.csv(file = "data/grass_cast_2022.csv", head = TRUE, sep=",") %>% anydate(NPP_ANPP_22$Forecast)

#Format the Dates
NPP_ANPP_22 <- NPP_ANPP_22 %>%
  mutate(anydate(NPP_ANPP_22$Forecast))

NPP_ANPP_20_21 <- NPP_ANPP_20_21 %>%
  mutate(anydate(NPP_ANPP_20_21$Forecast))

NPP_ANPP_20_21$Forecast2 <- anydate(NPP_ANPP_20_21$Forecast) 

NPP_ANPP_20_21$Forecast <- gsub("/", "-", NPP_ANPP_20_21$Forecast)




#Combine DF
NPP_ANPP_20_21_22 <- dplyr::bind_rows(NPP_ANPP_20_21, NPP_ANPP_22) #Append df with different columns 

#Replace symbols and fix dates
NPP_ANPP_20_21_22$Forecast <- gsub("/", "-", NPP_ANPP_20_21_22$Forecast)
NPP_ANPP_20_21_22$Forecast <- gsub("-22", "-2022", NPP_ANPP_20_21_22$Forecast)
NPP_ANPP_20_21_22$Forecast <- gsub("-21", "-2021", NPP_ANPP_20_21_22$Forecast)
NPP_ANPP_20_21_22$Forecast <- gsub("08-11-20", "08-11-2020", NPP_ANPP_20_21_22$Forecast)
NPP_ANPP_20_21_22$Forecast <- gsub("06-02-20", "06-02-2020", NPP_ANPP_20_21_22$Forecast)
NPP_ANPP_20_21_22$Forecast <- gsub("09-01-20", "09-01-2020", NPP_ANPP_20_21_22$Forecast)
NPP_ANPP_20_21_22$Forecast <- gsub("09-01-202022", "09-01-2022", NPP_ANPP_20_21_22$Forecast)

NPP_ANPP_20_21_22$Forecast2 <- anydate(NPP_ANPP_20_21_22$Forecast)

#Select only one forecast
ScarterPlot <- function(x){
  
  Forecast_1 <- subset(NPP_ANPP_20_21_22, Forecast == x)
  
  ggplot(Forecast_1, aes(x=meanNDVIgrid, y=NPP_predict_avg)) +
    geom_point() +
    stat_poly_line() +
    stat_poly_eq() +
    geom_smooth(method=lm) +
    labs(x = "Mean Grind NDVI",
         y = "NPP Prediction (lb/ac)",
         title = paste0("NPP/NDVI ",x))
  
}

# Get the unique values of the "Forecast" column
unique_dates <- unique(NPP_ANPP_20_21_22$Forecast)

# Loop through the unique values
for (i in 1:length(unique_dates)) {
  # Create a scatter plot for each unique value
  
  plot <- ScarterPlot(unique_dates[i])
  
  # Save the plot to a png file
  ggsave(plot, filename = paste0("scaterplots/",unique_dates[i], ".png"))
}






