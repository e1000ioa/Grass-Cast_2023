library(dbplyr) #pipes and more
library(janitor) #cleans col numbers and more
library(data.table) #for splits
library(lubridate) #to change the date
library(tidyverse)  #to load the core tidyverse and make it available in your current R session.


####################
#######Data base prep
#upload the data and select collumns 

#Grind ID 
GC <- read.csv(file = "data/az_nm_2000_2020.csv", head = TRUE, sep=",") 

#Forecast Data
gfg_data_22 <- read.csv(file = "data/grass_cast_2022.csv", head = TRUE, sep=",") %>% 
  subset(select=c("gridID","Year","Forecast","NPP_predict_below","NPP_predict_avg","NPP_predict_above","Order"))

gfg_data_21_20 <- read.csv(file = "data/grass_cast_20-21.csv", head = TRUE, sep=",") %>% 
  subset(select=c("gridID","Year","Forecast","NPP_predict_below","NPP_predict_avg","NPP_predict_above","Order"))

forcasts <- rbind(gfg_data_22,gfg_data_21_20) 

##Format Dates
parsed_dates <- parse_date_time(forcasts$Forecast, orders = c("mdy"))
forcasts$Forecast <- format(parsed_dates, "%Y-%m-%d")

sum(is.na(forcasts$Forecast))
unique(forcasts$Forecast)

#Region separation file
summerwinter <- read.csv(file = "data/RatioSummerWinter.csv", head = TRUE, sep=",") %>% 
  subset(select=c("gridID","latitude","longitude","pptRatioSummerWinter"))

#########
############## 
#Joins the table above
RatioSW_ForecastANPP <- left_join(gfg_data, summerwinter, by = c("gridID" = "gridID"))

#split each data in summer, transision, winter
ANPP_FORECAST_W <- subset(RatioSW_ForecastANPP, pptRatioSummerWinter < '0.8')
ANPP_FORECAST_T <- RatioSW_ForecastANPP  %>%   filter(pptRatioSummerWinter >0.8 &  pptRatioSummerWinter < 1.2)
ANPP_FORECAST_S <- subset(RatioSW_ForecastANPP, pptRatioSummerWinter > '1.2')

#Slip df in list with forecast
ANPP_FORECAST_W <- split(ANPP_FORECAST_W,ANPP_FORECAST_W$Forecast)
ANPP_FORECAST_T <- split(ANPP_FORECAST_T,ANPP_FORECAST_T$Forecast)
ANPP_FORECAST_S <- split(ANPP_FORECAST_S,ANPP_FORECAST_S$Forecast)


#############
##NPP CORRELATION
fc <- function(n, x, y) {
  
  #N is the observation we are testing
  #x is the final observation
  #y is data frame
  a <- cor(y[[n]]$NPP_predict_above, y[[x]]$NPP_predict_above)
  b <- cor(y[[n]]$NPP_predict_avg, y[[x]]$NPP_predict_avg)
  c <- cor(y[[n]]$NPP_predict_below, y[[x]]$NPP_predict_below)
  
  #Creates a matrtix 
  ANPP.r <- matrix(c(a,b,c),ncol = 3,byrow = TRUE)
  colnames(ANPP.r) <- c('Abv','Avg','Blw')
  ANPP.r <- as.data.frame(ANPP.r) 
  #chages row names to date
  #row.names(ANPP.r) <- as.Date(names(y[x]), format = "%m/%d/%Y")  
  
  return(ANPP.r)
}


##Creates an empty list for the for loop
r.list_W <- list()
r.list_T <- list()
r.list_S <- list()

##For loop to have all the rsquer in the list
#the final observation was on place 8 for 2020 and place 10 for 2021
for(i in 1:leght(ANPP_FORECAST_2022_W)) {
  r.list_W[[i]] <- fc(12, i,ANPP_FORECAST_2022_W)
  r.list_T[[i]] <- fc(12, i,ANPP_FORECAST_2022_T)
  r.list_S[[i]] <- fc(12, i,ANPP_FORECAST_2022_S)
}