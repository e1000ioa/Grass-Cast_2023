# Data Tools
library(ggplot2) #ggplot2 is a system for declaratively creating graphics, based on The Grammar of Graphics.
library(dplyr) #dplyr is a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges
library(tidyr) #clean messy data
library(ggthemes) #Extra Themes, Scales and Geoms for 'ggplot2'
library(plotrix) #Extra of plots, various labeling, axis and color scaling functions.
library(lubridate) #Options to works with date formats
library(zoo) #Infrastructure for Regular and Irregular Time Series
library(plotrix) #add talyor pltos
library(Metrics) #has the rmse fucntion
library(scales) #p value fomat

set.seed(108)

##########
#Load and Select Data
#COPY FROM CORREALTIONS_2020-2022
#Grind ID 
GC <- read.csv(file = "data/az_nm_2000_2020.csv", head = TRUE, sep=",")

#Forecast Data
gfg_data_22 <- read.csv(file = "data/grass_cast_2022.csv", head = TRUE, sep=",") %>% 
  subset(select=c("gridID","Year","Forecast","deltaNPP_below","deltaNPP_avg","deltaNPP_above","Order"))

gfg_data_21_20 <- read.csv(file = "data/grass_cast_20-21.csv", head = TRUE, sep=",") %>% 
  subset(select=c("gridID","Year","Forecast","deltaNPP_below","deltaNPP_avg","deltaNPP_above","Order"))

forcasts_0ld <- rbind(gfg_data_22,gfg_data_21_20) 
Dates_List_0ld <- unique(Forecast_Ratio$Forecast)

#Forecast 2024 # From File_Process_2024

forcasts <- combined_df %>% 
  subset(select=c("gridID","Year","Forecast","deltaNPP_below","deltaNPP_avg","deltaNPP_above"))

##Parse the dates into a consistent format
#parsed_dates <- parse_date_time(forcasts$Forecast, orders = c("mdy")) # specifying the possible formats using the orders argument
forcasts$Forecast <- format(parsed_dates, "%Y-%m-%d") #The possible formats are listed in order of preference, so the function will try to parse each date in the first format, and if that fails, it will move on to the next format, and so on.

#Region separation file
summerwinter <- read.csv(file = "data/RatioSummerWinter.csv", head = TRUE, sep=",") %>% 
  subset(select=c("gridID","latitude","longitude","pptRatioSummerWinter"))

#Joins the table 
Forecast_Ratio <- left_join(forcasts, summerwinter, by = c("gridID" = "gridID"))

#Check all the dates
Dates_List <- unique(Forecast_Ratio$Forecast)
i1 <- order(as.Date(Dates_List, format = "%Y-%m-%d"))
Dates_List[i1]

###### Make the lengths of the list match ######

# Load the data
df <- Forecast_Ratio

# Split into lists by Forecast
df_list <- split(df, df$Forecast)

# Find the shared gridID values between the two dates
SameID <- intersect(df_list[["2022-04-19"]]$gridID, df_list[["2022-05-03"]]$gridID)

# Filter each list based on whether gridID is in SameID, using lapply
filtered_list <- lapply(df_list, function(sub_df) {
  subset(sub_df, gridID %in% SameID)
})

# Combine filtered list back into a single dataframe
Forecast_Ratio <- do.call(rbind, filtered_list)
rownames(Forecast_Ratio) <- NULL # Reset row names

# Split again into the original list format if needed
df <- split(Forecast_Ratio, Forecast_Ratio$Forecast)

#######Stats Table to check Taylor Plots #######################################################################

# Define the core statistics function
Stats <- function(data, start_date, end_date, scenario) {
  # Filter data by date range
  df <- data[data$Forecast >= as.Date(start_date) & data$Forecast <= as.Date(end_date), ]
  df <- split(df, df$Forecast)
  
  # Nested function to compute metrics for each date subset
  compute_metrics <- function(date_index, full_data) {
    df_n <- full_data[[date_index]]
    df_x <- full_data[[length(full_data)]]
    
    mean_val <- mean(df_n[[scenario]], na.rm = TRUE)
    sd_val <- sd(df_n[[scenario]], na.rm = TRUE)
    count_val <- length(df_n[[scenario]])
    
    mod <- lm(df_n[[scenario]] ~ df_x[[scenario]])
    cf <- coef(mod)
    
    # Compute metrics
    metrics <- data.frame(
      Forecast = names(full_data)[date_index],
      MEAN = mean_val,
      SD = sd_val,
      COUNT = count_val,
      SLOPE = cf[2],
      INTERCEPT = cf[1],
      COR = cor(df_n[[scenario]], df_x[[scenario]]),
      R2 = summary(mod)$r.squared,
      STD_ERROR = summary(mod)$coefficients[2, 2],
      BIAS = mean(df_n[[scenario]] - df_x[[scenario]], na.rm = TRUE),
      RMSE = sqrt(mean((df_n[[scenario]] - df_x[[scenario]])^2, na.rm = TRUE)),
      P_VALUE = summary(mod)$coefficients[2, 4]
    )
    
    return(metrics)
  }
  
  # Apply metrics computation over each split
  results <- do.call(rbind, lapply(seq_along(df), compute_metrics, full_data = df))
  results <- results %>% mutate(across(where(is.numeric), round, 3))  # Round numeric columns
  
  return(results)
}

# Helper function to get seasonal statistics
getSeasonStats <- function(data, spring_dates, summer_dates, scenario, nom) {
  # Create a function to assemble stats for each season
  assemble_season <- function(dates) {
    results <- lapply(dates, function(date_range) {
      Stats(data, date_range[1], date_range[2], scenario)
    })
    season_data <- do.call(rbind, results)
    season_data <- cbind(season_data, Scenario = nom)
    return(season_data)
  }
  
  # Assemble Spring and Summer statistics
  Spring <- assemble_season(spring_dates)
  Summer <- assemble_season(summer_dates)
  
  list(Spring = Spring, Summer = Summer)
}

# Date ranges for Spring and Summer
spring_dates <- list(c("2020-05-15", "2020-06-02"), c("2021-04-14", "2021-06-01"), c("2022-04-05", "2022-05-31"))
summer_dates <- list(c("2020-06-16", "2020-09-01"), c("2021-06-16", "2021-08-24"), c("2022-06-14", "2022-09-01"))

# Compile regional data
buildRegionData <- function(data, region_name) {
  SpringB <- getSeasonStats(data, spring_dates, summer_dates, 4, "Blw")$Spring
  SpringA <- getSeasonStats(data, spring_dates, summer_dates, 5, "Avg")$Spring
  SpringC <- getSeasonStats(data, spring_dates, summer_dates, 6, "Abv")$Spring
  SummerB <- getSeasonStats(data, spring_dates, summer_dates, 4, "Blw")$Summer
  SummerA <- getSeasonStats(data, spring_dates, summer_dates, 5, "Avg")$Summer
  SummerC <- getSeasonStats(data, spring_dates, summer_dates, 6, "Abv")$Summer
  
  season_data <- rbind(SpringB, SpringA, SpringC, SummerB, SummerA, SummerC)
  season_data <- season_data %>%
    mutate(Season = if_else(str_detect(rownames(season_data), "Spring"), "Spring", "Summer"),
           Region = region_name) %>%
    select(Region, Season, everything()) %>%
    arrange(Season, Forecast, Scenario)
  
  return(season_data)
}

# Build and save data for each region
Southwest_Stats <- buildRegionData(Forecast_Ratio, "Southwest")
Winter_Region <- buildRegionData(ANPP_FORECAST_W, "Winter")
Transition_Region <- buildRegionData(ANPP_FORECAST_T, "Transition")
Summer_Region <- buildRegionData(ANPP_FORECAST_S, "Summer")

# Consolidate and write to CSV
Regions <- rbind(Southwest_Stats, Winter_Region, Transition_Region, Summer_Region) %>%
  mutate(Scenario = factor(Scenario, levels = c("Blw", "Avg", "Abv"))) %>%
  arrange(Region, Season, Forecast, Scenario)

write.csv(Regions, "data/regions_stats.csv")


##############################################################################
#Select data to add to the Taylor Diagram

#All regions as one
ANPP_FORECAST_ALL <- split(Forecast_Ratio, Forecast_Ratio$Forecast)
ANPP_FORECAST_W <- split(ANPP_FORECAST_W, ANPP_FORECAST_W$Forecast)
ANPP_FORECAST_T <- split(ANPP_FORECAST_T, ANPP_FORECAST_T$Forecast)
ANPP_FORECAST_S <- split(ANPP_FORECAST_S, ANPP_FORECAST_S$Forecast)

#############
################## TAYLOR PLOTS

Taylor_Maker <- function(df, zone, n, x, title) {
  
  #Where:
  #n = date start
  #x = date end
  #Season = Spring or Summer
  
  model_select <- function(df,scenario) {
    # df: Data frame to select models from
    
    # Extract the model names
    model_names <- names(df)
    
    # Initialize an empty list to hold the models
    models <- list()
    
    # Add the models to the list
    for (i in 1:length(df)) {
      model_name <- model_names[i]
      model <- df[[i]][[scenario]]
      models[[model_name]] <- model
    }
    
    return(models)
  }
  
  year <- year(as.Date(n))
  
  modelsA <- model_select(df,4) #Bellow
  modelsB <- model_select(df,5) #Average
  modelsC <- model_select(df,6) #Above
  
  ColorA <- "#CB3446"
  ColorB <- "#3446CB"
  ColorC <- "#46CB34"
  
  #Creates new dataframe  filtering the dates
  dfz <- orderModels[orderModels$name >= as.Date(n) & orderModels$name <= as.Date(x),]
  
  #Creates the point of re fences, the last observation all the values are on Models
  ref <- as.numeric(modelsA[[tail(dfz$order, n=1)]])
  
  #Generates the plot
  png(file= paste0("images/taylor/new/TaylorDiagram_",title,".png"),
      width=1000, height=1000, pointsize = 25) 
  
  #Creates first diagram
  taylor.diagram(ref,as.numeric(modelsA[[dfz$order[1]]]), col=ColorA, pch=15, cex=1, pcex = 2,
                 main = NULL,
                 pos.cor=FALSE,
                 show.gamma = T,
                 sd.arcs = T,
                 normalize = F, main.y = 0.2)
  
  #Adds diagrams
  for (i in 2:(length(dfz$order)-1)){
    
    #BElow avg Model
    taylor.diagram(ref,as.numeric(modelsA[[dfz$order[i]]]), add=TRUE,col=ColorA,pch=head(c(15,16,17,18,19,20), length(dfz$order)-1)[i], cex=1,
                   pcex = 2)
    
    #Separates the Null Hypothesis
    taylor.diagram(ref,as.numeric(modelsA[[tail(dfz$order, n=1)]]), add=TRUE,col="black",pch=4, cex=1,
                   pcex = 2) 
    
    #Cex = plotting text and symbols should be scaled relative to the default
    #pcex = point expansion for the plotted points.
  }
  
  
  for (i in 1:(length(dfz$order)-1)){
    #Average model
    taylor.diagram(ref,as.numeric(modelsB[[dfz$order[i]]]), add=TRUE,col=ColorB,pch=head(c(15,16,17,18,19,20), length(dfz$order)-1)[i], cex=1,
                   pcex = 2, order = -100) 
    #Above avg model
    taylor.diagram(ref,as.numeric(modelsC[[dfz$order[i]]]), add=TRUE,col=ColorC, pch=head(c(15,16,17,18,19,20), length(dfz$order)-1)[i], cex=1,
                   pcex = 2) 
  }
  
  legend("bottom", title="Forecast Dates and Scenarios", 
         legend=c(substr(dfz$name, 6, 10), "Bellow", "Average", "Above"), 
         col=c(rep("black",length(substr(dfz$name, 6, 10))),ColorA,ColorB,ColorC), pch=c(head(c(15,16,17,18,19,20), length(dfz$order)-1),13,rep(16,3)),
         bty="n", border=F, ncol=5, x.intersp = 1.5)
  
  
  
  # Add text box
  #text(x = x_text, y = y_text, pos = 1, srt = angle, label = "RMSE (lb/acre)", font = 3, cex = 0.8)
  
  dev.off()
  
}
########## ALL
#2020
Taylor_Maker(ANPP_FORECAST_ALL, "ALL", "2020-05-15","2020-06-02","Spring2020")
Taylor_Maker(ANPP_FORECAST_ALL, "ALL", "2020-06-16","2020-09-01","Summer2020")

#2021
Taylor_Maker(ANPP_FORECAST_ALL, "ALL", "2021-04-14","2021-06-01","Spring2021")
Taylor_Maker(ANPP_FORECAST_ALL, "ALL", "2021-06-16","2021-08-24","Summer2021")

#2022
Taylor_Maker(ANPP_FORECAST_ALL, "ALL", "2022-04-05","2022-05-31","Spring2022")
Taylor_Maker(ANPP_FORECAST_ALL, "ALL", "2022-06-14","2022-09-01","Summer2022")

##### WINTER ZONE
#2020
Taylor_Maker(ANPP_FORECAST_W, "W", "2020-05-15","2020-06-02","2020WinterZone_Spring")
Taylor_Maker(ANPP_FORECAST_W, "W", "2020-06-16","2020-09-01","2020WinterZone_Summer")

#2021
Taylor_Maker(ANPP_FORECAST_W, "W", "2021-04-14","2021-06-01","2021WinterZone_Spring")
Taylor_Maker(ANPP_FORECAST_W, "W", "2021-06-16","2021-08-24","2021WinterZone_Summer")

#2022
Taylor_Maker(ANPP_FORECAST_W, "W", "2022-04-05","2022-05-31","2022WinterZone_Spring")
Taylor_Maker(ANPP_FORECAST_W, "W", "2022-06-14","2022-09-01","2022WinterZone_Summer")

##### TRANSITION ZONE
#2020
Taylor_Maker(ANPP_FORECAST_T, "T", "2020-05-15","2020-06-02","2020TransitionZone_Spring")
Taylor_Maker(ANPP_FORECAST_T, "T", "2020-06-16","2020-09-01","2020TransitionZone_Summer")

#2021
Taylor_Maker(ANPP_FORECAST_T, "T", "2021-04-14","2021-06-01","2021TransitionZone_Spring")
Taylor_Maker(ANPP_FORECAST_T, "T", "2021-06-16","2021-08-24","2021TransitionZone_Summer")

#2022
Taylor_Maker(ANPP_FORECAST_T, "T", "2022-04-05","2022-05-31","2022TransitionZone_Spring")
Taylor_Maker(ANPP_FORECAST_T, "T", "2022-06-14","2022-09-01","2022TransitionZone_Summer")

##### SUMMER ZONE
#2020
Taylor_Maker(ANPP_FORECAST_S, "S", "2020-05-15","2020-06-02","2020SummerZone_Spring")
Taylor_Maker(ANPP_FORECAST_S, "S", "2020-06-16","2020-09-01","2020SummerZone_Summer")

#2021
Taylor_Maker(ANPP_FORECAST_S, "S", "2021-04-14","2021-06-01","2021SummerZone_Spring")
Taylor_Maker(ANPP_FORECAST_S, "S", "2021-06-16","2021-08-24","2021SummerZone_Summer")

#2022
Taylor_Maker(ANPP_FORECAST_S, "S", "2022-04-05","2022-05-31","2022SummerZone_Spring")
Taylor_Maker(ANPP_FORECAST_S, "S", "2022-06-14","2022-09-01","2022SummerZone_Summer")

