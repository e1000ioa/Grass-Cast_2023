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

forcasts <- rbind(gfg_data_22,gfg_data_21_20) 

#Forecast 2024 # From File_Process_2024

forecast <- combined_df %>% 
  subset(select=c("gridID","Year","Forecast","deltaNPP_below","deltaNPP_avg","deltaNPP_above"))

##Parse the dates into a consistent format
parsed_dates <- parse_date_time(forcasts$Forecast, orders = c("mdy")) # specifying the possible formats using the orders argument
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

#####
#Make the lengths of the list match
#Find the unique id's bettewn the unmatched len list
df <- Forecast_Ratio
df <- split(df, df$Forecast)
SameID <- intersect(df[["2022-04-19"]]$gridID,df[["2022-05-03"]]$gridID)

#Go back to DF and add indicator with SameID info
df <- Forecast_Ratio
df$Indicator <- 1*(df$gridID %in% SameID)
df <- split(df, df$Forecast) #go back to list

#Creates empty list
F.list <- list()

#For loops the values that have mismatched dim
for(i in 19:23) {
  F.list[[i]] <- subset(df[[i]], df[[i]]$Indicator!="0")
  
}

#Changes the content of the list
dfx <- df #Just to be safe
dfx[[19]] <- F.list[[19]]
dfx[[20]] <- F.list[[20]]
dfx[[21]] <- F.list[[21]]
dfx[[22]] <- F.list[[22]]
dfx[[23]] <- F.list[[23]]

#Know the Models names and order
orderModels <- data.frame(order=order(names(dfx)),
                          name=names(dfx))

Forecast_Ratio <- do.call(rbind, dfx) #Finish de DF back to begigin
rownames(Forecast_Ratio) <- NULL #Eliminates row names
df <- split(Forecast_Ratio, Forecast_Ratio$Forecast)

write.csv(Forecast_Ratio, "data/ANPP_2020-2022.csv")
#Spiting data in regions
ANPP_FORECAST_W <- subset(Forecast_Ratio, pptRatioSummerWinter < '0.8')
ANPP_FORECAST_T <- Forecast_Ratio  %>%   filter(pptRatioSummerWinter >0.8 &  pptRatioSummerWinter < 1.2)
ANPP_FORECAST_S <- subset(Forecast_Ratio, pptRatioSummerWinter > '1.2')

#######Stats Table to check Taylor Plots #######################################################################


Stats <- function(data, n, x, scenario) {
  df <- data[data$Forecast >= as.Date(n) & data$Forecast <= as.Date(x), ]
  df <- split(df, df$Forecast)
  
  fc <- function(n, x, scenario) {
    df_n <- df[[n]]
    df_x <- df[[x]]
    
    mean <- mean(df_n[[scenario]])
    sd <- sd(df_n[[scenario]], na.rm = TRUE)
    count <- length(df_n[[scenario]])
    
    mod <- lm(df_n[[scenario]] ~ df_x[[scenario]])
    cf <- coef(mod)
    slope <- cf[2]
    intercept <- cf[1]
    correlation <- cor(df_n[[scenario]], df_x[[scenario]])
    r2 <- summary(mod)$r.squared
    bias <- mean(df_n[[scenario]] - df_x[[scenario]], na.rm = TRUE)
    rmse <- sqrt(mean((df_n[[scenario]] - df_x[[scenario]])^2))
    std_error <- summary(mod)$coefficients[2, 2]
    p_value <- pvalue(summary(mod)$coefficients[2, 4], accuracy = 0.05, # Number to round to
                   decimal.mark = ".", # The character to be used to indicate the numeric decimal point
                   add_p = TRUE ) # Add "p=" before the value?
    
    ANPP.r <- data.frame(
      MEAN = mean,
      SD = sd,
      COUNT = count,
      SLOPE = slope,
      INTERCEPT = intercept,
      COR = correlation,
      R2 = r2,
      STD_ERROR = std_error,
      BIAS = bias,
      RMSE = rmse,
      P_VALUE = p_value
    )
    
    ANPP.r$Forecast <- names(df)[n]
    
    ANPP.r <- ANPP.r[, c("Forecast", "MEAN", "SD", "COUNT", "SLOPE", "INTERCEPT", "COR", "R2", "STD_ERROR", "BIAS", "RMSE", "P_VALUE")]
    
    return(ANPP.r)
  }
  
  r.list2 <- lapply(seq_along(df), fc, x = length(df), scenario = scenario)
  Stats <- do.call(rbind, r.list2)
  
  # Format numeric columns to display 3 decimal places
  numeric_columns <- names(Stats)[sapply(Stats, is.numeric)]
  Stats[numeric_columns] <- lapply(Stats[numeric_columns], function(x) sprintf("%.3f", x))
  
  return(Stats)
}

##Calculates Stats for each Season and Scenario

getSpringStats <- function(scenario,nom) {
  n2020 <- Stats(Forecast_Ratio, "2020-05-15","2020-06-02", scenario)
  n2021 <- Stats(Forecast_Ratio, "2021-04-14", "2021-06-01", scenario)
  n2022 <- Stats(Forecast_Ratio, "2022-04-05", "2022-05-31", scenario)
  
  Spring <- rbind(n2020, n2021, n2022)
  Spring <- data.frame(append(Spring, c(Scenario = nom), after = 1))
  
  return(Spring)
}

SpringB <- getSpringStats(4,"Blw")
SpringA <- getSpringStats(5,"Avg")
SpringC <- getSpringStats(6,"Abv")

Spring <- rbind(SpringB, SpringA, SpringC)

#Summer Stats
getSummerStats <- function(scenario,nom) {
  n2020 <- Stats(Forecast_Ratio, "2020-06-16","2020-09-01", scenario)
  n2021 <- Stats(Forecast_Ratio, "2021-06-16","2021-08-24", scenario)
  n2022 <- Stats(Forecast_Ratio, "2022-06-14","2022-09-01", scenario)
  
  Summer <- rbind(n2020, n2021, n2022)
  Summer <- data.frame(append(Summer, c(Scenario = nom), after = 1))
  
  return(Summer)
}

SummerB <- getSummerStats(4,"Blw")
SummerA <- getSummerStats(5,"Avg")
SummerC <- getSummerStats(6,"Abv")

Summer <- rbind(SummerB, SummerA, SummerC)

#Creates one dataframe

custom_order <- c("Blw", "Avg", "Abv")

Southwest_Stats <- list(Summer = Summer, Spring = Spring) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "RowNames") %>%
  mutate(Season = sub("\\..*$", "", RowNames)) %>%
  select(-RowNames) %>% 
  select(Season, everything()) %>% 
  mutate(Scenario = factor(Scenario, levels = custom_order)) %>%
  arrange(Season, Forecast, Scenario)

write.csv(Southwest_Stats, "data/Southwest_Stats2.csv")

####### STATS WINTER REGION ALL SCNEARIOS (SR = Winter Region)

#Sring_SR
getSpringSRStats <- function(scenario,nom) {
  n2020 <- Stats(ANPP_FORECAST_W, "2020-05-15","2020-06-02", scenario)
  n2021 <- Stats(ANPP_FORECAST_W, "2021-04-14", "2021-06-01", scenario)
  n2022 <- Stats(ANPP_FORECAST_W, "2022-04-05", "2022-05-31", scenario)
  
  Spring <- rbind(n2020, n2021, n2022)
  Spring <- data.frame(append(Spring, c(Scenario = nom), after = 1))
  
  return(Spring)
}

Spring_SR <- getSpringSRStats(4,"Blw") 
Spring_SR <- rbind(Spring_SR, getSpringSRStats(5,"Avg"))
Spring_SR <- rbind(Spring_SR, getSpringSRStats(6,"Abv"))

###SUMMER 
getSummerSRStats <- function(scenario,nom) {
  n2020 <- Stats(ANPP_FORECAST_W, "2020-06-16","2020-09-01", scenario)
  n2021 <- Stats(ANPP_FORECAST_W, "2021-06-16","2021-08-24", scenario)
  n2022 <- Stats(ANPP_FORECAST_W, "2022-06-14","2022-09-01", scenario)
  
  Summer <- rbind(n2020, n2021, n2022)
  Summer <- data.frame(append(Summer, c(Scenario = nom), after = 1))
  
  return(Summer)
}

Summer_SR <- getSummerSRStats(4,"Blw") 
Summer_SR <- rbind(Summer_SR, getSummerSRStats(5,"Avg"))
Summer_SR <- rbind(Summer_SR, getSummerSRStats(6,"Abv"))

#Creates one dataframe

Winter_Region <- list(Spring = Spring_SR, Summer = Summer_SR) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "RowNames") %>%
  mutate(Season = sub("\\..*$", "", RowNames)) %>%
  select(-RowNames)  %>% 
  select(Season, everything()) %>% 
  arrange(Season, Forecast, Scenario) 


####### STATS SRANSITION REGION
#SR = Transition Region
#Sring_SR
getSpringSRStats <- function(scenario,nom) {
  n2020 <- Stats(ANPP_FORECAST_T, "2020-05-15","2020-06-02", scenario)
  n2021 <- Stats(ANPP_FORECAST_T, "2021-04-14", "2021-06-01", scenario)
  n2022 <- Stats(ANPP_FORECAST_T, "2022-04-05", "2022-05-31", scenario)
  
  Spring <- rbind(n2020, n2021, n2022)
  Spring <- data.frame(append(Spring, c(Scenario = nom), after = 1))
  
  return(Spring)
}

Spring_SR <- getSpringSRStats(4,"Blw") 
Spring_SR <- rbind(Spring_SR, getSpringSRStats(5,"Avg"))
Spring_SR <- rbind(Spring_SR, getSpringSRStats(6,"Abv"))

###SUMMER 
getSummerSRStats <- function(scenario,nom) {
  n2020 <- Stats(ANPP_FORECAST_T, "2020-06-16","2020-09-01", scenario)
  n2021 <- Stats(ANPP_FORECAST_T, "2021-06-16","2021-08-24", scenario)
  n2022 <- Stats(ANPP_FORECAST_T, "2022-06-14","2022-09-01", scenario)
  
  Summer <- rbind(n2020, n2021, n2022)
  Summer <- data.frame(append(Summer, c(Scenario = nom), after = 1))
  
  return(Summer)
}

Summer_SR <- getSummerSRStats(4,"Blw") 
Summer_SR <- rbind(Summer_SR, getSummerSRStats(5,"Avg"))
Summer_SR <- rbind(Summer_SR, getSummerSRStats(6,"Abv"))


Transition_Region <- list(Spring = Spring_SR, Summer = Summer_SR) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "RowNames") %>%
  mutate(Season = sub("\\..*$", "", RowNames)) %>%
  select(-RowNames)  %>% 
  select(Season, everything()) %>% 
  arrange(Season, Forecast, Scenario) 


####### STATS SUMMER REGION
#Sring_SR
getSpringSRStats <- function(scenario,nom) {
  n2020 <- Stats(ANPP_FORECAST_S, "2020-05-15","2020-06-02", scenario)
  n2021 <- Stats(ANPP_FORECAST_S, "2021-04-14", "2021-06-01", scenario)
  n2022 <- Stats(ANPP_FORECAST_S, "2022-04-05", "2022-05-31", scenario)
  
  Spring <- rbind(n2020, n2021, n2022)
  Spring <- data.frame(append(Spring, c(Scenario = nom), after = 1))
  
  return(Spring)
}

Spring_SR <- getSpringSRStats(4,"Blw") 
Spring_SR <- rbind(Spring_SR, getSpringSRStats(5,"Avg"))
Spring_SR <- rbind(Spring_SR, getSpringSRStats(6,"Abv"))

###SUMMER 
getSummerSRStats <- function(scenario,nom) {
  n2020 <- Stats(ANPP_FORECAST_S, "2020-06-16","2020-09-01", scenario)
  n2021 <- Stats(ANPP_FORECAST_S, "2021-06-16","2021-08-24", scenario)
  n2022 <- Stats(ANPP_FORECAST_S, "2022-06-14","2022-09-01", scenario)
  
  Summer <- rbind(n2020, n2021, n2022)
  Summer <- data.frame(append(Summer, c(Scenario = nom), after = 1))
  
  return(Summer)
}

Summer_SR <- getSummerSRStats(4,"Blw") 
Summer_SR <- rbind(Summer_SR, getSummerSRStats(5,"Avg"))
Summer_SR <- rbind(Summer_SR, getSummerSRStats(6,"Abv"))


Summer_Region <- list(Spring = Spring_SR, Summer = Summer_SR) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "RowNames") %>%
  mutate(Season = sub("\\..*$", "", RowNames)) %>%
  select(-RowNames)  %>% 
  select(Season, everything()) %>% 
  arrange(Season, Forecast, Scenario) 


Regions <- list(Winter_Region = Winter_Region, Transition_Region = Transition_Region, Summer_Region = Summer_Region) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "RowNames") %>%
  mutate(Region = sub("\\..*$", "", RowNames)) %>%
  select(-RowNames) %>% 
  select(Region, everything()) %>% 
  mutate(Scenario = factor(Scenario, levels = custom_order)) %>%
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

