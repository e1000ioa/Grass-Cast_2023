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

set.seed(108)

##########
#Load and Select Data
#COPY FROM CORREALTIONS_2020-2022
#Grind ID 
GC <- read.csv(file = "data/az_nm_2000_2020.csv", head = TRUE, sep=",")

#Forecast Data
gfg_data_22 <- read.csv(file = "data/grass_cast_2022.csv", head = TRUE, sep=",") %>% 
  subset(select=c("gridID","Year","Forecast","NPP_predict_below","NPP_predict_avg","NPP_predict_above","Order"))

gfg_data_21_20 <- read.csv(file = "data/grass_cast_20-21.csv", head = TRUE, sep=",") %>% 
  subset(select=c("gridID","Year","Forecast","NPP_predict_below","NPP_predict_avg","NPP_predict_above","Order"))

forcasts <- rbind(gfg_data_22,gfg_data_21_20) 

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


#Spiting data in regions
ANPP_FORECAST_W <- subset(Forecast_Ratio, pptRatioSummerWinter < '0.8')
ANPP_FORECAST_T <- Forecast_Ratio  %>%   filter(pptRatioSummerWinter >0.8 &  pptRatioSummerWinter < 1.2)
ANPP_FORECAST_S <- subset(Forecast_Ratio, pptRatioSummerWinter > '1.2')

##############################################################################
########Stats Table to check Taylor Plots

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
    r2 <- summary(mod)$r.squared
    bias <- mean(df_n[[scenario]] - df_x[[scenario]], na.rm = TRUE)
    rmse <- sqrt(mean((df_n[[scenario]] - df_x[[scenario]])^2))
    std_error <- summary(mod)$coefficients[2, 2]
    p_value <- format.pval(summary(mod)$coefficients[2, 4])
    
    ANPP.r <- data.frame(
      MEAN = mean,
      SD = sd,
      COUNT = count,
      SLOPE = slope,
      INTERCEPT = intercept,
      R2 = r2,
      STD_ERROR = std_error,
      BIAS = bias,
      RMSE = rmse,
      P_VALUE = p_value
    )
    
    ANPP.r$Forecast <- names(df)[n]
    
    ANPP.r <- ANPP.r[, c("Forecast", "MEAN", "SD", "COUNT", "SLOPE", "INTERCEPT", "R2", "STD_ERROR", "BIAS", "RMSE", "P_VALUE")]
    
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


##############################################################################
#Select data to add to the Taylor Diagram

#All regions as one
df <- Forecast_Ratio
df <- split(df, df$Forecast)

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

#############
################## TAYLOR PLOTS

Taylor <- function(n, x, colors, main){
 
   #Where:
      #n = date start
      #x = date end
      #colors = color of dot
      #main = Tittle

  #Creates new dataframe, by filtering the dates of the 
  dfz <- orderModels[orderModels$name >= as.Date(n) & orderModels$name <= as.Date(x), ]
  if (nrow(dfz) == 0) {
    stop("No data available for the specified date range.")
  }
  
  #Creates the null hypothesis using  the last data on the list
  ref <- as.numeric(models[[tail(dfz$order, n=1)]])
  
  #Creates first diagram
  taylor.diagram(ref,as.numeric(modelsA[[dfz$order[1]]]), col=colors,pch=1, cex=1.2, pcex = 2.5,
                         main = main, xlab="Standart Deviation (lb/acre)",
                         pos.cor=TRUE, show.gamma = T, sd.arcs = T)

  for (i in 2:(nrow(dfz) + 1)) {
    if (i <= (length(colors) + 1)) {
      if (i <= length(colors)) {
        taylor.diagram(ref, as.numeric(modelsA[[dfz$order[i - 1]]]), add = TRUE, col = colors[i], pch = i + 13, cex = 1.2,
                       pcex = 2.5, main = main)
      } else {
        taylor.diagram(ref, as.numeric(modelsC[[dfz$order[i - 1]]]), add = TRUE, pch = i + 13, cex = 1.2,
                       pcex = 2.5, main = main)
      }
    } else {
      stop("Insufficient colors provided for the number of models.")
    }
  }
}
n <- "2020-06-01"
x <- "2020-12-31"

Legends <- function(n,x){

#Creates df outside function to use in naming legeds
dfz <- orderModels[orderModels$name >= as.Date(n) & orderModels$name <= as.Date(x),]
frst <- dfz$order[1] #Selects the first value of the list
lst <- tail(dfz$order, n=1) # Selects the Last value on the list


return(dfz)
}


##########################

#####
modelsA <- model_select(df,5)
modelsB <- model_select(df,4)
modelsC <- model_select(df,6)
colors <- c("blue", "red", "green", "purple") 
#Draw the Maps
#Spring 2020
png(file="images/ANPP_FORECAST_ALL_SP2020.png",
    width=1000, height=1000, pointsize = 25)
Sp20n <-"2020-01-01"
Sp20x <- "2020-06-03"
Taylor(Sp20n,Sp20x,colors,"Spring 2020")
#add RMSE label
text(x=95,
     y=58,
     pos=1, srt = 15, #str is the angle
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp20n,Sp20x)
legend(155,160,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col="#DF5327")
dev.off()

#Spring 2021
png(file="images/ANPP_FORECAST_ALL_SP2021.png",
    width=1000, height=1000, pointsize = 25)
Sp21n <-"2021-04-14"
Sp21x <- "2021-06-01"
c <- "#418AB3"
Taylor(Sp21n,Sp21x,colors,"Spring 2021")
text(x=110,
     y=100,
     pos=1, srt = 20,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp21n,Sp21x)
legend(200,240,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17),col=c)
dev.off()

#Spring 2022
png(file="images/ANPP_FORECAST_ALL_SP2022.png",
    width=1000, height=1000, pointsize = 25)
Sp22n <-"2022-04-05"
Sp22x <- "2022-05-31"
c <- "#838383"
Taylor(Sp22n,Sp22x,c, "Spring 2022")
text(x=90,
     y=58,
     pos=1, srt = 16,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp22n,Sp22x)
legend(190,200,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#####SUMMMER
#Summer 2020
png(file="images/ANPP_FORECAST_ALL_SU2020.png",
    width=1000, height=1000, pointsize = 25)
Su20n <-"2020-06-16"
Su20x <- "2020-09-01"
c <- "#DF5327"
Taylor(Su20n,Su20x,c, "Summer 2020")
text(x=220,
     y=216,
     pos=1, srt = 11,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su20n,Su20x)
legend(400,400,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#Summer 2021
png(file="images/ANPP_FORECAST_ALL_SU2021.png",
    width=1000, height=1000, pointsize = 25)
Su21n <-"2021-06-16"
Su21x <- "2021-08-24"
c <- "#418AB3"
Taylor(Su21n,Su21x,c,"Summer 2021")
text(x=250,
     y=400,
     pos=1, srt = 25,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su21n,Su21x)
legend(600,600,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#Summer 2022
png(file="images/ANPP_FORECAST_ALL_SU2022.png",
    width=1000, height=1000, pointsize = 25)
Su22n <-"2022-06-14"
Su22x <- "2022-12-31"
c <- "#838383"
Taylor(Su22n,Su22x,c,"Summer 2022")
text(x=300,
     y=210,
     pos=1, srt = 20,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su22n,Su22x)
legend(550,550,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

######
############DRAW BY ZONE
#####
##WINTER
ANPP_FORECAST_W <- split(ANPP_FORECAST_W, ANPP_FORECAST_W$Forecast)
models <- model_select(ANPP_FORECAST_W)
#Draw the Maps
#Spring 2020
png(file="images/ANPP_FORECAST_W_SP2020_3.png",
    width=1000, height=1000, pointsize = 25)
Taylor(Sp20n,Sp20x,"#DF5327","Winter Zone - Spring 2020")
text(x=85,
    y=55,
    pos=1, srt = 15,
    label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp20n, Sp20x)
legend(140,130,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col="#DF5327")
dev.off()

#Spring 2021
png(file="images/ANPP_FORECAST_W_SP2021_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#418AB3"
Taylor(Sp21n,Sp21x,c,"Winter Zone - Spring 2021")
text(x=90,
     y=50,
     pos=1, srt = 25,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp21n,Sp21x)
legend(155, 160, legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c) 
dev.off()

#Spring 2022
png(file="images/ANPP_FORECAST_W_SP2022_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#838383"
Taylor(Sp22n,Sp22x,c,"Winter Zone - Spring 2022")
text(x=45,
     y=45,
     pos=1, srt = 33,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp22n,Sp22x)
legend(110,120,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#####
#####SUMMMER
#Summer 2020
png(file="images/ANPP_FORECAST_W_SU2020_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#DF5327"
Taylor(Su20n,Su20x,c,"Winter Zone - Summer 2020")
text(x=100,
     y=105,
     pos=1, srt = 15,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su20n,Su20x)
legend(200,200,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#Summer 2021
png(file="images/ANPP_FORECAST_W_SU2021_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#418AB3"
Taylor(Su21n,Su21x,c,"Winter Zone - Summer 2021")
text(x=100,
     y=90,
     pos=1, srt = 30,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su21n,Su21x)
legend(250,250,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#Summer 2022
png(file="images/ANPP_FORECAST_W_SU2022_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#838383"
Taylor(Su22n,Su22x,c,"Winter Zone - Summer 2022")
text(x=100,
     y=80,
     pos=1, srt = 40,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su22n,Su22x)
legend(240,250,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()


#####
##SRANSITIONS
models <- model_select(ANPP_FORECAST_T)
#Draw the Maps
#####
#Spring 2020
png(file="images/ANPP_FORECAST_T_SP2020_3.png",
    width=1000, height=1000, pointsize = 25)
Taylor(Sp20n,Sp20x,"#DF5327", "Transition Zone - Spring 2020")
text(x=85,
     y=55,
     pos=1, srt = 15,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp20n,Sp20x)
legend(140,130,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col="#DF5327") +
  dev.off()

#Spring 2021
png(file="images/ANPP_FORECAST_T_SP2021_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#418AB3"
Taylor(Sp21n,Sp21x,c, "Transition Zone - Spring 2021")
text(x=90,
     y=50,
     pos=1, srt = 25,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp21n,Sp21x)
legend(155,160,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#Spring 2022
png(file="images/ANPP_FORECAST_T_SP2022_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#838383"
Taylor(Sp22n,Sp22x,c, "Transition Zone - Spring 2022")
text(x=45,
     y=45,
     pos=1, srt = 33,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp22n,Sp22x)
legend(110,120,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

##### 
##### WINTER ZONE - SUMMMER
#Summer 2020
png(file="images/ANPP_FORECAST_T_SU2020_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#DF5327"
Taylor(Su20n,Su20x,c, "Transition Zone - Summer 2020")
text(x=100,
     y=105,
     pos=1, srt = 15,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su20n,Su20x)
legend(200,200,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#Summer 2021
png(file="images/ANPP_FORECAST_T_SU2021_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#418AB3"
Taylor(Su21n,Su21x,c, "Transition Zone - Summer 2021")
text(x=100,
     y=90,
     pos=1, srt = 30,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su21n,Su21x)
legend(230,250,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#Summer 2022
png(file="images/ANPP_FORECAST_T_SU2022_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#838383"
Taylor(Su22n,Su22x,c, "Transition Zone - Summer 2022")
text(x=100,
     y=80,
     pos=1, srt = 40,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su22n,Su22x)
legend(240,250,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#####
##SUMMER
models <- model_select(ANPP_FORECAST_S)
#Draw the Maps
#Spring 2020
png(file="images/ANPP_FORECAST_S_SP2020_3.png",
    width=1000, height=1000, pointsize = 25)
Taylor(Sp20n,Sp20x,"#DF5327", "Summer Zone - Spring 2020")
text(x=85,
     y=55,
     pos=1, srt = 15,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp20n,Sp20x)
legend(155,180,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col="#DF5327") +
  dev.off()

#Spring 2021
png(file="images/ANPP_FORECAST_S_SP2021_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#418AB3"
Taylor(Sp21n,Sp21x,c, "Summer Zone - Spring 2021")
text(x=110,
     y=102,
     pos=1, srt = 20,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp21n,Sp21x)
legend(200,200, legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c) +
  dev.off()

#Spring 2022
png(file="images/ANPP_FORECAST_S_SP2022_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#838383"
Taylor(Sp22n,Sp22x,c, "Summer Zone - Spring 2022")
text(x=75,
     y=55,
     pos=1, srt = 20,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Sp22n,Sp22x)
legend(155,170,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#####SUMMMER
#Summer 2020
png(file="images/ANPP_FORECAST_S_SU2020_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#DF5327"
Taylor(Su20n,Su20x ,c, "Summer Zone - Summer 2020")
text(x=180,
     y=200,
     pos=1, srt = 20,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su20n,Su20x)
legend(360,400,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()


#Summer 2021
png(file="images/ANPP_FORECAST_S_SU2021_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#418AB3"
Taylor(Su21n,Su21x,c, "Summer Zone - Summer 2021")
text(x=350,
     y=205,
     pos=1, srt = 20,
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su21n,Su21x)
legend(600,600,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()

#Summer 2022
png(file="images/ANPP_FORECAST_S_SU2022_3.png",
    width=1000, height=1000, pointsize = 25)
c <- "#838383"
Taylor(Su22n,Su22x,c, "Summer Zone - Summer 2022")
text(x=200,
     y=190,
     pos=1, srt = 15, 
     label="RMSE (lb/acre)",font=3,cex=0.8)
dfz <- Legends(Su22n,Su22x)
legend(420,450,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)
dev.off()
