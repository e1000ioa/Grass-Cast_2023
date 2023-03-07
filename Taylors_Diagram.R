# Data Tools
library(ggplot2) #ggplot2 is a system for declaratively creating graphics, based on The Grammar of Graphics.
library(dplyr) #dplyr is a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges
library(tidyr) #clean messy data
library(ggthemes) #Extra Themes, Scales and Geoms for 'ggplot2'
library(plotrix) #Extra of plots, various labeling, axis and color scaling functions.
library(lubridate) #Options to works with date formats
library(zoo) #Infrastructure for Regular and Irregular Time Series

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
#Make the lenghts of the list match
#Find the unique id's bettewn the unmacthed len list
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


#########
##ALL Regions Stats

Stats <- function(n,x){
  
  #N is the observation we are testing
  #x is the final observation
  df <- Forecast_Ratio
  df <- df[df$Forecast >= as.Date(n) & df$Forecast <= as.Date(x),]
  df <- split(df, df$Forecast)
  
  #Check the lenght of variables in 3 years
  # Creates an empty list for the for loop
  r.lenght <- list()
  fc <- function(n, x) {
    
    # Interpolate missing values to make lengths equal
    df[[n]]$NPP_predict_avg <- na.approx(df[[n]]$NPP_predict_avg)
    
    #Calculate R2, standard deviation and bias
    mean1 <-mean(df[[n]]$NPP_predict_avg)
    mean2 <- mean(df[[x]]$NPP_predict_avg)
    sd <- sd(df[[n]]$NPP_predict_avg - df[[x]]$NPP_predict_avg, na.rm = TRUE)
    r2 <- cor(df[[n]]$NPP_predict_avg, df[[x]]$NPP_predict_avg)
    bias <- mean(df[[n]]$NPP_predict_avg - df[[x]]$NPP_predict_avg, na.rm = TRUE)
    
    #Creates a matrix 
    ANPP.r <- matrix(c(mean1,mean2,sd,bias,r2), ncol = 5, byrow = TRUE)
    colnames(ANPP.r) <- c("MEAN1","MEAN2",'SD','BIAS','R2')
    ANPP.r <- as.data.frame(ANPP.r) 
    
    # Create a new column "forecast" and assign the date from y[x]
    ANPP.r$Forecast <- names(df)[n]
    
    return(ANPP.r)
  }
  
  
  r.list2 <- list()
  
  for(i in 1:length(df)) {
    r.list2[[i]] <- fc(i,length(df))
    
  }
  
  Stats <- bind_rows(r.list2)
  
}

st <- Stats("2022-01-01","2022-05-31")

#####
#Try Taylor Diagram
df <- Forecast_Ratio
df <- split(df, df$Forecast)

# Define a vector of model names
model_names <- names(df)

# Initialize an empty list to hold the models
models <- list()

#Add the outputs of the models as a list
for(i in 1:length(df)) {
  model_name <- model_names[i]
  model <- df[[i]]$NPP_predict_avg
  models[[model_name]] <- model
}



Taylor <- function(n, x, c){
  
  #Creates new dataframe 
  dfz <- orderModels[orderModels$name >= as.Date(n) & orderModels$name <= as.Date(x),]
  frst <- dfz$order[1] #Selects the first value of the list
  lst <- tail(dfz$order, n=1) # Selects the Last value on the list
  
  #Creates the point of refences, the last observation
  ref <- models[[tail(dfz$order, n=1)]]
  
  #Creates first diagram
  oldpar<-taylor.diagram(ref,models[[dfz$order[1]]], col=c,pch=1)
  
  #Adds diagrams
  for (i in 2:tail(dfz$order, n=1)){
    taylor.diagram(ref,models[[dfz$order[i]]], add=TRUE,col=c,pch=i+13)
  }

  
}

n <- "2020-06-01"
x <- "2020-12-31"

Legends <- function(n,x){

#Creates df outside function to use in naming legeds
dfz <- orderModels[orderModels$name >= as.Date(n) & orderModels$name <= as.Date(x),]
frst <- dfz$order[1] #Selects the first value of the list
lst <- tail(dfz$order, n=1) # Selects the Last value on the list
dfz$name

return(dfz)
}

dfz
Taylor(n,x,c)
#####
#Draw the Maps
#Spring 2020
n <-"2020-01-01"
x <- "2020-06-30"
Taylor(n,x,"salmon")
dfz <- Legends(n,x)
legend(400,400,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col="salmon")

#Spring 2021
n <-"2021-01-01"
x <- "2021-06-30"
c <- "#826276"
Taylor(n,x,c)
dfz <- Legends(n,x)
legend(450,500,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)

#Spring 2022
n <-"2022-01-01"
x <- "2022-05-30"
c <- "plum"
Taylor(n,x,c)
dfz <- Legends(n,x)
legend(200,200,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)

#####SUMMMER
#Summer 2020
n <-"2020-06-01"
x <- "2020-12-31"
c <- "salmon"
Taylor(n,x,c)
dfz <- Legends(n,x)
legend(380,450,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)

#Summer 2021
n <-"2021-06-01"
x <- "2021-12-31"
c <- "#826276"
Taylor(n,x,c)
dfz <- Legends(n,x)
legend(600,750,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)

#Summer 2022
n <-"2022-06-01"
x <- "2022-12-31"
c <- "plum"
Taylor(n,x,c)
dfz <- Legends(n,x)
legend(540,750,legend=substr(dfz$name, 6, 10) ,pch=c(1,15,16,17,18,19,20),col=c)





