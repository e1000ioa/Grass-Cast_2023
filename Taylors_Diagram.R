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
df <- split(Forecast_Ratio, Forecast_Ratio$Forecast)


#Spiting data in regions
ANPP_FORECAST_W <- subset(Forecast_Ratio, pptRatioSummerWinter < '0.8')
ANPP_FORECAST_T <- Forecast_Ratio  %>%   filter(pptRatioSummerWinter >0.8 &  pptRatioSummerWinter < 1.2)
ANPP_FORECAST_S <- subset(Forecast_Ratio, pptRatioSummerWinter > '1.2')


#Test the functions
n <- "2021-06-29"
x <- "2021-06-01"
df <- ANPP_FORECAST_W
df <- df[df$Forecast >= as.Date(n) & df$Forecast <= as.Date(x),]
df <- split(df, df$Forecast)
mod <- lm(df[[n]]$NPP_predict_avg ~ df[[x]]$NPP_predict_avg)
cf <- coef(mod)
slope <- cf[2]
std_error <- summary(mod)$coefficients[2, 2]
intercept <- cf[1]
r2 <- summary(mod)$r.squared
bias <- mean(df[[n]]$NPP_predict_avg - df[[x]]$NPP_predict_avg, na.rm = TRUE)
rmse <- rmse(df[[n]]$NPP_predict_avg, df[[x]]$NPP_predict_avg)
p_value <- format(summary(mod)$coefficients[2, 4],scientific = TRUE, digits = 2) # extract p-value

summary(mod)

#########
##ALL Regions Stats

Stats <- function(data,n,x){
  
  #Select only data from the preset dates
    #N is the observation we are testing
    #x is the final observation
  df <- data
  df <- df[df$Forecast >= as.Date(n) & df$Forecast <= as.Date(x),]
  df <- split(df, df$Forecast)
  
  #Starts internal function to calculate stats
  fc <- function(n, x) {
    
    #Calculate Descriptive Stats
      mean <- mean(df[[n]]$NPP_predict_avg)
      sd <- sd(df[[n]]$NPP_predict_avg, na.rm = TRUE)
      count <- length(as.numeric(df[[n]]$NPP_predict_avg))
      
      
    #Calculate model coefficients
        mod <- lm(df[[n]]$NPP_predict_avg ~ df[[x]]$NPP_predict_avg)
        cf <- coef(mod)
          slope <- cf[2]
          intercept <- cf[1]
          r2 <- summary(mod)$r.squared
          bias <- mean(df[[n]]$NPP_predict_avg - df[[x]]$NPP_predict_avg, na.rm = TRUE)
          rmse <- rmse(df[[n]]$NPP_predict_avg, df[[x]]$NPP_predict_avg)
          std_error <- summary(mod)$coefficients[2, 2]
          p_value <- summary(mod)$coefficients[2, 4]
          
    #Creates a matrix 
      ANPP.r <- matrix(c(mean,sd,count,slope,intercept,r2,std_error,bias,rmse,p_value), ncol = 10, byrow = TRUE)
      colnames(ANPP.r) <- c("MEAN","SD","COUNT","SLOPE","INTERCEPT","R2","STD_ERROR","BIAS","RMSE","P_VALUE")
      ANPP.r <- as.data.frame(ANPP.r) 
    
    # Create a new column "forecast" and "Compare" and assign the date from y[x]
      ANPP.r$Forecast <- names(df)[n]
      #ANPP.r$FinalForecast <- rep(names(df), each = nrow(df[[x]])))
      
      
      # Reorder the columns
      ANPP.r <- select(ANPP.r, Forecast,MEAN,SD,COUNT,SLOPE,INTERCEPT,R2,STD_ERROR,BIAS,RMSE,P_VALUE)
    
    
    return(ANPP.r)
  }
  
  
  r.list2 <- list()
  
  for(i in 1:length(df)) {
    r.list2[[i]] <- fc(i,length(df))
    
    #Order Columns
    
  }
  
  Stats <- bind_rows(r.list2)
  
}


####### STATS ALL THE REGIONS
#Spring
sp1 <- Stats(Forecast_Ratio,"2020-05-15","2020-06-02")
sp2 <- Stats(Forecast_Ratio,"2021-04-14","2021-06-01")
sp3 <- Stats(Forecast_Ratio,"2022-04-05","2022-05-31")

Spring <- rbind(sp1, sp2, sp3)

#Summer
su1 <- Stats(Forecast_Ratio,"2020-06-16","2020-09-01")
su2 <- Stats(Forecast_Ratio,"2021-06-16","2021-08-24")
su3 <- Stats(Forecast_Ratio,"2022-06-14","2022-09-01")

Summer <- rbind(su1, su2, su3)

####### STATS WINTER REGION
#WR = Winter Region
#Spring
wsp1 <- Stats(ANPP_FORECAST_W,"2020-05-15","2020-06-02")
wsp2 <- Stats(ANPP_FORECAST_W,"2021-04-14","2021-06-01")
wsp3 <- Stats(ANPP_FORECAST_W,"2022-04-05","2022-05-31")

WR_Spring <- rbind(sp1, sp2, sp3)

#Summer
wsu1 <- Stats(ANPP_FORECAST_W,"2020-06-16","2020-09-01")
wsu2 <- Stats(ANPP_FORECAST_W,"2021-06-16","2021-08-24")
wsu3 <- Stats(ANPP_FORECAST_W,"2022-06-14","2022-09-01")

WR_Summer <- rbind(su1, su2, su3)

####### STATS TRANSITION REGION
#TR = Transition Region
tsp1 <- Stats(Forecast_Ratio,"2020-05-15","2020-06-02")
tsp2 <- Stats(Forecast_Ratio,"2021-04-14","2021-06-01")
tsp3 <- Stats(Forecast_Ratio,"2022-04-05","2022-05-31")

TR_Spring <- rbind(sp1, sp2, sp3)

#Summer
tsu1 <- Stats(Forecast_Ratio,"2020-06-16","2020-09-01")
tsu2 <- Stats(Forecast_Ratio,"2021-06-16","2021-08-24")
tsu3 <- Stats(Forecast_Ratio,"2022-06-14","2022-09-01")

TR_Summer <- rbind(su1, su2, su3)


####### STATS SUMMER REGION
ssp1 <- Stats(Forecast_Ratio,"2020-05-15","2020-06-02")
ssp2 <- Stats(Forecast_Ratio,"2021-04-14","2021-06-01")
ssp3 <- Stats(Forecast_Ratio,"2022-04-05","2022-05-31")

SR_Spring <- rbind(sp1, sp2, sp3)

#Summer
ssu1 <- Stats(Forecast_Ratio,"2020-06-16","2020-09-01")
ssu2 <- Stats(Forecast_Ratio,"2021-06-16","2021-08-24")
ssu3 <- Stats(Forecast_Ratio,"2022-06-14","2022-09-01")

SR_Summer <- rbind(su1, su2, su3)

#####
#Select data to add to the taylor diagram

#All regions as one
df <- Forecast_Ratio
df <- split(df, df$Forecast)


model_select <- function(df){
#Where:
  #df = Data frame to draw

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



return(models)
}

#############
################## TAYLOR PLOTS

Taylor <- function(n, x, c, main){
 
   #Where:
      #n = date start
      #x = date end
      #c = color of dot

  #Creates new dataframe 
  dfz <- orderModels[orderModels$name >= as.Date(n) & orderModels$name <= as.Date(x),]
  frst <- dfz$order[1] #Selects the first value of the list
  lst <- tail(dfz$order, n=1) # Selects the Last value on the list
  
  #Creates the point of refences, the last observation all the values are on Models
  ref <- as.numeric(models[[tail(dfz$order, n=1)]])
  
  #Creates first diagram
  taylor.diagram(ref,as.numeric(models[[dfz$order[1]]]), col=c,pch=1, cex=1.2, pcex = 2.5,
                         main = main,
                         xlab="Standart Deviation (lb/acre)",
                         pos.cor=TRUE,
                         show.gamma = T,
                         sd.arcs = T)

  
  #Adds diagrams
  for (i in 2:length(dfz$order)){
    taylor.diagram(ref,as.numeric(models[[dfz$order[i]]]), add=TRUE,col=c,pch=i+13, cex=1.2,
                   pcex = 2.5,main = main) 
    #Cex = plotting text and symbols should be scaled relative to the default
    #pcex = point expansion for the plotted points.
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

#####
models <- model_select(df)
#Draw the Maps
#Spring 2020
png(file="images/ANPP_FORECAST_ALL_SP2020.png",
    width=1000, height=1000, pointsize = 25)
Sp20n <-"2020-01-01"
Sp20x <- "2020-06-03"
Taylor(Sp20n,Sp20x,"#DF5327","Spring 2020")
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
Taylor(Sp21n,Sp21x,c,"Spring 2021")
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
##TRANSITIONS
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
