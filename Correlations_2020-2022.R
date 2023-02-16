library(dbplyr) #pipes and more
library(janitor) #cleans col numbers and more
library(data.table) #for splits
library(lubridate) #to change the date
library(tidyverse)  #to load the core tidyverse and make it available in your current R session.
library(ggrepel) # ggrepel provides geoms for ggplot2 to repel overlapping text labels
library(zoo)
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

#########
##ALL Regions Correlations

df <- Forecast_Ratio %>% filter(Year == 2020) 
df <- df[df$Forecast >= as.Date("2020-01-01") & df$Forecast <= as.Date("2020-06-30"),]
df <- split(df, df$Forecast)


Correlations <- function(y,first,last){
  
  df <- Forecast_Ratio %>% filter(Year == y) 
  df <- df[df$Forecast >= as.Date(first) & df$Forecast <= as.Date(last),]
  df <- split(df, df$Forecast)

  ##Creates an empty list for the for loop
  r.list <- list()
  
  ##CORRELATION
  fc <- function(n, x) {
    
    # Interpolate missing values to make lengths equal
    df[[n]]$NPP_predict_above <- na.approx(df[[n]]$NPP_predict_above)
    df[[n]]$NPP_predict_avg <- na.approx(df[[n]]$NPP_predict_avg)
    df[[n]]$NPP_predict_below <- na.approx(df[[n]]$NPP_predict_below)
    df[[x]]$NPP_predict_above <- na.approx(df[[x]]$NPP_predict_above)
    df[[x]]$NPP_predict_avg <- na.approx(df[[x]]$NPP_predict_avg)
    df[[x]]$NPP_predict_below <- na.approx(df[[x]]$NPP_predict_below)
    
    dim_n <- dim(df[[n]]$NPP_predict_above)
    dim_x <- dim(df[[x]]$NPP_predict_above)
    cat("n =", n, ", x =", x, ", dim(df[[n]]$NPP_predict_above) =", dim_n, ", dim(df[[x]]$NPP_predict_above) =", dim_x, "\n")
    
    
    #N is the observation we are testing
    #x is the final observation
    a <- cor(df[[n]]$NPP_predict_above, df[[x]]$NPP_predict_above)
    b <- cor(df[[n]]$NPP_predict_avg, df[[x]]$NPP_predict_avg)
    c <- cor(df[[n]]$NPP_predict_below, df[[x]]$NPP_predict_below)
    
    #Creates a matrix 
    ANPP.r <- matrix(c(a,b,c),ncol = 3,byrow = TRUE)
    colnames(ANPP.r) <- c('Abv','Avg','Blw')
    ANPP.r <- as.data.frame(ANPP.r) 
    
    # Create a new column "forecast" and assign the date from y[x]

    
    return(ANPP.r)
  }
  
  
  for(i in 1:length(df)) {
    r.list[[i]] <- fc(i,length(df))
    
  }
  
  Corelations <- bind_rows(r.list)
  
  Corelations$Forecast <- names(df) #adds Forecast colum
  
  Corelations <- Corelations %>% pivot_longer(cols = c(Abv,Avg,Blw), names_to = "Time",values_to = "R_squre")
  
  colnames(Corelations) <- c('Date','Level','Rsqure')
  
  Corelations$Date <- as.Date(Corelations$Date)
  
  return(Corelations)
}

Correlations(2022,"2022-01-01","2022-06-30")

ggpl <- function(df, season, first,last, nudge_y, nudge_x){

  
  a <- ggplot(data = df, aes(Date, Rsqure, label = Date)) +
    geom_line(aes(y=Rsqure, group=Level, colour =Level), size=0.3) +
    geom_point(aes(y=Rsqure, group=Level, colour =Level), size=2) +

    geom_text(data=df %>% filter(Level == "Blw"), aes(label=format(Date, "%b/%d")),
              nudge_y=nudge_y, nudge_x=nudge_x) +
    ylim(min(df$Rsqure)-0.06,1.05) +
    theme_bw() +
    scale_x_date(date_breaks = "2 week", date_labels ="%b/%d") +
    scale_color_manual(values = c("33339","#CC6600","firebrick")) +
    labs (title = paste0("Coefficient of Determination (R²)"),
          subtitle = paste("Season:",season,"   First Forecast:",first,"   Last Forecast:",last),
          x = "Date of Forecast", 
          y= "R squer of linear model") +
    theme(
      axis.text.x = element_text(angle = 0, vjust =0, hjust=0),
      axis.title.x =element_blank(),
      axis.title.y =element_text(face = "italic",size = 8),
      legend.position="bottom",
      legend.title=element_blank())
  
  ggsave(
    paste0("data/Corr_",substr(first, 1, 4),"_",season,".png"),
    plot = last_plot(),
    device = NULL,
    path = NULL,
    scale = 1,
    width = NA,
    height = NA,
    dpi = 300,
    limitsize = TRUE,
    bg = "white",
  )
  
  return(a)
}


easy_ggpl <- function(year, monthdate1, monthdate2, season, nugde_x, nugbe_y)
{
  
  ggpl(Correlations(year,paste0(year,"-",monthdate1),paste0(year,"-",monthdate2)),
       season,paste0(year,"-",monthdate1),paste0(year,"-",monthdate2),nugde_x,nugbe_y)
  
  
}

easy_ggpl(2022,"01-01","05-30","Spring",-0.03,2)
easy_ggpl(2020,"07-01","12-31","Summer",-0.04,2)

#######
##SPLIT REGIONS
  
df_W <- subset(Forecast_Ratio, Forecast_Ratio$pptRatioSummerWinter < '0.8') %>% filter(Year == 2020) 
df_T <- Forecast_Ratio  %>%   filter(Forecast_Ratio$pptRatioSummerWinter >0.8 
                              &  Forecast_Ratio$pptRatioSummerWinter < 1.2) %>% filter(Year == 2020) 
df_S <- subset(Forecast_Ratio, Forecast_Ratio$pptRatioSummerWinter > '1.2') %>% filter(Year == 2020) 

  df_W <- df[df$Forecast >= as.Date("2020-01-01") & df$Forecast <= as.Date("2020-06-21"),]


  df_W <- split(df_W, df_W$Forecast) 
  df_T <- split(df_T, df_T$Forecast)
  df_S <- split(df_S, d_S$Forecast)

Correlations_Regions <- function(y,first,last){
  
  df_W <- subset(Forecast_Ratio, Forecast_Ratio$pptRatioSummerWinter < '0.8') %>% filter(Year == y) 
  df_T <- Forecast_Ratio  %>%   filter(Forecast_Ratio$pptRatioSummerWinter >0.8 
                                       &  Forecast_Ratio$pptRatioSummerWinter < 1.2) %>% filter(Year == y) 
  df_S <- subset(Forecast_Ratio, Forecast_Ratio$pptRatioSummerWinter > '1.2') %>% filter(Year == y) 
  
  
  df_W <- df_W[df_W$Forecast >= as.Date(first) & df_W$Forecast <= as.Date(last),]
  df_T <- df_T[df_T$Forecast >= as.Date(first) & df_T$Forecast <= as.Date(last),]
  df_S <- df_S[df_S$Forecast >= as.Date(first) & df_S$Forecast <= as.Date(last),]
  
  df_W <- split(df_W, df_W$Forecast) 
  df_T <- split(df_T, df_T$Forecast)
  df_S <- split(df_S, df_S$Forecast)

  ##Creates an empty list for the for loop
  r.list_W <- list()
  r.list_T <- list()
  r.list_S <- list()
  
  ##CORRELATION
  fc <- function(n, x, y) {
    
    #N is the observation we are testing
    #x is the final observation
    a <- cor(y[[n]]$NPP_predict_above, y[[x]]$NPP_predict_above)
    b <- cor(y[[n]]$NPP_predict_avg, y[[x]]$NPP_predict_avg)
    c <- cor(y[[n]]$NPP_predict_below, y[[x]]$NPP_predict_below)
    
    #Creates a matrix 
    ANPP.r <- matrix(c(a,b,c),ncol = 3,byrow = TRUE)
    colnames(ANPP.r) <- c('Abv','Avg','Blw')
    ANPP.r <- as.data.frame(ANPP.r) 
    
    # Create a new column "forecast" and assign the date from y[x]
    
    return(ANPP.r)
  }
  
  
  for(i in 1:length(df)) {
    r.list_W[[i]] <- fc(i,length(df),df_W)
    r.list_T[[i]] <- fc(i,length(df),df_T)
    r.list_S[[i]] <- fc(i,length(df),df_S)
    
  }
  
  print(r.list_W)
}
  #Append results
  Corr_W <- bind_rows(r.list_W)
  Corr_T <- bind_rows(r.list_T)
  Corr_S <- bind_rows(r.list_S)
  
  #adds Forecast colum
  Corr_W <- names(df) 
  Corr_T <- names(df)
  Corr_S <- names(df)
  
  #pivot longer
  Corr_W <- Corr_W %>% pivot_longer(cols = c(Abv,Avg,Blw), names_to = "Time",values_to = "R_squre")
  Corr_T <- Corr_T %>% pivot_longer(cols = c(Abv,Avg,Blw), names_to = "Time",values_to = "R_squre")
  Corr_S <- Corr_S %>% pivot_longer(cols = c(Abv,Avg,Blw), names_to = "Time",values_to = "R_squre")
  
  #Colnames
  colnames(Corr_W) <- c('Date','Level','Rsqure')
  colnames(Corr_T) <- c('Date','Level','Rsqure')
  colnames(Corr_S) <- c('Date','Level','Rsqure')
  
  return(Corr_W)

  #######

Correlations_Regions(2021,"2021-01-01","2021-06-21")
