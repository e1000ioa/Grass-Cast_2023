library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggtext) #for shaowtext 

ENSOvsSpr1980_2020 <- read.csv("data/ENSOvsSpr1980_2020.csv")

#Rename colum to macht other DF
colnames(ENSOvsSpr1980_2020)[3] ="Year"

ENSOvsSU_1950_2020 <- read.csv("data/ENSOvsSU_1950_2020.csv")


4 <- "JMF"
5 <- "MAM"
6 <- "JJA"                    
9 <- "low_mean_percent_anom"
12 <- "med_mean_percent_anom"
15 <- "high_mean_percent_anom"


#FUNCTION
ENSO <- function(data,z,l,c,season, year){

  
data <- data %>% filter(Year >= year)
  
  
x <- unlist(data[z])
a <- colnames(data[z])

y <- unlist(data[l])
b <- colnames(data[l])

# Create a data frame with the data
df <- data.frame(x=x, y=y)

# Calculate R2 and p values
model <- lm(x ~ y)
rsq <- summary(model)$r.squared
pval <- summary(model)$coefficients[2,4]
cf <- coef(model)
slope <- cf[2]

# Create a scatterplot with linear model line
ggplot(df, aes(x = x, y = y, label=data$Year)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = c) +
  geom_text_repel(size=3,alpha = 0.3, nudge_y=-(max(y)/100))+
  labs(title = paste(season,"mean ANPP values vs ENSO Temperature"), 
       subtitle = paste0("Months: ", a," | ", "ANPP: ", b," | ","Years: ",year,"-2020")) + 
  xlab(paste(a,"(°C)")) +
  ylab(paste(b,"(lb/ac)")) +
  annotate("text", x = median(x), y = (max(y)/2), label = paste0("R2 = ", round(rsq, 2)),
           color = c, size = 8, fontface = "bold", alpha = 1, 
           hjust = 0)+
  annotate("text", x = median(x), y = (max(y)/3), label = paste0("P value = ", format.pval(pval))
           ,color = c, size = 8, fontface = "bold", alpha = 1, 
           hjust = 0)+
  annotate("text", x = median(x), y = (max(y)/6), label = paste0("Slope = ", format(round(slope, 2), nsmall = 2)),
           color = c, size = 8, fontface = "bold", alpha = 1, 
           hjust = 0) +
  theme_linedraw(base_size = 15)

ggsave(paste0("images/",season,a,"_",b,".png"))

return(c(rsq,pval,cf,summary(model)))

}

colnames(ENSOvsSpr1980_2020)
colnames(ENSOvsSU_1950_2020)

ENSOvsSpr1980_2020 <- read.csv("data/ENSOvsSpr1980_2020.csv")
ENSOvsSU_1950_2020 <- read.csv("data/ENSOvsSU_1950_2020.csv")

#SPRING
#JFM
ENSO(ENSOvsSpr1980_2020,4,9,"darkblue","Spring",1980)
ENSO(ENSOvsSpr1980_2020,4,12,"darkblue","Spring",1980)
ENSO(ENSOvsSpr1980_2020,4,15,"darkblue","Spring",1980)

#MAM
ENSO(ENSOvsSpr1980_2020,5,9,"darkgreen","Spring",1980)
ENSO(ENSOvsSpr1980_2020,5,12,"darkgreen","Spring",1980)
ENSO(ENSOvsSpr1980_2020,5,15,"darkgreen","Spring",1980)

#JJA
ENSO(ENSOvsSpr1980_2020,6,9,"#cb4154","Spring",1980)
ENSO(ENSOvsSpr1980_2020,6,12,"#cb4154","Spring",1980)
ENSO(ENSOvsSpr1980_2020,6,15,"#cb4154","Spring",1980)


#SUMMER
#MAM
ENSO(ENSOvsSU_1950_2020,4,8,"darkgreen","Summer",1980)

#JJA
ENSO(ENSOvsSU_1950_2020,5,8,"#cb4154","Summer",1980)

#OND
ENSO(ENSOvsSU_1950_2020,7,8,"#ffbf00","Summer",1980)


