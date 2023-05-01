library(ggplot2)

ENSOvsSpr1980_2020 <- read.csv("data/ENSOvsSpr1980_2020.csv")
ENSOvsSU_1950_2020 <- read.csv("data/ENSOvsSU_1950_2020.csv")

4 <- "JMF"
5 <- "MAM"
6 <- "JJA"                    
9 <- "low_mean_percent_anom"
12 <- "med_mean_percent_anom"
15 <- "high_mean_percent_anom"

ENSO <- function(data,z,l,c,season){

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
ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "grey", alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = c) +
  labs(title = paste(season,"mean ANPP values vs ENSO Temperature"), 
       subtitle = paste0("months=", a, " and y=", b)) + 
  xlab(a) +
  ylab(b) +
  annotate("text", x = median(x), y = median(y), label = paste0("R2 = ", round(rsq, 2)),
           color = c, size = 4, fontface = "bold", alpha = 1, 
           hjust = 0)+
  annotate("text", x = median(x), y = median(y)-(median(y)*2.5), label = paste0("P value = ", format.pval(pval))
           ,color = c, size = 4, fontface = "bold", alpha = 1, 
           hjust = 0)+
  annotate("text", x = median(x), y = median(y)+(median(y)*2.5), label = paste0("Slope = ", format(round(slope, 2), nsmall = 2)),
           color = c, size = 4, fontface = "bold", alpha = 1, 
           hjust = 0) +
  theme_linedraw()

ggsave(paste0("images/",season,a,"_",b,".png"))

}

colnames(ENSOvsSpr1980_2020)
colnames(ENSOvsSU_1950_2020)

ENSOvsSpr1980_2020 <- read.csv("data/ENSOvsSpr1980_2020.csv")
ENSOvsSU_1950_2020 <- read.csv("data/ENSOvsSU_1950_2020.csv")

#SPRING
#JFM
ENSO(ENSOvsSpr1980_2020,4,9,"darkblue","Spring")
ENSO(ENSOvsSpr1980_2020,4,12,"darkblue","Spring")
ENSO(ENSOvsSpr1980_2020,4,15,"darkblue","Spring")

#MAM
ENSO(ENSOvsSpr1980_2020,5,9,"darkgreen","Spring")
ENSO(ENSOvsSpr1980_2020,5,12,"darkgreen","Spring")
ENSO(ENSOvsSpr1980_2020,5,15,"darkgreen","Spring")

#JJA
ENSO(ENSOvsSpr1980_2020,6,9,"#cb4154","Spring")
ENSO(ENSOvsSpr1980_2020,6,12,"#cb4154","Spring")
ENSO(ENSOvsSpr1980_2020,6,15,"#cb4154","Spring")

ENSO_SU <- function(data,z,l,c,season){
  
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
  ggplot(df, aes(x = x, y = y)) +
    geom_point(color = "grey", alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = c) +
    labs(title = paste(season,"mean ANPP values vs ENSO Temperature"), 
         subtitle = paste0("months=", a, " and y=", b)) + 
    xlab(a) +
    ylab(b) +
    annotate("text", x = median(x), y = median(y), label = paste0("R2 = ", round(rsq, 2)),
             color = c, size = 4, fontface = "bold", alpha = 1, 
             hjust = 0)+
    annotate("text", x = median(x), y = median(y)-(median(y)*5), label = paste0("P value = ", format.pval(pval))
             ,color = c, size = 4, fontface = "bold", alpha = 1, 
             hjust = 0)+
    annotate("text", x = median(x), y = median(y)+(median(y)*5), label = paste0("Slope = ", format(round(slope, 2), nsmall = 2)),
             color = c, size = 4, fontface = "bold", alpha = 1, 
             hjust = 0) +
    theme_linedraw()
  
  ggsave(paste0("images/",season,a,"_",b,".png"))
  
}

#SUMMER
#MAM
ENSO_SU(ENSOvsSU_1950_2020,4,8,"darkgreen","Summer")

#JJA
ENSO_SU(ENSOvsSU_1950_2020,5,8,"#cb4154","Summer")

#OND
ENSO(ENSOvsSU_1950_2020,7,8,"gold","Summer")


