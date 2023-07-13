##Libraries
library(ggplot2)
library(dplyr)
library(ggrepel)

##Add Files
GC <- read.csv(file = "data/az_nm_2000_2020.csv", head = TRUE, sep=",") 

# Fit a linear regression model
model <- lm(GC$spring_delta_ndvi ~ GC$spring_delta_anpp, data=GC)


# Assuming that you have the NDVI and ANPP data in data frames 'ndvi' and 'anpp' respectively
ggplot(GC, aes(x=spring_delta_anpp, y=spring_delta_ndvi)) +
  geom_point() +
  ggtitle("Spring NDVI and ANPP Anomolies from 2000 to 2020") +
  xlab("Delta ANPP (lb/acre)") +
  ylab("NDVI(%)") +
  geom_hline(yintercept=0, linetype="dashed", color="gray") +
  geom_vline(xintercept=0, linetype="dashed", color="gray") +
  stat_smooth(method="lm", formula=y~x, se=FALSE, color="red") +
  annotate("text", x=min(GC$spring_delta_anpp)/1.3, y=max(GC$spring_delta_ndvi)/1.1, label=paste("R^2 = ", signif(summary(model)$r.squared, digits=3)), color="red", size=5) +
  annotate("text", x=min(GC$spring_delta_anpp)/1.3, y=max(GC$spring_delta_ndvi)/1.3, label=paste("Slope = ", signif(coef(model)[2], digits=3)), color="red", size=5) +
  theme_minimal()


##SUMMARY PER YEAR
GC_yearly <- GC %>%
  mutate(Year = year) %>%
  group_by(Year) %>%
  summarize(Sp_Avg_ANPP = mean(spring_delta_anpp),
            Sp_Avg_NDVI = mean(spring_delta_ndvi),
            Su_Avg_ANPP = mean(summer_delta_anpp),
            Su_Avg_NDVI = mean(summer_delta_ndvi),
            Sp_Z_ANPP = mean(spring_z_anpp),
            Sp_Z_NDVI = mean(spring_z_ndvi),
            Su_Z_ANPP = mean(summer_z_anpp),
            Su_Z_NDVI = mean(summer_z_ndvi),)


# Fit a linear regression model
model_yearly <- lm(Sp_Avg_ANPP ~ Sp_Avg_NDVI, data=GC_yearly)

ggplot(GC_yearly, aes(x=Sp_Avg_ANPP, y=Sp_Avg_NDVI)) +
  geom_point() +
  ggtitle("Average Spring NDVI and ANPP Anomolies from 2000 to 2020") +
  xlab("Delta ANPP (lb/acre)") +
  ylab("NDVI(%)") +
  geom_hline(yintercept=0, linetype="dashed", color="gray") +
  geom_vline(xintercept=0, linetype="dashed", color="gray") +
  stat_smooth(method="lm", formula=y~x, se=FALSE, color="blue") +
  annotate("text", x=min(GC_yearly$Sp_Avg_ANPP)/1.3, y=max(GC_yearly$Sp_Avg_NDVI)/1.1, label=paste("R^2 = ", signif(summary(model_yearly)$r.squared, digits=3)), color="blue", size=5) +
  annotate("text", x=min(GC_yearly$Sp_Avg_ANPP)/1.3, y=max(GC_yearly$Sp_Avg_NDVI)/1.3, label=paste("Slope = ", signif(coef(model_yearly)[2], digits=3)), color="blue", size=5) +
  theme_minimal()




yeary_plot <- function(a,b,c,d,e){
  # Fit a linear regression model
  model_yearly <- lm(a ~ b, data=d)
  
 plot<- ggplot(d, aes(x=a, y=b)) +
    geom_point() +
    ggtitle(paste0(c," NDVI and ANPP from 2000 to 2020")) +
    xlab("Delta ANPP (lb/acre)") +
    ylab("NDVI(%)") +
    geom_hline(yintercept=0, linetype="dashed", color="gray") +
    geom_vline(xintercept=0, linetype="dashed", color="gray") +
    stat_smooth(method="lm", formula=y~x, se=FALSE, color=e) +
    annotate("text", x=min(a)/1.3, y=max(b)/1.1, label=paste("R^2 = ", signif(summary(model_yearly)$r.squared, digits=3)), color=e, size=5) +
    annotate("text", x=min(a)/1.3, y=max(b)/1.3, label=paste("Slope = ", signif(coef(model_yearly)[2], digits=3)), color=e, size=5) +
    theme_minimal()
 
 return(plot)
}

#Average summer NDVI anomalies from 2000 t0 2020
yeary_plot(GC$spring_delta_anpp,GC$spring_delta_ndvi, "Spring Anomalies",GC, "#0066FF")
yeary_plot(GC$summer_delta_anpp,GC$summer_delta_ndvi, "Summer Anomalies",GC, "#FF6600")

#Average z values from 2000 to 2020
yeary_plot(GC$spring_z_anpp,GC$spring_z_ndvi, "Spring Z score",GC, "#0066FF")
yeary_plot(GC$summer_z_anpp,GC$summer_z_ndvi, "Summer Z score",GC, "#FF6600")


#Average summer NDVI anomaliely$Sp_Avg_NDVI, "Average Spring",GC_yearly)
yeary_plot(GC_yearly$Sp_Avg_ANPP,GC_yearly$Sp_Avg_NDVI, "Yearly Average Spring Anomalies",GC_yearly, "#0066FF") +
  geom_text_repel(aes(label = Year), hjust = 0, vjust = 0,segment.color = "gray",segment.alpha = 0.6)

yeary_plot(GC_yearly$Su_Avg_ANPP,GC_yearly$Su_Avg_NDVI, "Yearly Average Summer Anomalies",GC_yearly, "#FF6600") +
  geom_text_repel(aes(label = Year), hjust = 0, vjust = 0,segment.color = "gray",segment.alpha = 0.6)

#Average z values from 2000 to 2020
yeary_plot(GC_yearly$Sp_Z_ANPP,GC_yearly$Sp_Z_NDVI, "Yearly Average Spring Z score",GC_yearly, "#0066FF")+
  geom_text_repel(aes(label = Year), hjust = 0, vjust = 0,segment.color = "gray",segment.alpha = 0.6)

yeary_plot(GC_yearly$Su_Z_ANPP,GC_yearly$Su_Z_NDVI, "Yearly Average Summer Z score",GC_yearly, "#FF6600") +
  geom_text_repel(aes(label = Year), hjust = 0, vjust = 0,segment.color = "gray",segment.alpha = 0.6)


#idea
#Use yellow for summer and green for spring
