#libraries
library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggpubr)
library(ggtext) #for shaowtext 

##Add Files
GC <- read.csv(file = "data/az_nm_2000_2020.csv", head = TRUE, sep=",") 

# Fit a linear regression model
model <- lm(GC$spring_delta_ndvi ~ GC$spring_delta_anpp, data=GC)


# Assuming that you have the NDVI and ANPP data in data frames 'ndvi' and 'anpp' respectively
ggplot(GC, aes(x=spring_delta_anpp, y=spring_delta_ndvi)) +
  geom_point() +
  ggtitle("Spring NDVI and ANPP Anomolies from 2000 to 2020") +
  xlab("Δ ANPP (lb/acre)") +
  ylab("Δ iNDVI(%)") +
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
  xlab("Δ ANPP (lb/acre)") +
  ylab("Δ iNDVI(%)") +
  geom_hline(yintercept=0, linetype="dashed", color="gray") +
  geom_vline(xintercept=0, linetype="dashed", color="gray") +
  stat_smooth(method="lm", formula=y~x, se=FALSE, color="blue") +
  annotate("text", x=min(GC_yearly$Sp_Avg_ANPP)/1.3, y=max(GC_yearly$Sp_Avg_NDVI)/1.1, label=paste("R^2 = ", signif(summary(model_yearly)$r.squared, digits=3)), color="blue", size=5) +
  annotate("text", x=min(GC_yearly$Sp_Avg_ANPP)/1.3, y=max(GC_yearly$Sp_Avg_NDVI)/1.3, label=paste("Slope = ", signif(coef(model_yearly)[2], digits=3)), color="blue", size=5) +
  theme_minimal()



yeary_plot <- function(data, x, y, color) {
  # Fit a linear regression model
  model <- lm(y ~ x, data = data)
  
  # Set text sizes
  annotation_size <- 18 / .pt  # Annotation text size (13 points)
  general_text_size <- 15 / .pt  # General text size (12 points)
  axis_text_size <- 50 / .pt  # Axis text size (16 points for larger numbers)
  
  plot <- ggplot(data, aes(x = x, y = y, label = Year)) +
    geom_point(color = color, alpha = 1) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = color) +
    geom_text_repel(size = general_text_size, alpha = 0.5, nudge_y = -(max(y) / 100)) +
    
    xlab("Δ ANPP (lb/acre)") +
    ylab("Δ iNDVI(%)") +
    
    ylim(-1.5, 1.5) +
    xlim(-1.5, 1.5) +
    
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    
    annotate(
      "text",
      x = min(x) / 1.3,
      y = max(y) / 1.0,
      label = paste0("y = ", round(coef(model)[2], digits = 2), "x + ", signif(coef(model)[1], digits = 3)),
      color = "#595959",
      size = annotation_size
    ) + # Annotate linear equation
    
    annotate(
      "text",
      x = min(x) / 1.3,
      y = max(y) / 1.15,
      label = paste(expression("R²"), "=", signif(summary(model)$r.squared, digits = 3)),
      color = "#595959",
      size = annotation_size
    ) + # Annotate R2
    
    theme_classic() +
    theme(
      axis.text = element_text(size = axis_text_size),  # Larger axis text
      axis.title = element_text(size = axis_text_size),  # Larger axis titles
      axis.line = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.8)
    )
  
  return(plot)
}




#Average z values from 2000 to 2020
SP <- yeary_plot(GC_yearly, GC_yearly$Sp_Z_ANPP,GC_yearly$Sp_Z_NDVI,"#AAB645") 

SU <- yeary_plot(GC_yearly, GC_yearly$Su_Z_ANPP,GC_yearly$Su_Z_NDVI, "#FEC306") 

#### Combine multiple ggplot on one page #### 

ggarrange(SP, SU,
          labels = c("A", "B"),
          font.label = list(size = 20, color = "black"),
          ncol = 2, nrow = 1)

ggsave("images/new/NDVI_ANPP_lm.png", 
       dpi = 350,
       height = 18,
       width = 33,
       units = "cm")

