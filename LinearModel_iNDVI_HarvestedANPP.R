# Load the required libraries
library(ggplot2)
library(ggpubr)
library(dplyr)

# Read the data from the file
NPPiNDVI <- read.csv("data/NPPNDVI_SW_sites.csv")  %>%
  mutate(SITE = ifelse(SITE == "TORREL", "JORNADA", SITE)) %>%
  filter(ALL == "YES")

# Create a scatter plot

custom_colors <- c("#AAB645", "#DF5327", "#276EDF")

model_yearly <- lm(ANPP ~ NDVI, data=NPPiNDVI)

#Calculate RSME
# Calculate residuals
residuals <- NPPiNDVI$ANPP - NPPiNDVI$NDVI

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))

# Calculate residuals
residuals <- y_observed - y_predicted

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))

 ggplot(NPPiNDVI, aes(x = NDVI, y = ANPP)) +
  geom_point(aes(color = SITE)) +
  scale_color_manual(values = custom_colors) + 
  xlab("iNDVI") +
  ylab("ANPP (lb/acre)") +
  geom_smooth(method="lm", formula=y~x, se=FALSE, color="#595959") +
  
  ylim(0, 425) +  # Set x limits
  xlim(0.1, 0.45) +  # Set y limits +
  
  
  annotate(
    "text",
    y = 365,
    x = 0.12,
    label = paste(expression("R²"), "=", signif(summary(model_yearly)$r.squared, digits = 3)),
    color = "#595959",
    size = 4,
    hjust = 0 
  ) +  #Annotate R2
  
  
  annotate(
    "text",
    y = 380,
    x = 0.12,
    label = paste("RSME =",round(rmse,2),"lb/acre"),
    color = "#595959",
    size = 4,
    hjust = 0 
  ) + #Annotate linear equation
  
  theme_classic() +
  theme(axis.line = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        legend.position = "bottom", legend.title = element_blank(),
        panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank()  # Hide minor gridlines
  )

 ggsave(
  filename = "images/NDVI_ANPP_lm.png",
 plot=last_plot(),
 width  = 2100,
 height = 2100,
 units = "px",
 bg = "white"
 )
 

#Saves Plot and calculates p value
save <- function(data){
  
  plot <- ploter(data)
  ggsave(paste0("images/NDVI_ANPP_lm.png"), plot=last_plot(), bg="white")
  
  model <- lm(NDVI ~ ANPP, data = data) %>% summary()  
  p_value <- model$coefficients[2, 4]
  
  return(p_value)
    
}

#Filter data for darin's selection
DARIN <- NPPiNDVI[NPPiNDVI$ALL %in% "YES", ]
save(DARIN)



