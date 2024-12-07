# Load the required libraries
library(ggplot2)
library(ggpubr)

# Read the data from the file
data <- read.csv("data/NPPNDVI_SW_sites.csv")

set.seed(108)
getwd()

# Create a scatter plot

ploter <- function(data){
  
  custom_colors <- c("#AAB645", "#DF5327", "#FEC306") #"#276EDF")
  
  model_yearly <- lm(ANPP ~ NDVI, data=data)
  
  plot <- ggplot(data, aes(x = NDVI, y = ANPP)) +
    geom_point(aes(color = SITE), size = 4) +
    scale_color_manual(values = custom_colors) + 
    xlab("NDVI") +
    ylab("ANPP (lb/acre)") +
    geom_smooth(method="lm", formula=y~x, se=FALSE, color="#595959") +
    
    ylim(0, 425) +  # Set x limits
    xlim(0.1, 0.45) +  # Set y limits +
    
    
    annotate(
      "text",
      y = 345,
      x = 0.15,
      label = paste(expression("R²"), "=", signif(summary(model_yearly)$r.squared, digits = 3)),
      color = "#595959",
      size = 5
    ) + #Annotate R2
    
    
    annotate(
      "text",
      y = 380,
      x = 0.15,
      label = paste0("RMSE = ", round(sqrt(mean(model$residuals^2)),digits = 2)),
      color = "#595959",
      size = 5
    ) + #Annotate linear equation
  
    theme_classic() +
    theme(axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.8),
          legend.position = "bottom", legend.title = element_blank(),
          axis.ticks = element_line(color = "black", size = 0.2),
          axis.ticks.length = unit(-0.1, "cm"),
          axis.text = element_text(color = "#595959"),
          panel.grid.major = element_line(color = "darkgrey", linetype = "dashed"),
          panel.grid.minor = element_blank())
  
  
  return(plot)
  
}

#Saves Plot and calculates p value
save <- function(data){
  
  plot <- ploter(data)
  ggsave(paste0("NDVI_ANPP_lm_",substitute(data),".png"), plot=last_plot(), bg="white")
  
  model <- lm(NDVI ~ ANPP, data = data) %>% summary()  
  p_value <- model$coefficients[2, 4]
  
  return(p_value)
    
}

# Filter the data for three specific sites
selected_sites <- c("JORNADA", "SANTA RITA", "TORREL")
SITES <- data[data$SITE %in% selected_sites, ]
save(SITES)

#Filter data for darin's selection
DARIN <- data[data$ALL %in% "YES", ]
save(DARIN)



