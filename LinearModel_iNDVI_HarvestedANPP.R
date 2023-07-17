# Load the required libraries
library(ggplot2)
library(ggpubr)

# Read the data from the file
NPPiNDVI <- read.csv("data/NPPNDVI_SW_sites.csv")

# Create a scatter plot

ploter <- function(data){
  
  custom_colors <- c("#AAB645", "#DF5327", "#FEC306") #"#276EDF")
  
  model_yearly <- lm(ANPP ~ NDVI, data=data)
  
  plot <- ggplot(data, aes(x = NDVI, y = ANPP)) +
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
      x = 0.15,
      label = paste(expression("R²"), "=", signif(summary(model_yearly)$r.squared, digits = 3)),
      color = "#595959",
      size = 5
    ) + #Annotate R2
    
    
    annotate(
      "text",
      y = 400,
      x = 0.17,
      label = paste0("y = ", round(coef(model_yearly)[2], digits = 2), "x ", signif(coef(model_yearly)[1], digits = 3)),
      color = "#595959",
      size = 5
    ) + #Annotate linear equation
  
  theme_classic() +
    theme(axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.8),
          legend.position = "bottom", legend.title = element_blank())
  
  
  return(plot)
  
}

#Saves Plot and calculates p value
save <- function(data){
  
  plot <- ploter(data)
  ggsave(paste0("images/NDVI_ANPP_lm_",substitute(data),".png"), plot=last_plot(), bg="white")
  
  model <- lm(NDVI ~ ANPP, data = data) %>% summary()  
  p_value <- model$coefficients[2, 4]
  
  return(p_value)
    
}

#Filter data for darin's selection
DARIN <- NPPiNDVI[NPPiNDVI$ALL %in% "YES", ]
save(DARIN)



