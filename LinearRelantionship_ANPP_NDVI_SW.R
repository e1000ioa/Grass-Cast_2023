# Load the required libraries
library(ggplot2)
library(ggpubr)

#The equations does not seem to mach the equations of excel. 
#I belive is due to the incorrect paring of variables
#Because of this, the annotated boxes have two options, 
#The first one are the calculated intercepted and slope
#The second one are the results from excel, just change the values

# Read the data from the file
data <- read.csv("data/NPPNDVI_SW_sites.csv")

set.seed(108)


# Create a scatter plot

ploter <- function(data, subtitle){
  
  custom_colors <- c("#AAB645", "#DF5327", "#FEC306") #"#276EDF")
  
  model_yearly <- lm(ANPP ~ NDVI, data=data)
  
  plot <- ggplot(data, aes(x = NDVI, y = ANPP)) +
    geom_point(aes(color = SITE)) +
    scale_color_manual(values = custom_colors) + 
    ggtitle(label = "Harvested ANPP VS. Observed NDVI", subtitle = subtitle) +
    xlab("NDVI") +
    ylab("ANPP (lb/acre)") +
    geom_hline(yintercept=0, linetype="dashed", color="gray") +
    geom_vline(xintercept=0.1, linetype="dashed", color="gray") +
    geom_smooth(method="lm", formula=y~x, se=FALSE, color="#595959") +
    
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank())  +
    
    ylim(0, 450) +  # Set x limits
    xlim(0.1, 0.5) +  # Set y limits +
    
    
    annotate(
      "text",
      y = 360,
      x = 0.15,
      label = paste(expression("R²"), "=", signif(summary(model_yearly)$r.squared, digits = 3)),
      #label = "R² = 0.6552",
      color = "#595959",
      size = 4
    ) + #Annotate R2
    
    
    annotate(
      "text",
      y = 400,
      x = 0.17,
      label = paste0("y = ", round(coef(model_yearly)[2], digits = 2), "x ", signif(coef(model_yearly)[1], digits = 3)),
      #label = "y = 891.06x - 117",
      color = "#595959",
      size = 4
    ) #Annotate linear equation
  
  
  return(plot)
  
}

#Saves Plot and calcualtes p value
save <- function(data, subtitle){
  
  plot <- ploter(data,subtitle)
  ggsave(paste0("NDVI_ANPP_lm_",substitute(data),".png"), plot=last_plot(), bg="white")
  
  model <- lm(NDVI ~ ANPP, data = data) %>% summary()  
  p_value <- model$coefficients[2, 4]
  
  return(p_value)
    
}

# Filter the data for three specific sites
selected_sites <- c("JORNADA", "SANTA RITA", "TORREL")
SITES <- data[data$SITE %in% selected_sites, ]
save(SITES,"All sites")

#Filter data for darin's selection
DARIN <- data[data$ALL %in% "YES", ]
save(DARIN,"Darin's selection")



