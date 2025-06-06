
#Data Tools
library(ggplot2) #ggplot2 is a system for declaratively creating graphics, based on The Grammar of Graphics.
library(dplyr) #dplyr is a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges
library(tidyr) #clean messy data
library(ggthemes) #Extra Themes, Scales and Geoms for 'ggplot2'
library(stringr) # The stringr package provides a cohesive set of functions designed to make working with strings as easy as possible. 

#Spatial Tools
library(maps) #database of USA maps
#She dead + library(rgdal) #Provides bindings to the 'Geospatial' Data Abstraction Library ('GDAL') (>= 1.11.4) and access to projection/transformation operations from the 'PROJ' library.
library(sp) #Classes and methods for spatial data; the classes document where the spatial location information resides, for 2D or 3D data
library(sf) #Support for simple features, a standardized way to encode spatial vector data.
library(raster) #Reading, writing, manipulating, analyzing and modeling of spatial data. 
library(akima) #Interpolation of Irregularly and Regularly Spaced Data
library(scico) #Scientific colour map palettes
library(ggsn) #Arrow and scale for GGPLOT MAPS
library(ggspatial) #Spatial data plus the power of the ggplot2 framework means easier mapping
library(tigris) #spatial data from the Census Bureau, has tribal nations shp.

################
setwd("C:/GitHub/Grass-Cast_2023/Grass-Cast_2023")
#Load Data frame
Forecast_202122 <- read.csv("data/Forecast_Ratio_2024.csv")
unique(Forecast_202122$Forecast)

#Fixing the no data 
# replace -999 with 31 using mutate_all()
Forecast_202122 <- mutate_all(Forecast_202122, ~ifelse(. == -999, 31, .))
Dates_List <- unique(Forecast_202122$Forecast)
xi1 <- order(as.Date(Dates_List, format = "%Y-%m-%d"))
Forecast_List <- Dates_List[xi1]

# Date ranges for Spring and Summer
spring_dates <- list(c("2020-05-15", "2020-06-02"), c("2021-04-14", "2021-06-01"), c("2022-04-05", "2022-05-31"))
summer_dates <- list(c("2020-06-16", "2020-09-01"), c("2021-06-16", "2021-09-10"), c("2022-06-14", "2022-09-01"))

# Function to classify season
get_season <- function(date) {
  for (year_index in seq_along(spring_dates)) {
    spring_range <- as.Date(spring_dates[[year_index]])
    summer_range <- as.Date(summer_dates[[year_index]])
    
    if (date >= spring_range[1] && date <= spring_range[2]) {
      return("Spring")
    } else if (date >= summer_range[1] && date <= summer_range[2]) {
      return("Summer")
    }
  }
  return("Other") # Default if date doesn't fall in Spring or Summer
}

# Apply the function to each date in the Forecast column
Forecast_202122$Season <- sapply(Forecast_202122$Forecast, get_season)

# View the updated data frame
print(Forecast_202122)

unique_dates_seasons <- unique(Forecast_202122[, c("Forecast", "Season")])

#Check Values
check_data <- subset(Forecast_202122, Forecast_202122$Forecast == "2021-06-16")
hist(check_data$longitude)

##########
###################          CREATE SPATIAL OBJECTS
## Call features from state and county

#changes state maps to simple forms to make it easier to map
states_sf <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
county_sf <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) 

#select only AZ and NM Partial matching in the row ID
south_west_s1 <- states_sf %>% filter(str_detect(ID, c("arizona")))
south_west_s2 <- states_sf %>% filter(str_detect(ID, c("new mexico")))
south_west_s <- rbind(south_west_s1, south_west_s2)

#Shape merge for white background
south_west_merged <- st_union(south_west_s1,south_west_s2)

#Counties
south_west_nm <- county_sf %>% filter(str_detect(ID,"new mexico"))
south_west_az <- county_sf %>% filter(str_detect(ID,"arizona"))
south_west_c <- rbind(south_west_nm, south_west_az)

#Add neighboring states 
other_states_1 <- states_sf %>% filter(str_detect(ID, c("texas")))
other_states_2 <- states_sf %>% filter(str_detect(ID, c("nevada")))
other_states_3 <- states_sf %>% filter(str_detect(ID, c("utah")))
other_states_4 <- states_sf %>% filter(str_detect(ID, c("colorado")))
other_states_5 <- states_sf %>% filter(str_detect(ID, c("california")))
other_states_6 <- states_sf %>% filter(str_detect(ID, c("oklahoma")))
other_states <- rbind(other_states_1, other_states_2,other_states_3,other_states_4,other_states_5,other_states_6)

### PREPAPRE Tribal Nations MAPS

# Call the native nations map
nat <- tigris::native_areas(cb = TRUE)

# Transform the southwest counties to match the CRS of nat
sf_c <- st_transform(south_west_c, st_crs(nat))

# Crop the extent of the Tribal Nations to the southwest
nat <- st_crop(nat, st_bbox(sf_c))

# Merge all the shapes in nat to use in difference
nat_union <- st_union(nat)
nat_union <- st_simplify(nat_union, dTolerance = 1000)

# Conserve only the districts that are outside Tribal Nations
diff_c <- st_difference(sf_c, nat_union)

# Plot the difference
ggplot() +
  #geom_sf(data = sf_c, fill = "red", color="gold") +
  geom_sf(data = nat, fill = "yellow", color="black") +
  geom_sf(data = diff_c, color = "black", fill="yellow") +
  theme_void()

##########
############Function

colours = c("#f90207", "#fd9200", "#fffd05", "#baf9a1", "#05fdff", "#3e83f9", "#0200fc") # 7 colors
breaks = c(-30, -15, -5, 0, 5, 15, 30) # Define breakpoints
labels = c("<-30", "-30 to -15", "-15 to -5", "-5 to 5", "5 to 15","15 to 30",">30") # Matching the intervals
  

ggplot() +
  # Add Raster and modify Scale
  geom_raster(data = cr_raster, aes(x = x, y = y, fill = layer)) + 
  scale_fill_stepsn(
    name = paste(measure, unit),
    colours = c("#f90207", "#fd9200", "#fffd05", "#baf9a1", "#05fdff", "#3e83f9", "#0200fc"), # 7 colors
    breaks = c(-30, -15, -5, 0, 5, 15, 30), # Define breakpoints
    labels = c("<-30", "-30 to -15", "-15 to -5", "-5 to 5", "5 to 15","15 to 30",">30") # Matching the intervals
  )

map_annp <- function(date,scenario,measure,unit,colname) {
  
  df <- Forecast_202122 %>% 
  filter(Forecast == date) %>%
    subset(select=c("longitude","latitude",colname,"Season"))
  
  colnames(df) <- c('x', 'y', 'z','Season')

  ############
  ## CREATES RASTER
  # Interpolate the values to a regularly spaced grid
  f <- with(df, interp(x, y, z))
  
  # Create a raster from the interpolated values
  r <- raster(f, crs = "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs")
  
  #Crop the Raster and save it to data frame for ggplot
  cr <- crop(r, extent(south_west_merged))
  
  #Prepare Df to be drawn
  cr_raster <- as.data.frame(rasterToPoints(cr)) #Changes the colum name z to layer

  #Change df to spatial object
  st <- df %>% st_as_sf(coords = c("x", "y")) %>% st_set_crs(st_crs(south_west_s))
  
  ############
  ##DRAW
  
  a <- ggplot() +
    # Add Raster and modify Scale
    geom_raster(data = cr_raster, aes(x = x, y = y, fill = layer)) + 
    scale_fill_stepsn(
      name = paste(measure, unit),
      colours = c("#f90207", "#fd9200", "#fffd05", "#baf9a1", "#05fdff", "#3e83f9", "#0200fc"), # 7 colors
      breaks = c(-30, -15, -5, 0, 5, 15, 30), # Define breakpoints
      values = scales::rescale(c(-30, -15, -5, 0, 5, 15, 30)), # Rescaled full range
      labels = c("<-30", "-30 to -15", "-15 to -5", "-5 to 5", "5 to 15", "15 to 30", ">30"), # Matching the intervals
      limits = c(-35, 35) # Optional: define the limits for the scale
    ) +
    
    #Add Spatial Elements
    geom_sf(data = south_west_merged, fill = NA, color = "white", linewidth = 2, linetype = "solid") +
    geom_sf(data = south_west_s, fill = NA, color=alpha("#000000",1), linewidth= 1.4,linetype = "solid") +
    geom_sf(data = diff_c, fill = NA, color=alpha("#000000",0.6), linewidth=0.5,linetype = "dotted") +
    
    ##Add Tribal Nations Maps
    geom_sf(data = nat, fill = NA, color=alpha("#000000",0.6), linewidth=0.5,linetype = "dotted") +
    
    #Add the states in the background
    geom_sf(data = other_states, fill = "grey", color=alpha("grey40",0.4), linewidth=0.5,linetype = "dashed") +
    theme_minimal() +
    
    #Add Text Elements 
    xlab(NULL) + 
    ylab(NULL) +
    ggtitle(label = paste(measure, toupper(df$Season),"FORECAST",substr(date, 1, 4)), subtitle = paste(date,"- last forecast")) +
    
    #Element Modification
    theme(
      legend.direction = "vertical",  
      legend.position = c(0.1,0.3),
      legend.justification = c(0.5, 0.5),
      plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm"),
      legend.background = element_rect(), 
      legend.key = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 8), 
      legend.text = element_text(size = 8)
    ) +
    
    #zoom map to cordinates
    coord_sf(xlim = c(-117, -103),ylim = c(37, 30.8)) 
  
  ## ADD SCALE AN ARROW
  
  b <- a +
    ggspatial::annotation_scale(
      location = "br",
      bar_cols = c("black", "white")) +
    
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      pad_x = unit(15, "cm"),
      pad_y = unit(1, "cm"),
      style = ggspatial::north_arrow_orienteering(
        line_width = 1,
        line_col = "black",
        fill = c("white", "black"),
        text_col = "black",
        #text_family = "",
        text_face = NULL,
        text_size = 10,
        text_angle = 0))
  
  #Save the Plots
  
  ggsave(
    paste0("images2025/",measure,"_",df$Season,"_",date,".png"),
    plot = last_plot(),
    device = NULL,
    path = NULL,
    scale = 1,
    width = NA,
    height = NA,
    #units = c("in", "cm", "mm", "px"),
    dpi = 300,
    limitsize = TRUE,
    bg = "white",
  )
  return(b)
}

hist(Forecast_202122$deltaNPP_above)
map_annp(Forecast_List[3],"below normal","ANPP","(%)","deltaNPP_below")

getwd()

############
#Result
# I choose to get a for loop for all the maps at once.
#I add what Season in the parameters, and then, erase the other season. 

for (i in 1:length(Forecast_List)){
  
  map_annp(Forecast_List[i],"above normal","ANPP","(%)","deltaNPP_above")
  
  
}
  
