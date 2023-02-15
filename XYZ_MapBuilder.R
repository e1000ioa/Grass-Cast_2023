
#Data Tools
library(ggplot2) #ggplot2 is a system for declaratively creating graphics, based on The Grammar of Graphics.
library(dplyr) #dplyr is a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges
library(tidyr) #clean messy data
library(ggthemes) #Extra Themes, Scales and Geoms for 'ggplot2'
library(stringr) # The stringr package provides a cohesive set of functions designed to make working with strings as easy as possible. 

#Spatial Tools
library(maps) #database of USA maps
library(rgdal) #Provides bindings to the 'Geospatial' Data Abstraction Library ('GDAL') (>= 1.11.4) and access to projection/transformation operations from the 'PROJ' library.
library(sp) #Classes and methods for spatial data; the classes document where the spatial location information resides, for 2D or 3D data
library(sf) #Support for simple features, a standardized way to encode spatial vector data.
library(raster) #Reading, writing, manipulating, analyzing and modeling of spatial data. 
library(akima) #Interpolation of Irregularly and Regularly Spaced Data
library(scico) #Scientific colour map palettes
library(ggsn) #Arrow and scale for GGPLOT MAPS
library(ggspatial) #Spatial data plus the power of the ggplot2 framework means easier mapping



###########
############ Slip the large data files (only used it once)

#Data_2000_2020 <- read.csv("data/Annual_Mean_PPT_AET_1982-2022_with_regression_eqns_ANPP_csv.csv")

#Slips the columns in half
#split_dfs <- split.screen(Data_2000_2020, f = 1:2)

#part1 <- Data_2000_2020[1:(nrow(Data_2000_2020)/2), ]
#part2 <- Data_2000_2020[(nrow(Data_2000_2020)/2 + 1):nrow(Data_2000_2020), ]

#slip1 <- write.csv(part1,"data/Annual_Grass-Cast_Data_Part1.csv")
#slip2 <- write.csv(part2,"data/Annual_Grass-Cast_Data_Part2.csv")

################
#Create initial data frame
Part1 <- read.csv("data/Annual_Grass-Cast_Data_Part1.csv")
Part2 <- read.csv("data/Annual_Grass-Cast_Data_Part2.csv")
df0 <- rbind(Part1, Part2)
colnames(df0)

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

###########
############### FUNCTION

map_Precip <- function(year,season,measure,unit,colname,color,limx,limy) {
  
  df <- df0 %>% mutate_if(is.character, as.numeric) %>% 
    filter(year.x == year) %>%
    subset(select=c("longitude.x","latitude.x",colname))
  
  colnames(df) <- c('x', 'y', 'z')
  
  df$z <- df$z*10
  
  ############
  ## CREATES RASTER
  # Interpolate the values to a regularly spaced grid
  f <- with(df, interp(x, y, z))
  
  # Create a raster from the interpolated values
  r <- raster(f, crs = "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs")
  
  #Crop the Raster and save it to data frame for ggplot
  r2 <- crop(r, extent(south_west_merged))
  
  #Prepare Df to be drawn
  df_raster <- as.data.frame(rasterToPoints(r2)) #Changes the colum name z to layer
  
  #Change df to spatial object
  st <- df %>% st_as_sf(coords = c("x", "y")) %>% st_set_crs(st_crs(south_west_s))
  
  
  
  ############
  ##DRAW
  
  a <- ggplot() +
    #Add Raster and modify Scale
    geom_raster(data = df_raster, aes(x = x, y = y, fill = layer)) + 
    scale_fill_gradientn(name = paste(measure,unit), colors = scico(10, palette = color, direction = -1), 
                         limits = c(limx, limy),
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(3, units = "mm"),
                           barwidth = unit(90, units = "mm"),
                           draw.ulim = F,
                           keyheight = unit(2, units = "mm"),
                           keywidth = unit(70 / length(labels), units = "mm"),
                           title.position = 'top',
                           # some shifting around
                           title.hjust = 0.5,
                           label.hjust = 0.5
                         ))+
    #Add Spatial Elements
    geom_sf(data = south_west_merged, fill = NA, color = "white", linewidth = 2, linetype = "solid") +
    geom_sf(data = south_west_s, fill = NA, color=alpha("#000000",1), linewidth= 1.4,linetype = "solid") +
    geom_sf(data = south_west_c, fill = NA, color=alpha("#000000",0.2), linewidth=0.8,linetype = "dotted") +
    geom_sf(data = other_states, fill = "grey", color=alpha("grey40",0.4), linewidth=0.5,linetype = "dashed") +
    theme_minimal() +
    
    #Add Text Elements 
    xlab(NULL) + 
    ylab(NULL) +
    ggtitle(label = paste(measure,season,year), subtitle = paste0(unit)) +
    
    #Element Modification
    theme(
      legend.direction = "horizontal",  
      legend.position=c(0.5, -0.15),
      legend.justification = c(0.5, 0.5),
      plot.margin = unit(c(0.4,0.4,2,0.2), "cm"),
      legend.background = element_blank(), 
      legend.key = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    
    #zoom map to cordinates
    coord_sf(xlim = c(-115, -103),ylim = c(37, 31)) 
  
  ## ADD SCALE AN ARROW
  
  b <- a +
    ggspatial::annotation_scale(
      location = "br",
      bar_cols = c("black", "white")) +
    
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      style = north_arrow_orienteering(
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
    paste0("data/",measure,"_",year,"_",season,".png"),
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


map_Precip(2020,"Summer","Precipitation","(mm)","PPTjja_.cm.","lapaz",0,500)


map_ANPP <- function(year,season,measure,unit,colname,color,limx,limy) {
  
  df <- df0 %>% mutate_if(is.character, as.numeric) %>% 
    filter(year.x == year) %>%
    subset(select=c("longitude.x","latitude.x",colname))
  
  colnames(df) <- c('x', 'y', 'z')
  
  ############
  ## CREATES RASTER
  # Interpolate the values to a regularly spaced grid
  f <- with(df, interp(x, y, z))
  
  # Create a raster from the interpolated values
  r <- raster(f, crs = "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs")
  
  #Crop the Raster and save it to data frame for ggplot
  r2 <- crop(r, extent(south_west_merged))
  
  #Prepare Df to be drawn
  df_raster <- as.data.frame(rasterToPoints(r2)) #Changes the colum name z to layer
  
  #Change df to spatial object
  st <- df %>% st_as_sf(coords = c("x", "y")) %>% st_set_crs(st_crs(south_west_s))
  
  
  
  ############
  ##DRAW
  
  a <- ggplot() +
    #Add Raster and modify Scale
    geom_raster(data = df_raster, aes(x = x, y = y, fill = layer)) + 
    scale_fill_gradientn(name = paste(measure,unit), colors = scico(10, palette = color, direction = -1), 
                         limits = c(limx, limy),
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(3, units = "mm"),
                           barwidth = unit(90, units = "mm"),
                           draw.ulim = F,
                           keyheight = unit(2, units = "mm"),
                           keywidth = unit(70 / length(labels), units = "mm"),
                           title.position = 'top',
                           # some shifting around
                           title.hjust = 0.5,
                           label.hjust = 0.5
                         ))+
    #Add Spatial Elements
    geom_sf(data = south_west_merged, fill = NA, color = "white", linewidth = 2, linetype = "solid") +
    geom_sf(data = south_west_s, fill = NA, color=alpha("#000000",1), linewidth= 1.4,linetype = "solid") +
    geom_sf(data = south_west_c, fill = NA, color=alpha("#000000",0.2), linewidth=0.8,linetype = "dotted") +
    geom_sf(data = other_states, fill = "grey", color=alpha("grey40",0.4), linewidth=0.5,linetype = "dashed") +
    theme_minimal() +
    
    #Add Text Elements 
    xlab(NULL) + 
    ylab(NULL) +
    ggtitle(label = paste(measure,season,year), subtitle = paste0(unit)) +
    
    #Element Modification
    theme(
      legend.direction = "horizontal",  
      legend.position=c(0.5, -0.15),
      legend.justification = c(0.5, 0.5),
      plot.margin = unit(c(0.4,0.4,2,0.2), "cm"),
      legend.background = element_blank(), 
      legend.key = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    
    #zoom map to cordinates
    coord_sf(xlim = c(-115, -103),ylim = c(37, 31)) 
  
  ## ADD SCALE AN ARROW
  
  b <- a +
    ggspatial::annotation_scale(
      location = "br",
      bar_cols = c("black", "white")) +
    
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      style = north_arrow_orienteering(
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
    paste0("data/",measure,"_",year,"_",season,".png"),
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


map_ANPP(2022,"Summer","ANPP","(lb/ac)","ANPP_summer_lbs_ac","bamako",-35,2000)
