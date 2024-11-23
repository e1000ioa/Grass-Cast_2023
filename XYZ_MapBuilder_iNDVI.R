
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
library(tigris) #spatial data from the Census Bureau, has tribal nations shp.

# List of libraries required
libraries <- c("maps", "rgdal", "sp", "sf", "raster", "akima", 
               "scico", "ggsn", "ggspatial", "tigris")

# Function to install and load libraries
install_and_load <- function(lib){
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
    library(lib, character.only = TRUE)
  }
}

# Loop through the list of libraries and install/load them
lapply(libraries, install_and_load)


################
#Load Data frame
#Grind ID 
GC_indvi <- read.csv(file = "data/az_nm_2000_2020.csv", head = TRUE, sep=",") %>% 
  subset(select = c("gridID", "year", "spring_delta_ndvi", "summer_delta_ndvi", "spring_z_ndvi", "summer_z_ndvi","lon","lat")) %>%
  filter(year == 2020) %>%
  pivot_longer(cols = c(spring_delta_ndvi, summer_delta_ndvi, spring_z_ndvi, summer_z_ndvi),
               names_to = c("Season", ".value"),
               names_pattern = "(spring|summer)_(.*)") 


Forecast_202122 <- read.csv("data/Forecast_Ratio.csv") 
unique(Forecast_202122$Forecast)

# Filter Forecast data for the year 2020 and specific Forecast dates
Forecast_GC_2020 <- Forecast_202122 %>%
  filter(Year == 2020) %>%
  filter(Forecast %in% c("2020-06-02", "2020-09-01")) %>%
  mutate(Season = case_when(
    Forecast == "2020-06-02" ~ "spring",
    Forecast == "2020-09-01" ~ "summer"
  ))

# Join GC_indvi and Forecast_GC_2020 by gridID
combined_data <- left_join(GC_indvi, Forecast_GC_2020, by = "gridID")

#Fixing the no data 
# replace -999 with 31 using mutate_all()
Forecast_202122 <- mutate_all(Forecast_202122, ~ifelse(. == -999, 31, .))


Dates_List <- unique(Forecast_202122$Forecast)
i1 <- order(as.Date(Dates_List, format = "%Y-%m-%d"))
Forecast_List <- Dates_List[i1]


#Check Values
check_data <- subset(Forecast_202122, Forecast_202122$Forecast == "2021-06-16")
hist(check_data$pct_diffNPP_avg)

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

#Call the native nations map 
nat <- tigris::native_areas(cb = TRUE)

#Transform the southwest counties in the same csr
sf_c <- st_transform(south_west_c, st_crs(nat))

#Crop the extend of the of the Tribal nations to the southwest 
nat <- st_crop(nat, st_bbox(sf_c))

#Merge all the shapes in nat to use in difference
nat_union <- st_union(sf_c)
nat_union <- st_simplify(nat_union, dTolerance = 1000)

#Conserve only the districts that are outside Tribal Nations
diff_c <- st_difference(sf_c,nat_union)
plot(diff_c)

#Test the map
ggplot() +
  #geom_sf(data = sf_c, fill = "red", color="gold") +
  geom_sf(data = nat, fill = "blue", color="black") +
  geom_sf(data = diff_c, color = "black", fill="yellow") +
  theme_void()
  
##########
############Function


# Load necessary libraries
library(dplyr)
library(akima)
library(ggplot2)
library(ggspatial)
library(sf)
library(raster)
library(scales)


map_indvi <- function(season, measure, units, year, colname) {
  
  # Filter data by the season and select required columns
  df <- GC_indvi %>%
    filter(Season == season) %>%
    dplyr::select(lon, lat, all_of(colname))
  
  # Rename columns to avoid conflicts with function names
  colnames(df) <- c("longitude", "latitude", "value")
  
  ############
  ## CREATE RASTER
  # Interpolate the values to a regularly spaced grid
  f <- akima::interp(x = df$longitude, y = df$latitude, z = df$value)
  
  # Create a raster from the interpolated values
  r <- raster(f, crs = "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs")
  
  # Crop the raster and save it to data frame for ggplot
  cr <- crop(r, extent(south_west_merged))
  
  # Prepare data frame for ggplot
  cr_raster <- as.data.frame(rasterToPoints(cr)) # Changes the column name 'value' to 'layer'
  
  # Convert df to spatial object
  st <- df %>%
    st_as_sf(coords = c("longitude", "latitude")) %>%
    st_set_crs(st_crs(south_west_s))
  
  
  ############
  ## DRAW MAP
  
  a <- ggplot() +
    # Add raster and modify scale
    geom_raster(data = cr_raster, aes(x = x, y = y, fill = layer)) +
    scale_fill_stepsn(
      name = paste(measure, units),  # Corrected here
      colours = c("#f90207", "#fd9200", "#fffd05", "#baf9a1", "#05fdff", "#3e83f9", '#0200fc'),
      limits = c(-40, 40),
      breaks = c(-30, -15, -5, 0, 5, 15, 30),
      values = scales::rescale(c(-30, -15, -5, 5, 15, 30)),
      labels = c("<-30", "-30 to -15", "-15 to -5", "-5 to 5", "5 to 15", "15 to 30", ">30")
    )  +
    
    # Add spatial elements
    geom_sf(data = south_west_merged, fill = NA, color = "white", linewidth = 2, linetype = "solid") +
    geom_sf(data = south_west_s, fill = NA, color = alpha("#000000", 1), linewidth = 1.4, linetype = "solid") +
    geom_sf(data = south_west_c, fill = NA, color = alpha("#000000", 0.6), linewidth = 0.5, linetype = "dotted") +
    geom_sf(data = diff_c, fill = NA, color = alpha("#000000", 0.6), linewidth = 0.5, linetype = "dotted") +
    
    # Add Tribal Nations Maps
    geom_sf(data = nat, color = alpha("grey", 0.8), fill = NA, linewidth = 0.5, linetype = "solid") +
    
    # Add the states in the background
    geom_sf(data = other_states, fill = "grey", color = alpha("grey40", 0.4), linewidth = 0.5, linetype = "dashed") +
    theme_minimal()  +
    
    # Add text elements
    xlab(NULL) + 
    ylab(NULL) +
    ggtitle(label = paste(year, measure, season)) +
    
    # Element modification
    theme(
      legend.direction = "vertical",  
      legend.position = c(0.12, 0.3),
      legend.justification = c(0.5, 0.5),
      plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
      legend.background = element_rect(), 
      legend.key = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 8), 
      legend.text = element_text(size = 8)
    ) +
    
    # Zoom map to coordinates
    coord_sf(xlim = c(-118, -103), ylim = c(37, 30)) 
    ## ADD SCALE AND NORTH ARROW
    
    b <- a +
    ggspatial::annotation_scale(
      location = "br",
      bar_cols = c("black", "white")
    ) 
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      pad_x = unit(9.5, "cm"),
      pad_y = unit(0.25, "cm"),
      style = ggspatial::north_arrow_orienteering(
        line_width = 1,
        line_col = "black",
        fill = c("white", "black"),
        text_col = "black",
        text_size = 10
      )
    )
    
  
    ggsave(
      paste0("images/",season,"_iNDVI",year,".png"),
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
  return(a) 
}

# Example usage
map_indvi("summer", "Δ iNDVI", "(%)", "2020", "delta_ndvi")
map_indvi("spring", "Δ iNDVI", "(%)", "2020", "delta_ndvi")


  