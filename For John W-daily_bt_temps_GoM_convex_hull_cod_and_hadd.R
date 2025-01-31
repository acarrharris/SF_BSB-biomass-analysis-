
# load packages 
pkgs_to_use <- c("sf", "ncdf4", "raster","rasterVis", "RColorBrewer" , "chron", "lattice", "RColorBrewer", "rlist", "data.table", 
                 "rlang","foreign","tidyverse", "sp", "sf", "here", "scales", "raster", "stringi", "ggplot2", "tigris",
                 "dplyr", "writexl", "readxl","mapview","geosphere", "sf",  "terra", "maps", "spatialEco", "ggsn", 
                 "ggspatial", "ggrepel", "marmap")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
library(readxl)
library(writexl)
library(geosphere)

#GoM shape file 
PROJ.USE = CRS('+proj=aea +lat_1=28 +lat_2=42 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ')

#I put all the stock area shape file data in a folder "W:/Stock_Areas"
GoM_shape <- st_read(dsn="W:/",
                     layer="Stock_Areas")

#ls(GoM_shape)

#Keep only the GOM Cod Stock Area
GoM_shape <-GoM_shape %>% 
  dplyr::filter(AREANAME=="GOM Cod Stock Area")

plot(GoM_shape['AREANAME'])


#Pull in east coast map data for spatial plotting
states <- map_data("state")
east_coast <- subset(states, region %in% c("maine", "massachusetts", "vermont", "new hampshire","rhode island", "connecticut", "new york", "pennsylvania",
                                           "new jersey", "maryland", "delaware", "district of columbia", "virginia", "west virginia", "north carolina"))


## Pull in the spatial lat-lon points from VTR

#Create a single access area for each state based on VTR points for all years combined
vtr_lat_lons_cod<-read_csv("W:/Yearly_Cod_Recreational_Lat_Lon.csv", show_col_types = FALSE)


lat_lons_ready <- vtr_lat_lons_cod %>%
  dplyr::select(Species, LAT, LON, YEAR) %>%
  drop_na()


#Calculate the mean lat/lon by state
lat_lons_ready <- lat_lons_ready %>%  
  group_by(Species) %>%
  mutate(mean_lat = mean(LAT))  %>%
  mutate(mean_lon = mean(LON)) %>% 
  ungroup()

##Calculate the distance of each lat/lon to the centroid by state
lat_lons_ready$distance<-distGeo(lat_lons_ready[,2:3], lat_lons_ready[,5:6])

ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
  #geom_text(aes(x = long, y = lat, label = region))+
  geom_point(data = lat_lons_ready, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
  geom_sf(data = GoM_shape['AREANAME'], fill = "blue", alpha = 0.3, show.legend = FALSE) +
  coord_sf(xlim = c(-74.6, -66), ylim = c(40, 44.7)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white")
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"    )
  )


#Keep only VTR lat lons falling within the GoM mgt. area
mgt_boundaries_subset<- st_as_sf(GoM_shape) 
mgt_boundaries_subset<-mgt_boundaries_subset %>% 
  dplyr::select(geometry, AREANAME) 

sp_points = st_as_sf(lat_lons_ready,coords = c('LON' ,'LAT' )) #make points spatial
st_crs(sp_points)= 4326

mgt_boundaries_subset=st_transform(mgt_boundaries_subset,st_crs(sp_points)) # Match the point and polygon CRS

#Spatial join the points to the polygon
VTR_GoM_points2 <- sf::st_join(sp_points, mgt_boundaries_subset)

#Allocate spatial points as inside the GOM if the AREANAME is not missing 
VTR_GoM_points2_inside <- VTR_GoM_points2 %>% 
  dplyr::filter(!is.na(AREANAME))  ###THIS IS THE DATA SET OF INTEREST: only data within the GOM

#St_cast spatial points in order to confirm allocations by plotting
VTR_GoM_points2_inside_plot = data.frame(st_coordinates(st_cast(VTR_GoM_points2_inside$geometry,"POINT")))
VTR_GoM_points2_inside_plot<-VTR_GoM_points2_inside_plot %>% 
  dplyr::rename(LON=X, LAT=Y)

#Allocate spatial points as outside the GOM if the AREANAME is missing 
VTR_GoM_points2_outside <- VTR_GoM_points2 %>% 
  dplyr::filter(is.na(AREANAME))

#St_cast spatial points in order to confirm allocations by plotting
VTR_GoM_points2_outside_plot = data.frame(st_coordinates(st_cast(VTR_GoM_points2_outside$geometry,"POINT")))
VTR_GoM_points2_outside_plot<-VTR_GoM_points2_outside_plot %>% 
  dplyr::rename(LON=X, LAT=Y)

#confirm spatial points are correctly allocated within/outside the mgt. area by plotting
ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
  #geom_text(aes(x = long, y = lat, label = region))+
  geom_point(data = VTR_GoM_points2_outside_plot, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
  geom_point(data = VTR_GoM_points2_inside_plot, aes(x = LON, y = LAT), color = "black", alpha = 0.3, show.legend = FALSE) +
  geom_sf(data = GoM_shape['AREANAME'], fill = "blue", alpha = 0.3, show.legend = FALSE) +
  coord_sf(xlim = c(-74.6, -66), ylim = c(40, 44.7)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white")
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"    )
  )



