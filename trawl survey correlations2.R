args = commandArgs(trailingOnly=TRUE)

#load needed packages and install if not currently installed.
pkgs_to_use <- c("rlang","foreign","tidyverse", "sp", "sf", "rgeos","rgdal", "here", "scales", "raster", "stringi", "ggplot2", "tigris",
                 "dplyr", "writexl", "readxl","mapview","geosphere", "sf",  "terra", "maps", "spatialEco" )
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)


# library(rlang)
# library(foreign)
# library(tidyverse)
# library(sp)
# library(sf)
# library(rgeos)
# library(rgdal)
# library(here)
# library(scales)
# library(raster)
# library(stringi)
# library(ggplot2)
# library(tigris)
# library(dplyr)
# library(writexl)
# library(readxl)
# library(xlsx)
# library(mapview)
# library(geosphere)



PROJ.USE = CRS('+proj=aea +lat_1=28 +lat_2=42 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ')

###########Input the state line#############
#This reads in the shapefiles, you might need to change the file path slightly
#depending on how you set up your folder structure for this project. 
#Here, I input these to make sure I do overlap indices based on the VTR shape files correctly 

state_boundaries <- readOGR( dsn="\\\\net.nefsc.noaa.gov/aharris/DisMap data",
                             layer="state_national_ocean_boundaries",
                             stringsAsFactors=FALSE, verbose=FALSE)

state_boundaries = spTransform(state_boundaries, CRS=PROJ.USE)


#New code to create a single access area for each state based on VTR points for all years combined

lat_lons <- subset(data.frame(read_excel("//net.nefsc.noaa.gov/home2/aharris/DisMap data/all_pc_trips_1994_2021.xlsx")), 
                   state1 %in% c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC"))

lat_lons_ready <- lat_lons %>%
  dplyr::select(state1, lat, lon) %>%
  rename(State=state1) %>%
  drop_na()


##Calculate the mean lat/lon by state
lat_lons_ready <- lat_lons_ready %>%  
  group_by(State) %>%
  mutate(mean_lat = mean(lat))  %>%
  mutate(mean_lon = mean(lon))
ungroup

##Calculate the distance of each lat/lon to the centroid by state
lat_lons_ready$distance<-distHaversine(lat_lons_ready[,2:3], lat_lons_ready[,4:5])

##Calculate tthe 95 pctile of distance to centroid by state
lat_lons_ready <- lat_lons_ready %>%  
  group_by(State) %>%
  mutate(q95 = quantile(distance, 0.95)) %>%
  ungroup

###Count the number of observations by state
lat_lons_ready <- lat_lons_ready %>%  
  group_by(State) %>%
  add_count(State) %>%
  ungroup

###Drop any VTR points that are outside the 95 pctile of the average lat/lon,
### BUT do not drop if there are fewer than three lat/lons in a state 
lat_lons_ready <- lat_lons_ready %>%  
  filter(distance <= q95 | (distance > q95 & n<=3))


lat_lon_hulls <- lat_lons_ready %>%
  group_by(State) %>%
  slice(chull(lat, lon)) %>%
  ungroup()

states <- map_data("state")
east_coast <- subset(states, region %in% c("maine", "massachusetts", "vermont", "new hampshire","rhode island", "connecticut", "new york", "pennsylvania",
                                           "new jersey", "maryland", "delaware", "district of columbia", "virginia", "west virginia", "north carolina"))

ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#D1D1E0", color = "black") +
  geom_point(data = lat_lons_ready, aes(x = lon, y = lat, group = State, colour = State), size = 1, shape = 20) +
  geom_polygon(data = lat_lon_hulls, aes(x = lon, y = lat, group = State, colour = State), alpha = 0.3, show.legend = FALSE) +
  coord_sf(xlim = c(-80, -64), ylim = c(34, 46)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))



# Create convex lat_lon_hulls shapefile
hull_poly <- lat_lon_hulls %>%
  group_by(State) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# write to shapefile
st_write(hull_poly, dsn = "//net.nefsc.noaa.gov/home2/aharris/DisMap data/VTR_chulls/hull_poly_all_years.shp", 
         layer = "hull_poly_all_years", driver = "ESRI Shapefile", delete_layer = TRUE) 

##import and transform the hull poly's
hull_poly_boundaries <- readOGR( dsn="\\\\net.nefsc.noaa.gov/aharris/DisMap data/VTR_chulls",
                                 layer="hull_poly_all_years",
                                 stringsAsFactors=FALSE, verbose=FALSE)

VTR_boundaries <- spTransform(hull_poly_boundaries, CRS=PROJ.USE)



# Input the trawl survey data
# Spring
dat <- read_excel("//net.nefsc.noaa.gov/home2/aharris/DisMap data/sf bsb survey-points-data spring.xlsx")
VTRs<- st_as_sf(VTR_boundaries)

sp_points = st_as_sf(dat,coords = c('lon' ,'lat' ))#make points spatial
st_crs(sp_points)= 4326
sp_points=st_transform(sp_points,st_crs(VTRs)) # Match the point and polygon CRS

sp_points$state_name <- apply(st_intersects(VTRs, sp_points, sparse = FALSE), 2, 
                                function(col) {VTRs[which(col), ]$State}) 
sp_points$state_name<-as.character(sp_points$state_name)

write_xlsx(sp_points,"\\\\net.nefsc.noaa.gov/aharris/DisMap data/Spring trawl haul locations.xlsx")


#Check to see if trawl survey hauls that are not associated with a state indeed fall outside the state biomass access areas
sp_points_outside<- subset(sp_points, state_name=="character(0)")
dat_merge<- subset(dat, select=c("haulid", "lat", "lon"))
sp_points_outside = merge(sp_points_outside, dat_merge, by = "haulid", all.y=FALSE)

sp_points_inside<- subset(sp_points, state_name!="character(0)")
sp_points_inside = merge(sp_points_inside, dat_merge, by = "haulid", all.y=FALSE)


#The following graph produced points of all the trawl hauls; those in red are outside the state access areas
#and those in blue are inside the state areas. 
ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#D1D1E0", color = "black") +
  geom_point(data = sp_points_outside, aes(x = lon, y = lat), colour = "red", size = 1, shape = 20) +
  geom_point(data = sp_points_inside, aes(x = lon, y = lat), colour = "black", size = 1, shape = 20) +
  geom_polygon(data = lat_lon_hulls, aes(x = lon, y = lat, group = State, colour = State), alpha = 0.3, show.legend = FALSE) +
  coord_sf(xlim = c(-80, -64), ylim = c(34, 46)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


# Fall
dat <- read_excel("//net.nefsc.noaa.gov/home2/aharris/DisMap data/sf bsb survey-points-data fall.xlsx")
VTRs<- st_as_sf(VTR_boundaries)

sp_points = st_as_sf(dat,coords = c('lon','lat'))#make points spatial
st_crs(sp_points)= 4326 # Give the points a coordinate reference system (CRS)
sp_points=st_transform(sp_points,st_crs(VTRs)) # Match the point and polygon CRS

sp_points$state_name <- apply(st_intersects(VTRs, sp_points, sparse = FALSE), 2, 
                              function(col) {VTRs[which(col), ]$State}) 

sp_points$state_name<-as.character(sp_points$state_name)

write_xlsx(sp_points,"\\\\net.nefsc.noaa.gov/aharris/DisMap data/Fall trawl haul locations.xlsx")


#Check to see if trawl survey hauls that are not associated with a state indeed fall outside the state biomass access areas
sp_points_outside<- subset(sp_points, state_name=="character(0)")
dat_merge<- subset(dat, select=c("haulid", "lat", "lon"))
sp_points_outside = merge(sp_points_outside, dat_merge, by = "haulid", all.y=FALSE)

sp_points_inside<- subset(sp_points, state_name!="character(0)")
sp_points_inside = merge(sp_points_inside, dat_merge, by = "haulid", all.y=FALSE)


#The following graph produced points of all the trawl hauls; those in red are outside the state access areas
#and those in blue are inside the state areas. 
ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#D1D1E0", color = "black") +
  geom_point(data = sp_points_outside, aes(x = lon, y = lat), colour = "red", size = 1, shape = 20) +
  geom_point(data = sp_points_inside, aes(x = lon, y = lat), colour = "black", size = 1, shape = 20) +
  geom_polygon(data = lat_lon_hulls, aes(x = lon, y = lat, group = State, colour = State), alpha = 0.3, show.legend = FALSE) +
  coord_sf(xlim = c(-80, -64), ylim = c(34, 46)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
