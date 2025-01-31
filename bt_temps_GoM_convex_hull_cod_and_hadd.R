
# load packages 
pkgs_to_use <- c("sf", "ncdf4", "raster","rasterVis", "RColorBrewer" , "chron", "lattice", "RColorBrewer", "rlist", "data.table", 
                 "rlang","foreign","tidyverse", "sp", "sf", "rgeos","rgdal", "here", "scales", "raster", "stringi", "ggplot2", "tigris",
                 "dplyr", "writexl", "readxl","mapview","geosphere", "sf",  "terra", "maps", "spatialEco", "ggsn", 
                 "ggspatial", "ggrepel", "marmap")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
library(readxl)
library(writexl)
# set path and filename
# ncpath <- "\\\\net.nefsc.noaa.gov/socialsci/Bottom_temperature/"
# ncname <- "roms_glorys_psy_monthly_1959_2021"  
# ncfname <- paste(ncpath, ncname, ".nc", sep="")
# dname <- "bt_temp"  # note: tmp means temperature (not temporary)


ncpath <- "\\\\net.nefsc.noaa.gov/aharris/DisMap data/"
ncname <- "bottom_temp_combined_product_1959_2022_soe"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "sea_water_temperature_at_sea_floor"  # note: tmp means temperature (not temporary)


ncin <- nc_open(ncfname)
print(ncin)

ncin

# get longitude and latitude
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"latitude")
nlat <- dim(lat)
head(lat)

# get time
time <- ncvar_get(ncin, "year")
tunits <- ncatt_get(ncin,"year","units")
nt <- dim(time)
nt

# get temperature
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)


# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")



#Loop through years to get bt_tmp by month. Then combine all the years into one dataset

lat_lons = list()
lat_lons_all = list()


for (m in 1:dim(time)){
  dim(tmp_array)
  
  m <- m #get a single slice or layer (year m)
  tmp_slice <- tmp_array[,,,m]
  dim(tmp_slice)
  
  # # levelplot of the slice
  # grid <- expand.grid(lon=lon, lat=lat)
  #  levelplot(tmp_slice ~ lon * lat, data=grid,   pretty=T, 
  #            col.regions=(rev(brewer.pal(10,"RdBu"))))
  
  
  # create dataframe -- reshape data
  # matrix (nlon*nlat rows by 2 cols) of lons and lats
  lonlat <- as.matrix(expand.grid(lon,lat))
  dim(lonlat)
  
  
  # vector of `tmp` values
  tmp_vec <- as.vector(tmp_slice)
  length(tmp_vec)
  
  # reshape the array into vector
  tmp_vec_long <- as.vector(tmp_vec)
  length(tmp_vec_long)
  head(tmp_vec_long)
  
  #tmp_vec_long<-na.omit(tmp_vec_long)
  # reshape the vector into a matrix
  tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=366) #one column for each day +1 to align with data
  dim(tmp_mat)
  
  
  head(na.omit(tmp_mat))
  
  
  # create a dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  tmp_df02 <- data.frame(cbind(lonlat,tmp_mat))
  
  #Reshape the columns to days, noting that the first two are the lon lats
  tmp_df02_long <- tmp_df02 %>% 
    pivot_longer(
      cols = 'V3':'V368', 
      names_to = "day",
      values_to = "value"
    )
  
  
  
  tmp_df02_long<-na.omit(tmp_df02_long) #remove lon-lats with no bt_data
  tmp_df02_long$day<-substring(tmp_df02_long$day, 2) #extract the day of year
  tmp_df02_long$day<-as.numeric(tmp_df02_long$day) 
  tmp_df02_long$day<-tmp_df02_long$day-3 #subtract 3 from day because there were two extra columns for lat and lon 
  # in the reshape, and in the code below, as.date starts at 0 
  tmp_df02_long$date<-as.Date(tmp_df02_long$day, origin = "0001-01-01") # day of the year to date 
  tmp_df02_long$month <- format(tmp_df02_long$date, "%m") #extract the month 
  
  
  mean_tmps <- tmp_df02_long %>% 
    dplyr::select(-c("day", "date")) %>% 
    data.table()
  mean_tmps <- mean_tmps[, lapply(.SD, mean), by=list(Var1,Var2, month)] %>% 
    tibble() #%>% #collpase daily bt_tmpe by month
  
  names(mean_tmps) <- c("lon","lat","month", "bt_tmp")
  
  lat_lons[[m]] <- mean_tmps
  lat_lons[[m]]$year<- 1958+m #add year, which starts at 1959
  
}
lat_lons_all= rbindlist(lat_lons, fill=TRUE)
ls(lat_lons_all)

# lat_lons_all_check<-lat_lons_all %>% 
#   dplyr::filter(year==2020)
# 
# 
# ggplot(lat_lons_all_check)+aes(x=month,y=bt_tmp)+geom_boxplot()


lat_lons_all_long <-lat_lons_all
write_xlsx(lat_lons_all_long,"\\\\net.nefsc.noaa.gov/aharris/DisMap data/hist_monthly_bt_tmps.xlsx")


lat_lons_all_long<-read_excel("\\\\net.nefsc.noaa.gov/aharris/DisMap data/hist_monthly_bt_tmps.xlsx")

###GoM shape file 
PROJ.USE = CRS('+proj=aea +lat_1=28 +lat_2=42 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ')

GoM_shape <- st_read(dsn="\\\\net.nefsc.noaa.gov/aharris/DisMap data/GOM-GB_Regulated_Mesh_Areas",
                     layer="GOM-GB_regulated_Mesh_Areas")

GoM_shape <-GoM_shape %>% 
  dplyr::filter(COMMNAME=="GOM RMA")

#plot(GoM_shape['AREANAME'])


states <- map_data("state")
east_coast <- subset(states, region %in% c("maine", "massachusetts", "vermont", "new hampshire","rhode island", "connecticut", "new york", "pennsylvania",
                                           "new jersey", "maryland", "delaware", "district of columbia", "virginia", "west virginia", "north carolina"))
#east_coast <- subset(states, region %in% c("maine", "massachusetts", "vermont", "new hampshire","rhode island", "connecticut", "new york"))



#####################################
#####################################
###########Input the VTR points######
#####################################
#####################################


#Create a single access area for each state based on VTR points for all years combined
vtr_lat_lons_cod<-read_csv("//net.nefsc.noaa.gov/home2/aharris/DisMap data/Yearly_Cod_Recreational_Lat_Lon.csv")
vtr_lat_lons_hadd<-read_csv("//net.nefsc.noaa.gov/home2/aharris/DisMap data/Yearly_Haddock_Recreational_Lat_Lon.csv")

vtr_lat_lons_both<-rbind(vtr_lat_lons_cod, vtr_lat_lons_hadd)
vtr_lat_lons_both<-vtr_lat_lons_both %>% 
  dplyr::mutate(Species="BOTH")

lat_lons_ready <- vtr_lat_lons_both %>%
  dplyr::select(Species, LAT, LON, YEAR) %>%
  drop_na()


##Calculate the mean lat/lon by state
lat_lons_ready <- lat_lons_ready %>%  
  group_by(Species) %>%
  mutate(mean_lat = mean(LAT))  %>%
  mutate(mean_lon = mean(LON)) %>% 
  ungroup()

##Calculate the distance of each lat/lon to the centroid by state
library(geosphere)

lat_lons_ready$distance<-distGeo(lat_lons_ready[,2:3], lat_lons_ready[,5:6])

###
#lat_lons_ready_2020<-lat_lons_ready %>%
#  dplyr::filter(YEAR==2005)

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



# Before taking the 95th and creating the convex hull, keep only VTR lat lons falling within the GoM mgt. area
# lat_lons_ready_2020<-lat_lons_ready %>%
#   dplyr::filter(YEAR==2005)

mgt_boundaries_subset<- st_as_sf(GoM_shape) 
mgt_boundaries_subset<-mgt_boundaries_subset %>% 
  dplyr::select(geometry, COMMNAME) 

sp_points = st_as_sf(lat_lons_ready,coords = c('LON' ,'LAT' ))#make points spatial
st_crs(sp_points)= 4326


mgt_boundaries_subset=st_transform(mgt_boundaries_subset,st_crs(sp_points)) # Match the point and polygon CRS

#VTR_GoM_points <- st_intersection(mgt_boundaries_subset, sp_points) #test to see it works

VTR_GoM_points2 <- sf::st_join(sp_points, mgt_boundaries_subset)

VTR_GoM_points2_outside <- VTR_GoM_points2 %>% 
  dplyr::filter(is.na(COMMNAME))
VTR_GoM_points2_outside = data.frame(st_coordinates(st_cast(VTR_GoM_points2_outside$geometry,"POINT")))
VTR_GoM_points2_outside<-VTR_GoM_points2_outside %>% 
  dplyr::rename(LON=X, LAT=Y)

VTR_GoM_points2_inside <- VTR_GoM_points2 %>% 
  dplyr::filter(!is.na(COMMNAME))
VTR_GoM_points2_inside = data.frame(st_coordinates(st_cast(VTR_GoM_points2_inside$geometry,"POINT")))
VTR_GoM_points2_inside<-VTR_GoM_points2_inside %>% 
  dplyr::rename(LON=X, LAT=Y)

ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
  #geom_text(aes(x = long, y = lat, label = region))+
  geom_point(data = VTR_GoM_points2_outside, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
  geom_point(data = VTR_GoM_points2_inside, aes(x = LON, y = LAT), color = "black", alpha = 0.3, show.legend = FALSE) +
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



###Now use only VTR points within the GoM to make convex hull
VTR_GoM_points2_inside <- VTR_GoM_points2_inside %>%  
  mutate(LON_mean = mean(LON))  %>%
  mutate(LAT_mean = mean(LAT)) 

##Calculate the distance of each lat/lon to the centroid and 95th percentile 
VTR_GoM_points2_inside$distance<-distGeo(VTR_GoM_points2_inside[,1:2], VTR_GoM_points2_inside[,3:4])

VTR_GoM_points2_inside <- VTR_GoM_points2_inside %>%  
  mutate(q95 = quantile(distance, 0.95)) 

VTR_GoM_points2_inside2 <- VTR_GoM_points2_inside %>%  
  dplyr::filter(distance<=q95)

VTR_GoM_points2_inside3 <- VTR_GoM_points2_inside %>%  
  dplyr::filter(distance>q95)

ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
  #geom_text(aes(x = long, y = lat, label = region))+
  #geom_point(data = VTR_GoM_points2_outside, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
  geom_point(data = VTR_GoM_points2_inside3, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
  geom_point(data = VTR_GoM_points2_inside2, aes(x = LON, y = LAT), color = "black", alpha = 0.3, show.legend = FALSE) +
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




##Now create convex hull
lat_lon_hulls <- VTR_GoM_points2_inside2 %>%
  slice(chull(LAT, LON))


# Create convex lat_lon_hulls shapefile
hull_poly <- lat_lon_hulls %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# write to shapefile
# st_write(hull_poly, dsn = "//net.nefsc.noaa.gov/home2/aharris/DisMap data/VTR_chulls/GoM_cod_hull_all_years.shp", 
#          layer = "GoM_cod_hull_all_years", driver = "ESRI Shapefile", delete_layer = TRUE) 
# 
# ##import and transform the hull poly's
# hull_poly_boundaries <- readOGR( dsn="\\\\net.nefsc.noaa.gov/aharris/DisMap data/VTR_chulls",
#                                  layer="GoM_cod_hull_all_years",
#                                  stringsAsFactors=FALSE, verbose=FALSE)
# 
# VTR_boundaries <- spTransform(hull_poly_boundaries, CRS=4326)


hull_poly<-hull_poly %>% 
  dplyr::mutate(area="VTR_convex_hull")

hull_poly2 = data.frame(st_coordinates(st_cast(hull_poly$geometry,"POINT")))
hull_poly2<-hull_poly2 %>% 
  dplyr::rename(LON=X, LAT=Y) %>% 
  dplyr::mutate(area="VTR_convex_hull")


ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
  geom_point(data = VTR_GoM_points2_inside3, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
  geom_point(data = VTR_GoM_points2_inside2, aes(x = LON, y = LAT), color = "black", alpha = 0.3, show.legend = FALSE) +
  geom_polygon(data = hull_poly2, aes(x = LON, y = LAT,group = area), color = "black", alpha = 0.3, show.legend = FALSE) +
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



####
hull_poly3<-hull_poly2[!duplicated(hull_poly2), ] #remove duplicates points from convex hull

VTR_boundaries_subset<- st_as_sf(hull_poly3, coords = c('LON' ,'LAT' )) 
VTR_boundaries_subset <- VTR_boundaries_subset %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  mutate(area="VTR_convex_hull")


sp_points = st_as_sf(lat_lons_all_long,coords = c('lon' ,'lat' ))#make points spatial
st_crs(sp_points)= 4326
st_crs(VTR_boundaries_subset)= 4326

VTR_boundaries_subset=st_transform(VTR_boundaries_subset,st_crs(sp_points)) # Match the point and polygon CRS

VTR_boundaries2 <- sf::st_join(sp_points, VTR_boundaries_subset)
##now make sure I have correctly bt_temps inside and outside the convex hull

VTR_boundaries2_outside <- VTR_boundaries2 %>% 
  dplyr::filter(is.na(area))
VTR_boundaries2_outside = data.frame(st_coordinates(st_cast(VTR_boundaries2_outside$geometry,"POINT")))
VTR_boundaries2_outside<-VTR_boundaries2_outside %>% 
  dplyr::rename(LON=X, LAT=Y)


VTR_boundaries2_inside <- VTR_boundaries2 %>% 
  dplyr::filter(!is.na(area))
VTR_boundaries2_inside = data.frame(st_coordinates(st_cast(VTR_boundaries2_inside$geometry,"POINT")))
VTR_boundaries2_inside<-VTR_boundaries2_inside %>% 
  dplyr::rename(LON=X, LAT=Y)


ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
  geom_point(data = VTR_boundaries2_outside, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
  geom_point(data = VTR_boundaries2_inside, aes(x = LON, y = LAT), color = "black", alpha = 0.3, show.legend = FALSE) +
  geom_polygon(data = hull_poly3, aes(x = LON, y = LAT,group = area), color = "black", alpha = 0.3, show.legend = FALSE) +
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



###now retain only the bt_tmp points within the convex hull
VTR_boundaries2_inside <- VTR_boundaries2 %>% 
  dplyr::filter(!is.na(area))


ggplot(VTR_boundaries2_inside)+aes(x=month,y=bt_tmp)+geom_boxplot()

inside_bt_tmps_aggregate<-VTR_boundaries2_inside %>% 
  dplyr::group_by(month, year) %>% 
  dplyr::summarize(mean_bt_tmp=mean(bt_tmp), 
                   sd_bt_tmp=sd(bt_tmp), .groups="drop") %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)


write_xlsx(inside_bt_tmps_aggregate,"\\\\net.nefsc.noaa.gov/aharris/DisMap data/hist_bt_temps_cod_and_haddVTR_cnvx_hull.xlsx")
inside_bt_tmps_aggregate<-inside_bt_tmps_aggregate %>% 
  mutate(year=as.factor(year))
ggplot(inside_bt_tmps_aggregate)+aes(x=year,y=mean_bt_tmp)+geom_boxplot()


cod<-read_excel("\\\\net.nefsc.noaa.gov/aharris/DisMap data/hist_bt_temps_codVTR_cnvx_hull.xlsx")
hadd<-read_excel("\\\\net.nefsc.noaa.gov/aharris/DisMap data/hist_bt_temps_haddVTR_cnvx_hull.xlsx")
both<-read_excel("\\\\net.nefsc.noaa.gov/aharris/DisMap data/hist_bt_temps_cod_and_haddVTR_cnvx_hull.xlsx")

mean(cod$mean_bt_tmp)
mean(hadd$mean_bt_tmp)
mean(both$mean_bt_tmp)





