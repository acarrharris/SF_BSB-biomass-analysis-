# load packages 
pkgs_to_use <- c("sf", "ncdf4", "raster","rasterVis", "RColorBrewer" , "chron", "lattice", "RColorBrewer", "rlist", "data.table", 
                 "rlang","foreign","tidyverse", "sp", "sf", "here", "scales", "raster", "stringi", "ggplot2", "tigris",
                 "dplyr", "writexl", "readxl","mapview","geosphere", "sf",  "terra", "maps", "spatialEco", "WriteXLS")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
options(scipen=999)

#Pull in east coast map

states <- map_data("state")
east_coast <- subset(states, region %in% c("maine", "massachusetts", "vermont", "new hampshire","rhode island", "connecticut", "new york", "pennsylvania",
                                           "new jersey", "maryland", "delaware", "district of columbia", "virginia", "west virginia", "north carolina"))

GoM_shape <- st_read(dsn="U:/Stock_Areas",
                     layer="Stock_Areas")

GoM_shape <-GoM_shape %>% 
  dplyr::filter(AREANAME  %in% c("GOM Cod Stock Area"))

#####################################
#####################################
###########Input the VTR points######
#####################################
#####################################


#Create a single access area for each state based on VTR points for all years combined
vtr_lat_lons_cod<-read_csv("U:/Yearly_Cod_Recreational_Lat_Lon.csv", show_col_types = FALSE)
vtr_lat_lons_hadd<-read_csv("U:/Yearly_Haddock_Recreational_Lat_Lon.csv", show_col_types = FALSE)

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


# ggplot(east_coast) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
#   geom_point(data = lat_lons_ready, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
#   geom_sf(data = GoM_shape['AREANAME'], fill = "blue", alpha = 0.3, show.legend = FALSE) +
#   coord_sf(xlim = c(-74.6, -66), ylim = c(40, 44.7)) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   ggspatial::annotation_scale(
#     location = "br",
#     bar_cols = c("grey60", "white")
#   ) +
#   ggspatial::annotation_north_arrow(
#     location = "tl", which_north = "true",
#     pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
#     style = ggspatial::north_arrow_nautical(
#       fill = c("grey40", "white"),
#       line_col = "grey20"    )
#   )


# Before taking the 95th percentile and creating the convex hull, keep only VTR lat lons falling within the GoM mgt. area
mgt_boundaries_subset<- st_as_sf(GoM_shape) 
mgt_boundaries_subset<-mgt_boundaries_subset %>% 
  dplyr::select(geometry, COMMNAME) 

sp_points = st_as_sf(lat_lons_ready,coords = c('LON' ,'LAT' ))#make points spatial
st_crs(sp_points)= 4326

mgt_boundaries_subset=st_transform(mgt_boundaries_subset,st_crs(sp_points)) # Match the point and polygon CRS

VTR_GoM_points2 <- sf::st_join(sp_points, mgt_boundaries_subset)


#Allocate spatial points as outside the GOM if after the st_join the COMMNAME is missing 
VTR_GoM_points2_outside <- VTR_GoM_points2 %>% 
  dplyr::filter(is.na(COMMNAME))
VTR_GoM_points2_outside = data.frame(st_coordinates(st_cast(VTR_GoM_points2_outside$geometry,"POINT")))
VTR_GoM_points2_outside<-VTR_GoM_points2_outside %>% 
  dplyr::rename(LON=X, LAT=Y)

#Allocate spatial points as inside the GOM if the COMMNAME is not missing 
VTR_GoM_points2_inside <- VTR_GoM_points2 %>% 
  dplyr::filter(!is.na(COMMNAME))
VTR_GoM_points2_inside = data.frame(st_coordinates(st_cast(VTR_GoM_points2_inside$geometry,"POINT")))
VTR_GoM_points2_inside<-VTR_GoM_points2_inside %>% 
  dplyr::rename(LON=X, LAT=Y)

#Check to make sure points are correctly allocated within the VTR convex hull area. 
# ggplot(east_coast) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
#   geom_point(data = VTR_GoM_points2_outside, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
#   geom_point(data = VTR_GoM_points2_inside, aes(x = LON, y = LAT), color = "black", alpha = 0.3, show.legend = FALSE) +
#   geom_sf(data = GoM_shape['AREANAME'], fill = "blue", alpha = 0.3, show.legend = FALSE) +
#   coord_sf(xlim = c(-74.6, -66), ylim = c(40, 44.7)) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   ggspatial::annotation_scale(
#     location = "br",
#     bar_cols = c("grey60", "white")
#   ) +
#   ggspatial::annotation_north_arrow(
#     location = "tl", which_north = "true",
#     pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
#     style = ggspatial::north_arrow_nautical(
#       fill = c("grey40", "white"),
#       line_col = "grey20"    )
#   )


###Now use only VTR points within the GoM to make convex hull
VTR_GoM_points2_inside <- VTR_GoM_points2_inside %>%  
  mutate(LON_mean = mean(LON))  %>%
  mutate(LAT_mean = mean(LAT)) ##centroid

##Calculate the distance of each lat/lon to the centroid and 95th percentile 
library(geosphere)
VTR_GoM_points2_inside$distance<-distGeo(VTR_GoM_points2_inside[,1:2], VTR_GoM_points2_inside[,3:4])

VTR_GoM_points2_inside <- VTR_GoM_points2_inside %>%  
  mutate(q95 = quantile(distance, 0.95)) 

VTR_GoM_points2_inside2 <- VTR_GoM_points2_inside %>%  
  dplyr::filter(distance<=q95)

VTR_GoM_points2_inside3 <- VTR_GoM_points2_inside %>%  
  dplyr::filter(distance>q95)

# ggplot(east_coast) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
#   geom_point(data = VTR_GoM_points2_inside3, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
#   geom_point(data = VTR_GoM_points2_inside2, aes(x = LON, y = LAT), color = "black", alpha = 0.3, show.legend = FALSE) +
#   geom_sf(data = GoM_shape['AREANAME'], fill = "blue", alpha = 0.3, show.legend = FALSE) +
#   coord_sf(xlim = c(-74.6, -66), ylim = c(40, 44.7)) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   ggspatial::annotation_scale(
#     location = "br",
#     bar_cols = c("grey60", "white")
#   ) +
#   ggspatial::annotation_north_arrow(
#     location = "tl", which_north = "true",
#     pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
#     style = ggspatial::north_arrow_nautical(
#       fill = c("grey40", "white"),
#       line_col = "grey20"    )
#   )



##Now create convex hull
lat_lon_hulls <- VTR_GoM_points2_inside2 %>%
  slice(chull(LAT, LON))

# Create convex lat_lon_hulls shapefile
hull_poly <- lat_lon_hulls %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

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

st_crs(VTR_boundaries_subset)= 4326

#sp_points = st_as_sf(lat_lons_all_long,coords = c('lon' ,'lat' ))#make points spatial
st_crs(sp_points)= 4326

VTR_boundaries_subset=st_transform(VTR_boundaries_subset,st_crs(sp_points)) # Match the point and polygon CRS




#Pull in bt temp deltas
# set path and filename
ncpath <- "U:/"
ncname <- "CM2.6_2xCO2_USNES_bottom_temp_delta_monthly_years_1_40"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "TEMP_BOTTOM_DELTA"  # note: tmp means temperature (not temporary)


ncin <- nc_open(ncfname)
print(ncin)

ncin

# get longitude and latitude
lon <- ncvar_get(ncin,"XT_OCEAN2001_225021_150")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"YT_OCEAN1560_180961_177")
nlat <- dim(lat)
head(lat)

# get time
time <- ncvar_get(ncin, "TIME")
tunits <- ncatt_get(ncin,"TIME","units")
nt <- dim(time)
print(nt)
print(tunits)
length(tunits)


#obtain time dimension in date format
# install.packages("ncdf4")
# install.packages("ncdf4.helpers")
# library(ncdf4.helpers)
# 
# Time_in_Date_format <- nc.get.time.series(f = ncin, time.dim.name = "TIME")
# 
# 
# time_obs <- as.POSIXct(time, origin = "01-JAN-0001", tz="GMT")
# dim(time_obs) #should be 480
# range(time_obs)


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

dim(time)
for (m in 1:dim(time)){
  
  #m <- m #get a single slice or layer (year m)
  tmp_slice <- tmp_array[,,m]
  dim(tmp_slice)
  
  # levelplot of the slice
  grid <- expand.grid(lon=lon, lat=lat)
  # levelplot(tmp_slice ~ lon * lat, data=grid,   pretty=T, 
  #           col.regions=(rev(brewer.pal(10,"RdBu"))))
  
  
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
  
  
  # reshape the vector into a matrix
  tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat)
  dim(tmp_mat)
  
  
  head(na.omit(tmp_mat))
  
  
  # create a dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  tmp_df2 <- data.frame(cbind(lonlat,tmp_mat))
  names(tmp_df2) <- c("lon","lat","tmp")
  
  
  tmp_df2 <- na.omit(tmp_df2)
  tmp_df2$month <- m
  
  lat_lons[[m]] <- tmp_df2
  
}
bt_temp_deltas= rbindlist(lat_lons, fill=TRUE)
ls(bt_temp_deltas)
sp_points = st_as_sf(bt_temp_deltas,coords = c('lon' ,'lat'), crs = 4326) #make points spatial

# #Read in stock area shapefiles 
# mgt_boundaries <- st_read(dsn="U:/Stock_Areas/", layer="Stock_Areas", stringsAsFactors=FALSE)
# mgt_boundaries_subset <- subset(mgt_boundaries, AREANAME  %in% c("GOM Cod Stock Area"))
# 
# 
# ##Now find overlap between the bt_tmp lat lons and mgt_boundaries_subset
# # Input the bt_tmp data 
# mgt_boundaries_subset<- st_as_sf(mgt_boundaries_subset)
# mgt_boundaries_subset$region<-"GOM"
# mgt_boundaries_subset <- st_transform(mgt_boundaries_subset, crs = 4326)
# 
# #plot GOM mgt boundary
# ggplot(mgt_boundaries_subset) +
#   geom_sf(aes(group = region), fill = "#D1D1E0", color = "black")

###Check
sp_points_2020 <- subset(bt_temp_deltas, month==480)
sp_points_2020 = st_as_sf(sp_points_2020,coords = c('lon' ,'lat'), crs = 4326) #make points spatial
sp_points_2020$region<-"GOM"

ggplot(sp_points_2020) +
  geom_sf(data=VTR_boundaries_subset, aes(group = area), fill = "#D1D1E0", color = "black")+
  geom_sf(aes(group = region), fill = "#D1D1E0", color = "red")
###

sp_points=st_transform(sp_points,st_crs(VTR_boundaries_subset)) # Match the point and polygon CRS

# #Test intersection of bt_temp and mgt_boundaries_subset polygon - full intersection takes a while
# sp_points_check1<- sample_n(sp_points, 100000)
# sp_points_check1 <- sf::st_join(sp_points_check1, VTR_boundaries_subset)
# sp_points_check1<-as.data.frame(sp_points_check1)
# sp_points_check1<-sp_points_check1 %>%
#   dplyr::select(area, tmp, geometry, month)
# sp_points_check1<-sp_points_check1 %>% replace_na(list(area = 'outside'))
# 
# #Allocate spatial points as outside the GOM if region=="outside"
# sp_points_check1_outside <- sp_points_check1 %>%
#   dplyr::filter(area=="outside")
# sp_points_check1_outside = data.frame(st_coordinates(st_cast(sp_points_check1_outside$geometry,"POINT")))
# sp_points_check1_outside<-sp_points_check1_outside %>%
#   dplyr::rename(LON=X, LAT=Y)
# 
# #Allocate spatial points as inside the GOM if region=="GOM"
# sp_points_check1_inside <- sp_points_check1 %>%
#   dplyr::filter(area=="VTR_convex_hull")
# sp_points_check1_inside = data.frame(st_coordinates(st_cast(sp_points_check1_inside$geometry,"POINT")))
# sp_points_check1_inside<-sp_points_check1_inside %>%
#   dplyr::rename(LON=X, LAT=Y)
# 
# ggplot(east_coast) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
#   geom_point(data = sp_points_check1_outside, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
#   geom_point(data = sp_points_check1_inside, aes(x = LON, y = LAT), color = "black", alpha = 0.3, show.legend = FALSE) +
#   geom_sf(data = VTR_boundaries_subset['area'], fill = "blue", alpha = 0.3, show.legend = FALSE) +
#   coord_sf(xlim = c(-74.6, -66), ylim = c(40, 44.7)) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   ggspatial::annotation_scale(
#     location = "br",
#     bar_cols = c("grey60", "white")
#   ) +
#   ggspatial::annotation_north_arrow(
#     location = "tl", which_north = "true",
#     pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
#     style = ggspatial::north_arrow_nautical(
#       fill = c("grey40", "white"),
#       line_col = "grey20"    )
#   )
# # 
# #collapse mean bottom temperatures by year and month
# GOM_bt_temps_check<- sp_points_check1 %>%
#   dplyr::filter(area=="VTR_convex_hull") %>%
#   as.data.frame()  %>% select(c(month, tmp)) %>%
#   dplyr::mutate(month=as.factor(month))
# 
# GOM_mean_bt_temps_check<-GOM_bt_temps_check %>%
#   dplyr::group_by(month) %>%
#   dplyr::summarise(mean_temp=mean(tmp))
# 
# ggplot(GOM_bt_temps_check)+aes(x=month,y=tmp)+geom_boxplot()



#full data intersection
kept_points_all <- sf::st_join(sp_points, VTR_boundaries_subset)
kept_points_all1<-kept_points_all %>% 
  dplyr::select(area, tmp, month, geometry)
kept_points_all1<-kept_points_all1 %>% replace_na(list(area = 'outside'))
kept_points_all1<-kept_points_all1 %>% 
  dplyr::filter(area=="VTR_convex_hull")


#collapse mean bottom temperatures by year and month
GOM_bt_temps<- kept_points_all1 %>% as.data.frame()  %>% select(c(month, tmp)) %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise(mean_temp=mean(tmp))


GOM_bt_temps$year <- c(0, rep(1:(nrow(GOM_bt_temps)-1)%/%12))
GOM_bt_temps$year <-GOM_bt_temps$year+1

#save the within-GOM mean bottom temps by month 
write_xlsx(GOM_bt_temps,"U:/mean_bottom_temp_deltas_GOM_convex_hull_yrs_1_40.xlsx")




###now repeat the whole thing for years 41-80

# set path and filename
ncpath <- "U:/"
ncname <- "CM2.6_2xCO2_USNES_bottom_temp_delta_monthly_years_41_80"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "TEMP_BOTTOM_DELTA"  # note: tmp means temperature (not temporary)


ncin <- nc_open(ncfname)
print(ncin)

ncin

# get longitude and latitude
lon <- ncvar_get(ncin,"XT_OCEAN2001_225021_150")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"YT_OCEAN1560_180961_177")
nlat <- dim(lat)
head(lat)

# get time
time <- ncvar_get(ncin, "TIME")
tunits <- ncatt_get(ncin,"TIME","units")
nt <- dim(time)
print(nt)
print(tunits)
length(tunits)


#obtain time dimension in date format
# install.packages("ncdf4")
# install.packages("ncdf4.helpers")
# library(ncdf4.helpers)
# 
# Time_in_Date_format <- nc.get.time.series(f = ncin, time.dim.name = "TIME")
# 
# 
# time_obs <- as.POSIXct(time, origin = "01-JAN-0001", tz="GMT")
# dim(time_obs) #should be 480
# range(time_obs)




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

dim(time)
for (m in 1:dim(time)){
  
  #m <- m #get a single slice or layer (year m)
  tmp_slice <- tmp_array[,,m]
  dim(tmp_slice)
  
  # levelplot of the slice
  grid <- expand.grid(lon=lon, lat=lat)
  # levelplot(tmp_slice ~ lon * lat, data=grid,   pretty=T, 
  #           col.regions=(rev(brewer.pal(10,"RdBu"))))
  
  
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
  
  
  # reshape the vector into a matrix
  tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat)
  dim(tmp_mat)
  
  
  head(na.omit(tmp_mat))
  
  
  # create a dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  tmp_df2 <- data.frame(cbind(lonlat,tmp_mat))
  names(tmp_df2) <- c("lon","lat","tmp")
  
  
  tmp_df2 <- na.omit(tmp_df2)
  tmp_df2$month <- m
  
  lat_lons[[m]] <- tmp_df2
  
}
bt_temp_deltas= rbindlist(lat_lons, fill=TRUE)
ls(bt_temp_deltas)
sp_points = st_as_sf(bt_temp_deltas,coords = c('lon' ,'lat'), crs = 4326) #make points spatial


sp_points=st_transform(sp_points,st_crs(VTR_boundaries_subset)) # Match the point and polygon CRS

# #Test intersection of bt_temp and mgt_boundaries_subset polygon - full intersection takes a while
# sp_points_check1<- sample_n(sp_points, 100000)
# sp_points_check1 <- sf::st_join(sp_points_check1, VTR_boundaries_subset)
# sp_points_check1<-as.data.frame(sp_points_check1)
# sp_points_check1<-sp_points_check1 %>%
#   dplyr::select(area, tmp, geometry, month)
# sp_points_check1<-sp_points_check1 %>% replace_na(list(area = 'outside'))
# 
# #Allocate spatial points as outside the GOM if region=="outside"
# sp_points_check1_outside <- sp_points_check1 %>%
#   dplyr::filter(area=="outside")
# sp_points_check1_outside = data.frame(st_coordinates(st_cast(sp_points_check1_outside$geometry,"POINT")))
# sp_points_check1_outside<-sp_points_check1_outside %>%
#   dplyr::rename(LON=X, LAT=Y)
# 
# #Allocate spatial points as inside the GOM if region=="GOM"
# sp_points_check1_inside <- sp_points_check1 %>%
#   dplyr::filter(area=="VTR_convex_hull")
# sp_points_check1_inside = data.frame(st_coordinates(st_cast(sp_points_check1_inside$geometry,"POINT")))
# sp_points_check1_inside<-sp_points_check1_inside %>%
#   dplyr::rename(LON=X, LAT=Y)
# 
# ggplot(east_coast) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "grey40", alpha = 0.3, color = "black") +
#   geom_point(data = sp_points_check1_outside, aes(x = LON, y = LAT), color = "red", alpha = 0.3, show.legend = FALSE) +
#   geom_point(data = sp_points_check1_inside, aes(x = LON, y = LAT), color = "black", alpha = 0.3, show.legend = FALSE) +
#   geom_sf(data = VTR_boundaries_subset['area'], fill = "blue", alpha = 0.3, show.legend = FALSE) +
#   coord_sf(xlim = c(-74.6, -66), ylim = c(40, 44.7)) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   ggspatial::annotation_scale(
#     location = "br",
#     bar_cols = c("grey60", "white")
#   ) +
#   ggspatial::annotation_north_arrow(
#     location = "tl", which_north = "true",
#     pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
#     style = ggspatial::north_arrow_nautical(
#       fill = c("grey40", "white"),
#       line_col = "grey20"    )
#   )
# # 
# #collapse mean bottom temperatures by year and month
# GOM_bt_temps_check<- sp_points_check1 %>%
#   dplyr::filter(area=="VTR_convex_hull") %>%
#   as.data.frame()  %>% select(c(month, tmp)) %>%
#   dplyr::mutate(month=as.factor(month))
# 
# GOM_mean_bt_temps_check<-GOM_bt_temps_check %>%
#   dplyr::group_by(month) %>%
#   dplyr::summarise(mean_temp=mean(tmp))
# 
# ggplot(GOM_bt_temps_check)+aes(x=month,y=tmp)+geom_boxplot()



#full data intersection
kept_points_all <- sf::st_join(sp_points, VTR_boundaries_subset)
kept_points_all1<-kept_points_all %>% 
  dplyr::select(area, tmp, month, geometry)
kept_points_all1<-kept_points_all1 %>% replace_na(list(area = 'outside'))
kept_points_all1<-kept_points_all1 %>% 
  dplyr::filter(area=="VTR_convex_hull")


#collapse mean bottom temperatures by year and month
GOM_bt_temps<- kept_points_all1 %>% as.data.frame()  %>% select(c(month, tmp)) %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise(mean_temp=mean(tmp))

GOM_bt_temps$year <- c(0, rep(1:(nrow(GOM_bt_temps)-1)%/%12))
GOM_bt_temps$year <-GOM_bt_temps$year+1
GOM_bt_temps$year <-GOM_bt_temps$year+40 #this is the second half of the data 
GOM_bt_temps$month <-GOM_bt_temps$month+480 #this is the second half of the data 

#save the within-GOM mean bottom temps by month 
write_xlsx(GOM_bt_temps,"U:/mean_bottom_temp_deltas_GOM_convex_hull_yrs_41_80.xlsx")




#Comined the two datasets
first_half<-read_excel("U:/mean_bottom_temp_deltas_GOM_convex_hull_yrs_1_40.xlsx")
second_half<-read_excel("U:/mean_bottom_temp_deltas_GOM_convex_hull_yrs_41_80.xlsx")

full_data<-rbind(first_half, second_half)
write_xlsx(full_data,"U:/mean_bottom_temp_deltas_GOM_convex_hull_yrs_1_80.xlsx")

ggplot(full_data)+aes(x=month,y=mean_temp)+geom_line()

# compare to older version of the data
# old_data<-read_excel("U:/mean_bottom_temp_deltas_GOM_yrs_1_80.xlsx")
# old_data<-old_data %>%
#   dplyr::rename(mean_temp_old=tmp) %>%
#   dplyr::mutate(year=as.numeric(year))
# 
# both<-full_data %>%
#   dplyr::left_join(old_data, by=c("month", "year"))
# 
# ggplot(both)+geom_line(aes(x = month, y = mean_temp, color="new"), linewidth=.5) +
#              geom_line(aes(x = month, y = mean_temp_old, color="old"), linewidth=.5)+
#   scale_color_manual(name="", values=c("new"="blue", "old"="red"))
# 
# 
# both_decade<- both %>% 
#   dplyr::mutate(decade = case_when(year>=1 & year<=10~1,
#                                    year>=11 & year<=20~2, 
#                                    year>=21 & year<=30~3, 
#                                    year>=31 & year<=40~4, 
#                                    year>=41 & year<=50~5, 
#                                    year>=51 & year<=60~6, 
#                                    year>=61 & year<=70~7, 
#                                    year>=71 & year<=80~8)) %>% 
#   dplyr::group_by(decade) %>% 
#   dplyr::summarise(mean_temp_new=mean(mean_temp), 
#                    mean_temp_old=mean(mean_temp_old))
# 
# ggplot(both_decade)+geom_line(aes(x = decade, y = mean_temp_new, color="new"), linewidth=.5) +
#   geom_line(aes(x = decade, y = mean_temp_old, color="old"), linewidth=.5)+
#   scale_color_manual(name="", values=c("new"="blue", "old"="red")) + 
#   scale_x_continuous(name="decade", breaks=seq(1,8,1))
# 
# both_annual<-both %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(mean_temp_old=mean(mean_temp_old),
#                    mean_temp_new=mean(mean_temp))
# 
# ggplot(both_annual)+geom_line(aes(x = year, y = mean_temp_new, color="new"), linewidth=.5) +
#   geom_line(aes(x = year, y = mean_temp_old, color="old"), linewidth=.5)+
#   scale_color_manual(name="", values=c("new"="blue", "old"="red"))