
# load packages 
pkgs_to_use <- c("sf", "ncdf4", "raster","rasterVis", "RColorBrewer" , "chron", "lattice", "RColorBrewer", "rlist", "data.table", 
                 "rlang","foreign","tidyverse", "sp", "sf", "rgeos","rgdal", "here", "scales", "raster", "stringi", "ggplot2", "tigris",
                 "dplyr", "writexl", "readxl","mapview","geosphere", "sf",  "terra", "maps", "spatialEco")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)

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

ggplot(lat_lons_all)+aes(x=month,y=bt_tmp)+geom_boxplot()


lat_lons_all_long <-lat_lons_all



##Now make convex hulls by state from the population of VTR points
###now create the convex hull of VTR points
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

# ggplot(east_coast) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "#D1D1E0", color = "black") +
#   geom_point(data = lat_lons_ready, aes(x = lon, y = lat, group = State, colour = State), size = 1, shape = 20) +
#   geom_polygon(data = lat_lon_hulls, aes(x = lon, y = lat, group = State, colour = State), alpha = 0.3, show.legend = FALSE) +
#   coord_sf(xlim = c(-80, -64), ylim = c(34, 46)) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         plot.title = element_text(hjust = 0.5))



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



##Now find overlap between the bt_tmp lat lons and the VTR convex hulls
# Input the bt_tmp data 
dat <- lat_lons_all_long
VTRs<- st_as_sf(VTR_boundaries)

sp_points = st_as_sf(dat,coords = c('lon' ,'lat' ))#make points spatial
st_crs(sp_points)= 4326
sp_points=st_transform(sp_points,st_crs(VTRs)) # Match the point and polygon CRS

#allocate each point to a state convex hull
sp_points$state_name <- apply(st_intersects(VTRs, sp_points, sparse = FALSE), 2, 
                              function(col) {VTRs[which(col), ]$State}) 
sp_points$state_name<-as.character(sp_points$state_name)
sp_points$row_num <- seq.int(nrow(sp_points)) 


#save data and clean up in stata. I split the data into multiple files to copmply with excel limitations
sp_points1<-subset(sp_points,sp_points$row_num<=1000000 )
sp_points2<-subset(sp_points,sp_points$row_num>1000000 & sp_points$row_num<=2000000)
sp_points3<-subset(sp_points,sp_points$row_num>2000000  & sp_points$row_num<=3000000)

write_xlsx(sp_points1,"\\\\net.nefsc.noaa.gov/aharris/DisMap data/bottom_temps_new1.xlsx")
write_xlsx(sp_points2,"\\\\net.nefsc.noaa.gov/aharris/DisMap data/bottom_temps_new2.xlsx")
write_xlsx(sp_points3,"\\\\net.nefsc.noaa.gov/aharris/DisMap data/bottom_temps_new3.xlsx")


#plots
lat_lons_all_long_22<- subset(lat_lons_all_long, year==2022 & month=="01")

ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#D1D1E0", color = "black") +
  geom_point(data = lat_lons_all_long_22, aes(x = lon, y = lat, fill=bt_tmp)) +
  coord_sf(xlim = c(-80, -64), ylim = c(34, 46)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

# SIMPLE HEATMAP


#Check to see if bt_tmps that are not associated with a state indeed fall outside the state biomass access areas
sp_points_outside<- subset(sp_points, state_name=="character(0)")

dat$row_num<- seq.int(nrow(dat)) 
dat_merge<- subset(dat, select=c("row_num", "lat", "lon"))
sp_points_outside = merge(sp_points_outside, dat_merge, by = "row_num", all.y=FALSE)

sp_points_inside<- subset(sp_points, state_name!="character(0)")
sp_points_inside = merge(sp_points_inside, dat_merge, by = "row_num", all.y=FALSE)


#The following graph produced points of all the bt_tmps; those in red are outside the state access areas
#and those in black are inside the state areas. 
ggplot(east_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#D1D1E0", color = "black") +
  geom_point(data = sp_points_outside, aes(x = lon, y = lat), colour = "red", size = 1, shape = 20) +
  geom_point(data = sp_points_inside, aes(x = lon, y = lat), colour = "black", size = 1, shape = 20) +
  geom_polygon(data = lat_lon_hulls, aes(x = lon, y = lat, group = State, colour = State), alpha = 0.3, show.legend = FALSE) +
  coord_sf(xlim = c(-80, -64), ylim = c(34, 46)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
