
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


ncpath <- "U:/"
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

# state_boundaries <- readOGR( dsn="\\\\net.nefsc.noaa.gov/aharris/DisMap data",
#                              layer="state_national_ocean_boundaries",
#                              stringsAsFactors=FALSE, verbose=FALSE)
# 
# state_boundaries = spTransform(state_boundaries, CRS=PROJ.USE)



#Read in stock area shapefiles 
# mgt_boundaries <- readOGR( dsn="\\\\net.nefsc.noaa.gov/aharris/DisMap data",
#                              layer="Stock_Areas",
#                              stringsAsFactors=FALSE, verbose=FALSE)

mgt_boundaries <- st_read(dsn="\\\\net.nefsc.noaa.gov/aharris/DisMap data",
                           layer="Stock_Areas")

#mgt_boundaries = spTransform(mgt_boundaries, CRS=PROJ.USE)

mgt_boundaries_subset <- subset(mgt_boundaries, AREANAME  %in% c("GOM Cod Stock Area"))
plot(mgt_boundaries_subset)

#mgt_boundaries_subset <- spTransform(mgt_boundaries_subset, CRS=PROJ.USE)


##Now find overlap between the bt_tmp lat lons and the VTR convex hulls
# Input the bt_tmp data 
dat <- lat_lons_all_long #bt temp data
mgt_boundaries_subset<- st_as_sf(mgt_boundaries_subset)

sp_points = st_as_sf(dat,coords = c('lon' ,'lat' ))#make points spatial
st_crs(sp_points)= PROJ.USE
sp_points=st_transform(sp_points,st_crs(mgt_boundaries_subset)) # Match the point and polygon CRS
mgt_boundaries_subset$State<-"GOM"

sp_points_2020 <- subset(sp_points, year==2020 & month=="06")
kept_points <- st_intersection(mgt_boundaries_subset, sp_points_2020) #test to see it works
kept_points_all <- st_intersection(mgt_boundaries_subset, sp_points) #full data intersection


#kept_points<- st_as_sf(kept_points)

#save the within-GOM bottom temps 
write_xlsx(kept_points_all,"\\\\net.nefsc.noaa.gov/aharris/DisMap data/bottom_temps_GOM_cod_hadd.xlsx")


#collapse mean bottom temperatures by year and month
GOM_bt_temps<- kept_points_all %>% as.data.frame()  %>% select(c(month, bt_tmp, year))


all_vars<-c()
all_vars <- names(GOM_bt_temps)[!names(GOM_bt_temps) %in% c("year","month")]

GOM_mean_bt_temps<-GOM_bt_temps  %>% as.data.table() %>%
  .[,lapply(.SD, mean), by = c("year","month"), .SDcols = all_vars]

ggplot(GOM_bt_temps)+aes(x=month,y=bt_tmp)+geom_boxplot()

#save the within-GOM mean bottom temps by month 
write_xlsx(GOM_mean_bt_temps,"\\\\net.nefsc.noaa.gov/aharris/DisMap data/mean_bottom_temps_GOM_cod_hadd.xlsx")
library(readxl)
GOM_mean_bt_temps<-read_excel("\\\\net.nefsc.noaa.gov/aharris/DisMap data/mean_bottom_temps_GOM_cod_hadd.xlsx")
GOM_mean_bt_temps_decade<-GOM_mean_bt_temps %>% 
  dplyr::filter(year %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2020))


all_vars<-c()
all_vars <- names(GOM_bt_temps)[!names(GOM_bt_temps) %in% c("year")]
GOM_mean_bt_temps_year<- GOM_bt_temps  %>% select("year","bt_tmp") %>% as.data.table() 
ggplot(GOM_mean_bt_temps_year)+aes(x=year,y=bt_tmp, group=year)+geom_boxplot()

GOM_mean_bt_temps_year <- GOM_mean_bt_temps_year  %>%
  .[,lapply(.SD, mean), by = c("year"), .SDcols = "bt_tmp"]





#check that I have correclty allocated points within and outside the GOM 
kept_points_2020 <- subset(kept_points_all, year==2020 & month=="06") #full data intersection
kept_points_2020 <- subset(kept_points_2020, select=c(State, month, bt_tmp, year, geometry)) 

sp_points_2020 <- subset(sp_points, year==2020 & month=="06")

kept_points_2020<-as.data.frame(kept_points_2020)
sp_points_2020<-as.data.frame(sp_points_2020)

combined_data<- left_join(sp_points_2020, kept_points_2020, by=c("month", "year", "bt_tmp"))
combined_data<-subset(combined_data, select=-c(geometry.y))

combined_data<- combined_data %>% rename(geomtery=geometry.x ) 
combined_data<-combined_data %>% replace_na(list(State = 'outside')) 
unique(combined_data$State)

combined_data<- st_as_sf(combined_data)

states <- map_data("state")
east_coast <- subset(states, region %in% c("maine", "massachusetts", "vermont", "new hampshire","rhode island", "connecticut", "new york", "pennsylvania",
                                           "new jersey", "maryland", "delaware", "district of columbia", "virginia", "west virginia", "north carolina"))


east_coast <- east_coast %>%
  group_by(group) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

combined_data$region<-combined_data$State

ggplot(east_coast) +
  geom_sf(aes(group = group), fill = "#D1D1E0", color = "black")+
    geom_sf(data=combined_data, aes(colour = region)) 





