
args = commandArgs(trailingOnly=TRUE)

#load needed packages and install if not currently installed.
pkgs_to_use <- c("rlang","foreign","tidyverse", "sp", "sf", "rgeos","rgdal", "here", "scales", "raster", "stringi", "ggplot2", "tigris",
                 "dplyr", "writexl", "readxl","xlsx","mapview","geosphere", 
                 "tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
                 "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
                 "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs", 
                 "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "gridExtra")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)


PROJ.USE = CRS('+proj=aea +lat_1=28 +lat_2=42 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ')

###########Input the state lines#############
#This reads in the shapefiles, you might need to change the file path slightly
#depending on how you set up your folder structure for this project. 
#To access the files through my network drive I think you can use:

net_drive<-"\\\\net.nefsc.noaa.gov/home2/aharris/DisMap data"

# state_boundaries <- readOGR( dsn="\\\\net.nefsc.noaa.gov/aharris/DisMap data",
#                              layer="state_national_ocean_boundaries",
#                              stringsAsFactors=FALSE, verbose=FALSE)
state_boundaries <- readOGR( dsn=net_drive,
                             layer="state_national_ocean_boundaries",
                             stringsAsFactors=FALSE, verbose=FALSE)

state_boundaries = spTransform(state_boundaries, CRS=PROJ.USE)



#####################################
#####################################
##Input the biomass rasters #########
#####################################
#####################################

#This is for the files by species and survey
file_list_SF_F = list.files(path = "\\\\net.nefsc.noaa.gov/home2/aharris/DisMap data/Paralichthys dentatus_F", 
                            all.files = FALSE,
                            pattern = "*.tif$",
                            recursive = TRUE)

file_list_SF_S = list.files(path = "\\\\net.nefsc.noaa.gov/home2/aharris/DisMap data/Paralichthys dentatus_S", 
                            all.files = FALSE,
                            pattern = "*.tif$",
                            recursive = TRUE)

file_list_BSB_F = list.files(path = "\\\\net.nefsc.noaa.gov/home2/aharris/DisMap data/Centropristis striata_F", 
                             all.files = FALSE,
                             pattern = "*.tif$",
                             recursive = TRUE)

file_list_BSB_S = list.files(path = "\\\\net.nefsc.noaa.gov/home2/aharris/DisMap data/Centropristis striata_S", 
                             all.files = FALSE,
                             pattern = "*.tif$",
                             recursive = TRUE)


file_list_SF_BSB_F=c(file_list_SF_F, file_list_BSB_F)
file_list_SF_BSB_S=c(file_list_SF_S, file_list_BSB_S)



##For some reason these break when I did them all at once so I ran the code for each separately 
for (x in file_list_SF_F) {
  name = stri_sub(x, 1, nchar(x)-4)
  assign(paste0("Raster_",name, sep=""), 
         raster(here("\\\\net.nefsc.noaa.gov/home2/aharris/DisMap data/Paralichthys dentatus_F",x)))
  assign(paste0("Raster_",name, sep=""), 
         projectRaster(from=get(paste0("Raster_",name, sep="")),
                       crs=PROJ.USE))
}


for (x in file_list_BSB_F) {
  name = stri_sub(x, 1, nchar(x)-4)
  assign(paste0("Raster_",name, sep=""), 
         raster(here("\\\\net.nefsc.noaa.gov/home2/aharris/DisMap data/Centropristis striata_F",x)))
  assign(paste0("Raster_",name, sep=""), 
         projectRaster(from=get(paste0("Raster_",name, sep="")),
                       crs=PROJ.USE))
}



for (x in file_list_SF_S) {
  name = stri_sub(x, 1, nchar(x)-4)
  assign(paste0("Raster_",name, sep=""), 
         raster(here("\\\\net.nefsc.noaa.gov/home2/aharris/DisMap data/Paralichthys dentatus_S",x)))
  assign(paste0("Raster_",name, sep=""), 
         projectRaster(from=get(paste0("Raster_",name, sep="")),
                       crs=PROJ.USE))
}


for (x in file_list_BSB_S) {
  name = stri_sub(x, 1, nchar(x)-4)
  assign(paste0("Raster_",name, sep=""), 
         raster(here("\\\\net.nefsc.noaa.gov/home2/aharris/DisMap data/Centropristis striata_S",x)))
  assign(paste0("Raster_",name, sep=""), 
         projectRaster(from=get(paste0("Raster_",name, sep="")),
                       crs=PROJ.USE))
}





#####################################
#####################################
###########Input the VTR points######
#####################################
#####################################
install.packages("readxl")
library("readxl")
#Create a single access area for each state based on VTR points for all years combined
lat_lons<-read_excel("//net.nefsc.noaa.gov/home2/aharris/DisMap data/all_pc_trips_1994_2021.xlsx")
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
lat_lons_ready$distance<-geosp::distHaversine(lat_lons_ready[,2:3], lat_lons_ready[,4:5])

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





####Now the loop to create the overlap indices for all states and years by survey

###Years of trawl survey data for spring and fall 
year_list_F = c(1974, 1975, 1976, 1977, 1978, 1979,
                1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989,
                1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
                2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                2010, 2011, 2012, 2013, 2014, 2015, 2016,       2018, 2019)

year_list_S = c(1974,       1976, 1977, 1978, 1979,
                1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989,
                1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
                2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                2010, 2011, 2012, 2013,       2015, 2016, 2017, 2018, 2019)

year_list_F_check <- c(2018, 2019)
year_list_S_check <- c(2018, 2019)

state_list<- c(1, 2, 3, 4, 5, 6, 7, 8, 9)





##First loop is for the fall survey
RESULTS_F <- NULL
for (yr in year_list_F){
  
  yr <-yr
  
  
  for (s in state_list){
    s<-s
    state_poly <- VTR_boundaries[s, ]
    state <- state_poly@data[["State"]]
    
    #Raster A
    name = paste0("NEUS_F_Paralichthys_dentatus_", yr,".tif")
    R.A<-assign(paste0("Raster_",name), 
                raster(here("\\\\net.nefsc.noaa.gov/aharris/DisMap data/Paralichthys dentatus_F",name)))
    R.A<- assign(paste0("Raster_",name), 
                 projectRaster(from=get(paste0("Raster_",name)),
                               crs=PROJ.USE))
    R.A <- crop(R.A,state_poly)
    R.A <- mask(R.A,state_poly)
    # plot(R.A)
    # plot(state_poly, add=TRUE)
    
    
    #Raster B
    name = paste0("NEUS_F_Centropristis_striata_", yr,".tif")
    R.B<-assign(paste0("Raster_",name), 
                raster(here("\\\\net.nefsc.noaa.gov/aharris/DisMap data/Centropristis striata_F",name)))
    R.B<- assign(paste0("Raster_",name), 
                 projectRaster(from=get(paste0("Raster_",name)),
                               crs=PROJ.USE))
    R.B <- crop(R.B,state_poly)
    R.B <- mask(R.B,state_poly)
    # plot(R.B)
    # plot(state_poly, add=TRUE)
    
    share.RA <- (R.A/(cellStats(R.A, stat=sum))) # Take the cell's share of the value of the whole raster
    share.RB <- (R.B/(cellStats(R.B, stat=sum)))
    
    abs.differences <- abs(share.RA-share.RB)  # Absolute difference of shares between cells
    #plot(abs.differences)
    
    sums <- share.RA+share.RB # Sum of shares among cells
    #plot(sums)
    
    sum.abs.differences <- cellStats(abs.differences, stat=sum) #Sum of abs. differences
    sum.sums <- cellStats(sums, stat=sum) #Sum of sums of shares
    
    Cz.index <- 1-sum.abs.differences/sum.sums #Czekanowski index
    JCz.index <- Cz.index/(2-Cz.index) #Jaccard-Czekanowski index
    
    TEMP <- cbind(as.numeric(s), as.numeric(yr),as.numeric(JCz.index) )
    TEMP$state <- state
    RESULTS_F <- rbind(RESULTS_F,TEMP)
    
    
  }
  
}
RESULTS_F <- as.data.frame(RESULTS_F)
colnames(RESULTS_F) <- c('state_no','year','JC_index_fall', 'state')
rownames(RESULTS_F)<-NULL






##Second loop is for the spring survey
RESULTS_S <- NULL
for (yr in year_list_S){
  
  yr <-yr
  
  
  
  for (s in state_list){
    s<-s
    state_poly <- VTR_boundaries[s, ]
    state <- state_poly@data[["State"]]
    
    #Raster A
    name = paste0("NEUS_S_Paralichthys_dentatus_", yr,".tif")
    R.A<-assign(paste0("Raster_",name), 
                raster(here("\\\\net.nefsc.noaa.gov/aharris/DisMap data/Paralichthys dentatus_S",name)))
    R.A<- assign(paste0("Raster_",name), 
                 projectRaster(from=get(paste0("Raster_",name)),
                               crs=PROJ.USE))
    R.A <- crop(R.A,state_poly)
    R.A <- mask(R.A,state_poly)
    # plot(R.A)
    # plot(state_poly, add=TRUE)
    
    
    #Raster B
    name = paste0("NEUS_S_Centropristis_striata_", yr,".tif")
    R.B<-assign(paste0("Raster_",name), 
                raster(here("\\\\net.nefsc.noaa.gov/aharris/DisMap data/Centropristis striata_S",name)))
    R.B<- assign(paste0("Raster_",name), 
                 projectRaster(from=get(paste0("Raster_",name)),
                               crs=PROJ.USE))
    R.B <- crop(R.B,state_poly)
    R.B <- mask(R.B,state_poly)
    # plot(R.B)
    # plot(state_poly, add=TRUE)
    
    share.RA <- (R.A/(cellStats(R.A, stat=sum))) # Take the cell's share of the value of the whole raster
    share.RB <- (R.B/(cellStats(R.B, stat=sum)))
    
    abs.differences <- abs(share.RA-share.RB)  # Absolute difference of shares between cells
    #plot(abs.differences)
    
    sums <- share.RA+share.RB # Sum of shares among cells
    #plot(sums)
    
    sum.abs.differences <- cellStats(abs.differences, stat=sum) #Sum of abs. differences
    sum.sums <- cellStats(sums, stat=sum) #Sum of sums of shares
    
    Cz.index <- 1-sum.abs.differences/sum.sums #Czekanowski index
    JCz.index <- Cz.index/(2-Cz.index) #Jaccard-Czekanowski index
    
    TEMP <- cbind(as.numeric(s), as.numeric(yr),as.numeric(JCz.index) )
    TEMP$state <- state
    RESULTS_S <- rbind(RESULTS_S,TEMP)
    
    
  }
  
}
RESULTS_S <- as.data.frame(RESULTS_S)
colnames(RESULTS_S) <- c('state_no','year','JC_index_spring', 'state')
rownames(RESULTS_S)<-NULL

RESULTS_S$state<-as.character(RESULTS_S$state)
RESULTS_F$state<-as.character(RESULTS_F$state)
RESULTS_S$state_no<-as.numeric(RESULTS_S$state_no)
RESULTS_F$state_no<-as.numeric(RESULTS_F$state_no)
RESULTS_F$year<-as.numeric(RESULTS_F$year)
RESULTS_S$year<-as.numeric(RESULTS_S$year)


RESULTS_S_F <- merge(RESULTS_S, RESULTS_F,  by=c('state', 'year', 'state_no'), all.x = TRUE, all.y = TRUE)

RESULTS_S_F$JC_index_fall<-as.numeric(RESULTS_S_F$JC_index_fall)
RESULTS_S_F$JC_index_spring[is.null(RESULTS_S_F$JC_index_spring)]<-NA
RESULTS_S_F[RESULTS_S_F == "NaN"] <- NA
RESULTS_S_F[RESULTS_S_F == "NULL"] <- NA
RESULTS_S_F$JC_index_spring<-as.numeric(RESULTS_S_F$JC_index_spring)

write_xlsx(RESULTS_S_F,"\\\\net.nefsc.noaa.gov/aharris/DisMap data/RESULTS_S_F_VTRs_allyears.xlsx")



