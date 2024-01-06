#libraries
library(sp)
library(terra)
library(raster)
library(sf)
library(FedData)
library(hrbrthemes)
library(tidyverse)
library(units)
library(ggforce)
library(data.table)

#'* THE FUNCTIONS ARE AT THE BOTTOM, PLEASE LOAD THEM BEFORE USING THEM!!!! *
#I tried making this program or function very easy to do. There is three separate functions that serve a different function each
#technically, you can sightly modify what comes back from the get_raster() function and you actually
#obtain the full raster image with that buffer, which is very convenient.

#First you need to read the file where the data is located, of course
data <- read.csv("C:/Users/Jorge A. Rivera/Lab/sites_2015_2022.csv") #location of coordinates

#I made the program work with the data in a specific way, it must go in the following order
#"Name of the place", "Longitude", "Latitude"

data <- data[c(1,2,6,7)]
class(data$Latitude) #make sure they're numeric
class(data$Longitude)
coords <- data[c(2,3,4)]
#okay now we format the data
coords <- coords[c("site", "Longitude" , "Latitude")]
coords_x <- coords

#the years of interest can be a list to do multiple years at the same time
years_of_interest <- list(2001,2004,2006,2008,2011,2013,2016,2019,2021)
#now choose the buffer size
buffer_size <- 1000 # 1000 meters = 1km

data_2021 <- automatic_years(years_of_interest,coords_x,buffer_size)

fwrite(data_2021, file = "NLCDdataAllSitesAllyears.csv")






#================Functions===================

get_rast <- function(coords_data, buffer_size, year_){
  #same the site name for later binding
  ID_name <- as.character(coords_data[1,1])
  coords_only <- coords_data[c(2,3)]
  
  
  #change the coordinates to NAD83 format, which is used for the NLCD
  spatial_data <- SpatialPoints(coords_only)
  CRS_string <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  proj4string(spatial_data)  <- 
    CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 
+datum=WGS84 +no_defs +towgs84=0,0,0 ") #assign default data
  sites_transformed<-
    spTransform(spatial_data, CRS(CRS_string)) #change to raster projection 
  
  
  #strings for label naming
  string_coords_data <- as.character(sites_transformed@coords[1,1]) #ID name
  string_coords_data2 <- as.character(sites_transformed@coords[1,2]) #ID name
  string_buffer_size <- as.character(buffer_size)
  string_year <- as.character(year_)
  
  #create polygon
  template_sf <- st_as_sf(sites_transformed)
  template_buffer <- st_buffer(template_sf, dist = buffer_size)
  
  #cut nlcd raster for that polygon
  nlcd <- get_nlcd(template_buffer, label = paste( string_coords_data , "_"
                                                   , string_coords_data2 , "_" , string_year), year = year_)
  values_within_buffer <- raster::extract(nlcd, template_buffer, df = TRUE, factors = TRUE)
  
  #get number of distinct values of classes 
  classes <- distinct(values_within_buffer["Class"])
  num_list <- nrow(classes)
  
  
  #calculate percentages
  nlcd_summary <- values_within_buffer %>%
    group_by(ID, Class) %>%
    summarize(land_cover = n()) %>%
    ungroup() %>%
    select(ID, Class, land_cover) %>%
    pivot_wider(names_from = Class,
                values_from = land_cover)%>%
    mutate(Total = select(., 2:(num_list+1)) %>%
             apply(1, sum, na.rm = TRUE)) %>%
    pivot_longer(cols = 2:(num_list+1),
                 names_to = "class",
                 values_to = "land_cover") %>%
    mutate(Percent = (land_cover/Total * 100)) %>%
    mutate(Year = year_)
  class(nlcd_summary)
  
  nlcd_summary$ID<- ID_name
  
  return_df <- list("summary" = nlcd_summary, "raster" = nlcd)
  
  return(return_df)
}

automatic_sites <- function(spatial_data, buffer_size,year_){
  datalist <- list()
  for_num <- nrow(spatial_data)
  
  for(i in 1:for_num){
    summary_ <- get_rast(spatial_data[i,], buffer_size, year_)
    datalist[[i]] <- summary_$summary
  }
  data <- do.call(rbind, datalist)
  return(data)
}

automatic_years <- function(list_of_years,spatial_data, buffer_size){
  for_num <- length(list_of_years)
  datalist <- list()
  
  for(i in 1:for_num){
    datalist[[i]] <- automatic_sites(spatial_data, buffer_size,list_of_years[i])
  }
  data <- do.call(rbind, datalist)
  return(data)
}


