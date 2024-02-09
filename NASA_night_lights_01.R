library(terra)
library(raster)
library(hrbrthemes)
library(tidyverse)
library(units)
library(ggforce)
library(data.table)
library(curl)
library(sf)
library(geodata)
library(ggplot2)
library(blackmarbler)
library(dplyr)
library(plyr)

#'*==================PRODUCT INFO===================*'
# BlackMarble offers four products, 1 and 2 take a very long time to process. 
#product_id: One of the following:
  
#"VNP46A1": Daily (raw)
#"VNP46A2": Daily (corrected)
#"VNP46A3": Monthly
#"VNP46A4": Annual

#You need to get a token from NASA. Follow this website:https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/VNP46A3/
#create an account and actually add "your organization info" otherwise it won't work
#'*==================Function info===================*'
#dates must be in %y-%m-%d format
# if you want yearly, you do not have to modify the date you can also leave it in that format
# buffer size in m 
#coordinates must in "Name" "Longitude" "Latitude" format

bearer <- "eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6ImFyZ2VueXNsIiwiZXhwIjoxNzEyNDYyODUzLCJpYXQiOjE3MDcyNzg4NTMsImlzcyI6IkVhcnRoZGF0YSBMb2dpbiJ9.Y3KOBWQ2PP-YNGKBP8AmUGQRD19CrDqSA3V2oXOJObnsqC-WIwdS2Ex8WkTA8JNJ330x2k1AiMWlrVuEK7G7Bq8gMP0QUG5lcEtG0cDYUtebVS0S_50nrrhn7nle61lJS5MX3a9-4EImn-CZrhJwKrijjM4wa_8etjcIbfNiIznY3vJjCZ9Ltfx3EJdx76EQXIKwcfHq_eEt4AcMA0HryuHqXVlesB_jnSl-Ppr_W8tP1E2oUvUhG7sUAYpkT8gu-LWNKTH15oKhTl9G8t-kYkeeybV0P5GSwsIXvO178HoiOJGcf4uBo0HjcHdSOt3bOGoKUgW-ExW93ISPxxD_mA"
data <- read.csv(curl("https://raw.githubusercontent.com/Argenysl/urban-dollop/main/Data/sites_2015_2022.csv")) #location of coordinates
all_dates <- read.csv("C:/Users/Jorge A. Rivera/OneDrive/Documents/MYSE_21-main/MYSE_21-main/data/Seq_visits_long.csv")

all_dates <- na.omit(all_dates[,5], )
all_dates <- as.Date(all_dates, format = "%Y-%m-%d")

data <- data[c(1,2,6,7)]
class(data$Latitude) #make sure they're numeric
class(data$Longitude)
coords <- data[c(2,3,4)]
#okay now we format the data
coords <- coords[c("site", "Longitude" , "Latitude")]
coords_x <- coords
view(all_dates)
          
aqui <- auto_get(coords_x, 1000, all_dates, bearer, "VNP46A4")


examples <- auto_get(coordinates, buffer, date, bearer, product)

#=========================== Functions ============================
get_info <- function(coords_data, buffer_size, date_, bearer_, product_){

  ID_name <- as.character(coords_data[1,1])
  coords_only <- coords_data[c(2,3)]
  #change the coordinates to espg:4326 for MBlackarble
  spatial_data <- SpatialPoints(coords_only)
  proj4string(spatial_data)  <- 
    CRS("+init=epsg:4326") #assign default data
  
  sites_transformed <- spatial_data
  
  #create polygon
  template_sf <- st_as_sf(sites_transformed)
  template_buffer <- st_buffer(template_sf, dist = buffer_size)
  
  #download raster for that date, going to temp memory
  blackmarble_rstr <- bm_raster(roi_sf = template_buffer,
                                product_id = product_,
                                date = date_,
                                bearer = bearer_)
  
  #extract values from raster
  values_within_buffer <- raster::extract(blackmarble_rstr, template_buffer, df = TRUE, factors = TRUE)
  group <- colnames(values_within_buffer[2])
  
  colnames(values_within_buffer)[colnames(values_within_buffer) == group] <- "value"
  
  dt <- values_within_buffer
  #sum them all
  new_values <- plyr::ddply(dt,.(ID),summarize,sum=sum(na.omit(value)),number=length(ID))
  
  colnames(new_values)[colnames(new_values) == "sum"] <- "Gap_Filled_DNB_BRDF
Corrected_NTL"
  
  #create nice data frame with info
  coords_data <- coords_x
  blackmarble_summary <- new_values[,1:2]
  blackmarble_summary[,1] <- coords_data[,1]
  blackmarble_summary <- blackmarble_summary %>% mutate(date = date_)
  return_df <- blackmarble_summary
  
  return(return_df)
}

auto_get <- function(coords_data, buffer_size, all_dates, bearer_, product_){
  #choose a date format base on product
  if(product_ == "VNP46A3"){all_dates <- format(all_dates,"%y-%m" )}
  if(product_ == "VNP46A4"){all_dates <- format(all_dates,"%Y" )}
  all_dates <- unique(all_dates)
  for_num <- length(all_dates)
  
  datalist <- list()
  
  for(i in 1:for_num){
    datalist[[i]] <- get_info(coords_data, buffer_size, all_dates[i], bearer_, product_)
  }
  
  data <- do.call(rbind, datalist)
  return(data)
}
