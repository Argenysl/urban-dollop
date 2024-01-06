#This program will attempt to predict the data for the years missing from the NLCD
#This program will assume there is a linear relationship from the years 2013,2016,2019,2021 to approximate the other years
library(tidyverse)
library(dplyr)
library(data.table)
NLCD_data <- read.csv("C:/Users/Jorge A. Rivera/Lab/Data/NLCDdata.csv",row.names=NULL)
approx_data <- year_pred(NLCD_data,2013:2022)
aaaa <- Filter(Negate(is.null), approx_data)
zz <- do.call(rbind, aaaa)
zz[zz<0] <- 0

zz[zz$land_cover<0,] <- 0
zz[2,181]
write.csv(zz, file = "approx.csv")


year_pred <- function(data, years_){

  data = NLCD_data
  years_ = 2013:2022
  all_sites <- unique(data$ID)
  all_classes <- unique(data$class)
  
  #all possible combination of sites and classes
  allcombos <- expand.grid(all_sites,all_classes)
  num_combos <- nrow(allcombos)
  
  
  ####
  datalist <- list()
  for(i in 1:num_combos){
  subData <- data %>%
    filter(ID == allcombos[i,]$Var1, class == allcombos[i,]$Var2) %>%
    group_by(ID) %>%
    as.data.frame() #we can do i parameter here for for loop
  if(nrow(subData) == 0){next}
  subData_YLC <- subData[c("land_cover", "Year")]
  years <- data.frame(Year = years_) ##we can do parameter here
  df <- merge(subData_YLC, years, by = c("Year"), all = TRUE)
  df[is.na(df) | df=="Inf"] <- NA
  #assuming linear model
  model.1 <- lm(formula = land_cover ~ Year, data = df, na.action=na.exclude)
  #confidence interval 
  broom::tidy(model.1, conf.int = T)
  #predict values for NAs
  predicted_data <- predict(object = model.1, newdata = df) 
  
  df$land_cover <- predicted_data
  
  df$ID <- rep(unique(subData$ID),nrow(df))
  df$class <- rep(unique(subData$class),nrow(df))
  head(df)
  df <- df[c("ID", "class", "land_cover", "Year")]
  df$land_cover <- round(df$land_cover)
  datalist[[i]] <- df
  }
  #data_ <- rbindlist(datalist)
 # data_ <- do.call(rbind, datalist)
  return(datalist)
}

