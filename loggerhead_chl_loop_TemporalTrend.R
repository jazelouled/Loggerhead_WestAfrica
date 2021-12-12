library(ncdf4)
library(raster)
library(lubridate)
library(doParallel)
library(dplyr)
library(ggplot2)
library(scales)

chl_WestAfrica_df <- data.frame(date = dmy(), mean_chl = numeric(), stringsAsFactors = F)
chl_files <- list.files(full.names = F , recursive =T, pattern = "_chl.nc", include.dirs = FALSE)                            
chl_rasters <- stack()

for(i in 1:length(chl_files)) {
  aa_chl<-nc_open(chl_files[i])
  chl<-ncvar_get(aa_chl,varid="chl") #chl
  lat_chl<-ncvar_get(aa_chl,varid="latitude") #latitude
  lon_chl<-ncvar_get(aa_chl,varid="longitude") #longitude: -180-180	
  time_chl<-ncvar_get(aa_chl,varid="time") #hours since 1950-01-01 00:00:00	
  dates_chl <- as.POSIXct(3600*time_chl, origin = "1950-01-01", tz = "GMT") # we transform number of days (time) into actual dates
  chl_mean <- mean(chl, na.rm=T)
  chl_mean_r <- raster(chl) # transform the matrix into a raster
  chl_mean_r <- t(flip(chl_mean_r, direction="x")) #to rotate the raster
  bb<-extent(min(lon_chl),max(lon_chl),min(lat_chl),max(lat_chl)) #  generate an object with the bounding box: xmin, xmax, ymin, ymax
  chl1<-setExtent(chl_mean_r,bb,keepres=FALSE, snap=FALSE) # apply the bounding box to the raster
  chl_WestAfrica_df[i,1] = as.POSIXct(dates_chl)
  chl_WestAfrica_df[i,2] = mean(chl_mean)
  nc_close(aa_chl)
} 
  
chl_rasters <- stack(chl_rasters, chl1)




cores <- 3   #detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

y <- foreach(i=1:length(chl_files), .packages=c("lubridate", "ncdf4", "raster"), .inorder=TRUE) %dopar% {
  print(i)
  aa_chl<-nc_open(chl_files[i])
  chl<-ncvar_get(aa_chl,varid="chl") #chl
  lat_chl<-ncvar_get(aa_chl,varid="latitude") #latitude
  lon_chl<-ncvar_get(aa_chl,varid="longitude") #longitude: -180-180	
  time_chl<-ncvar_get(aa_chl,varid="time") #hours since 1950-01-01 00:00:00	
  dates_chl <- as.POSIXct(3600*time_chl, origin = "1950-01-01", tz = "GMT") # we transform number of days (time) into actual dates
  chl_mean <- mean(chl, na.rm=T)
  chl_mean_r <- raster(chl) # transform the matrix into a raster
  chl_mean_r <- t(flip(chl_mean_r, direction="x")) #to rotate the raster
  bb<-extent(min(lon_chl),max(lon_chl),min(lat_chl),max(lat_chl)) #  generate an object with the bounding box: xmin, xmax, ymin, ymax
  chl1<-setExtent(chl_mean_r,bb,keepres=FALSE, snap=FALSE) # apply the bounding box to the raster
  chl_WestAfrica_df[i,1] = as.POSIXct(dates_chl)
  chl_WestAfrica_df[i,2] = mean(chl_mean)
  nc_close(aa_chl)
  
}

chl_WestAfrica_df$monthYear <- format_ISO8601(chl_WestAfrica_df$date, precision = "ym")

chl_WestAfrica_df_month  <- chl_WestAfrica_df %>% 
  dplyr::group_by(monthYear) %>%
  dplyr::summarise(monthly_chl_mean = mean(mean_chl), sd=sd(mean_chl))
date_format(chl_WestAfrica_df_month$monthYear,"%m-%Y")



class(chl_WestAfrica_df$monthYear)
ggplot(chl_WestAfrica_df_month, aes(x=monthYear, y= monthly_chl_mean)) +
   geom_point() + geom_path()+scale_x_date(labels = date_format("%m-%Y"))
  ylab("MLD (m)")




plot(chl_WestAfrica_df_month$monthly_chl_mean, chl_WestAfrica_df_month$monthYear)


my(chl_WestAfrica_df$date)

??format_ISO8601()





