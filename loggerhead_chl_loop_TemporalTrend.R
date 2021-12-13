library(ncdf4)
library(raster)
library(lubridate)
library(doParallel)
library(dplyr)
library(ggplot2)
library(scales)

chl_WestAfrica_df <- data.frame(date = Date(), mean_chl = numeric(), stringsAsFactors = F)
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
  
chl_WestAfrica_df <- chl_WestAfrica_df %>% 
                      group_by(month=floor_date(date, "month")) %>%
                      dplyr::summarise(monthly_chl_mean = mean(mean_chl), sd=sd(mean_chl))


ggplot(chl_WestAfrica_df, aes(x=month, y= monthly_chl_mean)) +
  geom_point() + geom_path()+scale_x_date(labels = date_format("%m-%Y"))





ylab("MLD (m)")




















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


































as.pos
chl_WestAfrica_df$monthYear <-substr(chl_WestAfrica_df$date, 1,7)

AA<-chl_WestAfrica_df %>% 
  group_by(month = lubridate::floor_date(date, "month"))

??floor_date
class(AA$month)


<-my(chl_WestAfrica_df$date, )
chl_WestAfrica_df$monthYear<- as.Date(chl_WestAfrica_df$monthYear, "%YYYY-%mm")

AA<-chl_WestAfrica_df %>%
  mutate(monthYear = format(seq(
    as.Date(date),
    by = "month",
    length.out = length(chl_WestAfrica_df$date)
  ), "%Y-%m"))

as.POSIXct(chl_WestAfrica_df$monthYear, format="%Y-%m")

?as.POSIXct

ymd(chl_WestAfrica_df$date, truncated = 2)
ym(chl_WestAfrica_df$monthYear)
??as.Date
as.Date(chl_WestAfrica_df$date, format = )
class(chl_WestAfrica_df$monthYear)
chl_WestAfrica_df$monthYear <- format_ISO8601(chl_WestAfrica_df$date, precision = "ym")
??format_ISO8601


as.POSIXct(chl_WestAfrica_df$monthYear, origin = "1950-01", tz = "GMT")
date(chl_WestAfrica_df$monthYear)

chl_WestAfrica_df_month  <- chl_WestAfrica_df %>% 
  group_by(month = lubridate::floor_date(date, "month")) %>%
  dplyr::summarise(monthly_chl_mean = mean(mean_chl), sd=sd(mean_chl))
  

date_format(chl_WestAfrica_df_month$monthYear,"%m-%Y")



class(chl_WestAfrica_df$monthYear)
ggplot(chl_WestAfrica_df_month, aes(x=monthYear, y= monthly_chl_mean)) +
   geom_point() + geom_path()+scale_x_date(labels = date_format("%m-%Y"))
  ylab("MLD (m)")




plot(chl_WestAfrica_df_month$monthly_chl_mean, chl_WestAfrica_df_month$monthYear)


my(chl_WestAfrica_df$date)

??format_ISO8601()





