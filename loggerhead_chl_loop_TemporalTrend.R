# Jazel Ouled-Cheikh, 13/12/2021
# Universitat de Barcelona & Institut de Ciències del Mar


                                  ####################################################################
                                  #####	  Averaging Chlorophyll & Net primary productivity data   ####	  
                                  ####      in an area where loggerhead turtles were tracked      ####	  
                                  ####  for the 1999 - 2021 period from daily Remote sensing data ####	  
                                  ####################################################################



# Install packages
list.of.packages <- c("ncdf4", "raster", "lubridate", "doParallel", "dplyr", "ggplot2", "scales", 
                      "sf", "extractextractr", "here")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(ncdf4)
library(raster)
library(lubridate)
library(doParallel)
library(dplyr)
library(ggplot2)
library(scales)
library(sf)
library(exactextractr)
library(here)
library(patchwork)

                                            ######################
                                            #### Chlorophylll ####
                                            ######################

# Create an empty data frame
chl_WestAfrica_df <- data.frame(date = Date(), mean_chl = numeric(), stringsAsFactors = F)

# Search all the chlorophyll ncdf files in the project folder
chl_files <- list.files(full.names = F , recursive =T, pattern = "_chl.nc", include.dirs = FALSE)                            

# Read the MCP90 polygon (90% of the area covered by tracked loggerhead turtles)
MCP90 <- st_read(here::here("data/shapefiles/carcar_adult_mcp90.gpkg"))
# Transform into a spatial object
MCP90 <- as_Spatial(MCP90)


# For loop to go through all the ncdf files, extract their variables, apply the MCP90 mask, average
# [chlorophyll] values in it and populate the empty data frame created before. Result is a data frame 
# with daily [chlorophyll] values in the polygon defined by loggerhead tracks.
for(i in 1:length(chl_files)) {
  print(i)
  aa_chl<-nc_open(chl_files[i])
  chl<-ncvar_get(aa_chl,varid="chl") #chl
  lat_chl<-ncvar_get(aa_chl,varid="latitude") #latitude
  lon_chl<-ncvar_get(aa_chl,varid="longitude") #longitude: -180-180	
  time_chl<-ncvar_get(aa_chl,varid="time") #hours since 1950-01-01 00:00:00	
  dates_chl <- as.POSIXct(3600*time_chl, origin = "1950-01-01", tz = "GMT") # we transform number of days (time) into actual dates
  chl_r <- raster(chl) # transform the matrix into a raster
  chl_r <- t(flip(chl_r, direction="x")) #to rotate the raster
  bb<-extent(min(lon_chl),max(lon_chl),min(lat_chl),max(lat_chl)) #  generate an object with the bounding box: xmin, xmax, ymin, ymax
  chl1<-setExtent(chl_r,bb,keepres=FALSE, snap=FALSE) # apply the bounding box to the raster
  crs(chl1)<-crs("+proj=longlat +datum=WGS84 +no_defs")
  chl2<-mask(chl1, MCP90)
  crs(chl2)<-"+proj=longlat +datum=WGS84 +no_defs"
  chl_mean <- mean(chl2@data@values, na.rm=T) # average raster values
  chl_WestAfrica_df[i,1] = as.POSIXct(dates_chl)
  chl_WestAfrica_df[i,2] = chl_mean
  nc_close(aa_chl) # needed to avoid the error "too many files open"
} 

 
# Work on the populated data frame. We group by month and average monthly data 
# floor_date explanation: https://ro-che.info/articles/2017-02-22-group_by_month_r
# This function rounds down each date to the month boundary, so that dates in the same month
# are rounded down to the same date. Then we can group all dates of a given month by month
chl_WestAfrica_df_ <- chl_WestAfrica_df %>% 
                      group_by(month=floor_date(date, "month")) %>%
                      dplyr::summarise(monthly_chl_mean = mean(mean_chl), sd=sd(mean_chl))


# Save daily mean table
# write.csv(chl_WestAfrica_df, here::here("output/mean_daily_chlorophyll_MCP90.csv"))
# Save monthly mean table
# write.csv(chl_WestAfrica_df_, here::here("output/mean_monthly_chlorophyll_MCP90.csv"))



theme2Review<-theme_bw()+theme(axis.text.x=element_text(size=12),
                               axis.text.y = element_text(size=12),
                               axis.title.y=element_text(size=19),
                               axis.title.x = element_text(size=19),
                               legend.text = element_text(size=11),
                               legend.title = element_text(face="bold", size=13),
                               legend.key.size = unit(0.9, "cm"),
                               legend.background = element_rect(colour ="black", size=0.5, linetype = "dotted"),
                               legend.direction = "vertical",
                               legend.position = "none",
                               plot.margin = margin(2, 2, 2, 2, "cm"), strip.background =element_rect(fill="gray59"),
                               strip.text = element_blank(),
                               strip.placement = "right",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank())


# Plot points and path along them of the obtained mean
chl_monthly <- ggplot(chl_WestAfrica_df_, aes(x=month, y= monthly_chl_mean)) +
                      geom_point() + geom_path()+scale_x_date(labels = date_format("%m-%Y"))+
                      xlab("\nMonth-Year")+
                      ylab(expression(paste("Total Chlorophyll ", (mg/m^{3}), "\n\n")))+
                      theme2Review





# Same approach but using foreach instead of for. It should be faster. Still checking.
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


                                                          ##################################
                                                          #### Net Primary Productivity ####
                                                          ##################################

# Create an empty data frame
npp_WestAfrica_df <- data.frame(date = Date(), mean_npp = numeric(), stringsAsFactors = F)

# Search all the Net Primary Production ncdf files in the project folder
npp_files <- list.files(full.names = F , recursive =T, pattern = "_nppv.nc", include.dirs = FALSE)                            

# Read the MCP90 polygon (90% of the area covered by tracked loggerhead turtles)
MCP90 <- st_read(here::here("data/shapefiles/carcar_adult_mcp90.gpkg"))
# Transform into a spatial object
MCP90 <- as_Spatial(MCP90)


# For loop to go through all the ncdf files, extract their variables, apply the MCP90 mask, average
# Net Primary Production values in it and populate the empty data frame created before. Result is a data frame 
# with daily Net Primary Production values in the polygon defined by loggerhead tracks.
for(i in 1:length(npp_files)) {
  print(i)
  aa_npp<-nc_open(npp_files[i])
  npp<-ncvar_get(aa_npp,varid="nppv") #npp
  lat_npp<-ncvar_get(aa_npp,varid="latitude") #latitude
  lon_npp<-ncvar_get(aa_npp,varid="longitude") #longitude: -180-180	
  time_npp<-ncvar_get(aa_npp,varid="time") #hours since 1950-01-01 00:00:00	
  dates_npp <- as.POSIXct(3600*time_npp, origin = "1950-01-01", tz = "GMT") # we transform number of days (time) into actual dates
  npp_r <- raster(npp) # transform the matrix into a raster
  npp_r <- t(flip(npp_r, direction="x")) #to rotate the raster
  bb<-extent(min(lon_npp),max(lon_npp),min(lat_npp),max(lat_npp)) #  generate an object with the bounding box: xmin, xmax, ymin, ymax
  npp1<-setExtent(npp_r,bb,keepres=FALSE, snap=FALSE) # apply the bounding box to the raster
  crs(npp1)<-crs("+proj=longlat +datum=WGS84 +no_defs")
  npp2<-mask(npp1, MCP90)
  crs(npp2)<-"+proj=longlat +datum=WGS84 +no_defs"
  npp_mean <- mean(npp2@data@values, na.rm=T) # average raster values
  npp_WestAfrica_df[i,1] = as.POSIXct(dates_npp)
  npp_WestAfrica_df[i,2] = npp_mean
  nc_close(aa_npp) # needed to avoid the error "too many files open"
} 

# Work on the populated data frame. We group by month and average monthly data 
# floor_date explanation: https://ro-che.info/articles/2017-02-22-group_by_month_r
# This function rounds down each date to the month boundary, so that dates in the same month
# are rounded down to the same date. Then we can group all dates of a given month by month
npp_WestAfrica_df_ <- npp_WestAfrica_df %>% 
  group_by(month=floor_date(date, "month")) %>%
  dplyr::summarise(monthly_npp_mean = mean(mean_npp), sd=sd(mean_npp))

# Save daily mean table
# write.csv(npp_WestAfrica_df, here::here("output/mean_daily_npp_MCP90.csv"))
# Save monthly mean table
# write.csv(npp_WestAfrica_df_, here::here("output/mean_monthly_npp_MCP90.csv"))




theme2Review<-theme_bw()+theme(axis.text.x=element_text(size=12),
                               axis.text.y = element_text(size=12),
                               axis.title.y=element_text(size=15),
                               axis.title.x = element_text(size=15),
                               legend.text = element_text(size=11),
                               legend.title = element_text(face="bold", size=13),
                               legend.key.size = unit(0.9, "cm"),
                               legend.background = element_rect(colour ="black", size=0.5, linetype = "dotted"),
                               legend.direction = "vertical",
                               legend.position = "none",
                               plot.margin = margin(2, 2, 2, 2, "cm"), strip.background =element_rect(fill="gray59"),
                               strip.text = element_blank(),
                               strip.placement = "right",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank())


# Plot points and path along them of the obtained mean
npp_monthly <- ggplot(npp_WestAfrica_df_, aes(x=month, y= monthly_npp_mean)) +
  geom_point() + geom_path()+scale_x_date(labels = date_format("%m-%Y"))+
  xlab("\nMonth-Year")+
  ylab(expression(paste("Total Primary Production", (mg/m^{3} * day))))+
  theme2Review



chl_npp_plot <- chl_monthly/npp_monthly

# ggsave(here::here("output/chl_npp_combinedPlot.pdf"), width = 20, height = 10)

