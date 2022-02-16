#----------------------------------------------------------------------------------
# eddies_netCDF_processing.R  Loops to extract rasters from AVISO netCDF
#----------------------------------------------------------------------------------





list.of.packages <- c("here", "raster", "ncdf4", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)                                            

# install.packages("raster")   
# install.packages("Rtools")
library(here)
library(raster)
library(ncdf4)                     
library(lubridate)

  sss
aaa
setwd("D:/Eddies_AVISO")
catalog <- read.csv("eddies_catalog.csv", sep = ";")


for (k in nrow(catalog)) {

print(paste("Processing", catalog$path[k]))  

nc_eddies <- nc_open(catalog$path[k])


# set filter criteria
bb <- extent(-66,6,4,53)+2
date_start <- as.Date(catalog$date_start[k])
date_end <- as.Date(catalog$date_end[k])
dates <- seq.Date(date_start, date_end, by="day")

# create raster mask
r <- raster()
r <- setExtent(r, bb)
res(r) <- 0.083 # setting raster's resolution


# extract data for each eddy
lat<-ncvar_get(nc_eddies, varid="latitude")
lon<-ncvar_get(nc_eddies, varid="longitude")
lon <- ifelse(lon > 180, -360 + lon, lon)

time<-ncvar_get(nc_eddies, varid="time")
date <- as.Date(as.POSIXct(time*24*60*60, origin = "1950-01-01", tz = "GMT"))

# check eddies within study area
sel_lon <- which(lon >= bb@xmin & lon <= bb@xmax)
sel_lat <- which(lat >= bb@ymin & lat <= bb@ymax)


# select eddies
for(i in 1:length(dates)){
  
  YYYY <- year(dates)[i]
  MM <- sprintf("%02d", month(dates))[i]
  DD <- sprintf("%02d", day(dates))[i]
  product_folder <- paste("D:/Eddies_AVISO/DailyRasters/Mesoscale_Eddy_Trajectories_Atlas", "Mesoscale_Eddy_Trajectories_Atlas", YYYY, MM, sep="/")
  if (!dir.exists(product_folder)) dir.create(paste(product_folder, sep = ""), recursive = TRUE)  # create output directory if does not exist
  
  
  
  idate <- dates[i]
  sel_date <- which(date == idate)
  
  
  
  # select eddies in a given day
  sel_obs <- sel_date[which(sel_date %in% sel_lon & sel_date %in% sel_lat)]
  if(length(sel_obs)==0) next
  
  # loop to extract countour data for each eddy
  poly_list <- list()
  
  
  for(j in 1:length(sel_obs)){
    # select eddy data
    jobs <- sel_obs[j]
    mlat <- ncvar_get(nc_eddies, varid="effective_contour_latitude", start=c(1,jobs), count=c(catalog$num_points_contour[k], 1))  # [NbSample,obs], [nt,ntrac]   
    mlon <- ncvar_get(nc_eddies, varid="effective_contour_longitude", start=c(1,jobs), count=c(catalog$num_points_contour[k], 1))  # [NbSample,obs], [nt,ntrac] 
    mlon <- ifelse(mlon > 180, -360 + mlon, mlon)
    
    # convert to polygon
    P1 <- Polygon(cbind(mlon, mlat))
    Ps1 <- SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    
    # append to polygon list
    poly_list[[j]] <- Ps1
    
    
  }
  
  # combine polygons
  
  if (length(poly_list) == 1) {
    
    r <- rasterize(poly_list[[j]], r)
    # 1 for anticyclonic // 2 for cyclonic // NA for no data
    r[!is.na(r)] <- catalog$value[k]
    r[is.na(r)] <- 0
    
    # save raster for day i
    # filename <- sprintf("%s_Mesoscale-Eddy-Trajectories-Atlas-EddyPrAbsType.tif", gsub("-","", idate))
    # writeRaster(r, filename=paste0(product_folder, sep="","/", filename), overwrite = T)
    
                    if (catalog$value[k] == 1) {
                                                 filename <- sprintf("%s_Mesoscale-Eddy-Trajectories-Atlas-EddyAnticyclonic.tif", gsub("-","", idate))
                                                 writeRaster(r, filename=paste0(product_folder, sep="","/", filename), overwrite = T)
                      
                    } else {
                             filename <- sprintf("%s_Mesoscale-Eddy-Trajectories-Atlas-EddyCyclonic.tif", gsub("-","", idate))
                             writeRaster(r, filename=paste0(product_folder, sep="","/", filename), overwrite = T)}
         

                  
    
  } else {
    
    multipol <- bind(unlist(poly_list))
    
    # rasterize polygons
    
    r <- rasterize(multipol, r)
    r[!is.na(r)] <- catalog$value[k]
    r[is.na(r)] <- 0
    
                    if (catalog$value[k] == 1) {
                      filename <- sprintf("%s_Mesoscale-Eddy-Trajectories-Atlas-EddyAnticyclonic.tif", gsub("-","", idate))
                      writeRaster(r, filename=paste0(product_folder, sep="","/", filename), overwrite = T)
                      
                    } else {
                      filename <- sprintf("%s_Mesoscale-Eddy-Trajectories-Atlas-EddyCyclonic.tif", gsub("-","", idate))
                      writeRaster(r, filename=paste0(product_folder, sep="","/", filename), overwrite = T)}
    
    
  }
  
  
  
 }

}


print("netCDF processing ready")
