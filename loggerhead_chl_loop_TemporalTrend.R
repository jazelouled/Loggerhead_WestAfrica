library(ncdf4)
library(raster)

setwd("C:/Users/Jouled Cheikh/Dropbox/PhD/Alessandra/phy/sss")
aa_sss<-nc_open("C:/Users/Jouled Cheikh/Dropbox/PhD/Alessandra/phy/global-reanalysis-phy-001-030-monthly_1633100946575.nc") #open de .nc file.
sss<-ncvar_get(aa_sss,varid="so") #sss
time_sss<-ncvar_get(aa_sss,varid="time") #hours since 1950-01-01 00:00:00
dates_sss <- as.POSIXct(3600*time_sss, origin = "1950-01-01", tz = "GMT") # we transform number of days (time) into actual dates
seq_year <- seq(1, length(dates_sss), 1)    

listRastersSSS<-list()
 
chl_years <- dir(here::here("data/RemoteSensing/GLOBAL_REANALYSIS_BIO_001_029-TDS/global-reanalysis-bio-001-029-daily")) #include in the "files" object all the chl files
chl_daily <- dir(here::here("data/RemoteSensing/GLOBAL_REANALYSIS_BIO_001_029-TDS/global-reanalysis-bio-001-029-daily/1999/08/")) #include in the "files" object all the chl files

setwd(here::here("data/RemoteSensing/GLOBAL_REANALYSIS_BIO_001_029-TDS/global-reanalysis-bio-001-029-daily"))                

for (i in 1:length(chl_years)) { chl_months <- chl_years[i]
      chl_months[i] <- dir(here::here("data/RemoteSensing/GLOBAL_REANALYSIS_BIO_001_029-TDS/global-reanalysis-bio-001-029-daily")) #include in the "files" object all the chl files

          for (j in 1:length(chl_months)){ chl_daily <- chl_months[j]
          
                  chl_daily

                              
                              
                              
                              
                            }
                             
  
  
  
}




  for j
      for k 

dir(here::here("data/RemoteSensing/GLOBAL_REANALYSIS_BIO_001_029-TDS/global-reanalysis-bio-001-029-daily"[j])) #include in the "files" object all the chl files

chl_files <- dir(here::here("data/RemoteSensing/bio"), pattern="chl") #include in the "files" object all the chl files

for (j in 1:length(chl_files)){
  aa_sss<-nc_open(chl_files[j]) #open netCDF file.
  sss<-ncvar_get(aa_sss,varid="chl") #sss
  lat_sss<-ncvar_get(aa_sss,varid="latitude") #latitude
  lon_sss<-ncvar_get(aa_sss,varid="longitude") #longitude: -180-180 
  time_sss<-ncvar_get(aa_sss,varid="time") #hours since 1950-01-01 00:00:00
  sss <- raster(sss[,,i]) # transform the matrix into a raster
  sss1 <- t(flip(sss, direction="x")) #to rotate the raster
  bb<-extent(min(lon_sss),max(lon_sss),min(lat_sss),max(lat_sss)) #  generate an object with the bounding box: xmin, xmax, ymin, ymax
  projection(sss1)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  sss2<-setExtent(sss1,bb,keepres=FALSE, snap=FALSE) # apply the bounding box to the raster
  bb2<-extent(-20, -10, 14, 23)
  sss2<-crop(sss2, bb2)
  sss2raster <- paste0('sss_', i, '.tif')
  
}





for (i in 1:nrow(chl_years)){
  #### SST preferred ranges (AquaMaps)
  year_sst <- seq(1999,2019, 1)
  sst_sp <- list() 
}
  
  for (j in 1:length(year_sst)){ 
    month_chl <- sst_med[[j]]
    m <- c(0, species$min_sst[i], 0, species$min_sst[i] , species$max_sst[i], 1, species$max_sst[i], 50,  0) 
    rcl <- matrix(m, ncol=3, byrow=TRUE) #reclass matrix
    sst_mean_reclass<-reclassify(sst1, rcl, na.rm=TRUE)
    sst_sp[j] <- sst_mean_reclass
    print(j)
  }
  
  sst_sp <- stack(sst_sp)
  sst_sp_sum <- sum(sst_sp)




























