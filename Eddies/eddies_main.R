#----------------------------------------------------------------------------------
# eddies_main.R            Main script for processing eddies netcdf from AVISO
#----------------------------------------------------------------------------------


#---------------------------------------------------------------
# 1. NetCDF processing & daily raster creation
#---------------------------------------------------------------

source("eddies_netCDF_processing.R")



#---------------------------------------------------------------
# 2. Stack creation and combination of daily output  (cyclonic + anticyclonic)
#---------------------------------------------------------------

setwd("D:/Eddies_AVISO/DailyRasters/Mesoscale_Eddy_Trajectories_Atlas/Mesoscale_Eddy_Trajectories_Atlas")
dir<-"D:/Eddies_AVISO/DailyRasters/Mesoscale_Eddy_Trajectories_Atlas/Mesoscale_Eddy_Trajectories_Atlas"


files<-list.files(pattern=".tif", recursive = T, full.names = F)
dates <- substr(files, 9, 16)

for (i in length(dates)) {

              idate <- dates[i]
              sel_file <- files[which(substr(files, 9, 16) == idate)]
              if(length(sel_file)<2) next
              rst<-sum(stack(sel_file))
              filename <- sprintf("%s_Mesoscale-Eddy-Trajectories-Atlas-EddyPrAbsType.tif", gsub("-","", idate))
              writeRaster(rst, filename=paste0(dir, sep="","/", filename), overwrite = T)
                          
              }

