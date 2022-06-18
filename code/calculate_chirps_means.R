# Calculate means of CHIRPS rainfall data for Rondonia
# Uses calibrated CHIRPS data as documented in 
# Mu, Y., Biggs, T., & Shen, S. S. P. (2021). Satellite-based precipitation estimates using a dense rain gauge network over the Southwestern Brazilian Amazon: Implication for identifying trends in dry season rainfall. 
  # Atmospheric Research, 105741. https://doi.org/10.1016/j.atmosres.2021.105741

# Load CHIRPS grids
{
  library(ncdf4)
  library(raster)
  library(rgdal)
  library(exactextractr)  # Super fast raster extraction over polygons
}

write.files = 0  # Change to 1 to write files
indir.chirps = "K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/water_variables/CHIRPS_grids/monthly/dnCHIRPS_2_3rd_calibration/"
flist.chirps = list.files(indir.chirps,pattern="bil")

{
  indir.rond.shp = "K:/Shared drives/Rondonia_CNH_public/data/spatial_data/administrative/Rondonia_state_boundary"
  fname.rnd = "rondonia_bnd_dd"
  rond.state = readOGR(dsn=indir.rond.shp,layer=fname.rnd)
}

setwd(indir.chirps)
r = raster(flist.chirps[1])
plot(r)
plot(rond.state,add=TRUE)
plot(shp.dd,add=TRUE)

#  
dates.chirps = as.Date(paste0(substr(flist.chirps,13,18),"-01"),format="%Y%m-%d") # Adds a day (the 1st) to each yyyy-mm combo to create dates
yyyymm = substr(flist.chirps,13,18)
yyyy = as.numeric(format(dates.chirps,"%Y"))
mm = as.numeric(format(dates.chirps,"%m"))
wy = yyyy
wy[mm>=9] = wy[mm>=9]+1
check.df = data.frame(dates.chirps,yyyy,wy)
# Calculate mean dry season rainfall
flist.dryseas = flist.chirps[mm %in% c(6,7,8)]
drystack = stack(flist.dryseas)
drymean = mean(drystack)

plot(drymean)
plot(rond.state,add=TRUE)

setwd("K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/water_variables/CHIRPS_grids/monthly/dnCHIRPS_2_3rd_calibration/processed/")

if (write.files==1){
  writeRaster(drymean,filename="mean.1981.2019.JJA.bil")
}

ylist = unique(yyyy)
for (y in ylist){
  print(y)
  flush.console()
  flist.dry.yy = flist.chirps[(yyyy==y) & (mm %in% c(6,7,8))]
  setwd(indir.chirps)
  stacktmp = stack(flist.dry.yy)
  drymeantmp = 3*mean(stacktmp)
  setwd("K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/water_variables/CHIRPS_grids/monthly/dnCHIRPS_2_3rd_calibration/processed/")
  
  if (write.files==1){
    writeRaster(drymeantmp,filename=paste0("mean",y,"JJA.bil"))
  }
  
}

# Calculate annual total, annual means
for (y in ylist){
  print(y)
  flush.console()
  flist.dry.yy = flist.chirps[(yyyy==y)]
  setwd(indir.chirps)
  stacktmp = stack(flist.dry.yy)
  meantmp = mean(stacktmp)
  setwd("K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/water_variables/CHIRPS_grids/monthly/dnCHIRPS_2_3rd_calibration/processed/")
  
  if (write.files==1){
    writeRaster(meantmp,filename=paste0("mean",y,"wy.bil"))
  }
}

# Calculate annual mean
setwd("K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/water_variables/CHIRPS_grids/monthly/dnCHIRPS_2_3rd_calibration/processed/annual/")
flist.ann = list.files(pattern="wy.bil")

stack.ann = stack(flist.ann)
annmean = 12*mean(stack.ann)
plot(annmean)

if (write.files==1){
  writeRaster(annmean,filename="mean.1982.2019.wateryear.mm.yr.bil")
}
