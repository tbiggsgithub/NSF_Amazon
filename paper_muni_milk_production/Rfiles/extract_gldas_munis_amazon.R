# Extract GLDAS variables over Brazil muni boundaries.
  
rm(list=ls())
  {  
  library(raster)
  library(ncdf4)
  library(rgdal)
  library(exactextractr)
}

use.past.crop.mask = 0
{
indir.gldas.veg = "K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/water_variables/GLDAS/earthscan/"

  if (use.past.crop.mask==1){
    outdir.gldas.munis = "K:/My Drive/Gdrive/mydocuments/amazon/writeups/2022_11_laura_milk_climate/data/pastmasked/"
  } else {
    outdir.gldas.munis = "K:/My Drive/Gdrive/mydocuments/amazon/writeups/2022_11_laura_milk_climate/data/allveg/"
  }

fname.veg = "GLDASp5_domveg_CLSMF2.5_025d.nc4"
fname.veg.br = "GLDAS.veg.amazon.tif"
indir.gldas.vars = "K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/water_variables/GLDAS/earthscan/ambasin/"
varnames = c("Evap","SoilMoist_RZ","TWS","Swnet")

varnames = c("AvgSurfT_tavg")

#varnames = "TWS"
units = c("kg.m2.s","kg.m2","mm","Wm2")
conversion.to.mm = c(86400,1,1,1)   # kg/m2/s x 3600sx24h/day x 1m3/1000kg x 1000mm/m  = 86400
                         # kg/m2  x 1m3/1000kg x 1000m/m  = 1
}

# Read in legal amazon state bnds
{
indir.states.legal.amazon = "K:/Shared drives/Rondonia_CNH_public/data/spatial_data/administrative/legal_amazon/"
fname.states.lam = "states_legal_amazon"
states.lam = readOGR(dsn=indir.states.legal.amazon,layer=fname.states.lam)
}

# Load one GLDAS layer to serve as mask for veg
{
setwd(indir.gldas.vars)
btmp = stack("Evap_tavg_2003.tif")
rtmp = btmp[[1]]
plot(rtmp)
plot(states.lam,add=TRUE)
extent.gldas = extent(rtmp)
}

# Load global veg layer, subset to Brazil
{
extract.veg = 0
setwd(indir.gldas.veg)
if (extract.veg==1) {
r.veg = raster(fname.veg)  # Global veg map
r.veg.amazon = crop(r.veg,extent.gldas)
plot(r.veg.amazon)
plot(states.lam,add=TRUE)
writeRaster(r.veg.amazon,"GLDAS.veg.amazon.tif",format="tif", overwrite=TRUE)  # So can read in arcgIS, run only first time.
  # Legend: https://docs.google.com/document/d/18AIxWk9oUoGZjUdpOODsY7ck0Bng__AiwyjjjkPOQpI/edit?usp=sharing
}
}

# Load GLDAS veg for Brazil
setwd(indir.gldas.veg)
r.veg.amazon = raster("GLDAS.veg.amazon.tif")
plot(r.veg.amazon)
# Read in RO shape files
#  Read in Rondonia shapefile
{
  indir.rond.shp = "K:/Shared drives/Rondonia_CNH_public/data/spatial_data/administrative/Rondonia_state_boundary"
  fname.rnd = "rondonia_bnd_dd"
  rond.state = readOGR(dsn=indir.rond.shp,layer=fname.rnd)
  
  # Project State boundary into geographic WGS 84
  crs.state = crs(rond.state)
  #shp.dd = spTransform(shp,crs.state)
}

#  Read in municipal boundaries
{
  #indir.munis.brasil.2020 = "K:/Shared drives/Rondonia_CNH_public/data/spatial_data/administrative/municipalities/brasil/BR_Municipios_2020/"
  #fname.muni = "BR_Municipios_2020"
  indir.munis.brasil.2015 = "K:/Shared drives/Rondonia_CNH_public/data/spatial_data/administrative/municipalities/brasil/"
    fname.munis.br.2015 = "amz2015_Albers"
  #br.munis = readOGR(dsn=indir.munis.brasil.2020,layer=fname.muni)
    br.munis = readOGR(dsn=indir.munis.brasil.2015,layer=fname.munis.br.2015)
    br.munis.dd = spTransform(br.munis,crs.state)
  br.munis.sp = as(br.munis.dd,"SpatialPolygons")
}

# Plot to check that all line up
{
plot(r.veg.amazon)
plot(rond.state,add=TRUE)
plot(br.munis.sp,add=TRUE)
#plot(rond.state,col="red",add=TRUE)
}
# Click on raster to find veg values
# click(r.veg)  # Takes a long time for the global veg map

# Set up a mask for the GLDAS grids so only class 7 land use (open woodlands --- pasture) is used
# 
  # Load example GLDAS file, check all works.
setwd(indir.gldas.vars)
{var.index = 1
f=1
flist.var = list.files(indir.gldas.vars,pattern=varnames[var.index])
b.var = stack(flist.var[f],bands=1)
past.mask = r.veg.amazon
}

if (use.past.crop.mask==1){
past.mask[(past.mask != 7) & (past.mask != 11) & (past.mask != 10)] = NA  # 7 is pasture, 10 grassland, 11 is cropland
past.mask[(past.mask==7) | (past.mask==11) | past.mask==10] = 1
} else {
  past.mask[past.mask>0] = 1  # Include all veg if use.past.crop.mask <> 1
}

plot(past.mask)
plot(rond.state,add=TRUE)
plot(br.munis.sp,add=TRUE)

# Extract GLDAS time series over each variable

for (var.index in 1:length(varnames)){
  flist.var = list.files(indir.gldas.vars,pattern=varnames[var.index])
  for (f in 1:length(flist.var)){
    print(flist.var[f])
    flush.console()
    setwd(indir.gldas.vars)
    b.var.all = stack(flist.var[f])
    nlays = nlayers(b.var.all)
    
    # Process in 2 stacks (DOY 1-180 and 181-365/6)--1:365 too big
      b.crop = crop(b.var.all[[1:180]],past.mask)
      b.masked = mask(b.crop,past.mask)

    var.ext = exact_extract(b.masked,br.munis.sp,fun="mean")
    var.mm = var.ext*conversion.to.mm[var.index]
    
      b.crop = crop(b.var.all[[181:nlays]],past.mask)
      b.masked = mask(b.crop,past.mask)
    
    var.ext = exact_extract(b.masked,br.munis.sp,fun="mean")
    var.mm.2 = var.ext*conversion.to.mm[var.index]

    var.out = data.frame(t(cbind(var.mm,var.mm.2)),row.names=NULL)
    var.out = round(var.out,2)
    names(var.out) = br.munis.dd$CD_GEOCMU
    nchar.fname = nchar(flist.var[f])
    year = substr(flist.var[f],nchar.fname-7,nchar.fname-4)
    if (year=="2003"){jdays = seq(32,365)} else {jdays=seq(1:nrow(var.out))}# 2003 GLDAS starts on Feb 1, 2003.
    var.out$Date = as.Date(paste0(year,jdays),format="%Y%j")
    if (f==1){
      var.df = var.out
    } else {
      var.df = rbind(var.df,var.out)
    }
  }  # end scrolling through years
  setwd(outdir.gldas.munis)
  fname.out = paste0(varnames[var.index],".csv")
  write.csv(var.df,fname.out)
}  # end scrolling through variables


