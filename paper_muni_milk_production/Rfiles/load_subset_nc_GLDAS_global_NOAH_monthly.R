# Load GLDAS data, plot, subset for the Amazon

rm(list=ls())

{
library(raster)
library(ncdf4)
library(rgdal)
}

{
indir.ambasin = "K:/Shared drives/Rondonia_CNH_public/data/spatial_data/hydrology/watersheds/whole_amazon/CD06_CAMREX_1086/data/amztrbbas_v"
shp.amazon = readOGR(dsn=indir.ambasin,layer="amztrbbas_v")
bbox.amazon = extent(shp.amazon)
}

# Read in legal amazon state bnds---has larger footprint than amazon basin.
{
  indir.states.legal.amazon = "K:/Shared drives/Rondonia_CNH_public/data/spatial_data/administrative/legal_amazon/"
  fname.states.lam = "states_legal_amazon"
  states.lam = readOGR(dsn=indir.states.legal.amazon,layer=fname.states.lam)
  bbox.legalam = extent(states.lam)
}

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

#  Read in municipal boundaries--larger than legal amazon (for some reason)
{
  #indir.munis.brasil.2020 = "K:/Shared drives/Rondonia_CNH_public/data/spatial_data/administrative/municipalities/brasil/BR_Municipios_2020/"
  #fname.muni = "BR_Municipios_2020"
  indir.munis.brasil.2015 = "K:/Shared drives/Rondonia_CNH_public/data/spatial_data/administrative/municipalities/brasil/"
  fname.munis.br.2015 = "amz2015_Albers"
  #br.munis = readOGR(dsn=indir.munis.brasil.2020,layer=fname.muni)
  br.munis = readOGR(dsn=indir.munis.brasil.2015,layer=fname.munis.br.2015)
  br.munis.dd = spTransform(br.munis,crs.state)
  br.munis.sp = as(br.munis.dd,"SpatialPolygons")
  bbox.amaz.munis = extent(br.munis.dd)
}

{
#indir.gldas = "K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/water_variables/GLDAS/earthscan/"
indir.gldas = "C:/Users/tbiggs/GLDAS/"
  indir.gldas = "C:/Users/tbiggs/GLDAS/GLDAS_NOAH_1980_2014/"  # Laptop
  indir.gldas = "C:/Users/tbiggs/GLDAS/GLDAS_NOAH_2000_2021_v21/"  # Laptop
outdir.gldas.ambasin =  "K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/water_variables/GLDAS/earthdata/GLDAS_NOAH025_M/v2.1.2000.2021/"
setwd(indir.gldas)
flist = list.files(pattern="nc")
}

# Get list of variables
{
nctmp = nc_open(flist[1])
print(nctmp)
names(nctmp$var)
nc_close(nctmp)
}

# Extractd variables:
   #Evap_tavg evapotranspiration
  #  SoilMoist_RZ_tavg  soil moisture in the root zone
   # TWS_tavg  terrestrial water storage
   # SWneet net SW radiation, w/m2
  # Metadata in https://docs.google.com/document/d/18AIxWk9oUoGZjUdpOODsY7ck0Bng__AiwyjjjkPOQpI/edit?usp=sharing

varlist = c("Tair_f_inst","Swnet_tavg","RootMoist_inst","Rainf_f_tavg","SWdown_f_tavg", "PotEvap_tavg","Qair_f_inst")

flist.yymm = as.numeric(substr(flist,18,23))
flist.years = as.numeric(substr(flist,18,21))
year.unique = unique(flist.years)

# Extract amazon basin for each year
for (varnum in 1:length(varlist)){
  varname.tmp = varlist[varnum]
  for (y in 1:length(year.unique)){
  flist.sub = flist[flist.years==year.unique[y]]
  setwd(indir.gldas)
  for (d in 1:length(flist.sub)){
    print(paste(varname.tmp,flist.sub[d]))
    flush.console()
    r.tmp.soilm = raster(flist.sub[d],varname=varname.tmp)
    r.clip = crop(r.tmp.soilm,bbox.amaz.munis)
    if (d==1){
      b.out = r.clip
    } else {
      b.out = addLayer(b.out,r.clip)
    }
  }
  setwd(outdir.gldas.ambasin)
  writeRaster(b.out,paste0(varname.tmp,"_",year.unique[y],".tif"),overwrite=TRUE)
}
}

plot(r.tmp.soilm)
plot(shp.amazon,add=TRUE)
plot(bbox.amazon,add=TRUE)

plot(b.out[[15]])

# Test clip

plot(r.clip)
plot(shp.amazon,add=TRUE)

setwd(outdir.gldas.ambasin)




