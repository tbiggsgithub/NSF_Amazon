# Load and plot ERA5 soil moisture

layer = 1
cover = "Short and Tall Grass"
fncode = "ShortTallGrassAll"  # Of the original files
indir = paste0("K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/Water/soils/GEE_ERA5_SoilMoistAll_municipality/",cover,"/")
fn.out = paste0("ERA5_muni_SoilMoist_19790102_20201231_",gsub(" ","",cover),"_Layer",layer,"_RO.csv")

x = read.csv(paste0(indir,fn.out))
#x1.ariq = x1[x1$]

# Load muni names and codes
indir.municodes = "K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/Water/soils/GEE_ERA5_SoilMoistAll_municipality/"
fn.municodes = "MuniCodes_Names_All_Amazon.csv"
x.mcode.mname = read.csv(paste0(indir.municodes,fn.municodes))

RO.codes = names(x)[grep("11",names(x))]
RO.codes = substr(RO.codes,2,8)
RO.codes.df = data.frame(RO.codes,foo=NA)
RO.namelist = merge(RO.codes.df,x.mcode.mname,by.x="RO.codes",by.y="mcode",all.x=TRUE)

# Select a few to plot
x$Date = as.Date(x$Date)
plot(x$Date,x$X1100023,type="l",xlim=as.Date(c("2004-01-01","2019-12-31")))

years.to.plot = c(2000,2005,2010,2015,2016,2019)
x$Year = as.numeric(format(x$Date,"%Y"))
colvec = c("black","yellow","orange","red","purple","blue")
lwdvec = c(1,1,1,1,1,2)
for (j in 1:length(years.to.plot)){
  y = years.to.plot[j]
  xsub = x[x$Year==y,]
  if (j==1){
    xkeep = xsub # Keep for the date vector
    plot(xkeep$Date[1:365],xsub$X1100023[1:365],type="l",col=colvec[j],ylim=range(x$X1100023),xlab="",ylab=paste0("Soil moisture Layer ",layer),las=1)
  } else {
    lines(xkeep$Date[1:365],xsub$X1100023[1:365],col=colvec[j],lwd=lwdvec[j])
  }
  legend("bottomleft",legend=years.to.plot,col=colvec,lty=1,bty="n",lwd=c(1,1,1,1,1,2))
}

