# Load and compare GLDAS data for Brazil Munis.

indir.gldas.by.muni =  "K:/My Drive/Gdrive/mydocuments/amazon/writeups/2022_11_laura_milk_climate/data/allveg/"
flist = list.files(indir.gldas.by.muni)

var = "SWdown"
fnames = flist[grep(var,flist)]
fnamev20 = fnames[grep("v20",fnames)]
fnamev21 = fnames[grep("v21",fnames)]

setwd(indir.gldas.by.muni)
xv20 = read.csv(fnamev20)
xv21 = read.csv(fnamev21)

loadgldas <- function(var){
  {
    fnames = flist[grep(var,flist)]
  fnamev20 = fnames[grep("v20",fnames)]
  fnamev21 = fnames[grep("v21",fnames)]

  setwd(indir.gldas.by.muni)
  xv20 = read.csv(fnamev20)
  xv21 = read.csv(fnamev21)
  }

{
xv20names = names(xv20)
xv20datestr = xv20names[3:length(xv20names)]
xv20data = xv20[,3:length(xv20names)]
xv20Date = as.Date(paste0(substr(xv20datestr,2,nchar(xv20datestr)),"01"),format="%Y%m%d")
}

{
  xv21names = names(xv21)
  xv21datestr = xv21names[3:length(xv21names)]
  xv21data = xv21[,3:length(xv21names)]
  xv21Date = as.Date(paste0(substr(xv21datestr,2,nchar(xv21datestr)),"01"),format="%Y%m%d")
}
  out = list(xv20Date,xv20data,xv21Date,xv21data)
  return(out)
}


mindex.vec = c(1,50,100,150,200,250,300,350,400,450)

plotgldas <- function(varname,unit,ylims){
par(mfrow=c(5,2),mar=c(1,0,0,0),oma=c(2,4,1,2))
atdates = as.Date(c("2000-01-01","2005-01-01","2010-01-01"))
for (i in 1:length(mindex.vec)){
  mindex = mindex.vec[i]
  plot(xv20Date,as.numeric(xv20data[mindex,]),type="l",xlim=as.Date(c("2000-01-01","2014-12-01")),xaxt="n",ylim=ylims,yaxt="n")
  lines(xv21Date,as.numeric(xv21data[mindex,]),col="blue")
  if (i %in% c(9,10)){
    axis(side=1,at=atdates,labels=format(atdates,"%Y"))
  } else {
    axis(side=1,at=atdates,labels=FALSE)
  }
  if (i %in% c(1,3,5,7,9)){
    axis(side=2)
  } else {
      axis(side=2,labels=FALSE)
    }
}
mtext(side=2,paste(varname,unit),outer=TRUE,line=2.5)
}

xlist = loadgldas("SWdown")
xv20Date = xlist[[1]]
xv20data = xlist[[2]]
xv21Date = xlist[[3]]
xv21data = xlist[[4]]

plotgldas("SWdown",unit="W/m2",ylims=c(150,300))

# Combine v20 and v21 for SW and save
index.v20.1999.12 = which(as.Date(xv20Date)<as.Date("2000-01-01"))
xv20.21.combined = cbind(xv20data[,index.v20.1999.12],xv21data)
xv20.21.names = names(xv20.21.combined)
xv20.21.comb.dates = as.Date(paste0(substr(xv20.21.names,2,nchar(xv20.21.names)),"01"),format="%Y%m%d")
plot(xv20.21.comb.dates,xv20.21.combined[450,],type="l")
xv20.21.comb.out = data.frame(xv20$GEOCMU,xv20.21.combined)
writem=0
if (writem==1){
  write.csv(xv20.21.comb.out,"GLDAS_NOAH025_M_v_20_21_combined.csv")
}

var="Tair"
xlist = loadgldas(var)
xv20Date = xlist[[1]]
xv20data = xlist[[2]]
xv21Date = xlist[[3]]
xv21data = xlist[[4]]
plotgldas(var,unit="K",ylims=c(295,305))

var="RootMoist"
xlist = loadgldas(var)
xv20Date = xlist[[1]]
xv20data = xlist[[2]]
xv21Date = xlist[[3]]
xv21data = xlist[[4]]
plotgldas(var,unit="kg/m2",ylims=c(100,600))



