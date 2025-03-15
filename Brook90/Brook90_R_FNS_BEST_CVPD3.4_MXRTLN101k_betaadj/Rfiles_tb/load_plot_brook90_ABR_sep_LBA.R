#  Run Brook90 and format Brook90 output
#  Runs 2 versions of Brook90P: Abracos period (high LAI) and LBA period (low LAI)

{
library(dplyr)
library(DataCombine)
  library(plotrix)
}

# Run B90
# Need to run each model twice due to changing LAI parameters

#  To change models, change the model directory path in BV90.R and MainProg.R
#  and in three locations below:
{
indir.base = "K:/My Drive/Gdrive/mydocuments/amazon/writeups/2019_NSF_SSiB_1D/Rfiles/Brook90/Brook90_R_FNS_BEST_sensitivity/"
indir.model = "Brook90_R_FNS_BEST_CVPD3.4/"  # Sensitivity on CVPD
#indir.model = "Brook90_R_FNS_BEST_sensitivity/Brook90_R_FNS_BEST_3_layer/"

#indir.model = "Brook90_R_FNS_v4_5soil_layer_updateveg_ABRLBA/"
indir.code = paste0(indir.base,indir.model,"Rfiles_tb/B90V4.R")
input.dir = paste0(indir.base,indir.model,"Input_data/")

#  ABRACOS veg
setwd(input.dir)
file.copy(from="canopy_ABR.txt",to="canopy.txt",overwrite=TRUE)
file.copy(from="location_ABR.txt",to="location.txt",overwrite=TRUE)
source(indir.code)
ts.abr = timeseries_df
indir.base = "K:/My Drive/Gdrive/mydocuments/amazon/writeups/2019_NSF_SSiB_1D/Rfiles/Brook90/Brook90_R_FNS_BEST_sensitivity/"
indir.model = "Brook90_R_FNS_BEST_CVPD3.4/"  # Sensitivity on CVPD
#indir.model = "Brook90_R_FNS_BEST_sensitivity/Brook90_R_FNS_BEST_3_layer/"
outdir.output = paste0(indir.base,indir.model,"output/")
write.csv(ts.abr,paste0(outdir.output,"ts.abr.csv"),row.names=FALSE)

# LBA veg
indir.code = paste0(indir.base,indir.model,"Rfiles_tb/B90V4.R")
input.dir = paste0(indir.base,indir.model,"Input_data/")
setwd(input.dir)
file.copy(from="canopy_LBA.txt",to="canopy.txt",overwrite=TRUE)
file.copy(from="location_LBA.txt",to="location.txt",overwrite=TRUE)
source(indir.code)
ts.lba = timeseries_df
indir.base = "K:/My Drive/Gdrive/mydocuments/amazon/writeups/2019_NSF_SSiB_1D/Rfiles/Brook90/Brook90_R_FNS_BEST_sensitivity/"
indir.model = "Brook90_R_FNS_BEST_CVPD3.4/"  # Sensitivity on CVPD

outdir.output = paste0(indir.base,indir.model,"output/")
write.csv(ts.lba,paste0(outdir.output,"ts.lba.csv"),row.names=FALSE)
}

{
#  Combine into one
ts.abr = read.csv(paste0(outdir.output,"ts.abr.csv"),stringsAsFactors = FALSE, header=TRUE)
ts.abr$MODELDATE = as.Date(ts.abr$MODELDATE)
ts.abr$TRUDATE = as.Date(ts.abr$TRUDATE)
ts.lba$MODELDATE = as.Date(ts.lba$MODELDATE)
ts.lba$TRUDATE = as.Date(ts.lba$TRUDATE)
ts.comb = ts.abr
index.lba = which(ts.comb$TRUDATE>as.Date("1995-01-01"))
ncol = length(ts.comb[1,])
ts.comb[index.lba,c(5:ncol)] = ts.lba[index.lba,5:ncol]
ts.comb$ET[ts.comb$ET<0] = NA
}

{
dates = as.Date(paste(MData[[1]],MData[[2]],MData[[3]],sep="-"))
truedate = as.Date(MData[[13]])
flag = MData[[11]]
MJtommd = 0.408  # http://www.fao.org/3/x0490e/x0490e04.htm
Wm2toMJm2h = 0.0036
Wm2tommd = 24*Wm2toMJm2h*MJtommd
# Find dates with missing data that was filled
# 1999-01-01 to 1999-02-04 was filled for all variables
nmissing = MData[11,]
force.data = data.frame(Date=truedate,Modeldate=dates,Rn=MData[[4]],Tmax=MData[[5]],Tmin=MData[[6]],ea=MData[[7]],u=MData[[8]],P=MData[[9]])
} 
 
{
# Load observed data
indir.R = "K:/My Drive/Gdrive/mydocuments/amazon/writeups/2019_NSF_SSiB_1D/Rfiles/"
source(paste0(indir.R,"1_load_obs_data.R"))

# Load daymean LBA data
indir.daymean = "K:/Shared drives/Rondonia_CNH_public/data/hydrology_data/ABRACOS_LBA_data_radiation_ET_met/LBA/CD32_BRAZIL_FLUX_NETWORK_1174/data/RON_FNS/"
fname.daymean = "FNS_Avg_day.csv"
xin.daymean.lba = read.csv(paste0(indir.daymean,fname.daymean),skip=2,header=TRUE,stringsAsFactors = FALSE)
xin.daymean.lba[xin.daymean.lba==-9999.0000]=NA
xin.daymean.lba$Date = as.Date(paste(xin.daymean.lba$year,xin.daymean.lba$day,sep="-"),format="%Y-%j")
xin.daymean.lba[,c("Date","LE")]
}

#  Load ABRACOS observed data
{
indir.abr = setwd("K:/Shared drives/Rondonia_CNH_public/models/SSiB/Amazonia/PRE-LBA_ABRACOS_899/processed_by_DKim/ABRACOS_micromet/allpast/")
fn.abracos = "PASTM3.M45.M7_no_soildata.csv"
obs_df_abr = read.csv(fn.abracos,header=TRUE,stringsAsFactors = FALSE)

obs_df_abr$Date.time = strptime(paste(obs_df_abr$YR,obs_df_abr$Doy,obs_df_abr$Hr,sep="-"),format="%Y-%j-%H")
obs_df_abr$Date = format(obs_df_abr$Date.time,"%Y-%m-%d")
obs_df_abr$cnt = 1
# Close energy balance on ABRACOS data: Bowen ratio method:  Already closed, so no corrected needed.
obs_df_abr_daytime = obs_df_abr[obs_df_abr$Rs>0,]

obs_daily_abr = aggregate(obs_df_abr,by=list(obs_df_abr$Date),FUN="mean",na.rm=FALSE)
obs_daily_numdata = aggregate(obs_df_abr$cnt,by=list(obs_df_abr$Date),FUN="sum",na.rm=FALSE)
obs_daily_abr$numdata = obs_daily_numdata$x  # number of hours in a day with data
        # Used to exclude data later on

obs_daily_daytime_abr = aggregate(obs_df_abr_daytime,by=list(obs_df_abr_daytime$Date),FUN="mean",na.rm=FALSE)
obs_daily_daytime_daylength_agg = aggregate(obs_df_abr_daytime$cnt,by=list(obs_df_abr_daytime$Date),FUN="sum",na.rm=FALSE)

# Insert a NA line to give a gap so there's no line adjoining the time series
obs_daily_abr$LEdaytime = obs_daily_daytime_abr$LE
obs_daily_abr$Rnetdaytime = obs_daily_daytime_abr$Rnet
obs_daily_abr$Daylength = obs_daily_daytime_daylength_agg$x
obs_daily_abr = obs_daily_abr[obs_daily_abr$numdata==24,]
obs_daily_abr = obs_daily_abr[obs_daily_abr$YR<1994,c(1,2,5:length(obs_daily_abr[1,]))]  # Exclude 1994, Doy, Hr
obs_daily_abr_yrdiff = diff(obs_daily_abr$YR)
index.yrchg = which(obs_daily_abr_yrdiff==1)
#obs_daily_abr[index.yrchg,]
obs_daily_abr = InsertRow(obs_daily_abr,NewRow = c("1993-01-01",1993,rep(NA,times=length(obs_daily_abr[1,])-2)),RowNum=index.yrchg+1)

#  InsertRow converts all columns to character---convert numeric back.
for (foo in 3:length(obs_daily_abr[1,])){
  obs_daily_abr[,foo] = round(as.numeric(obs_daily_abr[,foo]),2)
}
obs_daily_abr[,"YR"] = as.numeric(obs_daily_abr[,"YR"])
obs_daily_abr$Date = as.Date(obs_daily_abr$Group.1)

# Calcualte daily ET from LE
obs_daily_abr$ET.mmday = obs_daily_abr$LE*MJtommd*Wm2toMJm2h*24
obs_daily_abr$ET.mmday.daytime = obs_daily_abr$LEdaytime*MJtommd*Wm2toMJm2h*obs_daily_abr$Daylength
obs_daily_abr$Modeldate = as.Date(paste0(obs_daily_abr$YR+5,"-",format(obs_daily_abr$Date, "%m-%d")))
#obs_daily_abr[,c("Date","LE")]
}

# Load ABRACOS soil data
{
  indir = "K:/My Drive/Gdrive/mydocuments/amazon/amazondata/met_and_soil_moisture_data/ABRACOS/PRE-LBA_ABRACOS_899/data/SOIL_PHYSICS/"
  fname.pasture = "NSPSOIL.DAT"
  fname.forest = "RJFSOIL.DAT"
  
  # Values are moisture volume fraction
  
  cols = c(9,16,22,29,36,seq(43,155,by=7))
  cwidths = c(cols[1],diff(cols))
  sm.abr.in = read.fwf(paste0(indir,fname.pasture),widths=cwidths,stringsAsFactors=FALSE)
  sm.abr = sm.abr.in[2:length(sm.abr.in[,1]),]
  names(sm.abr) = paste0("SM.",gsub(" ","",as.character(sm.abr.in[1,])))
  head(sm.abr)
  sm.abr$Date = as.Date(paste0(sm.abr$SM.YEAR,sm.abr$SM.DAY),format="%Y%j")
  sm.abr$MYear = as.numeric(sm.abr$SM.YEAR)+5
  sm.abr$Modeldate = as.Date(paste0(sm.abr$MYear,sm.abr$SM.DAY),format="%Y%j")
  abr.depths = c(10,10,20,20,20,20,20,20,20,20,20)
  
  for (r in 1:length(sm.abr[,1])){
    sm.abr$sm.0.2m[r] = 10*sum(sm.abr[r,4:14]*abr.depths)
  }
  
}

#  Calculate mean values, to calculate normalised RMSEs
#{
  #lhf_mean = mean(obs_dailydaytime_simdates$LEclosed.bowen,na.rm=TRUE)
  #lhf_dry_mean = mean(obs_dailydaytime_simdates$LEclosed.bowen[obs_dailydaytime_simdates$season=="dry"],na.rm=TRUE)
  #lhf_wet_mean = mean(obs_dailydaytime_simdates$LEclosed.bowen[obs_dailydaytime_simdates$season=="wet"],na.rm=TRUE)
  #obs_df_sub = obs_dailydaytime_simdates[obs_dailydaytime_simdates$Date %in% seq.Date(as.Date("2000-05-14"),as.Date("2002-04-02"),by="day"),]  
  # sm_mean = mean(obs_df_sub$sm.interp.0.2.5m.mm/2500)
  #LHdivRng_mean = mean(obs_dailydaytime_simdates$LH.div.RnmG,na.rm=TRUE)
#}

# Load SSIB data
{
  indir.ssib = "K:/My Drive/Gdrive/mydocuments/amazon/writeups/2019_NSF_SSiB_1D/Rfiles/Brook90/other_data/SSIB/"
  ssib.fname = "set623.out.rst.txt"
  ssib.fname = "set0.out.new.txt"
  x.ssib = read.table(paste0(indir.ssib,ssib.fname),header=TRUE)
  x.ssib$Date = as.Date(paste(x.ssib$year,x.ssib$doy,sep="-"),format="%Y-%j")
  x.ssib.aggcols = c("rnet","lhf")
  #  Fill missing
  x.ssib[x.ssib==-999] = NA
  x.ssib[,x.ssib.aggcols] = na.approx(x.ssib[,x.ssib.aggcols],maxgap=4)
  x.ssib.small = x.ssib[,c("Date",x.ssib.aggcols)]
  x.ssib.daily = aggregate(x.ssib.small,by=list(x.ssib.small$Date),FUN="mean")  
  x.ssib.daily$ET.mmday = x.ssib.daily$lhf*MJtommd*Wm2toMJm2h*24
}

#  Biggs plot output
{
  xlimlist = list()
  atslist = list()
  
  #  1997-2002:  check whole period together for consistency
  xlimdates = as.Date(c("1996-10-01","2002-11-01"))
  ats = as.Date(c("1996-10-01","1997-01-01","1997-06-01","1998-01-01","1998-06-01","1999-01-01","1999-06-01","2000-01-01","2000-06-01","2001-01-01","2001-07-01","2002-01-01","2002-06-01","2002-11-01"))
  xlimlist[[1]]=xlimdates
  atslist[[1]] = ats
  
  #  Wet season 2001-02
  xlimdates = as.Date(c("2001-12-01","2002-03-01"))
  ats = as.Date(c("2001-12-01","2002-01-01","2002-02-01","2002-03-01"))
  xlimlist[[2]]=xlimdates
  atslist[[2]] = ats
  
  # Dry season 2000
  xlimdates = as.Date(c("2000-07-01","2000-10-01"))
  ats = as.Date(c("2000-07-01","2000-08-01","2000-09-01","2000-10-01"))
  xlimlist[[3]]=xlimdates
  atslist[[3]] = ats
  
  # Dry season 2001
  xlimdates = as.Date(c("2001-07-01","2001-10-01"))
  ats = as.Date(c("2001-07-01","2001-08-01","2001-09-01","2001-10-01"))
  xlimlist[[4]]=xlimdates
  atslist[[4]] = ats
  
  # Dry season 1999
  xlimdates = as.Date(c("1999-05-01","1999-11-01"))
  ats = as.Date(c("1999-05-01","1999-07-01","1999-09-01","1999-11-01"))
  xlimlist[[5]]=xlimdates
  atslist[[5]] = ats
  
  # All season 1999
  xlimdates = as.Date(c("1999-02-01","1999-11-01"))
  ats = as.Date(c("1999-02-01","1999-05-01","1999-08-01","1999-11-01"))
  xlimlist[[6]]=xlimdates
  atslist[[6]] = ats
  
  # ABRACOS period
  xlimdates = as.Date(c("1996-10-01","1998-10-01"))
  ats = as.Date(c("1996-10-01","1997-10-01","1998-10-01"))
  xlimlist[[7]]=xlimdates
  atslist[[7]] = ats
  
  # 1992 dry season
  xlimdates = as.Date(c("1997-05-01","1997-11-01"))
  ats = as.Date(c("1997-05-01","1997-07-01","1997-09-01","1997-11-01"))
  xlimlist[[8]]=xlimdates
  atslist[[8]] = ats
  
  # 1993 dry season
  xlimdates = as.Date(c("1998-05-01","1998-11-01"))
  ats = as.Date(c("1998-05-01","1998-07-01","1998-09-01","1998-11-01"))
  xlimlist[[9]]=xlimdates
  atslist[[9]] = ats
  
  # LBA period
  xlimdates = as.Date(c("1999-01-01","2002-11-01"))
  ats = as.Date(c("1999-05-01","1999-07-01","2001-09-01","2002-11-01"))
  xlimlist[[10]]=xlimdates
  atslist[[10]] = ats
  
}

# Get date ranges of missing data, add as shaded polygons
{flagNA = flag
flagNA[flagNA>0]=NA
x_rle = rle(is.na(flagNA))
x_rle_summary = x_rle$lengths[x_rle$values==TRUE]
x_rle_table = table(x_rle_summary)
runs.lengths.cumsum = cumsum(x_rle$lengths)
na.index.end = runs.lengths.cumsum[x_rle$values==TRUE]
na.index.begin = c(1,runs.lengths.cumsum[x_rle$values==FALSE]+1)
timeseries_rn[flag>1]=NA
}

plotssib = 0
plotall = 0
if (plotall==1){
for (k in 1:length(xlimlist)){
  
  {
    dev.new(h=10,w=10)
    par(mfrow=c(3,1),mar=c(0,0,0,0),oma=c(4,4,1,1))
    xlimdates=xlimlist[[k]]
    ats = atslist[[k]]
    
    plotsw=0
    if (plotsw==1){
      # RN (net radiation)
      plot(dates,ts.comb$slrad,type="l",col="blue",xaxt="n",xlim=xlimdates)
      axis(side=1,at=ats,labels=FALSE,cex=0.5)
      lines(obs_daily_nomissing$Date,obs_daily_nomissing$Rgs,col="grey")
      lines(obs_daily_abr$Modeldate,obs_daily_abr$Rs,col="grey")
      lines(dates,ts.comb$rn,type="l",col="green")
      lines(obs_daily_abr$Modeldate,obs_daily_abr$Rnet,col="black")
      lines(obs_daily_nomissing$Date,obs_daily_nomissing$Rnet,col="black")
      legend("topright",c("SWm","SWobs","RNm","RNobs"),col=c("blue","grey","green","black"),lty=1,bty="n")
      mtext(side=2,"W/m2",line=2.5)
    }
    
    # RN (net radiation)
    plot(obs_daily_abr$Modeldate,obs_daily_abr$Rnet,col="black",type="l",xaxt="n",xlim=xlimdates,ylim=c(0,250))
    axis(side=1,at=ats,labels=FALSE,cex=0.5)
    lines(dates,ts.comb$rn,type="l",col="green")
    lines(obs_daily_nomissing$Date,obs_daily_nomissing$Rnet,col="black")
    #legend("topright",c("RNm","RNobs"),col=c("green","black"),lty=1,bty="n")
    mtext(side=2,"W/m2",line=2.5)
    
    # Add boxes for filled or missing data
    
    # ET
    plot(dates,ts.comb$ET,type="l",ylab="ET",xlab="",xaxt="n",col="lightblue",ylim=c(0,6),xlim=xlimdates)
    axis(side=1,at=ats,labels=FALSE,cex=0.5)
    #lines(dates,timeseries_trand,col="green")  # transpiration
    #lines(dates,timeseries_trand+timeseries_irvp,col="blue")  # transpiration + evap from interception
    lines(obs_daily_nomissing$Date,obs_daily_nomissing$ET.mm.day,col="black")  
    lines(obs_daily_abr$Modeldate,obs_daily_abr$ET.mmday,col="black")
    #lines(dates,timeseries_ptran,col="blue")   # potential transpiration
    #lines(dates,timeseries_ptran+timeseries_pint,col="purple")
    #legend("topright",c("ET","T","OBSET","POTTRANS"),col=c("grey","green","black","blue"),lty=1,bty="n")
    
    if (plotssib==1){
    lines(as.Date(x.ssib.daily$Date),x.ssib.daily$ET.mmday,col="lightblue")
    legend("topright",c("OBSET","ET.Brook90","ET.SSIB"),col=c("black","blue","lightblue"),lty=1,bty="n")
    } else {
      legend("topright",c("OBSET","ET.Brook90"),col=c("black","blue"),lty=1,bty="n")
    }
    #lines(dates,timeseries_irvp,col="blue")
    mtext(side=2,"mm/day",line=2.5)
    
    #  Soil moisture
    plot(dates,ts.comb$soilmoist.02m,type="l",col="purple",xlab="",xaxt="n",ylab="Soil moisture",ylim=c(0,800),xlim=xlimdates)
    axis(side=1,at=ats,labels=ats,cex=0.5)
    lines(obs_daily_nomissing$Date,obs_daily_nomissing$sm.0.2m,col="black")
    lines(sm.abr$Modeldate,sm.abr$sm.0.2m,col="black")
    legend("topright",c("SMm","SM.obs"),lty=1,col=c("purple","black"))
    mtext(side=2,"mm",line=2.5)
    # Discharge
    plotdischarge=0
    if (plotdischarge==1){
      par(new=TRUE)
      plot(dates,timeseries_gwat,type="l",col="grey",xaxt="n",yaxt="n",xlim=xlimdates)
    }

  }
}
}

RATIO = timeseries_slrad/(24*timeseries_daylen *timeseries_iohday)  # Ratio of SW to SW_exo
#plot(dates,RATIO,type="l")

# Join together observation time series, merge with model

# Pad observed data with NAs for model dates outside the observed range, so that model values will be included in the plots
{
modelyr = format(dates,"%Y")
modelmmdd = format(dates,"%m-%d")
obsyr = as.numeric(modelyr)-5
obsdates = as.Date(paste0(obsyr,"-",modelmmdd))

df.date.modeldate.abr = data.frame(Date=obsdates,Modeldate.pad=dates)
df.date.modeldate.abr = df.date.modeldate.abr[df.date.modeldate.abr$Date<as.Date("1995-01-01"),]
obs_daily_abr_pad = merge(df.date.modeldate.abr,obs_daily_abr,by="Date",all.x=TRUE)
obs_daily_abr_pad$Modeldate=obs_daily_abr_pad$Modeldate.pad
}

{

obs_daily_abr_pad$ET.mmday.LEclose = obs_daily_abr_pad$ET.mmday  # ABRACOS already used LE closure method
obs_to_join_abr = obs_daily_abr_pad[,c("Date","Modeldate","Rnet","Rnetdaytime","Rs","LE","h","ET.mmday","ET.mmday.LEclose","ET.mmday.daytime","Prec")]
obs_daily_nomissing$Modeldate = obs_daily$Date
obs_daily_w_daytime$Modeldate = obs_daily$Date
obs_daily_nomissing$ET.mm.day.LEclose = obs_daily_nomissing$LEclosed.residual*Wm2tommd
obs_daily_w_daytime$ET.mm.day.LEclose = obs_daily_w_daytime$LEclosed.residual*Wm2tommd
#obs_to_join_lba = obs_daily_nomissing[,c("Date","Modeldate","Rnet","Rgs","LEclosed.bowen","H","ET.mm.day","prec","ET.mm.day.LEclose")]
obs_to_join_lba = obs_daily_w_daytime[,c("Date","Modeldate","Rnet","Rnet.daytime","Rgs","LEclosed.bowen.limit","H","ET.mm.day","ET.mm.day.LEclose","ET.mm.day.daytime","prec")]


names(obs_to_join_lba) = names(obs_to_join_abr)

obs_join = rbind(obs_to_join_abr,obs_to_join_lba)
obs_join$Year = format(obs_join$Date,"%Y")
obs_join$Month = as.numeric(format(obs_join$Date,"%m"))
plotcolor.yr = data.frame(Year=c(1992,1993,1999,2000,2001,2002),col=c("black","blue","green","red","orange","purple"))
obs_join_pc = merge(obs_join,plotcolor.yr,by="Year",all.x=TRUE)

obs_join_pc$ET.mmday.daytime[is.na(obs_join_pc$ET.mmday)]=NA

dflink.ET = data.frame(Date=dates,evp=ts.comb$ET,PET=ts.comb$PET,PTRAN=ts.comb$PTRAN)
ETjoin = merge(obs_join_pc[,c("Date","Modeldate","Rnet","ET.mmday","ET.mmday.LEclose","ET.mmday.daytime","col")],data.frame(Date=dates,evp=ts.comb$ET,PET=ts.comb$PET,PTRAN=ts.comb$PTRAN,PTRANNIGHT=ts.comb$PTRANNIGHT),by.x="Modeldate",by.y="Date",all.x=TRUE)
RNjoin = merge(obs_join_pc[,c("Date","Modeldate","Rnet","Rnetdaytime","ET.mmday","ET.mmday.daytime","col")],data.frame(Date=dates,rn=ts.comb$rn,rnday = ts.comb$rnday, rnnight= ts.comb$rnnight, ea=force.data$ea),by.x="Modeldate",by.y="Date",all.x=TRUE)

ETjoin[ETjoin$Date %in% as.Date(c("1992-10-07","1993-01-01","1993-03-31")),c(3:length(ETjoin[1,]))] = NA
ETjoin$ET.mmday.daytime[is.na(ETjoin$ET.mmday)]=NA
ETjoin$evp.nomissobs = ETjoin$evp
ETjoin$evp.nomissobs[is.na(ETjoin$ET.mmday)]=NA  # Creates a field where model values are NA when observed values are NA. Used for calc monthly means for both model and obs.
}



#  Load 8day NDVI data
{indir.NDVI = "K:/Shared drives/Rondonia_CNH_public/data/spatial_data/landcover/Station FNS/NDVI/"
f.NDVI = "MOD09A1_NDVI_2000-01-01_2019-12-31.csv"
xNDVI = read.csv(paste0(indir.NDVI,f.NDVI))
xNDVI$Date = as.Date(xNDVI$Date,format="%m/%d/%Y")

#  NDVI values from May 1 to Oct 1 seem valid, but outside seem cloud contaminated...
xNDVI$month = as.numeric(format(xNDVI$Date,"%m"))
#xNDVI$NDVI[xNDVI$month %in% c(1,2,3,4,10,11,12)] = NA
xNDVI$NDVI[is.na(xNDVI$Flag)] = NA
}

# Exploratory plots
# Plot Cloudcor, EFFEM
exploreplot = 0
if (exploreplot==1){
  
plot(dates,timeseries_cldcorlw)
lines(dates,timeseries_effemlw)

# Plot RN error over time
plot(RNjoin$Date,RNjoin.error)
abline(0,0)

# RN error vs ea
plot(RNjoin$ea,RNjoin.error,col=RNjoin$col,xlab="vp, kPa",ylab="Rnet error, W/m2")
abline(0,0)
}

{
  ETjoin$ET.Rn.model = ETjoin$evp/(Wm2tommd*RNjoin$rn)
  ETjoin$ET.Rn.obs = ETjoin$ET.mmday/(Wm2tommd*RNjoin$Rnet)
  
  ETjoin$ET.PTRAN.model = ETjoin$evp/ETjoin$PTRAN
  ETjoin$ET.PTRAN.obs = ETjoin$ET.mmday/ETjoin$PTRAN
  
  ETjoin$ET.PET.model = ETjoin$evp/ETjoin$PET
  ETjoin$ET.PET.obs = ETjoin$ET.mmday/ETjoin$PET
  ETjoin$ET.PET.obs.LEclose = ETjoin$ET.mmday.LEclose/ETjoin$PET
  
  ETjoin$ETday.PET.obs = ETjoin$ET.mmday.daytime/ETjoin$PET
}


#  Write output files
setwd(outdir.output)
write.csv(ETjoin,"ETjoin.csv")
write.csv(RNjoin,"RNjoin.csv")
