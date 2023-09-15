# Load and plot ERA5 soil moisture

layer = 4
cover = "Short and Tall Grass"
indir = paste0("K:/Shared drives/Rondonia_CNH/Data/Data processing and outputs/Water/soils/GEE_ERA5_SoilMoistAll_municipality/",cover,"/Layer",layer,"/")
flist = list.files(indir)

for (i in 1:length(flist)){
  xtmp = read.csv(paste0(indir,flist[i]))
  xtmp.ro = xtmp[(xtmp$CD_GEOCMU >= 1100000) & (xtmp$CD_GEOCMU<1200000),] 
  yymm.indices = grep("volumetric_soil_water",names(xtmp.ro))
  sm.data = xtmp.ro[yymm.indices]
  yymmdd = substr(names(xtmp.ro)[yymm.indices],2,9)
  dates = as.Date(yymmdd,format="%Y%m%d")
  df.out.tmp = data.frame(t(sm.data),row.names = NULL)
  names(df.out.tmp)=paste0("X",xtmp.ro$CD_GEOCMU)
  df.out.tmp$Date = dates
  if (i==1){
    df.out.all = df.out.tmp
  } else {
    df.out.all = rbind(df.out.all,df.out.tmp)
  }
  }

df.out.all = df.out.all[order(df.out.all$Date),]

fn.out = paste0("ERA5_muni_SoilMoist_19790102_20201231_",gsub(" ","",cover),"_Layer",layer,".csv")
write.csv(df.out.all,paste0(indir,fn.out))

#x1.ariq = x1[x1$]