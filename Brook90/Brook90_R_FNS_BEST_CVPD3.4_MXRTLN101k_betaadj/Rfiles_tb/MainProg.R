
{
  rm(list = setdiff(ls(), lsf.str()))
  NPINT <- 1         
  SUBDAYDATA <- FALSE
  RRD <- 0.55
  UWD <- 3.0
}

projectpath<-"K:/My Drive/Gdrive/mydocuments/amazon/writeups/2019_NSF_B90/2023_09_boston_USE_THIS_ONE/2019_NSF_SSiB_1D/Rfiles/Brook90/Brook90_R_FNS_BEST_sensitivity/Brook90_R_FNS_BEST_CVPD3.4_MXRTLN101k_betaadj/"
Rfilepath = paste0(projectpath,"Rfiles_tb/")
#output_html<-file.path(projectpath,"Documentation\\HTML_Files")

# Load main functions
{
source(paste0(Rfilepath,"GLOBDECL.R"))
source(paste0(Rfilepath,"AXX.R"))
source(paste0(Rfilepath,"KPT.R"))
source(paste0(Rfilepath,"EVP.R"))
source(paste0(Rfilepath,"PET.R"))
source(paste0(Rfilepath,"SUN.R"))
source(paste0(Rfilepath,"SNO.R"))
source(paste0(Rfilepath,"WAT.R"))
source(paste0(Rfilepath,"B90V4_sub.R"))
}
  
# Load met data
{
  metfile = "data_input_WB_1999_point.txt"
  metfname = "FNSmet.txt"
meteoFile <-read.table(file.path(projectpath,"Input_data\\",metfname))
MData<-matrix(meteoFile)

precfname = "P_WBG_0060_Minuten_1999_point.txt"
precfname = "FNSprecip.txt"
precFile <-read.table(file.path(projectpath,"Input_data\\",precfname))
MhhData<-matrix(precFile)
}

# Load Canopy parameters
{
canopy_file <- read.table(file.path(projectpath,"Input_data\\","canopy.txt"), header = FALSE, fill=TRUE , dec = "." , sep = ",", strip.white = TRUE)

#canopy parameters
canopy_varname <- as.character(canopy_file[,1])
canopy_varvalue <- as.character(canopy_file [,2])
quant_canopy <- 18  # number of canopy parameters

for (i in 1:quant_canopy){
  a <- unlist(canopy_varname[i])
  b <- as.numeric(unlist(canopy_varvalue[i]))
  assign(a,b)
  rm(a,b)
}

#ROOTDEN
roota <- strsplit(canopy_varname[20],"[ ]+")
rootb <- unlist(roota)
rootc <- as.numeric(rootb)
rootaa <- strsplit(canopy_varname[22],"[ ]+")
rootbb <- unlist(rootaa)
rootcc <- as.numeric(rootbb)
root_a <- c(rootc,rootcc)
root_a[is.na(root_a)] <- 0

rootd <- strsplit(canopy_varname[21],"[ ]+")
roote <- unlist(rootd)
rootf <- as.numeric(roote)
rootdd <- strsplit(canopy_varname[23],"[ ]+")
rootee <- unlist(rootdd)
rootff <- as.numeric(rootee)
root_b <- c(rootf,rootff)
root_b[is.na(root_b)] <- 0

rootden <- as.vector(rbind(root_a,root_b)) 
assign(canopy_varname[19],rootden)

rm(roota,rootb,rootc,rootd,roote,rootf,rootaa,rootbb,rootcc,root_a,rootdd,rootee,rootff,root_b,quant_canopy,canopy_varvalue,canopy_varname,rootden)
}

#  Fixed parameters
{
fixed_file <-read.table(file.path(projectpath,"Input_data\\","fixed.txt"), header = FALSE, fill=TRUE, sep = "," , dec = "." , row.names = 1)

fixed_varname <- rownames(fixed_file)
fixed_varvalue <- fixed_file [,c(1)]

quant_fixed = length(fixed_varname) - 1

for (i in 1:quant_fixed){
  a <- unlist(fixed_varname[i])
  b <- unlist(fixed_varvalue[i])
  assign(a,b)
  rm(a,b)
}

rm(fixed_varname,fixed_varvalue,quant_fixed)
}

#  Flow parameters
{
flow_file <-read.table(file.path(projectpath,"Input_data\\","flow_standard.txt"), header = FALSE, fill=TRUE, sep = "," , dec = "." , row.names = 1)

flow_varname <- rownames(flow_file)
flow_varvalue <- flow_file [,c(1)]

quant_flow = length(flow_varname) - 1

for (i in 1:quant_flow){
  a <- unlist(flow_varname[i])
  b <- unlist(flow_varvalue[i])
  assign(a,b)
  rm(a,b)
}

rm(flow_varname,flow_varvalue,quant_flow)
}

# Initial
{
initial_file <-read.table(file.path(projectpath,"Input_data\\","initial.txt"),header = FALSE,fill=TRUE,sep = ",",dec = ".",row.names = 1)

initial_varname <- rownames(initial_file)
initial_varvalue <- as.character(initial_file [,1])
quant_initial <- 4

for (i in 1:quant_initial){
  a <- unlist(initial_varname[i])
  b <- as.numeric(unlist(initial_varvalue[i]))
  assign(a,b)
  rm(a,b)
}

#PSIMIN
PSIMIN <- as.numeric(initial_varvalue[6:30])

rm(initial_varname,initial_varvalue,quant_initial)
}

# Location
{
location_file <-read.table(file.path(projectpath,"Input_data\\","location.txt"), header = FALSE, fill=TRUE, sep = "," , dec = ".", strip.white = TRUE)

location_varname <- as.character(location_file[,1])
location_varvalue <- as.character(location_file [,2])
quant_location <- 5

for (i in 1:quant_location){
  a <- unlist(location_varname[i])
  b <- as.numeric(unlist(location_varvalue[i]))
  assign(a,b)
  rm(a,b)
}

#DURATN
duratna <- strsplit(location_varvalue[6],"[ ]+")
duratnb <- unlist(duratna)
duratn<- as.numeric(duratnb)
assign(location_varname[6],duratn)

rm(duratna,duratnb,duratn)

#RELHT
hta <- strsplit(location_varvalue[7],"[ ]+")
htb<- unlist(hta)
ht_a <- suppressWarnings(as.numeric(htb))
ht_a[is.na(ht_a)] <- 0

htc <- strsplit(location_varname[8],"[ ]+")
htd <- unlist(htc)
ht_b <- suppressWarnings(as.numeric(htd))
ht_b[is.na(ht_b)] <- 0

relht <- as.vector(rbind(ht_a,ht_b)) 
assign(location_varname[7],relht)

rm(hta,htb,htc,htd,ht_a,ht_b,relht)

#RELLAI
laia <- strsplit(location_varvalue[9],"[ ]+")
laib<- unlist(laia)
lai_a <- suppressWarnings(as.numeric(laib))
lai_a[is.na(lai_a)] <- 0

laic <- strsplit(location_varname[10],"[ ]+")
laid <- unlist(laic)
lai_b <- suppressWarnings(as.numeric(laid))
lai_b[is.na(lai_b)] <- 0

rellai <- as.vector(rbind(lai_a,lai_b)) 
assign(location_varname[9],rellai)

rm(laia,laib,laic,laid,lai_a,lai_b,rellai)
rm(location_varname,location_varvalue,quant_location)
}

# Soil parameters
{
  soil_file <-read.table(file.path(projectpath,"Input_data\\","soil.txt"), header = FALSE, fill=TRUE , dec = "." , sep = "", strip.white = TRUE)
  
  soil_var1 <- as.character(soil_file[,1])
  soil_var2 <- as.character(soil_file[,2])
  soil_var3 <- as.character(soil_file[,3])
  soil_var4 <- as.character(soil_file[,4])
  soil_var5 <- as.character(soil_file[,5])
  soil_var6 <- as.character(soil_file[,6])
  soil_var7 <- as.character(soil_file[,7])
  soil_var8 <- as.character(soil_file[,8])
  
  # NLAYER
  a_nlayer <- unlist(soil_var1[1])
  a_nlayer <- as.character(gsub(",","",a_nlayer))
  b_nlayer <- as.numeric(unlist(soil_var2[1]))
  assign(a_nlayer,b_nlayer)
  rm(a_nlayer,b_nlayer)
  
  # THICK
  a_thick <- unlist(soil_var1[2])
  b_thick <- as.numeric(unlist(soil_var1[3:27]))
  assign(a_thick,b_thick)
  rm(a_thick,b_thick)
  
  # STONEF
  a_stonef <- unlist(soil_var2[2])
  b_stonef <- as.numeric(unlist(soil_var2[3:27]))
  assign(a_stonef,b_stonef)
  rm(a_stonef,b_stonef)
  
  # PSIF
  a_psif <- unlist(soil_var3[2])
  b_psif <- as.numeric(unlist(soil_var3[3:27]))
  assign(a_psif,b_psif)
  rm(a_psif,b_psif)
  
  # THETAF
  a_thetaf <- unlist(soil_var4[2])
  b_thetaf <- as.numeric(unlist(soil_var4[3:27]))
  assign(a_thetaf,b_thetaf)
  rm(a_thetaf,b_thetaf)
  
  # THSAT
  a_thsat <- unlist(soil_var5[2])
  b_thsat <- as.numeric(unlist(soil_var5[3:27]))
  assign(a_thsat,b_thsat)
  rm(a_thsat,b_thsat)
  
  # BEXP
  a_bexp <- unlist(soil_var6[2])
  b_bexp <- as.numeric(unlist(soil_var6[3:27]))
  assign(a_bexp,b_bexp)
  rm(a_bexp,b_bexp)
  
  # KF
  a_kf <- unlist(soil_var7[2])
  b_kf <- as.numeric(unlist(soil_var7[3:27]))
  assign(a_kf,b_kf)
  rm(a_kf,b_kf)
  
  # WETINF
  a_wetinf <- unlist(soil_var8[2])
  b_wetinf <- as.numeric(unlist(soil_var8[3:27]))
  assign(a_wetinf,b_wetinf)
  rm(a_wetinf,b_wetinf)
  
  rm(soil_var1,soil_var2,soil_var3,soil_var4,soil_var5,soil_var6,soil_var7,soil_var8)
}

