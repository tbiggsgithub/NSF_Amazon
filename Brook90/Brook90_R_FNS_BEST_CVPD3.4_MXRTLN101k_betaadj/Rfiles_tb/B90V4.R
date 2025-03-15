#  Lines copied from B90V4.Rmd by tbiggs
library(zoo)
library(dplyr)

projectpath<-"K:/My Drive/Gdrive/mydocuments/amazon/writeups/2019_NSF_B90/2023_09_boston_USE_THIS_ONE/2019_NSF_SSiB_1D/Rfiles/Brook90/Brook90_R_FNS_BEST_sensitivity/Brook90_R_FNS_BEST_CVPD3.4_MXRTLN101k_betaadj/Rfiles_tb/"
source(paste0(projectpath,"MainProg.R"))

# Initial values
runflag=0
if((runflag == 0) || (runflag == 1)){
  DAYMO = rep(NA,times=12)
  DAYMO[1] = 31
  DAYMO[2] = 28
  DAYMO[3] = 31
  DAYMO[4] = 30 
  DAYMO[5] = 31
  DAYMO[6] = 30
  DAYMO[7] = 31
  DAYMO[8] = 31
  DAYMO[9] = 30
  DAYMO[10] = 31
  DAYMO[11] = 30
  DAYMO[12] = 31
  IDAY =1
  IInterValDay=1
  NDAYS=length(MData[[1]])
  NITSR = 0
  NITSY = 0
  NITSM = 0
  YEARN = as.numeric(MData[[1]][IDAY])
  
  daymax=NDAYS-IDAY+1
  maxF=0
  timeseries_prec=rep(0,daymax)
  timeseries_evp=rep(0,daymax)
  timeseries_flow=rep(0,daymax)
  timeseries_rnet=rep(0,daymax)
  timeseries_ptran=rep(0,daymax)
  timeseries_ptrand=rep(0,daymax)
  timeseries_irvp=rep(0,daymax)
  timeseries_isvp=rep(0,daymax)
  timeseries_snow=rep(0,daymax)
  timeseries_swat=rep(0,daymax)
  timeseries_pint=rep(0,daymax)
  timeseries_snvp=rep(0,daymax)
  timeseries_slvp=rep(0,daymax)
  timeseries_trand=rep(0,daymax)
  timeseries_mesfld=rep(0,daymax)
  timeseries_smltd=rep(0,daymax)
  timeseries_slfld=rep(0,daymax)
  timeseries_rfald=rep(0,daymax)
  timeseries_sfald=rep(0,daymax)
  timeseries_awat=rep(0,daymax)
  timeseries_adef=rep(0,daymax)
  timeseries_sintd=rep(0,daymax)
  timeseries_rintd=rep(0,daymax)
  timeseries_rthrd=rep(0,daymax)
  timeseries_sthrd=rep(0,daymax)
  timeseries_rsnod=rep(0,daymax)
  timeseries_aa=rep(0,daymax)
  timeseries_asubs=rep(0,daymax)
  timeseries_rn=rep(0,daymax)
  timeseries_rnday=rep(0,daymax)
  timeseries_rnnight=rep(0,daymax)
  timeseries_lngnet=rep(0,daymax)
  timeseries_slrad=rep(0,daymax)  # Solar radiation
  timeseries_daylen=rep(0,daymax)
  timeseries_gwat = rep(0,daymax)
  timeseries_safrac = rep(0,daymax)
  timeseries_iohday = rep(0,daymax)
  timeseries_cldcorlw = rep(0,daymax)
  timeseries_effemlw = rep(0,daymax)
  timeseries_srfl = rep(0,daymax)
  timeseries_dsfl =  rep(0,daymax)
  timeseries_seep =  rep(0,daymax)
  timeseries_gwfl =  rep(0,daymax)
  timeseries_byfl = rep(0,daymax)
  timeseries_byfld = rep(0,daymax)
  timeseries_ptranday = rep(0,daymax)
  timeseries_ptrannight = rep(0,daymax)
  timeseries_byfrac = data.frame(matrix(NA,nrow=daymax,ncol=NLAYER))
  timeseries_byfldi = data.frame(matrix(NA,nrow=daymax,ncol=NLAYER))
  timeseries_wetnes = data.frame(matrix(NA,nrow=daymax,ncol=NLAYER))
  timeseries_petpm_biggs = rep(0,daymax)
  ts_frint = rep(0,daymax)  # Integrated PAR limit to ET  Obtained by modifying SRSC in PET.R, and B90V4_sub.R.
  ts_fR = rep(0,daymax)  # fR, reduction in stomatal conductance due to radiation.
  ts_fd = rep(0,daymax)   #  VPD limit to ET
  ts_ft = rep(0,daymax)   #  Temperature limit to ET
  ts_vpd = data.frame(matrix(NA,nrow=daymax,ncol=3))  # VPD, ES, EA
  
  timeseries_swati = data.frame(matrix(NA,nrow=daymax,ncol=NLAYER))  # Soil moisture in all layers modelled
  names(timeseries_swati) = THICK[1:NLAYER]
  if( YEARN < 100){
    if(YEARN > 20){
      YEARN = YEARN + 1900
    }else{
      YEARN = YEARN + 2000
    }
  }
  MONTHN = as.numeric(MData[[2]][IDAY])
  DOM = as.numeric(MData[[3]][IDAY])
  DOY = DOYF(DOM,MONTHN,DAYMO)
  
  if (fnleap()) {
    DAYMO[2] = 29
  }else{
    DAYMO[2] = 28
  }
  if (SUBDAYDATA) {
    DTP = DT / NPINT
  }else{
    DTP = DT
  }
  # zero accumulators
  zyear()
  zmonth()
  # initial values
  SNOW = 0
  GWAT = GWATIN
  INTR = INTRIN  # Intercepted rain
  INTS = INTSIN  # Intercepted snow
  for( i in 1:NLAYER){
    PSIM[i] = PSIMIN[i]
  }
  # soil water parameters and initial variables
  soilp<-SOILPAR()
  PSIG<-unlist(soilp[2])
  SWATMX<-unlist(soilp[3])
  WETF<-unlist(soilp[4])
  WETC<-unlist(soilp[5])
  CHM<-unlist(soilp[6])
  CHN<-unlist(soilp[7]) 
  WETNES<-unlist(soilp[8])
  SWATI<-unlist(soilp[9])
  KSAT<-unlist(soilp[10])
  # ^^
  # initial soil water variables
  soil<-SOILVAR()
  PSITI<-soil[1:ML]
  THETA<-soil[(ML+1):(2*ML)]
  KK<-soil[(2*ML+1):(3*ML)]
  SWAT<-soil[(3*ML+1)]
  # ^^
  # initial total water in system
  STORD = INTR + INTS + SNOW + SWAT + GWAT
  STORM = STORD
  STORY = STORD
  # any initial snow has zero liquid water and cold content
  CC = 0
  SNOWLQ = 0
}


# Parameter initialization
# parameter conversions
{
  GLMAX = GLMAXC / 100  # Converts from cm/s to m/s (note: the html PET has the units as s/cm)
  # rc for grasslands (7 to 59 s m-1) -->  GLMAX of 0.14 to 0.017 m/s, or 14 to 1.7 cm/s
  GLMIN = GLMINC / 100
  LAT = LATD / 57.296
  ESLOPE = ESLOPED / 57.296
  DSLOPE = DSLOPED / 57.296
  ASPECT = ASPECTD / 57.296
  # equivalent slope for radiation calculations
  equi<-EQUIVSLP(LAT, ESLOPE, ASPECT)
  L1<-unlist(equi[1])
  L2<-unlist(equi[2])
  # ^^
  # infiltration parameters
  infpa<-INFPAR(INFEXP, IDEPTH, NLAYER, THICK)
  ILAYER<-unlist(infpa[1])
  INFRAC<-unlist(infpa[2])
  # ^^
  # source area parameters
  srfp<-SRFPAR(QDEPTH, NLAYER, THETAF, THICK, STONEF, SWATMX)
  QLAYER<-unlist(srfp[1]) 
  SWATQX<-unlist(srfp[2])
  SWATQF<-unlist(srfp[3])
  # ^^
  # root density parameters
  RELDEN<-RTDEN(ROOTDEN, NLAYER, THICK)
}

#  Running the model
while( IDAY <= NDAYS){  
  NITSD = 0
  subdatafileline(IDAY)
  if( IDAY == INIDAYS + 1){
    # end of initialization, reinitialize year and month accumulators
    STORD = INTR + INTS + SNOW + SWAT + GWAT
    STORM = STORD
    STORY = STORD
    NITSY = 0
    NITSM = 0
    zyear()
    zmonth()
  }
  # calculate derived variables
  MSBSETVARS()
  #
  #* * * * *  B E G I N   D A Y - N I G H T   E T   L O O P  * * * * * * * * *
  #potential and actual interception, evaporation, and transpiration
  MSBDAYNIGHT()  # in B90V4_sub
  #
  #* * * * * * * *  E N D   D A Y - N I G H T   L O O P  * * * * * * * * * *
  # average rates over day
  PTRAN = (PTR[1] * DAYLEN + PTR[2] * (1 - DAYLEN)) / DT # Penman Monteith potential Transpiration
  PTRANDAY = PTR[1]
  PTRANNIGHT = PTR[2]
  RNMEAN = (RNOUT[1] * DAYLEN + RNOUT[2] * (1 - DAYLEN)) / DT  # mean of day and night.  RNMEAN is daily total RN
  RNDAY = RNOUT[1]  # Added by tbiggs, to get RN during daytime only
  RNNIGHT = RNOUT[2]
  SWMEAN = (SLRADOUTVEC[1] * DAYLEN + SLRADOUTVEC[2] * (1 - DAYLEN)) / DT
  GEVP = (GER[1] * DAYLEN + GER[2] * (1 - DAYLEN)) / DT
  PINT = (PIR[1] * DAYLEN + PIR[2] * (1 - DAYLEN)) / DT
  GIVP = (GIR[1] * DAYLEN + GIR[2] * (1 - DAYLEN)) / DT
  for(i in 1:NLAYER){
    TRANI[i] = (ATRI[1, i] * DAYLEN + ATRI[2, i] * (1 - DAYLEN)) / DT
  }
  # zero daily integrators
  zday()
  #
  #* * * * * * * * B E G I N   P R E C I P   I N T E R V A L * * * * * * * * *
  for( N in 1:NPINT){  
    if (SUBDAYDATA){
      subprfileline(IInterValDay)
      if (MESFLP <= -0.01) {MESFLP = MESFL / DT}
    }else{
      # precip data from data file
      PREINT = PRECIN / DT
      MESFLP = MESFL / DT
    }
    # interception and snow accumulation/melt
    MSBPREINT()
    # initialize for iterations
    # initial time remaining in iteration time step = precip time step
    DTRI = DTP
    # initialize iteration counter
    NITS = 0
    # zero precip interval integrators
    zpint()
    #
    #  *  *  *  *  *  *  B E G I N   I T E R A T I O N   *  *  *  *  *  *  *  *
    while(!(DTRI <= 0)){  
      NITS = NITS + 1
      # check for events
      if (NITS %% 100 == 0) {}
      # water movement through soil
      MSBITERATE() 
      # iteration calculations
      # calculate SLFLI vertical macropore infiltration out of layer
      SLFLI[1] = SLFL - INFLI[1] - BYFLI[1]
      if (ILAYER >= 2){
        if (NLAYER >= ILAYER +1){
          for (i in 2:ILAYER){ 
            # does not execute if ILAYER% = 1 or 0
            SLFLI[i] = SLFLI[i - 1] - INFLI[i] - BYFLI[i]
          }
          for( i in (ILAYER + 1):NLAYER){ 
            # does not execute if NLAYER% < ILAYER% + 1
            SLFLI[i] = 0
          }
        }
      }
      # integrate below ground storages over iteration interval
      for( i in 1:NLAYER){
        SWATI[i] = SWATI[i] + NTFLI[i] * DTI
      }
      GWAT = GWAT + (VRFLI[NLAYER] - GWFL - SEEP) * DTI
      # new soil water variables and test for errors
      for (i in 1:NLAYER){
        swchek(i)
        WETNES[i] = SWATI[i] / SWATMX[i]
        PSIM[i] = FPSIMF(WETNES[i], PSIF[i], BEXP[i], WETINF[i], WETF[i], CHM[i], CHN[i])
      }
      soil<-SOILVAR()
      PSITI<-soil[1:ML]
      THETA<-soil[(ML+1):(2*ML)]
      KK<-soil[(2*ML+1):(3*ML)]
      SWAT<-soil[(3*ML+1)]
      # ^^
      # iteration output
      # flows accumulated over precip interval
      paccum()
      # time remaining in precipitation time-step
      DTRI = DTRI - DTI
      NITSR = NITSR + 1  # for visible display of iterations
    }
    #
    #  *  *  *  *   E N D   i T E R A T I O N    L O O P  *  *  *  *  *  *  *  *
    # display iterations
    # integrate interception storages over precip interval
    INTS = INTS + (SINT - ISVP) * DTP
    INTR = INTR + (RINT - IRVP) * DTP
    #  flows for precip interval summed from components
    psum()
    # precipitation interval output
    # flows accumulated over day
    daccum()
    # accumulate iterations
    NITSD = NITSD + NITS
    NITSM = NITSM + NITS
    NITSY = NITSY + NITS
    IInterValDay<-IInterValDay+1
  }
  #
  #* * * * *  E N D   P R E C I P   I N T E R V A L   L O O P  * * * * * * * *
  # flows for day summed from components
  dsum()
  # check for water balance error
  BALERD = STORD - (INTR + INTS + SNOW + SWAT + GWAT) + PRECD - EVAPD - FLOWD - SEEPD
  STORD = INTR + INTS + SNOW + SWAT + GWAT
  # flows accumulated over month
  maccum()
  # date checking on
  if(DOM == DAYMO[MONTHN]){
    # set up for next month
    zmonth()
    MONTHN = MONTHN + 1
    DOM = 0
    NITSM = 0
  }  # for end of month
  if (MONTHN == 13) {
    # end of year
    # set up for next year
    MONTHN = 1
    DOM = 1
    DOY = 1
    YEARN = YEARN + 1
    zyear()
    if (fnleap() ){
      DAYMO[2] = 29
    }else{
      DAYMO[2] = 28
    }
    NITSY = 0
    NITSM = 0
  } 
  #set up for next day
  IDAY = IDAY + 1
  MONTHN = as.numeric(MData[[2]][IDAY])
  DOM = as.numeric(MData[[3]][IDAY])
  YEARN = as.numeric(MData[[1]][IDAY])
  if(IDAY <= NDAYS)
    DOY=DOYF(DOM,MONTHN,DAYMO)
  
  #* * * I N P U T   W E A T H E R   L I N E   F R O M   D F I L E * * *
  #subdatafileline()
  #
  # ***************   E N D    D A Y   L O O P    **************************
  
  timeseries_prec[daymax-NDAYS+IDAY-1]<-PRECD   #  *** Precip
  timeseries_evp[daymax-NDAYS+IDAY-1]<-EVAPD   #  *** Evapotranspiration for the day
  timeseries_flow[daymax-NDAYS+IDAY-1]<-FLOWD  #  total flow (mm)
  timeseries_rnet[daymax-NDAYS+IDAY-1]<-RNET  #   rain reaching soil surface
  timeseries_irvp[daymax-NDAYS+IDAY-1]<-IRVPD   #  *** Evap intercepted rain
  timeseries_ptrand[daymax-NDAYS+IDAY-1]<-PTRAND  # *** Potential transpiration (mm). Not sure how PTRAND differs from PTRAN
  timeseries_ptran[daymax-NDAYS+IDAY-1]<-PTRAN  # *** Potential transpiration (mm)
  timeseries_ptranday[daymax-NDAYS+IDAY-1]<-PTRANDAY
  timeseries_ptrannight[daymax-NDAYS+IDAY-1]<-PTRANNIGHT
  timeseries_swat[daymax-NDAYS+IDAY-1]<-SWAT    # *** Total soil water in all layers
  timeseries_pint[daymax-NDAYS+IDAY-1]<-PINTD    # Potential interception
  timeseries_snvp[daymax-NDAYS+IDAY-1]<-SNVPD   # evap from snowpack
  timeseries_slvp[daymax-NDAYS+IDAY-1]<-SLVPD   # *** Soil evaporation mm
  timeseries_trand[daymax-NDAYS+IDAY-1]<-TRAND   # *** Transpiration mm
  timeseries_mesfld[daymax-NDAYS+IDAY-1]<-MESFLD  # measured streamflow
  timeseries_slfld[daymax-NDAYS+IDAY-1]<-SLFLD  # input to soil surface
  timeseries_rfald[daymax-NDAYS+IDAY-1]<-RFALD  # rainfall
  timeseries_awat[daymax-NDAYS+IDAY-1]<-AWAT   # avail water in root zone
  timeseries_adef[daymax-NDAYS+IDAY-1]<-ADEF  # avail water deficit in root zone
  timeseries_sintd[daymax-NDAYS+IDAY-1]<-SINTD  # snow interception
  timeseries_rintd[daymax-NDAYS+IDAY-1]<-RINTD  # rain interception
  timeseries_rthrd[daymax-NDAYS+IDAY-1]<-RTHRD  # rain throughfall
  timeseries_srfl[daymax-NDAYS+IDAY-1] <- SRFL  # Surface flow from source areas
  timeseries_dsfl[daymax-NDAYS+IDAY-1] <- DSFL  # Downslope flow
  timeseries_seep[daymax-NDAYS+IDAY-1] <- SEEP  # Seep below watershed boundary
  timeseries_gwfl[daymax-NDAYS+IDAY-1] <- GWFL  # Groundwater discharge to stream
  timeseries_byfl[daymax-NDAYS+IDAY-1] <- BYFL
  timeseries_byfld[daymax-NDAYS+IDAY-1] <- BYFLD  # Byflow for day (BYFL is always zero for some reason)
  timeseries_byfrac[daymax-NDAYS+IDAY-1,] <- BYFRAC[1:NLAYER]
  timeseries_byfldi[daymax-NDAYS+IDAY-1,] <- BYFLDI[1:NLAYER]  # BYFLOW for each layer
  timeseries_wetnes[daymax-NDAYS+IDAY-1,] <- WETNES[1:NLAYER]  # Soil wetness.
  timeseries_petpm_biggs[daymax-NDAYS+IDAY-1] <- PMBIGGS(AA, VPD, GLMAX, HEIGHT, LAI, DELTA, UA) # Penman Monteith PET
  
  #  Add net radiation and other variables
  timeseries_aa[daymax-NDAYS+IDAY-1]<-AA  # Available energy W/m2  Calculated in B90V4_sub.R, function in SUN.R
  timeseries_asubs[daymax-NDAYS+IDAY-1]<-ASUBS  # available energy at ground W/m2 Calculated in B90V4_sub.R
  timeseries_rn[daymax-NDAYS+IDAY-1]<-RNMEAN  # Net radiation, W/m2 in B90V4_sub.R
  timeseries_rnday[daymax-NDAYS+IDAY-1] <- RNDAY  # added by tbiggs
  timeseries_rnnight[daymax-NDAYS+IDAY-1] <- RNNIGHT  # added by tbiggs
  timeseries_slrad[daymax-NDAYS+IDAY-1]<-SWMEAN[1]  # Incoming solar radiation on slope, W/m2 in B90V4_sub.R, used to calculate RN
  timeseries_lngnet[daymax-NDAYS+IDAY-1]<-LNGNET # Net longwave radiation
  timeseries_daylen[daymax-NDAYS+IDAY-1]<-DAYLEN
  timeseries_gwat[daymax-NDAYS+IDAY-1]<-GWAT
  timeseries_safrac[daymax-NDAYS+IDAY-1]<-SAFRAC  # saturation fraction of the surface area.  See WAT.R L588
  # Always zero at FNS
  timeseries_swati[daymax-NDAYS+IDAY-1,]<-SWATI[1:NLAYER]  # Soil water in each layer
  timeseries_iohday[daymax-NDAYS+IDAY-1] = I0HDAY  # Max SW radiation for given day
  timeseries_cldcorlw[daymax-NDAYS+IDAY-1] = CLDCORLW  # Cloud correction for LW radiation
  timeseries_effemlw[daymax-NDAYS+IDAY-1] = EFFEMLW   #  atmospheric emissivity for LW radiation
  ts_frint[daymax-NDAYS+IDAY-1] = FRINT # 
  ts_fR[daymax-NDAYS+IDAY-1] = fR # Radiation reduction of GLMAX (fR)
  ts_fd[daymax-NDAYS+IDAY-1] = FD  # VPD fraction (fD)
  ts_ft[daymax-NDAYS+IDAY-1] = FT  # Temperature fraction (fT)
  ts_vpd[daymax-NDAYS+IDAY-1,] = c(VPD,ES,EA)  # Check VPD calculations
}

dates = as.Date(paste(MData[[1]],MData[[2]],MData[[3]],sep="-"))
truedate = as.Date(MData[[13]])
flag = MData[[11]]

timeseries_PET = timeseries_pint + timeseries_ptran + timeseries_slvp

{CUMTHICK = cumsum(THICK)
  INDEX.2m = which(CUMTHICK==2000)
  timeseries_swat_0_2m = rowSums(timeseries_swati[,1:INDEX.2m])
}
timeseries_df = data.frame(MODELDATE=dates,TRUDATE=truedate,flag=flag, SLRAD = timeseries_slrad, PREC=timeseries_prec, ET=timeseries_evp, TRANS=timeseries_trand,soilevap=timeseries_slvp, evap.intercept=timeseries_irvp, PET=timeseries_PET, PTRAN = timeseries_ptran, PTRANDAY = timeseries_ptranday, PTRAND = timeseries_ptrand, PTRANNIGHT = timeseries_ptrannight, rn=timeseries_rn, rnday = timeseries_rnday, rnnight = timeseries_rnnight, slrad = timeseries_slrad, SWAT=timeseries_swat, AWAT=timeseries_awat, soilmoist.02m=timeseries_swat_0_2m, FLOW=timeseries_flow, DAYLEN = 24*timeseries_daylen)
ts_limits = data.frame(MODELDATE=dates,TRUDATE=truedate,FRINT=ts_frint,fR=ts_fR, FD=ts_fd,FT=ts_ft)