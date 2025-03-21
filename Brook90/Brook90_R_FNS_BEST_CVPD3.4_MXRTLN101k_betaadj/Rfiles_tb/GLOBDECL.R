#' ---
#' title: "GLOBLDECL"
#' author: "Rico Kronenberg, Lisa Marie Oehlschl?gel"
#' date: "3 Mai 2018"
#' output: html_document
#' ---
#' 
#' ### GLOBDECL - Global declaration of catchment area
#' In this script the catchment the catchment is characterised.
#' 
#' ### Content
#' 
#' * [Files](#files)
#' * [Auxiliary to run the programm](#auxiliary-to-run-the-programm)
#' * [Constants](#constants)
#' * [Parameters and variables](#parameters-and-variables)
#' 
#' ### Files
#' The Parameters are from the files:
#' 
#' * CTemperateDeciduousForest.txt
#' * SCscl.txt
#' * LMcu.txt
#' * IDefault.txt
#' * Xdefault.txt
#' * FUni50.txt
#' 
#' The symbols in the next table are characterizing the constants, parameters and variables more specific. They are written in front of the explenation of them in their topics. 
#' 
#' |Symbol|Description
#' |------|----------------
#' |*     |indicates an input from the parameter file
#' |**    |indicates a variable in the data file or precip input file
#' |***   |indicates a constant
#' |****  |indicates a variable initialized or parameter set in the data file
#' |&     |indicates a variable initialized in the parameter file
#' |@     |indicates a calculated parameter (constant for run)
#' |(none)|all others are variables
#' |(none)|at end of variable name indicates integer variable
#' |&     |at end of variable name indicates long integer variable
#' |$     |at end of variable name indicates string variable
#' |xxxxP |flow for precipitation interval (mm)
#' |xxxxD |flow for day (mm)
#' |xxxxM |flow for month (mm)
#' |xxxxY |flow for year (mm)
#' 
#' ### Auxiliary to run the programm
#' The following table includes some numbers and limits for numbers in the model:
#' 
#' Name          |Value|Description
#' --------------|-----|---------------------------------------------------------------
#' ML            |25   |number of values in layer parameter arrays, set to 25 for B90V4
#' gctMaxoutvars |60   |maximum number of output variables, an array dimension
#' gctGenoutv    |19   |number of variables in GENOUT form
#' gctIntoutv    |22   |number of variables in INTRNOUT form
#' gctLayoutv    |12   |number of variables in LAYOUT form
#' gvals         |8    |max number of graph output values
#' maxgraphs     |100  |-
#' 
#' Auxiliary explenations:
#' 
#' * A
#'     + parsed values from INSTRNG
#' * aparsed
#'     + parsed values from gsbparsestring, first subscript is 0
#' * bad
#'     + error indicator passed back to calling routine
#' * B90path$
#'     + path for running program, used for .INI and .TMP
#' * cancelsave As Boolean
#'     + true if save or print should be cancelled
#' * chkout(1 To gctMaxoutvars, 1 To 5) As Integer
#'     + 1 for output, 5 intervals; indicates which variables/interval selected; stored in .INI; commadec As Boolean
#' * dfiletype
#'     + data file type, 1-csv,2-decimal period, 3-decimal comma
#' * EV(1 To 25) As New frmevalout
#'     + EVAL windows
#' * FG(1 To maxgraphs) As New frmgraph
#'     + multiple instances of graph
#' * gnumber
#'     + graph number
#' * graphon
#'     + true if graph has been initialized
#' * inoutdir$
#'     + latest input-output directory
#' * intrvl
#'     + output interval index, 1 ANN, 2 MON, 3 DAY, 4 PRE, 5 ITR
#' * ivar(1 To gctMaxoutvars), pvar(1 To gctMaxoutvars), dvar(1 To gctMaxoutvars), mvar(1 To gctMaxoutvars), yvar(1 To gctMaxoutvars)
#'     + list of var numbers for output, does not count mulltiple soil layers
#' * msg As String
#' * noenderror As Integer
#'     + 0 if no END in parameter file, -1 if END is found
#' * noruncontinue
#'     + 1 to prevent Run-Continue after soil parameter changes
#' * NLN As String
#'     + new line
#' * outfilestatus(1 To 5) As Integer
#'     + 0 if no output, 1 if output, subscript 1 ann, 2 mon, 3 day, 4 pre, 5 itr
#'     + indicates presence of .TMP output files, controls mnuview???.Enabled
#' * outselect(1 To 5)  As Integer
#'     + True(-1) if output selected for that time interval, False(0) if not
#'     + controls checking in Select Output menu
#' * strng$
#' * Title$(1 To gctMaxoutvars)
#'     + column titles for gctMaxoutvars variables
#' * txt$(1 To 30)
#'     + text strings for help
#' * userfont As String
#' * varno
#'     + output variable number, from 1 to gctMaxoutvars, always locally used
#' * yvars, mvars, dvars, pvars, ivars
#'     + number of vars/cols in output
#' 
#' Auxiliary numbers, constants, parameters and variables with an allocated value:
#' 
#' * dum         - dummy variable
#' * errnum      - error number from ERR
#' * evalnumber  - number of EVAL screens
#' * INIDAYS     - number of days to initialize run
#' * lstart      - starting output layer
#' * lend        - ending output layer
#' * lstep       - step for output layers
#' * parserr     - true if parse error in Soil parameters
#' * prfiletype  - precip file type, 1-csv,2-decimal period, 3-decimal comma
#' * rerunnumber - 0 for new run, 1 to 9 for each successive add run
#' * rstop       - if run should stop at end of day, 2 if EOF and date check on, 3 if crash error in run or immediate stop request, 4 if max number of graphs
#' * RUNDAYS     - number of output days to run after initialize
#' * runflag     - 0 for new run, 1 for add run, 2 for continue run
#' 
## -------------------------------------------------------------------------------------------------------------------------
ML <- 25    
gctMaxoutvars <- 60 
gctGenoutv <- 19 
gctIntoutv <- 22 
gctLayoutv <- 12 
gvals <- 8 
maxgraphs <- 100
#
#A <- numeric(ML) # parsed values from INSTRNG
#aparsed  # parsed values from gsbparsestring, first subscript is 0
#bad      # error indicator passed back to calling routine
#B90path$  # path for running program, used for .INI and .TMP
#cancelsave As Boolean #true if save or print should be cancelled
#chkout(1 To gctMaxoutvars, 1 To 5) As Integer #1 for output, 5 intervals
#        indicates which variables/interval selected
#        stored in .INI
#commadec As Boolean
#dfiletype    #data file type, 1-csv,2-decimal period, 3-decimal comma
#
dum   <-0  
errnum <-0   
#
#EV(1 To 25) As New frmevalout # EVAL windows
#
evalnumber <-0 
#
#FG(1 To maxgraphs) As New frmgraph  # multiple instances of graph
#gnumber   # graph number
#graphon    # true if graph has been initialized
#
INIDAYS<-0  
#
#inoutdir$  # latest input-output directory
#intrvl     #output interval index, 1 ANN, 2 MON, 3 DAY, 4 PRE, 5 ITR
#ivar(1 To gctMaxoutvars), pvar(1 To gctMaxoutvars), dvar(1 To gctMaxoutvars), mvar(1 To gctMaxoutvars), yvar(1 To gctMaxoutvars) # list of var numbers for output, does not count mulltiple soil layers
#
lstart <-0
lend <-0 
lstep <-0 
#
#msg As String#
#noenderror As Integer  # 0 if no END in parameter file, -1 if END is found
#noruncontinue #1 to prevent Run-Continue after soil parameter changes
#NLN As String  # new line
#outfilestatus(1 To 5) As Integer  #0 if no output, 1 if output, subscript 1 ann, 2 mon, 3 day, 4 pre, 5 itr
##        indicates presence of .TMP output files, controls mnuview???.Enabled
#outselect(1 To 5)  As Integer # True(-1) if output selected for that time interval, False(0) if not
#        controls checking in Select Output menu
#
parserr <-0  
prfiletype  <-0  
rerunnumber<-0 
rstop <-0 
RUNDAYS<-0  
runflag <-0
#
#strng$
#Title$(1 To gctMaxoutvars)   # column titles for gctMaxoutvars variables
#txt$(1 To 30) # text strings for help
#userfont As String
#varno # output variable number, from 1 to gctMaxoutvars, always locally used
#yvars, mvars, dvars, pvars, ivars  # number of vars/cols in output

#' 
#' ### Constants
#' This table shows all constants which are used:
#' 
#' Constant|Value    |Description
#' --------|---------|-----------------------------------------------------------------------------------
#' DT      |1        |time step for DFILE interval,  must be 1 d
#' WTOMJ   |0.0864   |constant unit conversion, (MJ m-2 d-1)/(watt/m2) = 86400 s/d * .000001 MJ/J
#' ETOM    |0.4085   |(mm water)/(MJ/m2) using Lv 2448 MJ/Mg, density of water 1 Mg/m3 = 1E3 mm/m / (2448 MJ/Mg * 1 Mg/m3)
#' CPRHO   |1240     |volumetric heat capacity of air (J m-3 K-1)
#' GAMMA   |0.067    |psychrometer constant (kPa/K)
#' CVLQ    |0.00418  |volumetric heat capacity of water (MJ m-2 mm-1 K-1)
#' CVICE   |0.00192  |volumetric heat capacity of ice (MJ m-2 mm-1 K-1)
#' LF      |0.335    |heat of fusion of water (MJ m-2 mm-1)
#' LS      |2.824    |latent heat of sublimation of snow (MJ m-2 mm-1)
#' RHOWG   |0.00981  |density of water times gravity acceleration (MPa/m or kPa/mm)
#' SIGMA   |5.67E-8  |Stefan-Boltzmann constant (W m-2 K-4)
#' SC      |1367     |solar constant, value from [Lean (1991)](./Literature.html),(W/m2)
#' K       |0.4      |vonKarman constant
#' PI      |3.1416   |Pi
#' 
## -------------------------------------------------------------------------------------------------------------------------
DT <- 1
WTOMJ <- 0.0864
ETOM <- 0.4085
CPRHO <- 1240
GAMMA <- 0.067
CVLQ <- 0.00418
CVICE <- 0.00192
LF <- 0.335
LS <- 2.824
RHOWG <- 0.00981
SIGMA <- 0.0000000567
SC <- 1367
K <- 0.4
PI <- 3.1416

#' 
#' ### Parameters and variables
#' This list shows all variables that are used in alphabetic order:
#' 
#' * AA      - average available energy over daytime or nighttime (W/m2)
#' * ADEF    - available water deficit in root zone (mm), output only
#' * ALB     - * albedo with no snow
#' * ALBEDO  - albedo
#' * ALBSN   - * albedo with snow on the ground
#' * ALPHA   - modified Cowan alpha (MPa)
#' * ASPECT  - aspect, radians through east from north
#' * ASPECTD - * aspect, degrees through east from north
#' * ASUBS   - average avail. energy at ground over day or night (W/m2)
#' * ATR     - actual transpiration rate for daytime or night (mm/d)
#' * ATRANI  - actual transp.rate from layer for daytime or night (mm/d)
#' * ATRI    - actual transp.rate from layer for daytime and night (mm/d)
#' * AWAT    - available soil water in root zone (mm), output only
#' 
#' * BALERD  - error in water balance (mm)
#' * BALERM  - error in water balance (mm)
#' * BALERY  - error in water balance (mm)
#' * BEXP    - * exponent for psi-theta relation
#' * BYFL    - bypass flow rate from all layers for iteration (mm/d)
#' * BYFLI   - bypass flow rate from layer (mm/d)
#' * BYFLPI  - bypass flow from layer (mm)
#' * BYFLDI  - bypass flow from layer (mm)
#' * BYFLMI  - bypass flow from layer (mm)
#' * BYFLYI  - bypass flow from layer (mm)
#' * BYFLP   - total bypass flow (mm)
#' * BYFLD   - total bypass flow (mm)
#' * BYFLM   - total bypass flow (mm)
#' * BYFLY   - total bypass flow (mm)
#' * BYFRAC  - fraction of layer infiltration to bypass flow
#' * BYPAR   - * 1 to allow BYFL, or 0 to prevent BYFL
#' 
#' * C       - dummy input string
#' * C1      - * intercept of relation of solar rad. to sunshine duration
#' * C2      - * slope of relation of solar radiation to sunshine duration
#' * C3      - * longwave correction factor for overcast sky
#' * CC      - cold content of snowpack (positive) (MJ m-2)
#' * CCFAC   - * cold content factor (MJ m-2 d-1 K-1)
#' * CHM     - @ Clapp-Hornberger m parameter (kPa)
#' * CHN     - @ Clapp-Hornberger n parameter
#' * CINTRL  - * maximum interception storage of rain per unit LAI (mm)
#' * CINTRS  - * maximum interception storage of rain per unit SAI (mm)
#' * CINTSL  - * maximum interception storage of snow per unit LAI (mm)
#' * CINTSS  - * maximum interception storage of snow per unit SAI (mm)
#' * CR      - * light extinction coefficient for projected LAI + SAI
#' * CS      - * ratio of projected SAI to canopy height (m-1)
#' * CVPD    - * vapor pressure deficit at which conductance is halved (kPa)
#' * CZR     - * ratio of roughness to HEIGHT for rough closed canopies
#' * CZS     - * ratio of roughness to HEIGHT for smooth closed canopies
#' 
#' * DAYLEN  - daylength in fraction of day (d-1)
#' * DAYMO   - * days in month
#' * DD      - ** day of the month from data files
#' * DELTA   - dES/dT at some temperature T (kPa/K)
#' * DENSEF  - * density or thinning multiplier for MAXLAI,CS,RTLEN,RPLANT, not <.001
#' * DISP    - zero-plane displacement (m)
#' * DISPC   - zero-plane displacement for closed canopy of HEIGHT (m)
#' * DOM     - day of month
#' * DOY     - **** first day of the year from DFILE header
#' * DPSIDW  - rate of change of total potential with watercontent (kPa/mm)
#' * DPSIMX  - * maximum potential difference considered equal (kPa)
#' * DRAIN   - * multiplier of VFLUX(n) for drainage to groundwater
#' * DSFL    - downslope flow rate from all layers for iteration (mm/d)
#' * DSFLI   - downslope flow rate from layer (mm/d)
#' * DSFLP   - downslope flow (mm)
#' * DSFLD   - downslope flow (mm)
#' * DSFLM   - downslope flow (mm)
#' * DSFLY   - downslope flow (mm)
#' * DSFLPI  - downslope drainage from layer (mm)
#' * DSFLDI  - downslope drainage from layer (mm)
#' * DSFLMI  - downslope drainage from layer (mm)
#' * DSFLYI  - downslope drainage from layer (mm)
#' * DSLOPE  - slope for DSFL (radians)
#' * DSLOPED - * slope for DSFL (degrees)
#' * DSWMAX  - * maximum change allowed in SWATI (percent of SWATMX(i))
#' * DTI     - time step for iteration interval (d)
#' * DTIMAX  - * maximum iteration time step (d)
#' * DTINEW  - second estimate of DTI
#' * DTP     - @ time step for precipitation interval, may be <= 1 d
#' * DTRI    - time remaining in precipitation interval (d)
#' * DUMM    - dummy array for subroutine calls
#' * dummy   - dummy variable for subroutine calls
#' * DURATN  - * average duration of daily precip by month (hr)
#' 
#' * EA      - ** vapor pressure for the day (kPa)
#' * ES      - saturated vapor pressure (kPa)
#' * ESLOPE  - slope for evapotranspiration and snowmelt (radians)
#' * ESLOPED - * slope for evapotranspiration and snowmelt (degrees)
#' * EVAPP   - evapotranspiration (mm)
#' * EVAPD   - evapotranspiration (mm) 
#' * EVAPM   - evapotranspiration (mm) 
#' * EVAPY   - evapotranspiration (mm)
#' 
#' * FARR    - array of simulated daily flow for statistics
#' * FETCH   - * weather station fetch (m)
#' * FLOWP   - total flow (mm)
#' * FLOWD   - total flow (mm) 
#' * FLOWM   - total flow (mm)
#' * FLOWY   - total flow (mm)
#' * FRINTL  - * intercepted fraction of rain per unit LAI
#' * FRINTS  - * intercepted fraction of rain per unit SAI
#' * FSINTL  - * intercepted fraction of snow per unit LAI
#' * FSINTS  - * intercepted fraction of snow per unit SAI
#' * FXYLEM  - * fraction of plant resistance in xylem
#' 
#' * GER     - ground evaporation rate for daytime or night (mm/d)
#' * GEVP    - average ground evaporation for day (mm/d)
#' * GIR     - ground evap. rate with intercep. for daytime or night (mm/d)
#' * GIVP    - average ground evaporation for day with interception (mm/d)
#' * GLMAX   - maximum leaf conductance (m/s)
#' * GLMAXC  - * maximum leaf conductance (cm/s)
#' * GLMIN   - minimum leaf conductance (m/s)
#' * GLMINC  - * minimum leaf conductance (cm/s)
#' * GRAPH   - * runtime graphics output, 0-none, 1-continuous, 2-pause
#' * GRDMLT  - * rate of groundmelt of snowpack (mm/d)
#' * GSC     - * discharge from GWAT, fraction per day (d-1)
#' * GSP     - * fraction of discharge to seepage
#' * GWAT    - groundwater storage below soil layers (mm)
#' * GWATIN  - **** initial groundwater storage below soil layers (mm)
#' * GWFL    - streamflow rate from groundwater (mm/d)
#' * GWFLP   - groundwater flow (mm)
#' * GWFLD   - groundwater flow (mm)
#' * GWFLM   - groundwater flow (mm)
#' * GWFLY   - groundwater flow (mm)
#' 
#' * mnuhalfiter
#' 
#' * HEIGHT  - canopy height (m)
#' * HR      - * height above which CZR applies (m)
#' * HS      - * height below which CZS applies (m)
#' 
#' * I       - index variable for layer number
#' * I0HDAY  - potential insolation on horizontal (MJ m-2 d-1)
#' * IDAY    - day number in run
#' * II      - ** input precipitation interval number
#' * ILAYER  - number of layers over which infiltration is distributed
#' * IDEPTH  - * depth over which infiltration is distributed
#' * IMPERV  - * impervious fraction of area for SRFL
#' * INFEXP  - * infiltration exponent, 0-all to top to 1-uniform with depth
#' * INFRAC  - @ fraction of infiltration to each layer
#' * INFLI   - infiltration rate into layer (mm/d)
#' * INFLP   - infiltration into soil matrix (mm)
#' * INFLD   - infiltration into soil matrix (mm)
#' * INFLM   - infiltration into soil matrix (mm)
#' * INFLY   - infiltration into soil matrix (mm)
#' * INFLPI  - infiltration to layer (mm)
#' * INFLDI  - infiltration to layer (mm)
#' * INFLMI  - infiltration to layer (mm)
#' * INFLYI  - infiltration to layer (mm)
#' * INTR    - intercepted rain (mm)
#' * INTRIN  - & initial intercepted rain (mm)
#' * INTS    - intercepted snow (mm)
#' * INTSIN  - & initial intercepted snow (mm)
#' * IRVP    - evaporation rate of intercepted rain (mm/d)
#' * IRVPD   - evaporation of intercepted rain (mm)
#' * IRVPM   - evaporation of intercepted rain (mm)
#' * IRVPY   - evaporation of intercepted rain (mm)
#' * ISVP    - evaporation rate of intercepted snow (mm/d)
#' * ISVPD   - evaporation of intercepted snow (mm)
#' * ISVPM   - evaporation of intercepted snow (mm)
#' * ISVPY   - evaporation of intercepted snow (mm)
#' 
#' * J       - index variable for day-night separation
#' 
#' * KF      - * hydraulic conductivity at field capacity (mm/d)
#' * KK      - hydraulic conductivity (mm/d)
#' * KSAT    - @ saturated hydraulic conductivity (mm/d)
#' * KSNVP   - * multiplier to fix snow evaporation problem
#' 
#' * L1      - @ latitude of equivalent slope (radians)
#' * L2      - @ time shift of equivalent slope (radians)
#' * LAI     - leaf area index (m2/m2)
#' * LAIMLT  - * parameter for snowmelt dependence on LAI (Globalensionless)
#' * LAT     - latitude (radians)
#' * LATD    - **** latitude (degrees)
#' * LENGTH  - * slope length for DSFL (m)
#' * LPC     - * minimum LAI defining a closed canopy
#' * LWIDTH  - * leaf width (m)
#' 
#' * MARR    - array of measured daily flow for statistics
#' * MAXHT   - * maximum height for the year (m)
#' * MAXLAI  - * maximum projected leaf area index for the year (m2/m2)
#' * MAXLQF  - * maximum liquid water fraction of SNOW (Globalensionless)
#' * MELFAC  - * degree day melt factor for open (MJ m-2 d-1 K-1)
#' * MESFL   - ** measured streamflow for day (mm)
#' * MESFLD  - measured streamflow (mm)
#' * MESFLM  - measured streamflow (mm)
#' * MESFLY  - measured streamflow (mm)
#' * MESFLP  - ** measured streamflow rate for precip interval (mm/d)
#' * MM      - ** month from data files
#' * MONTHN  - month number
#' * MXKPL   - * maximum plant conductivity ((mm/d)/MPa)
#' * MXRTLN  - * maximum root length per unit land area (m/m2)
#' 
#' * N       - index variable for precipitation interval
#' * NN      - * wind/diffusivity extinction coefficient
#' * NDAYS   - * number of days in run
#' * NITS    - number of iterations in precipitation interval
#' * NITSD   - total number of iterations for day
#' * NITSM   - total number of iterations for month
#' * NITSY   - total number of iterations for year
#' * NITSR   - total number of iterations for run
#' * NLAYER  - * number of soil layers to be used in model, <= ML
#' * NOOUTF  - * 1 if no outflow allowed from roots, otherwise 0
#' * NTFLI   - net flow rate into layer (mm/d)
#' * NTFLPI  - net flow into layer (mm)
#' * NTFLDI  - net flow into layer (mm)
#' * NTFLMI  - net flow into layer (mm)
#' * NTFLYI  - net flow into layer (mm)
#' 
#' * PINT    - average potential interception for day (mm/d)
#' * PINTD   - potential interception (mm)
#' * PINTM   - potential interception (mm) 
#' * PINTY   - potential interception (mm)
#' * PIR     - potential interception rate for daytime or night (mm/d)
#' * PREC    - precipitation rate (mm/d)
#' * PRECD   - precipitation (mm)
#' * PRECM   - precipitation (mm)
#' * PRECY   - precipitation (mm)
#' * PREINT  - ** precipitation for precipitation interval (mm)
#' * PRECIN  - ** daily precipitation (mm)
#' * PSICR   - * minimum plant leaf water potential (MPa)
#' * PSIF    - * matric potential at field capacity (kPa)
#' * PSIG    - @ gravity potential (kPa)
#' * PSIM    - & matric soil water potential for layer (kPa)
#' * PSIMIN  - initial PSIM()
#' * PSITI   - total potential (kPa)
#' * PSNVP   - potential snow evaporation (mm/d)
#' * PTR     - potential transpiration rate for daytime or night (mm/d)
#' * PTRAN   - average potential transpiration rate for day (mm/d)
#' * PTRAND  - potential transpiration (mm)
#' * PTRANM  - potential transpiration (mm) 
#' * PTRANY  - potential transpiration (mm)
#' 
#' * QFFC    - * quick flow fraction (SRFL or BYFL) at field capacity
#' * QFPAR   - * quick flow parameter (SRFL or BYFL)
#' * QLAYER  - number of soil layers for SRFL, 0 to prevent SRFL
#' * QDEPTH  - * soil depth for SRFL calculation, 0 to prevent SRFL
#' 
#' * R5      - * solar radiation at which conductance is halved (W/m2)
#' * RAA     - Shuttleworth-Wallace atmosphere aerodynamic resistance (s/m)
#' * RAC     - Shuttleworth-Wallace canopy aerodynamic resistance (s/m)
#' * RAS     - Shuttleworth-Wallace ground aerodynamic resistance (s/m)
#' * RELHT   - * ten pairs of DOY and relative canopy height
#' * RELLAI  - * ten pairs of DOY and relative LAI
#' * ROOTDEN - * 25 pairs of root layer thickness (mm) and relative root density per unit volume
#' * RELDEN  - relative root density per unit volume for soil layers
#' * RFAL    - rainfall rate (mm/d)
#' * RFALD   - rainfall (mm)
#' * RFALM   - rainfall (mm)
#' * RFALY   - rainfall (mm)
#' * RHOTP   - * ratio of total leaf area to projected area
#' * RINT    - rainfall catch rate (mm/d)
#' * RINTD   - rain interception (mm)
#' * RINTM   - rain interception (mm)
#' * RINTY   - rain interception (mm)
#' * RM      - * maximum solar radiation, at which FR = 1 (W/m2)
#' * RNET    - rain reaching soil surface (mm/d)
#' * RNETD   - rainfall to soil surface (mm)
#' * RNETM   - rainfall to soil surface (mm)
#' * RNETY   - rainfall to soil surface (mm)
#' * RPLANT  - plant resistivity to water flow (MPa d/mm)
#' * RROOTI  - root resistance for layer (MPa d/mm)
#' * RTHR    - rain throughfall rate (mm/d)
#' * RTHRD   - rain throughfall (mm)
#' * RTHRM   - rain throughfall (mm)
#' * RTHRY   - rain throughfall (mm)
#' * RTLEN   - root length per unit land area (m/m2)
#' * RTRAD   - * average root radius (mm)
#' * RRD     - Default Radiation Ratio
#' * RSC     - Shuttleworth-Wallace canopy surface resistance (s/m)
#' * RSNO    - rain added to snowpack (mm/d)
#' * RSNOD   - rain on snow (mm)
#' * RSNOM   - rain on snow (mm)
#' * RSNOY   - rain on snow (mm)
#' * RSS     - Shuttleworth-Wallace soil surface resistance (s/m)
#' * RSSA    - * soil evaporation resistance at field capacity (s/m) 
#' * RSSB    - * exponent in relation of soil evap res to water potential
#' * RSTEMP  - * base temperature for snow-rain transition (degC)
#' * RXYLEM  - xylem resistance (MPa d/mm)
#' 
#' * SAFRAC  - source area fraction
#' * SAI     - stem area index (m2/m2)
#' * SAIMLT  - * parameter for snowmelt dependence on SAI (Globalensionless)
#' * SEEP    - deep seepage loss from groundwater (mm/d)
#' * SEEPP   - seepage loss (mm)
#' * SEEPD   - seepage loss (mm)
#' * SEEPM   - seepage loss (mm)
#' * SEEPY   - seepage loss (mm)
#' * SFAL    - snowfall rate (mm/d)
#' * SFALD   - snowfall (mm)
#' * SFALM   - snowfall (mm)
#' * SFALY   - snowfall (mm)
#' * SHEAT   - @ average soil heat flux for the day (W/m2, fixed at 0)
#' * SINT    - snowfall catch rate (mm/d)
#' * SINTD   - snow interception (mm) 
#' * SINTM   - snow interception (mm)
#' * SINTY   - snow interception (mm)
#' * SLFDAY  - ratio of potential insolation on slope to horizontal (map area)
#' * SLFL    - input rate to soil surface (mm/d)
#' * SLFLP   - input to soil surface (mm)
#' * SLFLD   - input to soil surface (mm)
#' * SLFLM   - input to soil surface (mm)
#' * SLFLY   - input to soil surface (mm)
#' * SLFLI   - macropore infiltration rate down from layer (mm/d)
#' * SLFLPI  - vertical macropore infiltration from layer (mm)
#' * SLFLDI  - vertical macropore infiltration from layer (mm)
#' * SLFLMI  - vertical macropore infiltration from layer (mm)
#' * SLFLYI  - vertical macropore infiltration from layer (mm)
#' * SLRAD   - average solar radiation on slope over daytime (W/m2)
#' * SLVP    - evaporation rate from soil (mm/d)
#' * SLVPD   - soil evaporation (mm)
#' * SLVPM   - soil evaporation (mm)
#' * SLVPY   - soil evaporation (mm)
#' * SMLT    - melt drainage rate from snowpack (mm/d)
#' * SMLTD   - snowmelt (mm)
#' * SMLTM   - snowmelt (mm)
#' * SMLTY   - snowmelt (mm)
#' * SNODEN  - * snow density (mm/mm)
#' * SNOEN   - energy flux density to snow surface (MJ m-2 mm-1 d-1)
#' * SNOFRC  - fraction of precipitation for the day as snow (unitless)
#' * SNOW    - water equivalent of snow on the ground (mm)
#' * SNOWIN  - **** initial water equivalent of snow on the ground (mm)
#' * SNOWLQ  - liquid water content of snow on the ground (mm)
#' * SNVP    - evaporation rate from snowpack (mm/d)
#' * SNVPD   - evaporation from snowpack (mm)
#' * SNVPM   - evaporation from snowpack (mm)
#' * SNVPY   - evaporation from snowpack (mm)
#' * SOLRAD  - ** solar radiation for the day, horizontal surface (MJ/m2)
#' * SOLRADC - SOLRAD as corrected if necessary by WEATHER routine (MJ/m2)
#' * SRFL    - source area flow rate (mm/d)
#' * SRFLP   - source area flow (mm)
#' * SRFLD   - source area flow (mm)
#' * SRFLM   - source area flow (mm)
#' * SRFLY   - source area flow (mm)
#' * STHR    - snow throughfall rate (mm/d)
#' * STHRD   - snow throughfall (mm)
#' * STHRM   - snow throughfall (mm)
#' * STHRY   - snow throughfall (mm)
#' * STONEF  - * stone volume fraction (unitless)
#' * STORD   - total water storage in system (mm)
#' * STORM   - total water storage in system (mm)
#' * STORY   - total water storage in system (mm)
#' * STRES   - TRAN / PTRAN for time period
#' * STRX    - string variable to trap q or esc
#' * SWAT    - total soil water in all layers (mm)
#' * SWATI   - water volume in layer (mm)
#' * SWATMX  - maximum water storage for layer (mm)
#' * SWATQF  - @water storage at field capacity for layers 1 to QLAYER (mm)
#' * SWATQX  - @ maximum water storage for layers 1 to QLAYER (mm)
#' 
#' * T1      - * lowest temp. at which stomates not temp. limited (degC)
#' * T2      - * highest temp. at which stomates not temp. limited (degC)
#' * TA      - mean temperature for the day at reference height (degC)
#' * TADTM   - average daytime temperature at reference height (degC)
#' * TAJ     - TADTM or TANTM depending on J
#' * TANTM   - average nighttime temperature at reference height (degC)
#' * TEMP    - temporary integer variable - apparently no longer used
#' * TH      - * temperature above which stomates are closed (degC)
#' * THETA   - water content (mm water / mm soil matrix)
#' * THETAF  - * volumetric water content at field capacity
#' * THICK   - * layer thicknesses (mm)
#' * THSAT   - * theta at saturation, matrix porosity
#' * TL      - * temperature below which stomates are closed (degC)
#' * TMAX    - ** maximum temperature for the day (degC)
#' * TMIN    - ** minimum temperature for the day (degC)
#' * TRANI   - average transpiration rate from layer (mm/d)
#' * TRANP   - transpiration (mm)
#' * TRAND   - transpiration (mm)
#' * TRANM   - transpiration (mm)
#' * TRANY   - transpiration (mm)
#' * TRANPI  - layer transpiration (mm)
#' * TRANDI  - layer transpiration (mm)
#' * TRANMI  - layer transpiration (mm)
#' * TRANYI  - layer transpiration (mm)
#' * TSNOW   - snowpack temperature (isothermal assumed, degC)
#' 
#' * UA      - average wind speed for the day at reference height (m/s)
#' * UADTM   - average wind speed for daytime at reference height (m/s)
#' * UAJ     - UADTN or UANTM depending on J
#' * UANTM   - average wind speed for nighttime at reference height (m/s)
#' * UW      - ** average wind speed for day at weather station (m/s)
#' 
#' * VPD     - vapor pressure deficit at reference height (kPa)
#' * VRFLI   - vertical matrix drainage rate from layer (mm/d)
#' * VRFLPI  - vertical matrix drainage from layer (mm)
#' * VRFLDI  - vertical matrix drainage from layer (mm)
#' * VRFLMI  - vertical matrix drainage from layer (mm)
#' * VRFLYI  - vertical matrix drainage from layer (mm)
#' * VV      - temporary VRFLI
#' 
#' * WETC    - @ wetness at PSICR (Globalensionless)
#' * WETF    - @ wetness at field capacity (Globalensionless)
#' * WETFR   - fraction of precipitation interval that canopy is wet
#' * WETINF  - * wetness at dry end of near-saturation range
#' * WETNES  - wetness (fraction of saturation)
#' * WNDRAT  - * ratio of nighttime to daytime wind speed
#' 
#' * XMAX    - maximum value for x-axis
#' * XMIN    - minimum value for x-axis
#' 
#' * YEARN   - **** first year from DFILE header
#' * YMAX    - * maximum value for y-axis
#' * YMIN    - @ minimum value for y-axis
#' * YNAME   - @ variable name
#' * YVAL    - plot value for y-axis
#' * YY      - ** year from data files, used only to check dates and to determine end of year for output
#' 
#' * Z0      - roughness parameter (m)
#' * Z0C     - roughness parameter for closed canopy of HEIGHT (m)
#' * Z0G     - * ground surface roughness (m)
#' * Z0GS    - ground or snow surface roughness (m)
#' * Z0S     - * snow surface roughness (m)
#' * Z0W     - * weather station roughness parameter (m)
#' * ZA      - reference height for TA, EA, UA, above ground (m)
#' * ZMINH   - * ZA minus HEIGHT, reference height above canopy top (m)
#' * ZW      - * weather station measurement height for wind (m)
#' 
## -------------------------------------------------------------------------------------------------------------------------
AA  <-0       
ADEF<-0   
ALB   <-0.07       
ALBEDO<-0    
ALBSN <-0.3           
ALPHA<-numeric(ML) 
ASPECT  <-0         
ASPECTD <-0       
ASUBS   <-0          
ATR<-numeric(2)          
ATRANI<-numeric(ML)  
ATRI<-matrix(0,2,ML) 
AWAT    <-0          
BALERD<-0 
BALERM<-0  
BALERY<-0 
BEXP<-rep(0,ML)  
BEXP[1]<-5.37433
BEXP[2]<-4.03320
BEXP[3]<-5.64096
BEXP[4]<-6
BEXP[5]<-5
BYFL   <-0       
BYFLI<-numeric(ML)  
BYFLPI<-numeric(ML)
BYFLDI<-numeric(ML)
BYFLMI<-numeric(ML)
BYFLYI<-numeric(ML)
BYFLP<-rep(0,ML)
BYFLD<-rep(0,ML) 
BYFLM<-rep(0,ML)  
BYFLY<-rep(0,ML)
BYFRAC<-numeric(ML)  
BYPAR<-1            
#C    <-0             
C1    <-0.25
C2    <-0.5     
C3    <-0.1            
CC      <-0   
CCFAC  <-0.3          
CHM<-numeric(ML)     
CHN<-numeric(ML)     
CINTRL<-0.15            
CINTRS<-0.15            
CINTSL<-0.6         
CINTSS<-0.6             
CR   <-0.5              
CS  <-0.035           
CVPD <-2           
CZR  <-0.05              
CZS <-0.13          
DAYLEN <-0           
DAYMO<-numeric(12)     
DD  <-0            
DELTA <-0           
DENSEF <-0.8580571           
DISP  <-0        
DISPC  <-0           
DOM  <-0        
DOY  <-0            
DPSIDW<-numeric(ML) 
DPSIMX  <-0.01          
DRAIN  <-0.626259     
DSFL  <-0         
DSFLI<-numeric(ML)   
DSFLP<-0 
DSFLD<-0 
DSFLM<-0 
DSFLY<-0   
DSFLPI<-numeric(ML)
DSFLDI<-numeric(ML)
DSFLMI<-numeric(ML)
DSFLYI<-numeric(ML)
DSLOPE  <-0         
DSLOPED  <-0      
DSWMAX   <-2        
DTI   <-0          
DTIMAX  <-0.5          
DTINEW  <-0.0          
DTP    <-1           
DTRI  <-0             
DUMM<-rep(0,gctMaxoutvars)  
dummy   <-0          
DURATN<-c(4,4,4,4,4,4,4,4,4,4,4,4)   
EA    <-0             
ES   <-0              
ESLOPE  <-0           
ESLOPED  <-2         
EVAPP<-0 
EVAPD<-0  
EVAPM<-0  
EVAPY <-0
FARR<-rep(0,366)    
FETCH   <-5000           
FLOWP<-0 
FLOWD<-0 
FLOWM<-0 
FLOWY<-0  
FRINTL  <-0.06           
FRINTS  <-0.06        
FSINTL  <-0.04           
FSINTS  <-0.04          
FXYLEM  <-0.5           
GER<-numeric(2)            
GEVP  <-0       
GIR<-numeric(2)          
GIVP <-0       
GLMAX  <-0           
GLMAXC  <- 1.7          
GLMIN <-0           
GLMINC  <-0.03        
GRAPH <-0            
GRDMLT <-0.35           
GSC   <-0            
GSP  <-0.085              
GWAT   <-0            
GWATIN <-20            
GWFL  <-0             
GWFLP<-0 
GWFLD<-0 
GWFLM<-0 
GWFLY<-0   
mnuhalfiter<-FALSE
HEIGHT  <-0          
HR   <-10              
HS    <-1            
#I    <-0            
I0HDAY <-0            
IDAY<-0             
II <-0               
ILAYER <-0          
IDEPTH <-1000            
IMPERV  <-0.025 
INFEXP  <-0.8797636          
INFRAC<-rep(0,ML)  
INFLI<-rep(0,ML) 
INFLP<-rep(0,ML)
INFLD<-rep(0,ML)
INFLM<-rep(0,ML)
INFLY<-rep(0,ML) 
INFLPI<-rep(0,ML)
INFLDI<-rep(0,ML)
INFLMI<-rep(0,ML)
INFLYI<-rep(0,ML)
INTR   <-0           
INTRIN  <-0           
INTS   <-0            
INTSIN <-0            
IRVP <-0              
IRVPD<-0 
IRVPM<-0 
IRVPY <-0             
ISVP  <-0           
ISVPD<-0 
ISVPM<-0 
ISVPY<-0  
J   <-0              
KF<-rep(0,ML)      
KF[1]<-6.9
KF[2]<-2.7
KF[3]<-2.9
KF[4]<-1
KF[5]<-3.5
KK<-rep(0,ML)      
KSAT<-rep(0,ML)    
KSNVP <-0.009734            
L1  <-0               
L2  <-0               
LAI  <-0              
LAIMLT <-0.2           
LAT   <-  0.183       
LATD <- -10.5              
LENGTH  <-0           
LPC  <-4              
LWIDTH    <-0.004      
MARR<-c(seq(1,366,1))    
MAXHT<-25             
MAXLAI  <-7.693270           
MAXLQF <-0.05            
MELFAC <-1.728930            
MESFL <-0            
MESFLD<-0
MESFLM<-0
MESFLY<-0 
MESFLP <-0           
MM    <-0     
MONTHN <-0        
MXKPL  <-7.03463      
MXRTLN <-3000.001    
N   <-0            
NN  <-2.5             
NDAYS<-0            
NITS<-0             
NITSD<-0            
NITSM<-0           
NITSY<-0            
NITSR<-0            
NLAYER  <-5         
NOOUTF <-1          
NTFLI<-rep(0,ML)  
NTFLPI<-rep(0,ML)
NTFLDI<-rep(0,ML)
NTFLMI<-rep(0,ML)
NTFLYI<-rep(0,ML)
PINT <-0        
PINTD<-0 
PINTM<-0 
PINTY <-0 
PIR<-c(0,0)          
PREC  <-0      
PRECD<-0 
PRECM<-0 
PRECY<-0             
PREINT  <-0          
PRECIN  <-0          
PSICR  <--2           
PSIF<-rep(0,ML)   
PSIF[1]<--11.818
PSIF[2]<--11.516 
PSIF[3]<--10.22
PSIF[4]<--10
PSIF[5]<--10
PSIG<-rep(0,ML)     
PSIM<-rep(0,ML)      
PSIMIN<-rep(-10,ML)  
PSITI<-rep(0,ML)    
PSNVP <-0          
PTR<-c(0,0)
PTRAN  <-0          
PTRAND<-0 
PTRANM<-0 
PTRANY<-0          
QFFC<-0.00104        
QFPAR <-0.834524        
QLAYER <-1        
QDEPTH <-0.1          
R5    <-100           
RAA    <-0           
RAC   <-0            
RAS  <-0           
RELHT<-c(1,1,366,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)    
RELLAI<-c(1,1,366,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)   
ROOTDEN<-c(50,1,50,1,50,1,50,1,50,1,50,.3,50,.2,50,.1,50,.1,50,.1,50,0.0,50,0.0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0)  
RELDEN<-rep(0,ML)  
RNOUT <- c(0,0)  # added by tbiggs
RFAL   <-0          
RFALD<-0
RFALM<-0
RFALY <-0            
RHOTP <-2            
RINT  <-0           
RINTD<-0
RINTM<-0
RINTY<-0           
RM <-1000               
RNET   <-0          
RNETD<-0
RNETM<-0 
RNETY <-0            
RPLANT<-0            
RROOTI<-rep(0,ML)    
RTHR   <-0           
RTHRD<-0
RTHRM<-0
RTHRY<-0            
RTLEN <-0           
RTRAD <-0.35           
RRD<-0.55           
RSC <-0              
RSNO <-0           
RSNOD<-0
RSNOM<-0
RSNOY<-0         
RSS<-0              
RSSA <-500             
RSSB <-1             
RSTEMP<- -1.29978         
RXYLEM <-0           
SAFRAC <-0           
SAI <-0              
SAIMLT <-0.5           
SEEP <-0             
SEEPP<-0
SEEPD<-0
SEEPM<-0
SEEPY<-0             
SFAL <-0     
SFALD<-0
SFALM<-0
SFALY<-0             
SHEAT<-0        
SINT <-0           
SINTD<-0 
SINTM<-0
SINTY <-0
SLFDAY  <-0          
SLFL  <-0            
SLFLP<-0
SLFLD<-0
SLFLM<-0
SLFLY<-0
SLFLI<-rep(0,ML)  
SLFLPI<-rep(0,ML)
SLFLDI<-rep(0,ML)
SLFLMI<-rep(0,ML)
SLFLYI<-rep(0,ML)
SLRAD <-0            
SLRADd<-0
SLRADOUTVEC <-c(0,0)  # tbigs
SLVP <-0            
SLVPD<-0
SLVPM<-0
SLVPY<-0          
SMLT  <-0            
SMLTD<-0
SMLTM<-0 
SMLTY <-0           
SNODEN <-0.3         
SNOEN <-0           
SNOFRC <-0        
SNOW <-0             
SNOWIN  <-0          
SNOWLQ<-0            
SNVP  <-0            
SNVPD<-0
SNVPM<-0
SNVPY <-0            
SOLRAD <-0           
SOLRADC<-0           
SRFL <-0             
SRFLP<-0
SRFLD<-0
SRFLM<-0
SRFLY <-0
STHR <-0             
STHRD<-0
STHRM<-0
STHRY<-0 
STONEF<-rep(0.00,ML)
STONEF[1]<-0.02
STONEF[2]<-0.2
STONEF[3]<-0.25
STONEF[4]<-0.25
STONEF[5]<-0.7
STORD<-0
STORM<-0
STORY<-0 
STRES<-0            
STRX<-0             
SWAT<-0    
SWATI<-rep(0,ML)  
SWATMX<-rep(0,ML)  
SWATQF   <-0         
SWATQX <-0           
T1    <-10           
T2    <-30           
TA   <-40            
TADTM <-0           
TAJ  <-0             
TANTM <-0            
TEMP  <-0           
TH   <-40            
THETA<-rep(0,ML)   
THETAF<-rep(0,ML) 
THETAF[1]<-0.34062341
THETAF[2]<-0.39705807  
THETAF[3]<-0.24359704
THETAF[4]<-0.35
THETAF[5]<-0.23
THICK<-rep(0,ML)   
THICK[1]<-50
THICK[2]<-250
THICK[3]<-300
THICK[4]<-300
THICK[5]<-100
THSAT<-rep(0,ML)  
THSAT[1]<-0.6313342
THSAT[2]<-0.6189386
THSAT[3]<-0.4930716
THSAT[4]<-0.680
THSAT[5]<-0.600
TL    <-0           
TMAX  <-0            
TMIN <-0             
TRANI<-rep(0,ML)   
TRANP<-0
TRAND<-0
TRANM<-0
TRANY<-0             
TRANPI<-rep(0,ML)
TRANDI<-rep(0,ML)
TRANMI<-rep(0,ML) 
TRANYI<-rep(0,ML)
TSNOW  <-0          
UA     <-0           
UADTM  <-0           
UAJ   <-0           
UANTM <-0          
UW   <-0             
VPD  <-0             
VRFLI<-rep(0,ML)  
VRFLPI<-rep(0,ML)
VRFLDI<-rep(0,ML)
VRFLMI<-rep(0,ML)
VRFLYI<-rep(0,ML)
VV<-rep(0,ML)      
WETC<-rep(0,ML)    
WETF<-rep(0,ML)   
WETFR  <-0         
WETINF<-rep(0,ML)  
WETINF[1]<-0.92
WETINF[2]<-0.92
WETINF[3]<-0.92
WETINF[4]<-0.92
WETINF[5]<-0.92
WETNES<-rep(0,ML)  
WNDRAT <-.3           
XMAX  <-0            
XMIN  <-0           
YEARN <-0           
YMAX<-numeric(gvals)      
YMIN<-numeric(gvals)      
YNAME<-numeric(gvals)    
YVAL<-numeric(gvals)      
YY   <-0            
Z0   <-0             
Z0C  <-0             
Z0G  <-0.02             
Z0GS  <-0            
Z0S  <-0.001             
Z0W  <-1.0  # Weather station roughness parameters             
ZA   <-0            
ZMINH <-2           
ZW  <-8.5    # Weather station height for wind, m          

