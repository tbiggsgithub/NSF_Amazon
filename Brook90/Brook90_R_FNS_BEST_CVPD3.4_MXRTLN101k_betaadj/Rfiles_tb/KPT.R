#' ---
#' title: "KPT"
#' author: "Rico Kronenberg, Lisa Marie Oehlschl?gel"
#' date: "29 April 2018"
#' output: html_document
#' ---
#' 
#' ### KPT - Soil water properties
#' The KPT-script includes functions for soil water parameters and initial variables, matric potential from wetness and soil water variables.
#' 
#' This section uses both the standard algebraic notation for soil water variables as in [Clapp and Hornberger (1978)](./Literature.html) and the BROOK90 variable names. The correspondence is
#' 
#' * &theta; - THETA 
#' * W - WETNES
#' * &Psi; - PSIM; 	
#' * K - KK; 	
#' * b - BEXP;
#' * &theta;f - THETAF;
#' * Wf - WETF; 
#' * &Psi;f - PSIF; 
#' * Kf - KF; 
#' * n - CHN;
#' * &theta;s - THSAT;
#' * Wi - WETINF; 	
#' * &Psi;i - PSIINF; 
#' * Ks - KSAT; 
#' * m - CHM;
#' * T - THICK; 
#' * S - STONEF
#' 
#' Functional relationships among soil-water content, &theta;, matric potential, &Psi;, and hydraulic conductivity, K, are required  	for any simulation that moves water through the soil. There is a rather vast literature on this subject, though much of it emphasizes agricultural soils rather than natural soils, which are less disturbed and higher in organic matter. BROOK90 uses a modification of the [Campbell (1974)](./Literature.html) expressions with the near-saturation interpolation of [Clapp and Hornberger (1978)](./Literature.html). 
#' 
#' The wetness, W, or saturation fraction, is defined as the fraction of the pore space that is filled with water, or
#' 
#' * (1) W = (&theta; - &theta;r) / &theta;s - &theta;r)
#' 
#' where &theta;s is the saturated water content, or porosity, and &theta;r is the "residual water content".
#' 
#' The classic expressions of [Brooks and Corey (1964)](./Literature.html) are
#' 
#' * (2) &Psi; = &Psi;s W-b
#' 
#' where b is a parameter and &Psi;s is the potential to which the curve extrapolates when W = 1 (air-entry potential), and
#' 
#' * (3) K = Ks W2b+3
#' 
#' where Ks is the saturated hydraulic conductivity.
#' 
#' With the assumption that &theta;r is usually so small it can be set to zero, these equations are usually attributed to [Campbell (1974)](./Literature.html). [W?sten and vanGenuchten (1988)](./Literature.html) state that &theta;r can be set to zero "without significantly affecting the accuracy of the results". The Campbell form (&theta;r = 0) is used in BROOK90 and by [Clapp and Hornberger (1978)](./Literature.html).
#' 
#' Equation (2) fails as the soil approaches saturation, that is, as W approaches 1, because &Psi; at saturation must be 0, not &Psi;s. In this "near saturation region", [Clapp and Hornberger (1978)](./Literature.html) suggest that the parabolic expression
#' 
#' * (4) &Psi; = -m (W - n) (1 - W)
#' 
#' provides an appropriate approximation, and holds above an inflection wetness, Wi, at which the change to parabolic form occurs. They obtained the two parameters, m, and n, by matching (2) and (4) and their first derivatives at W = Wi, leading to
#' 
#' * (5) m = -&Psi;i [1 / (1 - Wi) - b / Wi] / (1 - Wi)
#' * (6) n = 2 Wi - 1 + (b &Psi;i / m Wi)
#' 
#' where &Psi;i is &Psi; at Wi obtained from (2). They suggest that Wi can be taken as 0.92 for all soils, but varying it allows a closer agreement with the [vanGenuchten (1980)](./Literature.html) equation. However, Wi must be greater than b/(1+b). (Note that in [Clapp and Hornberger (1978)](./Literature.html) &Psi; is always positive whereas here &Psi; is inherently negative.) Equation (3) does not inflect but holds up to Ks at &theta;s .
#' 
#' The more complicated expressions of [vanGenuchten (1980)](./Literature.html)are frequently used as an alternative to the Brooks-Corey equations. Conversion of parameters between the two forms can be accomplished [Ma and others (1999)](./Literature.html) but are less satisfactory because of the abrupt break at saturation in the Brooks-Corey expression. The Clapp-Hornberger curve shape should agree better with vanGenuchten than Brooks-Corey does, especially if Wi is allowed to vary, but I do not know of any direct comparison. Substitution of other relationships, such as van Genuchten in BROOK90 could conceptually be done but would require significant reprogramming. More sophisticated relations such as vanGenuchten hardly seem warranted given spatial variation, hysteresis, and the extra computer time required [Ross (1992)](./Literature.html).
#' 
#' The use of &Psi;s and Ks in (2) and (3) has been questioned (e.g. [Yates and others (1992)](./Literature.html)). Especially in forest soils, any field measurement of Ks will actually be a measure of macropore and pipe flow and probably is not directly related to the unsaturated or matrix flow properties. Therefore these equations are modified in BROOK90 to replace &Psi;s and Ks with &Psi;f and Kf at some unsaturated water content, &theta;f . Then &theta;s becomes the only parameter that affects the near saturation region. The modified equations are
#' 
#' * (7) &Psi; = &Psi;f (W / Wf) -b
#' * (8) K = Kf (W / Wf) 2b+3
#' 
#' where Wf = &theta;f / &theta;s is the wetness at &theta;f.
#' 
#' The wetness at which the &Psi;f , Kf , &theta;f "triple point" is specified does not matter (as long as it is less than Wi) and does not affect the simulation of unsaturated flow and transpiration in BROOK90. However, I chose to require that the triple point be at "field capacity" in order to use this value in simulations of soil evaporation (SLVP), source-area flow (SRFL), bypass flow (BYFL), and the output value of root-zone soil-water deficit (ADEF). For more on this bad decision and on the definition and specification of field capacity, see the Field Capacity section below.
#' 
#' ### Contents
#' 
#' * [Function FDPSIDWF](#function-fdpsidwf)
#' * [Function FPSIMF](#function-fpsimf)
#' * [Function SOILPAR](#function-soilpar)
#' * [Function SOILVAR](#function-soilvar)
#' 
#' ### Function FDPSIDWF
#' FDPSIDW returns d&Psi;i/dWi for one layer, which is needed for the selection of iteration time-step. Differentiation of (4) and (7) leads to
#' 
#' * d&Psi;i / dWi = (-b &Psi;f / Wf) (Wi / Wf)-b-1
#' 
#' in the unsaturated range,
#' 
#' * d&Psi;i / dWi= m (2 Wi - n - 1)
#' 
#' in the near saturation range, and
#' 
#' * d&Psi;i / dWi = 0
#' 
#' when the soil is saturated (Wi = 1). 
#' 
#' This is a function to receive the ratio of matric potential to wetness, used in 2nd approximation to iteration timestep. The input to FDPSIDWF is shown in the following table:
#' 
#' Input |Description
#' ------|---------------------
#' WETNES|wetness, fraction of saturation
#' PSIF  |matrix potential at field capacity (kPa)
#' BEXP  |exponent for psi-theta relation
#' WETINF|wetness at dry end of near-saturation range
#' WETF  |saturation fraction at field capacity
#' CHM   |Clapp and Hornberger m (kPa)
#' CHN   |Clapp and Hornberger n
#' 
#' Depending on WETNES[i], the ratio is calculated different. And so the output of FDPSIDWF is:
#' 
#' *  FDPSIDW - d PSI / d WETNES (kPa)
#' 
## -------------------------------------------------------------------------------------------------------------------------
FDPSIDWF<-function(i){
  if (WETNES[i] < WETINF[i]){ 
    FDPSIDW <- (-BEXP[i] * PSIF[i] / WETF[i]) * (WETNES[i] / WETF[i]) ^ (-BEXP[i] - 1)
  }else if (WETNES[i] < 1){
# in near-saturated range
    FDPSIDW <- CHM[i] * (2 * WETNES[i] - CHN[i] - 1)
  }else{
# saturated
    FDPSIDW <- 0
  }
  return(FDPSIDW)
}

#' 
#' ### Function FPSIMF
#' FPSIM obtains &Psi;i from Wi for one layer using equation (4) in the unsaturated region and equation (7) in the near-saturation region. This function is used at the end of every iteration. When Wi = 1, then &Psi;i= 0. 
#' 
#' This is used to calculate the matric potential from wetness. The input arguments are the same as for FDPSIDWF:
#' 
#' Input |Description
#' ------|---------------------
#' WETNES|wetness, fraction of saturation
#' PSIF  |matrix potential at field capacity (kPa)
#' BEXP  |exponent for psi-theta relation
#' WETINF|wetness at dry end of near-saturation range
#' WETF  |saturation fraction at field capacity
#' CHM   |Clapp and Hornberger m (kPa)
#' CHN   |Clapp and Hornberger n
#' 
#' The resulting output of this one is:
#' 
#' * FPSIM - matric potential (kPa)
#' 
#' If the wetness is below zero, the matric potential gets an arbitrary very negative value. Else there are different equatations for FPSIM, depending on the wetness of the soil.
#' 
## -------------------------------------------------------------------------------------------------------------------------
FPSIMF<-function(WETNESi, PSIFi, BEXPi, WETINFi, WETFi, CHMi, CHNi){
  if (WETNESi <= 0){ 
    FPSIM <- -10000000000
  }else if (WETNESi < WETINFi) {
    FPSIM <- PSIFi * (WETNESi / WETFi) ^ (-BEXPi)
  }else if (WETNESi < 1) {
# in near-saturated range
    FPSIM <- CHMi* (WETNESi - CHNi) * (WETNESi - 1)
  }else{
# saturated
  FPSIM <- 0
  }
  return(FPSIM)
}

#' 
#' ### Function SOILPAR
#' The parameters &theta;f , &Psi;f , Kf , Wi , b , &theta;s , soil layer thickness, T, and stone fraction by volume, S, are all provided for each soil layer in the soil parameter file. Subroutine SOILPAR is called at the beginning of a run to calculate more soil water parameters from these.
#' 
#' Wf is obtained from (1) using &theta;f ; then &Psi;i is obtained from (4) using Wi ; then m and n are obtained from (5) and (6). The maximum possible water content of a layer in mm, i.e. its water-holding capacity or saturation water content (SWATMX) is
#' 
#' * SWATMX = T &theta;s (1 - S).
#' 
#' Initial &Psi;, rather than initial water content, is provided in the initial value file. The initial wetness, or saturation fraction (W), is then obtained by solving (4) and (7) for W, so
#' 
#' * W = 1;	&Psi; = 0
#' * W = 0.5 (1 + n) + 0.5 (n2 - 2n + 1 + 4&Psi;/m) 0.5;  	&Psi;i < &Psi; < 0
#' * W = Wf (&Psi; / &Psi;f) -1/b; 	&Psi; < &Psi;i
#' 
#' The total water content in the layer in mm is
#' 
#' * SWATI = W * SWATMX.
#' 
#' The gravity potential, &Psi;g, is measured negatively down from zero at the soil surface. For the surface layer it is -&rho;wg T(1) / 2, where &rho;w is the density of water and g is the acceleration of gravity. In BROOK90, &rho;wg (RHOWG) is constant. For the i'th layer
#' 
#' * &Psi;g(i) = &Psi;g(i-1) - &rho;wg [T(i-1) + T(i)] / 2.
#' 
#' The saturated hydraulic conductivity, Ks, is needed in subroutine [VERT](./WAT.html) to prevent excessive conductivities. It is
#' 
#' * Ks = Kf (1 / Wf) 2b+3
#' 
#' from (8) with W = 1.
#' 
#' The wetness of each soil layer (WETC) at the minimum plant water potential (PSICR) is calculated. It is the lower limit of available water in the layer, which is used later to calculate the available water in the root zone (AWAT), an output variable. The upper limit of available water in a soil layer is PSIF. 
#' 
#' Soil water parameters and initial variables are computed in function SOILPAR. The necessary input is:
#' 
#' Input   |Description
#' --------|----------------------
#' NLAYER% |number of soil layers
#' THICK() |layer thicknesses (mm)
#' THSAT() |theta at saturation, matrix porosity
#' STONEF()|stone volume fraction (unitless)
#' THETAF()|volumetric water content at field capacity
#' PSIF()  |matric potential at field capacity (kPa)
#' BEXP()  |exponent for psi-theta relation
#' WETINF()|wetness at dry end of near-saturation range
#' PSIM()  |matric soil water potential for layer (kPa)
#' KF()    |hydraulic conductivity at field capacity (mm/d)
#' PSICR   |minimum plant leaf water potential (MPa)
#' 
#' Local are:
#' 
#' * Dim i% - soil layer
#' * PSIINF - potential at dry end of near saturation range (kPa)
#' 
#' The constant RHOWG is required for the calculation and it describes the density of water times acceleration of gravity (kPa/mm). The gravity potential is negative down from surface.
#' 
#' The output contains this parameters and variables:
#' 
#' * PSIG() - gravity potential (kPa)
#' * SWATMX() - maximum water storage for layer (mm)
#' * WETF() - wetness at field capacity (dimensionless)
#' * WETC() - wetness at PSICR (dimensionless)
#' * CHM() - Clapp and Hornberger m (kPa)
#' * CHN() - Clapp and Hornberger n
#' * WETNES() - wetness, fraction of saturation
#' * SWATI() - water volume in layer (mm)
#' * KSAT() - saturated hydraulic conductivity (mm/d)
#' 
## -------------------------------------------------------------------------------------------------------------------------
SOILPAR<-function(){
#local
  #Dim i%
  PSIINF<-rep(1,50)
#
  wetff<-WETF
  psigg<-PSIG
  thickk<-THICK
  SWATMx<-SWATMX
  PSIINf<-  PSIINF
  CHm<-CHM
  CHn<-CHN
  WETNEs<-WETNES
  SWATi<-SWATI
  KSAt<-KSAT
  WETc<-WETC
  
  for(i in 1:NLAYER){
    if (i == 1) {
      psigg[1] <- -RHOWG * thickk[1] / 2
    }else{
      psigg[i] <- psigg[i - 1] - RHOWG * ((thickk[i - 1] + thickk[i]) / 2)
    }
    SWATMx[i] <- thickk[i] * THSAT[i] * (1 - STONEF[i])
    wetff[i] <- THETAF[i] / THSAT[i]
    PSIINf[i] <- PSIF[i] * (WETINF[i] / wetff[i]) ^ -BEXP[i]
    CHm[i] <- (-PSIINf[i] / (1 - WETINF[i]) ^ 2) - BEXP[i] * (-PSIINf[i]) / (WETINF[i] * (1 - WETINF[i]))
    CHn[i] <- 2 * WETINF[i] - 1 - (-PSIINf[i] * BEXP[i] / (CHm[i] * WETINF[i]))
    if (PSIM[i] > 0) {
     # Stop
    }else if (PSIM[i] == 0) {
      WETNEs[i] <- 1
    }else{
      WETNEs[i]<- wetff[i] * (PSIM[i] / PSIF[i]) ^ (-1 / BEXP[i])
      if (WETNEs[i] > WETINF[i]){
        WETNEs[i] <- (1 + CHn[i]) / 2 + 0.5 * (CHn[i] ^ 2 - 2 * CHn[i] + 1 + 4 * PSIM[i] / CHm[i])^(1/2)
      }
    }
    SWATi[i] <- WETNEs[i] * SWATMx[i]
    KSAt[i] <- KF[i] * (1 / wetff[i]) ^ (2 * BEXP[i] + 3)
    WETc[i] <- wetff[i] * (1000 * PSICR / PSIF[i]) ^ (-1 / BEXP[i])
  }
return(list(PSICR, psigg, SWATMx, wetff, WETc, CHm, CHn, WETNEs, SWATi, KSAt))
}

#' 
#' ### Function SOILVAR
#' At the beginning of a run and the end of each iteration time-step BROOK90 calls SOILVAR to calculate other soil water variables from &Psi;, W, and SWATI. For each layer It calculates total potential, &Psi;t, and water content, &theta;,
#' 
#' * &Psi;t = &Psi; + &Psi;g
#' 
#' * &theta; = W &theta;s
#' 
#' as well as K from (5) and the total sum of SWATI over all layers (SWAT). 
#' 
#' The function SOILVAR calculates the needed soil water variables. The required input is included in the following table:
#' 
#' Input   |Description
#' --------|-------------------------
#' NLAYER% |number of soil layers
#' PSIG()  |gravity potential (kPa)
#' PSIM()  |matric soil water potential for layer (kPa)
#' WETNES()|wetness, fraction of saturation
#' THSAT() |theta at saturation, matrix porosity
#' KF()    |hydraulic conductivity at field capacity (mm/d)
#' BEXP()  |exponent for psi-theta relation
#' WETF()  |wetness at field capacity (dimensionless)
#' SWATI() |water volume in layer (mm)
#' 
#' The Dim i% describes the soil layer. Hydraulic conductivity can be calculated in different ways, depending on wetness. If the wetness is < 0.0001, then the soil is extremly dry and the hydraulic conductivity receives a very small value. The output variables of SOILVAR are:
#' 
#' * PSITI() - total potential (kPa)
#' * THETA() - water content (mm water / mm soil matrix)
#' * SWAT - total soil water in all layers (mm)
#' * KK() - hydraulic conductivity (mm/d)
#' 
## -------------------------------------------------------------------------------------------------------------------------
SOILVAR<-function(){
  PSITi<-PSITI
  THETa<-THETA
  Kk<-KK
  SWAt <- 0
  for (i in 1:NLAYER){
    PSITi[i] <- PSIM[i] + PSIG[i]
    THETa[i] <- WETNES[i] * THSAT[i]
    if(WETNES[i] > 0.0001){
      Kk[i] <- KF[i] * (WETNES[i] / WETF[i]) ^ (2 * BEXP[i] + 3)
    }else{
      Kk[i] <- 0.0000000001
    }
    SWAt <- SWAt + SWATI[i]
  }
return(c(PSITi, THETa, Kk, SWAt))
}

