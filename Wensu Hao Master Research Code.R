library(stringr)
library(lubridate)
library(parallel)
library(ncdf4)
library(sp)
library(raster)
library(purrr)
library(ggplot2)
library(maps)
library(raster)
library(scales)
library(sp)
library(terra)
library(RColorBrewer)
library(tidyverse)
library(gridExtra)
library(ggthemes)
library(cetcolor)
setwd()
rm(list = ls())
##########-------------------------------------------------------------------------
##Calculate the 7 days average data
##########-------------------------------------------------------------------------
#Import SWdown data
data <- rast(list.files("D:/Climate data/1991/SWdown", pattern=".nc",full.names = T))
#Summarized for the current day's data
hour2day_label <-  yday(ymd(str_split(time(data)," ",simplify = T)[,1]))
single_day=c()
for(i in unique(hour2day_label)){
  print(i)
  tag = which(hour2day_label == i)
  single_day <<- c(single_day,mean(data[[tag]]))
}
single_day <- do.call("c",single_day)
names(single_day) <- unique(ymd(str_split(time(data)," ",simplify = T)[,1]))
time(single_day) <- unique(ymd(str_split(time(data)," ",simplify = T)[,1]))
writeCDF(single_day, filename="D:/Climate data/1991/SWdown_single_day.nc", overwrite=TRUE, varname="SWdown",
         longname="SWdown_WFDE5_CRU_1991_day", unit="W m-2")
# averages are obtained at 7-day steps
rolled <- roll(single_day, n=7, "sum", "around", na.rm=TRUE)

week_order <- isoweek(ymd(str_split(time(single_day)," ",simplify = T)[,1]))
week_order[which(week_order == 1)[which(week_order == 1)>7]] = max(week_order)+1

single_week = tapp(rolled, index=week_order, fun=sum,cores=6)
names(single_week) <- unique(week_order)
writeCDF(single_week, filename="D:/Climate data/1991/SWdown_single_week.nc", overwrite=TRUE, varname="SWdown",
         longname="SWdown_WFDE5_CRU_1991_week", unit="W m-2")

#Transfer to 0.5 degree
tif <- rast("D:/Climate data/1991/yield_1991.tif")
r <- tif
res(r) <- 0.5
r <- resample(tif, r)

#Mask the area with tif
r[r==0]=NA
r[!is.na(r)]=1
single_week = r*single_week
single_week_crop = crop(single_week,r)
single_week_mask = mask(single_week,r)
names(single_week_mask) = paste0("week_",unique(week_order))
writeCDF(single_week_mask, filename="D:/Climate data/1991/SWdown_single_week_mask.nc", overwrite=TRUE, 
         varname="SWdown", longname="SWdown_WFDE5_CRU_1991_week", unit="W m-2")
#Calculate the PPFD value
nc_open("D:/Climate data/1991/SWdown_single_week_nc_crop1.nc")
SWdown <- nc_open("D:/Climate data/1991/SWdown_single_week_nc_crop1.nc", write = TRUE)
Rsw <- ncvar_get(SWdown, "SWdown")
# Define kEC as a constant value
kEC <- 2.04-6 # μmol J^-1
# Calculate PPFD
PPFD <- 3600 * 24 * 10^(-6) * Rsw * kEC
# Save the updated data back to the NetCDF file
writeRaster(PPFD, filename = "D:/Climate data/1991/PPFD_1991_global.nc", format = "CDF", overwrite = TRUE)

#Import Tair data
data <- rast(list.files("D:/Climate data/1991/Tair", pattern=".nc",full.names = T))
#Summarized for the current day's data
hour2day_label <-  yday(ymd(str_split(time(data)," ",simplify = T)[,1]))
single_day=c()
for(i in unique(hour2day_label)){
  print(i)
  tag = which(hour2day_label == i)
  single_day <<- c(single_day,mean(data[[tag]]))
}
single_day <- do.call("c",single_day)
names(single_day) <- unique(ymd(str_split(time(data)," ",simplify = T)[,1]))
time(single_day) <- unique(ymd(str_split(time(data)," ",simplify = T)[,1]))
writeCDF(single_day, filename="D:/Climate data/1991/Tair_single_day.nc", overwrite=TRUE, varname="Tair",
         longname="Tair_WFDE5_CRU_1991_day", unit="K")
# averages are obtained at 7-day steps
rolled <- roll(single_day, n=7, "sum", "around", na.rm=TRUE)

week_order <- isoweek(ymd(str_split(time(single_day)," ",simplify = T)[,1]))
week_order[which(week_order == 1)[which(week_order == 1)>7]] = max(week_order)+1

single_week = tapp(rolled, index=week_order, fun=sum,cores=6)
names(single_week) <- unique(week_order)
writeCDF(single_week, filename="D:/Climate data/1991/Tair_single_week.nc", overwrite=TRUE, varname="Tair",
         longname="Tair_WFDE5_CRU_1991_week", unit="K")

#Transfer to 0.5 degree
tif <- rast("D:/Climate data/1991/yield_1991.tif")
r <- tif
res(r) <- 0.5
r <- resample(tif, r)

#Mask the area with tif
r[r==0]=NA
r[!is.na(r)]=1
single_week = r*single_week
single_week_crop = crop(single_week,r)
single_week_mask = mask(single_week,r)
names(single_week_mask) = paste0("week_",unique(week_order))
writeCDF(single_week_mask, filename="D:/Climate data/1991/Tair_single_week_mask.nc", overwrite=TRUE, 
         varname="Tair", longname="Tair_WFDE5_CRU_1991_week", unit="K")

#Import Qair data
data <- rast(list.files("D:/Climate data/1991/Qair", pattern=".nc",full.names = T))
#Summarized for the current day's data
hour2day_label <-  yday(ymd(str_split(time(data)," ",simplify = T)[,1]))
single_day=c()
for(i in unique(hour2day_label)){
  print(i)
  tag = which(hour2day_label == i)
  single_day <<- c(single_day,mean(data[[tag]]))
}
single_day <- do.call("c",single_day)
names(single_day) <- unique(ymd(str_split(time(data)," ",simplify = T)[,1]))
time(single_day) <- unique(ymd(str_split(time(data)," ",simplify = T)[,1]))
writeCDF(single_day, filename="D:/Climate data/1991/Qair_single_day.nc", overwrite=TRUE, varname="Qair",
         longname="Qair_WFDE5_CRU_1991_day", unit="kg kg-1")
# averages are obtained at 7-day steps
rolled <- roll(single_day, n=7, "sum", "around", na.rm=TRUE)

week_order <- isoweek(ymd(str_split(time(single_day)," ",simplify = T)[,1]))
week_order[which(week_order == 1)[which(week_order == 1)>7]] = max(week_order)+1

single_week = tapp(rolled, index=week_order, fun=sum,cores=6)
names(single_week) <- unique(week_order)
writeCDF(single_week, filename="D:/Climate data/1991/Qair_single_week.nc", overwrite=TRUE, varname="Qair",
         longname="Qair_WFDE5_CRU_1991_week", unit="kg kg-1")

#Transfer to 0.5 degree
tif <- rast("D:/Climate data/1991/yield_1991.tif")
r <- tif
res(r) <- 0.5
r <- resample(tif, r)

#Mask the area with tif
r[r==0]=NA
r[!is.na(r)]=1
single_week = r*single_week
single_week_crop = crop(single_week,r)
single_week_mask = mask(single_week,r)
names(single_week_mask) = paste0("week_",unique(week_order))
writeCDF(single_week_mask, filename="D:/Climate data/1991/Qair_single_week_mask.nc", overwrite=TRUE, 
         varname="Qair", longname="Qair_WFDE5_CRU_1991_week", unit="kg kg-1")

#Import PSurf data
data <- rast(list.files("D:/Climate data/1991/PSurf", pattern=".nc",full.names = T))
#Summarized for the current day's data
hour2day_label <-  yday(ymd(str_split(time(data)," ",simplify = T)[,1]))
single_day=c()
for(i in unique(hour2day_label)){
  print(i)
  tag = which(hour2day_label == i)
  single_day <<- c(single_day,mean(data[[tag]]))
}
single_day <- do.call("c",single_day)
names(single_day) <- unique(ymd(str_split(time(data)," ",simplify = T)[,1]))
time(single_day) <- unique(ymd(str_split(time(data)," ",simplify = T)[,1]))
writeCDF(single_day, filename="D:/Climate data/1991/PSurf_single_day.nc", overwrite=TRUE, varname="PSurf",
         longname="PSurf_WFDE5_CRU_1991_day", unit="Pa")
# averages are obtained at 7-day steps
rolled <- roll(single_day, n=7, "sum", "around", na.rm=TRUE)

week_order <- isoweek(ymd(str_split(time(single_day)," ",simplify = T)[,1]))
week_order[which(week_order == 1)[which(week_order == 1)>7]] = max(week_order)+1

single_week = tapp(rolled, index=week_order, fun=sum,cores=6)
names(single_week) <- unique(week_order)
writeCDF(single_week, filename="D:/Climate data/1991/PSurf_single_week.nc", overwrite=TRUE, varname="PSurf",
         longname="PSurf_WFDE5_CRU_1991_week", unit="Pa")

#Transfer to 0.5 degree
tif <- rast("D:/Climate data/1991/yield_1991.tif")
r <- tif
res(r) <- 0.5
r <- resample(tif, r)

#Mask the area with tif
r[r==0]=NA
r[!is.na(r)]=1
single_week = r*single_week
single_week_crop = crop(single_week,r)
single_week_mask = mask(single_week,r)
names(single_week_mask) = paste0("week_",unique(week_order))
writeCDF(single_week_mask, filename="D:/Climate data/1991/PSurf_single_week_mask.nc", overwrite=TRUE, 
         varname="PSurf", longname="PSurf_WFDE5_CRU_1991_week", unit="Pa")

##########-------------------------------------------------------------------------
##Run the PC model designed by Qiao Shengchao(qsc17@mails.tsinghua.edu.cn) and WANG Han
##########-------------------------------------------------------------------------
##===================================
## define functions
##===================================

# $1. calculate air pressure in Pa
cal_patm <- function( elv ){
  #-----------------------------------------------------------------------
  # Input:    - elevation, m (elv)
  # Output:   - float, atmospheric pressure at elevation 'elv', Pa (patm)
  # Features: Returns the atmospheric pressure as a function of elevation
  #           and standard atmosphere (1013.25 hPa)
  # Depends:  - connect_sql
  #           - flux_to_grid
  #           - get_data_point
  #           - get_msvidx
  # Ref:      Allen et al. (1998)
  #-----------------------------------------------------------------------
  
  # Define constants:
  kPo <- 101325   # standard atmosphere, Pa (Allen, 1973)
  kTo <- 298.15   # base temperature, K (Prentice, unpublished)
  kL <- 0.0065    # temperature lapse rate, K/m (Allen, 1973)
  kG <- 9.80665   # gravitational acceleration, m/s^2 (Allen, 1973)
  kR <- 8.3143    # universal gas constant, J/mol/K (Allen, 1973)
  kMa <- 0.028963 # molecular weight of dry air, kg/mol (Tsilingiris, 2008)
  
  # Convert elevation to pressure, Pa:
  patm <- kPo*(1.0 - kL*elv/kTo)**(kG*kMa/(kR*kL))
  
  return (patm)
}

# $2. calculate K (MM coefficient of Rubisco) in Pa
cal_k <- function(temp, patm) {
  #-----------------------------------------------------------------------
  # Input:    - float, air temperature, deg C (temp)
  #           - float, atmospheric pressure, Pa (patm)
  # Output:   float, Pa (mmk)
  # Features: Returns the temperature & pressure dependent Michaelis-Menten
  #           coefficient, K (Pa).
  # Ref:      Bernacchi et al. (2001), Improved temperature response 
  #           functions for models of Rubisco-limited photosynthesis, 
  #           Plant, Cell and Environment, 24, 253--259.
  #-----------------------------------------------------------------------
  
  # Define constants
  kc25 <- 39.97      # Pa, assuming 25 deg C & 98.716 kPa
  ko25 <- 2.748e4    # Pa, assuming 25 deg C & 98.716 kPa
  dhac <- 79430      # J/mol
  dhao <- 36380      # J/mol
  kR   <- 8.3145     # J/mol/K
  kco  <- 2.09476e5  # ppm, US Standard Atmosphere
  
  vc <- kc25*exp(dhac*(temp - 25.0)/(298.15*kR*(temp + 273.15)))
  vo <- ko25*exp(dhao*(temp - 25.0)/(298.15*kR*(temp + 273.15)))
  k  <- vc*(1 + kco*(1e-6)*patm/vo)
  
  return(k)
  
}

# $3. calculate Gstar (CO2 compensation point) in Pa
cal_gstar_gepisat <- function( temp ) {
  #-----------------------------------------------------------------------
  # Input:    float, air temperature, degrees C (tc)
  # Output:   float, gamma-star, Pa (gs)
  # Features: Returns the temperature-dependent photorespiratory 
  #           compensation point, Gamma star (Pascals), based on constants 
  #           derived from Bernacchi et al. (2001) study.
  # Ref:      Bernacchi et al. (2001), Improved temperature response 
  #           functions for models of Rubisco-limited photosynthesis, 
  #           Plant, Cell and Environment, 24, 253--259.
  #-----------------------------------------------------------------------
  
  # Define constants
  gs25 <- 4.220    # Pa, assuming 25 deg C & 98.716 kPa)
  dha  <- 37830    # J/mol
  kR   <- 8.3145   # J/mol/K
  
  gs <- gs25 * exp( dha * ( temp - 25.0 ) / ( 298.15 * kR * ( temp + 273.15 ) ) )
  
  return( gs )
  
}

# $4. conver CO2 from ppm to Pa
cal_co2_to_ca <- function( co2, patm ){
  #-----------------------------------------------------------------------
  # Input:    - float, annual atm. CO2, ppm (co2)
  #           - float, monthly atm. pressure, Pa (patm)
  # Output:   - ca in units of Pa
  # Features: Converts ca (ambient CO2) from ppm to Pa.
  #-----------------------------------------------------------------------
  
  ca   <- ( 1.e-6 ) * co2 * patm         # Pa, atms. CO2
  return( ca )
  
}

# $5. calculate the fraction of absorbed PAR
cal_fapar <- function(LAI){
  #-----------------------------------------------------------------------
  ## Input:
  # LAI: leaf area index, dimensionless
  
  ## Output:
  # fAPAR: the fraction of the absorbed PAR to the incident PAR, dimensionless
  
  ## Features: calculate the fraction of absorbed PAR based on given LAI.
  #-----------------------------------------------------------------------
  
  fapar <- 1-exp(-0.5*LAI)
  
  return(fapar)
  
}

# $6. calculate vapour pressure deficit
cal_vpd <- function(RH,Ta){
  #-----------------------------------------------------------------------
  ## Input:
  # RH: relative humidity, percentage
  # Ta: air temperature, degree C
  
  ## Output:
  # VPD: vapour pressure deficit, kPa
  
  ## Features: calculate the vapour pressure defict based on given air temperature and relative humidity.
  #-----------------------------------------------------------------------
  
  VPD  <- 0.611*exp(17.502*Ta/(Ta+240.97))*(1-RH/100)
  return(VPD)
  
}

# $7. calculate the light use efficiency based on given environmental factors
cal_lue<-function(Ta,VPD,CO2,elv=NA,patm=NA){
  
  #-----------------------------------------------------------------------
  ## Input:
  # Ta: air temperature, degree C
  # PPFD: photosynthetic phtotn flux density, mol photon/m2
  # VPD: vapour pressure deficit, kPa
  # CO2: CO2 concentration, ppm
  # elv: elevation, m
  # patm: air pressure, kPa
  
  ## Output:
  # LUE: the light use efficiency
  
  ## Features: calculate the light use efficiency based on given environmental factors using Pmodel
  #-----------------------------------------------------------------------
  
  Tc.deg_C<-Ta 
  VPD.kPa  <-VPD
  CO2.ppm <- CO2 
  elv <- elv
  if (identical(NA, elv) && identical(NA, patm)) {
    rlang::abort("Aborted. Provide either elevation (arugment elv) or atmospheric pressure (argument patm).")
  }
  else if (!identical(NA, elv) && identical(NA, patm)) {
    rlang::warn("Atmospheric pressure (patm) not provided. Calculating it as a function of elevation (elv), assuming standard atmosphere (101325 Pa at sea level).")
    patm <- calc_patm(elv)
  } else {
    patm <- patm*1000 # convert kPa to Pa
  }
  
  # define constant
  beta <- 146# the ratio of cost factor b to a at reference temperature
  c <- 0.41# the cost factor of maintaining Jmax
  
  # instrinsic quantum yield, based on Cozettle et al.1998, unit: g C/ mol photon
  # Bernacchi et al. (2001)
  phi <- (0.352+0.021*Tc.deg_C-3.4*10^(-4)*(Tc.deg_C)^2)/8 # the tempereture-dependence of instrinsic quantum yield of C3
  maxQE <- phi*12 # convert unit from mol C/ mol photon to g C/ mol photon
  
  # light use efficiency
  K <- cal_k(Tc.deg_C,patm) # the effective Michaelis-Menten coefficient of Rubisco, Pa
  Gstar <- cal_gstar_gepisat(Tc.deg_C) # photorespiratory compensation point, Pa
  f1 <- exp(-0.0227*(Tc.deg_C-25)) # the viscosity of water relative to its value at 25˚C
  ca <- cal_co2_to_ca(CO2.ppm,patm) # ambient CO2 partical pressure, Pa
  
  m <- (ca - Gstar)/(ca + 2*Gstar + 3*Gstar*sqrt(1.6*VPD.kPa*1000*f1/(K + Gstar)/(beta)))
  M <- m*sqrt(1-(c/m)^(2/3))
  
  LUE <- M*maxQE # LUE controled by Vcmax, Jmax and instrinsic quantum yield
  
  return(LUE)
  
}

# $8. calculate gross primary productivity based on given environmental factors 
cal_gpp<-function(fAPAR,Ta,PPFD,VPD,CO2,elv=NA,patm=NA){
  #-----------------------------------------------------------------------
  ## Input:
  # fAPAR: the fraction of absorbed PAR to the incident PAR, dimensionless
  # Ta: air temperature, degree C
  # PPFD: photosynthetic phtotn flux density, mol photon/m2
  # VPD: vapour pressure deficit, kPa
  # CO2: CO2 concentration, ppm
  # elv: elevation, m
  # patm: air pressure, kPa
  
  ## Output:
  # GPP: gross primary productivity, g C/m2
  
  ## Features: calculate gross primary productivity
  #-----------------------------------------------------------------------
  
  LUE <- cal_lue(Ta = Ta,VPD = VPD,CO2 = CO2,elv = elv,patm = patm)
  Iabs <- fAPAR*PPFD
  GPP <- LUE*Iabs
  
  return(GPP)
}

# $9. calculate aboveground biomass
cal_ab<-function(GPP_ac,alpha){
  #-----------------------------------------------------------------------
  ## Input:
  # GPP_ac: accumulated gross primary productivity over growing season, g C/m2
  # alpha: moisture index, the ratio of AET to EET, dimensionless
  
  ## Output:
  # AB: aboveground biomass when harvest, g mass/ m2
  
  ## Features: calculate the aboveground biomass based on given alpha and GPP_ac
  #-----------------------------------------------------------------------
  
  # define constant
  f_tb <- 0.47 # the ration of total biomass to GPP
  f_C_to_mass <- 2.5 # the conversion coefficient from g C/m2 to g dry mass/m2
  f_alpha_h <- -0.05 # the sensitivity coefficient of root ratio (root biomass/ total biomass) to alpha
  
  # the root ratio when harvest
  f_root_h <- f_alpha_h*alpha+0.15
  # the total biomass, g dry mass/m2
  TB <- GPP_ac*f_tb*f_C_to_mass
  # aboveground biomass, g dry mass/m2
  AB<-TB*(1-f_root_h)
  
  return(AB)
  
}

# calculate yield based on given aboveground biomass and the amount of nitrogen
cal_yield <- function(AB, N){
  #-----------------------------------------------------------------------
  ## Input:
  # AB: aboveground biomass when harvest, g mass/ m2
  # N: the amount of applied pure nitrogen kg/ha
  
  ## Output:
  # yield: yield, g dry mass/m2
  
  ## Features: based on given aboveground biomass and the amount of nitrogen
  #-----------------------------------------------------------------------
  
  yield <- (0.32*N+1033.3)*(1-exp(-0.0008*AB))-82.9 # for site validation
  
  
  return(yield)
}

# Make a yield related diagram
order= 1
test_spdf <- as(raster(yield [[order]]), "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")
wr <- map_data("world")
n_colors <- 10
palette <- brewer.pal(n_colors, "Spectral")
ggplot() +
  geom_map(
    data = wr, map = wr,
    aes(long, lat, map_id = region),
    color = "black", fill = "white", size = 0.1
  ) +
  geom_tile(data = test_df , aes(x = x, y = y, fill = value))  +
  scale_fill_gradientn(
    colors = palette,
    limits = c(0, 12),
    breaks = seq(0, 12 , length.out = n_colors +1),
    labels = seq(0, 12 , length.out = n_colors +1),
    name = "yield(t ha-1)",
    oob = scales::squish
  ) +
  theme_bw() + 
  coord_fixed(xlim = c(-120, 150), ylim = c(-55, 70)) +
  theme(legend.key.size = unit(0.8, "cm"))

##########-------------------------------------------------------------------------
##calculate leaf area index based on environmental factors
##########-------------------------------------------------------------------------
# Author: Shengchao Qiao, Tsinghua University
# Created date: 24th August 2020
##----------------------------------------------
library('lamW')
library('geosphere') # to calculate day length (DL)

##########-------------------------------------------------------------------------
# function 1: calculate LAI_max based on environmental factors
##########-------------------------------------------------------------------------
cal_LAI_max <- function(Ta,PPFD_ac,VPD,CO2,elv=NA,patm=NA,alpha, index=NA){
  #-----------------------------------------------------------------------
  ## Input:
  # Ta: mean daily air temperature over growing season, degree C
  # PPFD_ac: total PPFD over growing season, mol photon/m2
  # VPD: mean daily vapour pressure deficit, kPa
  # CO2: annual CO2 concentration, ppm
  # elv: elevation, mm
  # patm: mean daily air pressure, kPa
  
  ## Output:
  # LAI_max: maximum LAI during growing season
  
  ##Features: calculate the maximum value of LAI during growing season based on mass-balance
  #-----------------------------------------------------------------------
  
  ## define constant
  LMA <- 35.7 # leaf mass area, g dry mass/m2
  k <- 0.5 # canopy light extinction coefficient
  f_tb <- 0.52 # the ratio of total biomass to GPP
  f_leaf <-0.5 # the ratio of leaf biomass to aboveground biomass before leaf senescent
  f_alpha_g <- -0.23 # the sensitivity coefficient of root ratio (root biomass/ total biomass) to alpha
  
  # the mean value of root ratio to total biomass before lead senescent
  f_root_g <- f_alpha_g*alpha+0.4
  # the fraction of leaf to accumulated GPP 
  eta <- f_tb*(1-f_root_g)*f_leaf
  # calculate LAI
  LUE <- cal_lue(Ta=Ta,VPD = VPD,CO2 = CO2,elv = elv,patm = patm)
  mu <- 0.5*LUE*PPFD_ac*eta/LMA
  LAI_ma <- mu+2*lambertW0(-k*mu*exp(-k*mu))
  if (is.na(index)){
    LAI_max <- LAI_ma
  } else {
    LAI_max <- min(LAI_ma,runif(1,(index-0.1),(index+0.1)))
  }
  
  
  return(LAI_max)
}


cal_LAI_w <- function(Ta,PPFD_ac,VPD,CO2,elv=NA,patm=NA,pre, index){
  
  
  Rue <- 0.9
  f_T_to_ET <- 0.7
  ET <- Rue*pre
  T <- ET*f_T_to_ET
  
  Ca <- cal_co2_to_ca(co2 = CO2,patm = patm*1000)
  gstar_Ca <- cal_gstar_gepisat(temp = Ta)/Ca
  f1 <- exp(-0.0227*(Ta-25))
  xi <-sqrt(146*(cal_k(temp = Ta,patm = patm*1000)+cal_gstar_gepisat(temp = Ta))/(1.6*f1))
  
  X <- gstar_Ca+(1-gstar_Ca)*xi/(xi+sqrt(VPD*1000))
  
  A <- 12*T*1000*Ca*(1-X)/(1.6*VPD*1000*18)
  LUE <- cal_lue(Ta=Ta,VPD = VPD,CO2=CO2,patm = patm)
  fAPAR <- index*A/(PPFD_ac*LUE)
  fAPAR <- ifelse(fAPAR>0.99,0.99,fAPAR)
  LAI_w <- log(1-fAPAR)/(-0.5)
  return(LAI_w)
}

##########-------------------------------------------------------------------------
#Plot the result
##########-------------------------------------------------------------------------
JP = rast(list.files("gdhy",full.names = T))
PC = rast(list.files("pc",full.names = T))

Tair = map(1:35,function(i){
  mean(rast(list.files("Tair",full.names = T)[i]),na.rm=T)
})
Tair = do.call("c",Tair)
CO2 = read.csv("CO2.csv")
CO2 = is.finite(Tair)*CO2$ppm
CO2[CO2==0] = NA
year = is.finite(Tair)*(1982:2016)
year[year==0] = NA

mean_values_JP <-global(JP, fun=mean,na.rm=T)
sd_values_JP <- global(JP,fun=sd,na.rm=T)
mean_values_PC <-global(PC, fun=mean,na.rm=T)

df <- data.frame(
  year = 1982:2016,
  JP = mean_values_JP,
  PC = mean_values_PC
)

df <- tidyr::gather(df, key = "raster", value = "mean_value", -year)
df$raster[df$raster == "mean"] = "JP"
df$raster[df$raster == "mean.1"] = "PC"

p1 = ggplot(df, aes(x = year, y = mean_value, color = raster)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Yield (t/ha)") +
  theme_bw() +
  guides(color = guide_legend(title = "")) +
  ylim(0,6)  +
  scale_y_continuous(breaks = seq(0, 6, by = 0.5))+
  scale_x_continuous(breaks = c(seq(1982, 2012, by = 5), 2016), limits = c(1982, 2016))+
  theme(legend.position=c(0.01,0.99), legend.justification=c(0,1),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(t = 35, unit = "pt")
        ),
        axis.title.y = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(l = 35, unit = "pt")
        ),
        plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                   size = 11,
                                   family = "serif"),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                   size = 14,
                                   family = "serif"),
        text = element_text(family = "serif"),
        legend.text = element_text(family = "serif",size = 12)#,
        #panel.grid.major.x = element_blank()
  )


df_jp <- na.omit(as.data.frame(values(JP)))
df_pc <- na.omit(as.data.frame(values(PC)))

colnames(df_jp) <- 1982:2016
colnames(df_pc) <- 1982:2016

df_jp$raster = "JP"
df_pc$raster = "PC"

df <- rbind(df_jp, df_pc)
df <- pivot_longer(df,cols = -raster ,names_to = "year",values_to = "yield")
df <- df %>% 
  group_by(raster,year) %>% 
  summarise(  y10 = quantile(yield, 0.1),
              y25 = quantile(yield, 0.25),
              y50 = median(yield),
              y75 = quantile(yield, 0.75),
              y90 = quantile(yield, 0.9))

p2 = ggplot(df, aes(x=factor(year), fill=factor(raster))) + 
  geom_boxplot(aes(ymin = y10, lower = y25, middle = y50, upper = y75, ymax = y90),stat = "identity",alpha=0.3,na.rm = T) +
  #geom_errorbar(aes(ymin = y10, ymax = y90), width = 0.2) +
  theme_bw() +
  guides(fill = guide_legend(title = "")) +
  theme(legend.position=c(0.01,0.99), legend.justification=c(0,1),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(t = 12, unit = "pt")
        ),
        axis.title.y = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(l = 35, unit = "pt")
        ),
        plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                   size = 11,
                                   family = "serif"),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                   size = 14,
                                   family = "serif"),
        text = element_text(family = "serif"),
        legend.text = element_text(family = "serif",size = 12)
  ) +
  scale_fill_brewer(palette="Dark2")  + 
  xlab("Year") +
  ylab("Yield (t/ha)") +
  scale_y_continuous(limits = c(0 , 10),
                     breaks = seq(0, 10, by = 1), 
                     labels = seq(0, 10, by = 1),
                     expand = c(0,0)) 


p3 = p2+ 
  geom_rect(aes(xmin = 1.5, xmax = 2.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 3.5, xmax = 4.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 5.5, xmax = 6.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 9.5, xmax = 10.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 11.5, xmax = 12.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 13.5, xmax = 14.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 15.5, xmax = 16.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 17.5, xmax = 18.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 19.5, xmax = 20.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01)

#"JP~Tair"
sr <- sds(JP,Tair) 
pearson1 <- lapp(sr, \(x,y) {
  out <- rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    out[i] <- cor(x[i,], y[i,], "pearson", use="everything")
  }
  out
})

#"JP~CO2"
sr <- sds(JP,CO2) 
pearson2 <- lapp(sr, \(x,y) {
  out <- rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    out[i] <- cor(x[i,], y[i,], "pearson", use="everything")
  }
  out
})
#"PC~Tair"
sr <- sds(PC,Tair) 
pearson4 <- lapp(sr, \(x,y) {
  out <- rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    out[i] <- cor(x[i,], y[i,], "pearson", use="everything")
  }
  out
})

#"PC~CO2"
sr <- sds(PC,CO2) 
pearson5 <- lapp(sr, \(x,y) {
  out <- rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    out[i] <- cor(x[i,], y[i,], "pearson", use="everything")
  }
  out
})

WorldSHP=terra::vect(spData::world)

layout(matrix(c(1, 2, 3, 4, 5, 6,7,7), nrow = 4, ncol = 2, byrow = TRUE))
layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2, byrow = TRUE))
plot(pearson1, 
     col=cet_pal(20, name = "r2") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("JP~Tair", line = 2.5, cex = 1.5),
     breaks = seq(-1,1,by=0.1), 
     legend = F,
     type= "continuous",
     loc.main = "bottomright"
)
plot(pearson4, 
     col=cet_pal(20, name = "r2") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("PC~Tair", line = 2.5, cex = 1.5),
     breaks = seq(-1,1,by=0.1), 
     legend = F,
     type= "continuous",
     loc.main = "bottomright"
)

speed1 <- ((JP$yield_2014 - JP$yield_1982)/JP$yield_1982) #*100
speed2 <- ((PC[[35]] - PC[[1]])/ PC[[1]]) #*100
m <- c(-Inf, -0.5, 1,
       -0.5,  0.5, 2,
       0.5,1.5,3,
       1.5,Inf,4)   
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(speed1, rclmat, include.lowest=TRUE)
rc2 <- classify(speed2, rclmat, include.lowest=TRUE)

reclassified1  = as.factor(rc1)
reclassified2  = as.factor(rc2)

tar_df <- data.frame(ID = 1:4, label = c("Limited", "Marginal", "Moderate", "Optimal"))
levels(reclassified1) <- list(tar_df)
levels(reclassified2) <- list(tar_df)

tar<-levels(reclassified1)[[1]]
tar[["yield_2016"]]<-c("Limited","Marginal","Moderate","Optimal")
levels(reclassified1)<-tar

tar<-levels(reclassified2)[[1]]
tar[["sum"]]<-c("Limited","Marginal","Moderate","Optimal")
levels(reclassified2)<-tar


par(mfrow=c(1,1))
plot(reclassified1, 
     col=cet_pal(4, name = "r2") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("JP", line = 2.5, cex = 1.5),
     legend = F,
     loc.main = "bottomright"
)
plot(reclassified2, 
     col=cet_pal(4, name = "r2") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("PC", line = 2.5, cex = 1.5),
     #plg=list( loc="bottomright", cex=1.2),
     loc.main = "bottomright",
)

writeRaster(reclassified1,"reclassified1.tif")
writeRaster(reclassified2,"reclassified2.tif")
df_rc1 <- as.data.frame(values(reclassified1))
df_rc2 <- as.data.frame(values(reclassified2))

df_jp <- as.data.frame(values(JP))
df_pc <- as.data.frame(values(PC))

colnames(df_jp) <- 1982:2016
colnames(df_pc) <- 1982:2016


df_jp$raster = "JP"
df_pc$raster = "PC"
df_jp$rc = df_rc1$yield_2014
df_pc$rc = df_rc2$sum

df <- na.omit(rbind(df_jp, df_pc))
df <- pivot_longer(df,cols = -c(raster,rc) ,names_to = "year",values_to = "yield")
df <- df[,-3] %>% 
  group_by(raster,rc) %>% 
  summarise(  y10 = quantile(yield, 0.1),
              y25 = quantile(yield, 0.25),
              y50 = median(yield),
              y75 = quantile(yield, 0.75),
              y90 = quantile(yield, 0.9))

df$rc = factor(df$rc)
levels(df$rc) = c("Limited","Marginal","Moderate","Optimal")

p2 = ggplot(df, aes(x=factor(rc), fill=factor(raster))) + 
  geom_boxplot(aes(ymin = y10, lower = y25, middle = y50, upper = y75, ymax = y90),stat = "identity",alpha=0.3,na.rm = T,width=0.5) +
  stat_boxplot(geom = "errorbar",aes(ymin = y10, ymax = y90), width = 0.35) +
  theme_bw() +
  guides(fill = guide_legend(title = "")) +
  theme(legend.position=c(0.01,0.99), legend.justification=c(0,1),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(t = 20, unit = "pt")
        ),
        axis.title.y = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(l = 20, unit = "pt")
        ),
        plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                   size = 14,
                                   family = "serif"),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                   size = 14,
                                   family = "serif"),
        text = element_text(family = "serif"),
        legend.text = element_text(family = "serif",size = 12)
  ) +
  scale_fill_brewer(palette="Dark2")  + 
  xlab("Type") +
  ylab("Yield (t/ha)") +
  #labs(fill = "Land Use Type") +
  scale_y_continuous(limits = c(0 , 10),
                     breaks = seq(0, 10, by = 1), 
                     labels = seq(0, 10, by = 1),
                     expand = c(0,0))
p3 = p2+ 
  geom_rect(aes(xmin = 1.5, xmax = 2.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 3.5, xmax = 4.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) 
m <- c(-Inf, 10, 1,
       10,  15, 2,
       15,Inf,3)   
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc_Tair <- classify(Tair[[1]]-273.15, rclmat, include.lowest=TRUE)

df_Tair = as.data.frame(values(rc_Tair))


df_speed1 <- as.data.frame(values(speed1))
df_speed2 <- as.data.frame(values(speed2))
colnames(df_speed1) = "speed"
colnames(df_speed2) = "speed"

df_speed1$raster = "JP"
df_speed2$raster = "PC"

df_speed1$Tair = df_Tair$mean
df_speed2$Tair = df_Tair$mean

df <- na.omit(rbind(df_speed1, df_speed2))

df <- df %>% 
  group_by(raster,Tair) %>% 
  summarise(  y10 = quantile(speed, 0.1),
              y25 = quantile(speed, 0.25),
              y50 = median(speed),
              y75 = quantile(speed, 0.75),
              y90 = quantile(speed, 0.9))

df$Tair = factor(df$Tair)
levels(df$Tair) = c("<10","10-15",">15")

p2 = ggplot(df, aes(x=factor(Tair), fill=factor(raster))) + 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  geom_boxplot(aes(ymin = y10, lower = y25, middle = y50, upper = y75, ymax = y90),stat = "identity",alpha=0.3,na.rm = T,width=0.5) +
  stat_boxplot(geom = "errorbar",aes(ymin = y10, ymax = y90), width = 0.35) +
  theme_bw() +
  guides(fill = guide_legend(title = "")) +
  theme(legend.position=c(0.01,0.99), legend.justification=c(0,1),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(t = 20, unit = "pt")
        ),
        axis.title.y = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(l = 20, unit = "pt")
        ),
        plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                   size = 14,
                                   family = "serif"),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                   size = 14,
                                   family = "serif"),
        text = element_text(family = "serif"),
        legend.text = element_text(family = "serif",size = 12),
        panel.grid = element_blank()
  ) +
  scale_fill_brewer(palette="Dark2")  + 
  xlab("Temperature in baseline (℃)") +
  ylab("Yield CV change (%)") +
  #labs(fill = "Land Use Type") +
  scale_y_continuous(limits = c(-1 , 1),
                     breaks = seq(-1, 1, by = 10), 
                     labels = seq(-1, 1, by = 10),
                     expand = c(0,0)) 
speed_Tair = ( (Tair[[20]]-273.15) - (Tair[[1]]-273.15))/ (Tair[[1]]-273.15) /10
df = na.omit( cbind(as.data.frame(values(speed1)),as.data.frame(values(speed_Tair))) )
percentile_low <- apply(df, 2, quantile, probs = 0.01)
percentile_high <- apply(df, 2, quantile, probs = 0.99)

df_filtered <- df
for (col in names(df)) {
  df_filtered <- subset(df_filtered, df_filtered[, col] >= percentile_low[col] & df_filtered[, col] <= percentile_high[col])
}

colnames(df_filtered) = c("y","x")

lm_model <- lm(y ~ x, data = df_filtered )
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coefficients[2, 4]
a = round(summary(lm_model)$coefficients[2,1],3)
b = round(summary(lm_model)$coefficients[1,1],2)
p_value

p3 =ggplot(df_filtered , aes(x = x, y = y)) +
  geom_point(color = "royalblue", alpha = 0.4) +
  labs(x = "Yield CV change (%)", y = "Tair mean change (%)") +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) + 
  geom_smooth(aes(group = 1),method = "lm", se = T, color = "blue")+
  annotate(
    "text", x = -55, y = 320, label = paste0("y = ",a,"x+",b,"\n (p < 0.001) "), 
    hjust = 1, vjust = 1, size = 5
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_bw() +
  theme(
    axis.title.x = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(t = 20, unit = "pt")
    ),
    axis.title.y = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(l = 20, unit = "pt")
    ),
    plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                               size = 11,
                               family = "serif"),
    axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                               size = 14,
                               family = "serif"),
    text = element_text(family = "serif"),
    legend.text = element_text(family = "serif",size = 12),
    panel.grid = element_blank()
  )


