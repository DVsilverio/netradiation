# script para calcular radiacao global
library(doBy)
library(raster)
library(insol)
library(reshape)
#--------------------------------##
# funcao para calcular a radicao para o dia de acordo o dia do ano
energy.d = function(x, dia.jul){
#  dia.jul = 2455923
  dayl=daylength(x[7],x[8],dia.jul,-3)
  dayseq=seq(dayl[1],dayl[2],1)
  dayseq2 <- paste(floor(dayseq), round((dayseq-floor(dayseq))*60), sep=":")
  m=as.POSIXct(strptime(paste(data,dayseq2),format="%Y%j %H:%M"))
  
  deltat=1
  jdd = JD(m)
  sv = sunvector(jdd,x[6],x[7],-3)
  zenith=sunpos(sv)[,2]
  cos_inc_sfc=as.numeric(sv%*%as.vector(normalvector(0,0))) ## or sum(sunv*normalvector(0,0))
  cos_inc_sfc[cos_inc_sfc<0]=0
  ins.res = insolation(zenith = zenith,
                       jd=jdd, 
                       height = x[1],
                       visibility = x[2],
                       RH = x[3],
                       tempK = x[4],
                       O3 = x[5],
                       alphag = x[6])
  In  = ins.res[,'In']*as.numeric(cos_inc_sfc)*3600*deltat
  Id  = ins.res[,'Id']*3600*deltat
  sum((In*1e-6)+(Id*1e-6),na.rm=T)}


# teste para ver se faz sentido
energy.d(x = c(100, 300, 50, 300, 0.1, .40, -13, -55), dia.jul = 2455923)

#--------------------------------##
setwd("D:\\Dropbox (DadosTanguro)\\ArcView_Shp\\rnet_test")
dir()
# albedo
al=raster("MCD43A3.A2011361.005.Albedo_Bluesky_shortwave.grd")
#--------------------------------##
# Ozone
oz=raster("MOD08_E3.A2011361.Total_Ozone_Mean.grd")*0.00001

#--------------------------------##
# Aerozol optical deep
aod=raster("MOD08_E3.A2011361.Optical_Depth_Land.tif.grd")

aod[is.na(aod)]<-mean(getValues(aod),na.rm=T)
aod[aod<0]<-0
#Roman et al 2010. Remote Sensing of Environment 114:738–760
# # ((V=ln(50)/(0.01+AOD))
f.visibility=function(x){x=log(50)/(0.01+x);return(x)} #;x[x>60]=60;x[x<20]=20;return(x)} 
vis<-calc(aod,fun=f.visibility)

#--------------------------------##
# air - temp (in Kevin)
at=raster("AIR.TEMP.INMET.2011361.grd")+273.15
at[is.na(at)]<-mean(getValues(at),na.rm=T)
#-------------------------------##
# umidade relativa
ur=raster("UR.INMET.2011361.grd")
ur[is.na(ur)]<-mean(getValues(ur),na.rm=T)
#-------------------------------##
# altitude
altx=raster("BRA_msk_alt.grd")
alt=crop(projectRaster(altx,ur),ur)

#--------------------------------##
# CRIA RASTER COM LAT E LONG
library(sp)
library(rgdal)
newproj = CRS("+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 ")
xy = SpatialPoints(xyFromCell(al,1:ncell(al)),newproj)
latlon <- CRS("+proj=longlat +datum=WGS84")
xy2=spTransform(xy, latlon)
LAT=al
LON=al
LAT[]=coordinates(xy2)[,2]
LON[]=coordinates(xy2)[,1]
plot(LON)
plot(LAT)
#--------------------------------##
# junta todos os rasters
swr=stack(alt,vis,ur,at,oz,al,LAT,LON)

###--------------###
setwd("D:\\Dropbox (DadosTanguro)\\ArcView_Shp\\tanguro")
dir()
tang=readOGR(".","Tanguro_limites")
tang2=spTransform(tang,newproj)
swr2=crop(swr,tang2)


###--------------###
dia.jul = 2455923
radtang=calc(swr2,fun=function(x)energy.d(x, dia.jul))

plot(radtang)
plot(stack(swr2,radtang))



