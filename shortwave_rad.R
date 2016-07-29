
# function to estimate shortwave radiation based on remote sensing products
# output is a sum of direct and in and indirect radiation, see more in "insol::insolation"
# this function shoulb be used inside a calc function of raster package
# the imput "x" is a raster stack and "dia.jul"  is the julian day
# x=raster::stack(alt,vis,ur,at,oz,al,LAT,LON)
# alt=altitude,vis=visibility in km, ur = relative humidity
# at = air temperature in kelvin, oz= ozone, al = surface albedo, LAT =latitude, LON = longitude
energy.d = function(x, dia.jul){
  
  require(insol)
  
  dayl=daylength(x[7],x[8],dia.jul,-3)
  dayseq=seq(dayl[1],dayl[2],1)
  dayseq2 <- paste(floor(dayseq), round((dayseq-floor(dayseq))*60), sep=":")
  m=as.POSIXct(strptime(paste(data,dayseq2),format="%Y%j %H:%M"))
  
  deltat=1
  jdd = JD(m)
  sv = sunvector(jdd,x[7],x[8],-3)
  zenith=sunpos(sv)[,2]
  cos_inc_sfc=as.numeric(sv%*%as.vector(normalvector(0,0)))
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





# function to download modis08 .hdf files
#pathlocal = directory on your local computer
#pathMOD08<- ftp link
#yearb=2000
#yearf=2016
#pathlocal<-"/mnt/data/dados_publicos/Documents/MODIS_local/AMAZONIA/DADOS_BRUTOS/MOD08E3"
#pathM<- "ftp://ladsweb.nascom.nasa.gov/allData/6/MOD08_E3/"
#yearb=2000
#yearf=2016


download.mod08<-function(pathlocal,pahtMOD08,yearb,yearf){  
  
  # packages
  require(RCurl)
  require(rgdal)
  require(spatial.tools)
  
  # check dados no diretorio
  setwd(pathlocal)
  x1=list.files(,pattern=".hdf$");x1
  x2<-data.frame(hdf=x1,year=substr(x1,10,14),n=1)
  #summaryBy(n~year,x2,FUN=sum)
  
  
  options(download.file.method="auto")
  # get the list of directories (thanks to Barry Rowlingson):
  #items <- strsplit(getURL(MOD11A2), "\n")[[1]]
  MOD.X=list()
  year=yeari:yearf
  
  for(i in 1:length(year)){
    MOD <- paste(pathMOD08,year[i],"/",sep="")
    MOD.X[i]<-MOD
  }
  
  
  for(m in 1:length(MOD.X)){
    cat("Working with year ",year[m],"\n")
    
    items <- strsplit(getURL(MOD.X[m]), "\n")[[1]]
    
    # get the last word of any lines that start with 'd':
    folderLines <- items[substr(items, 1, 1)=='d']
    # get the directory names and create a new data frame:
    dirs <- unlist(lapply(strsplit(folderLines, " "), function(x){x[length(x)]}))
    dates <- data.frame(dirname=unlist(strsplit(dirs, "\r")))
    
    # get the list of *.hdf files:
    dates$BLOCK1 <- rep(NA, length(dates$dirname))
    
    dias.off=vector()
    #--------------------------------------------
    START = 1#start from where the for loop broke
    
    for (i in START:length(items)){
      
      cat("dealing with date nr ",i,format(Sys.time()),"\n")
      
      getlist <- strsplit(getURL(paste(MOD.X[m], dates$dirname[[i]], "/", sep=""), .opts=curlOptions(ftplistonly=TRUE)), "\r\n")[[1]]
      
      BLOCK1 <- gsub("\n","",getlist[grep(getlist, pattern="MOD08_E3.*.006.*.hdf")[1]])
      
      cat(BLOCK1,"\n")
      
      if(is.element(BLOCK1,x1)==FALSE){
        if(is.na(BLOCK1)==FALSE){
          #download.file(paste(MOD.X[m], dates$dirname[[i]], "/", BLOCK1,sep=""), destfile=paste(tempd, "/",BLOCK1, sep=""), mode='wb', method='auto', quiet=T, cacheOK=FALSE)
          download.file(paste(MOD.X[m], dates$dirname[[i]], "/", BLOCK1,sep=""), destfile=paste(tempd, "/",BLOCK1, sep=""), method='wget', quiet=T)
        }else{warning(paste(MOD.X[m],dates$dirname[[i]],"NA",sep="-"))
        }}else{warning(paste(BLOCK1,"was downloaded already",sep=" -> "))
        }  
      
      Sys.sleep(10)
      
    } # fecha loop =um ano
    
  }
} # fecha loop geral
