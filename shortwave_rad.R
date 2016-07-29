energy.d = function(x, dia.jul){
  dayl=daylength(x[7],x[8],dia.jul,-3)
  dayseq=seq(dayl[1],dayl[2],1)
  dayseq2 <- paste(floor(dayseq), round((dayseq-floor(dayseq))*60), sep=":")
  m=as.POSIXct(strptime(paste(data,dayseq2),format="%Y%j %H:%M"))
  
  deltat=1
  jdd = JD(m)
  sv = sunvector(jdd,x[6],x[7],-3)
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
