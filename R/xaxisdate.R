xaxisdate <-  function(dayvec
                       , type = NA
                       , formatd = "day"
                       , ydata = NA
                       , pch = 20
                       , cex = 1
                       , coltypep="red"
                       , las = 2
                       , tz = "UTC"){

  dayvec <- as.POSIXct(dayvec, tz = tz, origin = "1970-01-01")
  if(is.na(type)) type <- "n"

  dayvecc <- as.Date(dayvec, tz = tz)
  datep <- c()
  datepp <- unique(dayvecc)

  for(i in 1:length(datepp)){
    datep[i] <- which(dayvecc==datepp[i])[1]
  }

  if(formatd=="cw"){
    datepp <- lubridate::week(datepp)
    datep <- datep[which(!duplicated(datepp))]
    datepp <- datepp[which(!duplicated(datepp))]
  }

  if(formatd=="time"){
    datepp <- dayvec
    datep <- which( !duplicated( lubridate::hour( dayvec )))
    datepp <- datepp[which( !duplicated( lubridate::hour( dayvec )))]
    datepp <- substr(datepp, 12, 16)
  }

  if(formatd=="day"){
    datepp <- datepp
    datepp <- paste(lubridate::month(datepp, label = T, abbr = T), lubridate::day(datepp), sep = "-")
  }

  if(formatd=="year-month"){
    datepp <- strftime(datepp,format="%y-%m", tz = tz)
    datep <- datep[which(!duplicated(datepp))]
    datepp <- datepp[which(!duplicated(datepp))]
  }

  padj <- 0.5
  axis(1,at=datep,datepp, padj = padj, las = las, cex.axis = cex)
  axis(2)

  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],lwd=1.2)
  if(type=="l") abline(v=datep,lty=3)
  if(type=="p") points(datep,ydata[datep],pch=pch,col=coltypep,cex=cex)
  box()
}
