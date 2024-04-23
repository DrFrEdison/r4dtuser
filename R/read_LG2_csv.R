# LG2 read and merge csv files ####
read.csv.LG2 <- function(customer
                         , location
                         , line
                         , firstday
                         , lastday

                         , dt_Mixernummer = dt_Mixernummer

                         , MixerNumber = NA
                         , DTProductNumber = NA

                         , Produkt_fliesst
                         , Ringkessel = Produkt_fliesst
                         , typeof = c("ref","drk","spc")

                         , export_directory = "C://csvtemp"
                         , slim = T
                         , return.R = F
                         , export = T
                         , dir_wd = dir_wd){

  # create export dir, emtpy lists and setwd ####
  if(!is.na(export_directory)) dir.create(export_directory,showWarnings = F)

  # Abgespeckte Variante vom Dateiexport
  if(slim == T) coltoremove <- c("score", "Lamdba.Start", "Lambda.Ende", "Lambda.Delta", "leverages", "XValSamp", "Scores", "Produktprofil", "stopp", "Ring")

  # Vereinheitlichung von Ringkessel und Produkt fliesst
  if( !is.na( Produkt_fliesst )) Ringkessel <- Produkt_fliesst
  if( is.na( Ringkessel )) Ringkessel <- Produkt_fliesst

  # Leere Liste zum einlesen der .csv Dateien für LG2 ####
  read <- list()

  # Momentaner Pfad und Pfad für LG2-Dateien ####
  read$wd.o <- getwd()
  read$wd <- service_backup_path(customer, location, line, dir_wd = dir_wd)
  setwd(read$wd)

  # Produktnummern ####
  dt_Mixernummer <- data.frame(dt_Mixernummer)
  dt_Mixernummer <- dt_Mixernummer[dt_Mixernummer$customer == customer,]
  dt_Mixernummer <- dt_Mixernummer[dt_Mixernummer$location == location,]

  # Mehr als eine Linie für Produktnamen?
  if( any(!is.na( unique(dt_Mixernummer$line) )))   dt_Mixernummer <- dt_Mixernummer[dt_Mixernummer$line == line,]

  # Nur DTProduktnummer ####
  if( !is.na(DTProductNumber) & all(is.na(MixerNumber))){
    MixerNumber <- c(dt_Mixernummer$Mixer_ID_1[ dt_Mixernummer$DTProductNumber %in% DTProductNumber ]
                     , dt_Mixernummer$Mixer_ID_2[ dt_Mixernummer$DTProductNumber %in% DTProductNumber ]
                     , dt_Mixernummer$Mixer_ID_3[ dt_Mixernummer$DTProductNumber %in% DTProductNumber ])
    MixerNumber <- MixerNumber[ MixerNumber > 0 ]
  }

  # MixerNumber nicht %in% DTProductNumber ####
  if( !any( c(dt_Mixernummer$DTProductNumber[ dt_Mixernummer$Mixer_ID_1 %in% MixerNumber ]
              , dt_Mixernummer$DTProductNumber[ dt_Mixernummer$Mixer_ID_2 %in% MixerNumber ]
              , dt_Mixernummer$DTProductNumber[ dt_Mixernummer$Mixer_ID_3 %in% MixerNumber ]) %in% DTProductNumber)){

    MixerNumber <- c(MixerNumber
                     , dt_Mixernummer$Mixer_ID_1[ dt_Mixernummer$DTProductNumber %in% DTProductNumber ]
                     , dt_Mixernummer$Mixer_ID_2[ dt_Mixernummer$DTProductNumber %in% DTProductNumber ]
                     , dt_Mixernummer$Mixer_ID_3[ dt_Mixernummer$DTProductNumber %in% DTProductNumber ])
    MixerNumber <- MixerNumber[ MixerNumber > 0 ]

  }

  # Export-Datei-Name ####
  read$name$date1 <- gsub("-","",substr(as.character(firstday),3,nchar(as.character(firstday))))
  read$name$date2 <- gsub("-","",substr(as.character(lastday),3,nchar(as.character(lastday))))

  # Dateiname  ####
  suppressWarnings( read$name$product <- dt_Mixernummer$beverage[ unique(which(dt_Mixernummer$Mixer_ID_1==MixerNumber | dt_Mixernummer$Mixer_ID_2==MixerNumber | dt_Mixernummer$Mixer_ID_3==MixerNumber | dt_Mixernummer$DTProductNumber==DTProductNumber)) ])
  read$name$product <- paste(read$name$product, collapse = "_")

  read$name$file <- paste(read$name$date1, read$name$date2, location, line, read$name$product, paste(MixerNumber, collapse = "_"), paste0("DT", DTProductNumber), sep = "_")
  read$name$file <- gsub("__", "_", read$name$file)
  read$name$file <- gsub(" ", "_", read$name$file)
  read$name$file <- gsub("-", "_", read$name$file)
  if(all(is.na(MixerNumber)) & is.na(DTProductNumber)) read$name$file <- paste(read$name$date1, read$name$date2, location, line, sep = "_")

  if( !is.na(Produkt_fliesst) ) if( Produkt_fliesst ) read$name$file <- paste0(read$name$file, "_Produkt_fliesst")
  if( !is.na(Produkt_fliesst) ) if( !Produkt_fliesst ) read$name$file <- paste0(read$name$file, "_Produkt_fliesst_nicht")
  if( is.na(Produkt_fliesst) ) read$name$file <- paste0(read$name$file, "_Produkt_fliesst_und_fliesst_nicht")

  read$name$file <- gsub("_DTNA", "", read$name$file)

  # csv files ####
  csvfiles <- list()
  csvfiles$csv.file.names <- list() # save file names

  setwd("./CSV")
  # read csv files ####
  csvfiles$dir <- dir(pattern=".csv")
  csvfiles$prod <- gsub(".csv","",substr(csvfiles$dir,12,nchar(csvfiles$dir)))

  if(length(grep("Hand",csvfiles$prod))>0){
    csvfiles$dir <- csvfiles$dir[-grep("Hand",csvfiles$prod)]
    csvfiles$prod <- csvfiles$prod[-grep("Hand",csvfiles$prod)]
  }

  csvfiles$clear <- unlist(lapply(gregexpr("_",csvfiles$prod),function(x) x[[1]]))
  csvfiles$prod[which(csvfiles$clear>0)] <- substr(csvfiles$prod[which(csvfiles$clear>0)],1,csvfiles$clear[which(csvfiles$clear>0)]-1)

  suppressWarnings(csvfiles$dir <- csvfiles$dir[which(!is.na(as.numeric(csvfiles$prod)))])
  suppressWarnings(csvfiles$prod <- csvfiles$prod[which(!is.na(as.numeric(csvfiles$prod)))])

  csvfiles$prod <- csvfiles$prod[which(substr(csvfiles$dir,1,10)>=firstday & substr(csvfiles$dir,1,10)<=lastday)]
  csvfiles$dir <- csvfiles$dir[which(substr(csvfiles$dir,1,10)>=firstday & substr(csvfiles$dir,1,10)<=lastday)]

  if(length(which(is.na(csvfiles$dir)))>0) csvfiles$prod <- csvfiles$prod[which(!is.na(csvfiles$dir))]
  if(length(which(is.na(csvfiles$dir)))>0) csvfiles$dir <- csvfiles$dir[which(!is.na(csvfiles$dir))]

  if(!any(is.na(MixerNumber))) csvfiles$files <- csvfiles$dir[ csvfiles$prod %in% MixerNumber ] else {csvfiles$files <- csvfiles$dir}

  if(length(csvfiles$files)==0) stop(paste0("Keine Produktion von ", read$name$product," (",MixerNumber,") im ausgewählten Zeitraum"))

  csvfiles$dat <- lapply(csvfiles$files,function(x) fread(x,sep="\t",dec=",",header=F,encoding = "Latin-1",skip=1, fill = T))

  csvfiles$names <- mapply(function(x,y) scan(x,what="",sep="\t",y,quiet=T, encoding = "Latin-1")
                           , x = csvfiles$files
                           , y = lapply(csvfiles$dat,ncol)
                           , SIMPLIFY = F)

  csvfiles$names <- lapply(csvfiles$names, function(x) gsub( "\xfc", "ue", x, useBytes = T))
  csvfiles$names <- lapply(csvfiles$names, function(x) gsub( "\xe4", "ae", x, useBytes = T))

  if(!is.list(csvfiles$names)){
    csvfiles$names1 <- list()
    for(i in 1:ncol(csvfiles$names)) csvfiles$names1[[i]] <- csvfiles$names[,i]
    csvfiles$names <- csvfiles$names1
    csvfiles$names1 <- NULL
  }

  for(i in 1:length(csvfiles$dat)) colnames(csvfiles$dat[[i]]) <- csvfiles$names[[i]]

  # Ringkessel und Produkt fliesst ####
  for(i in 1:length( csvfiles$dat)){
    if( is.na( charmatch("Ringkessel", colnames( csvfiles$dat[[ i ]])))) next
    csvfiles$dat[[ i ]] <- csvfiles$dat[[ i ]][ , 1 : charmatch("Ringkessel", colnames(csvfiles$dat[[ i ]]))]
  }

  for(i in 1:length( csvfiles$dat)){
    if( is.na( charmatch("Produkt fliesst", colnames( csvfiles$dat[[ i ]])))) next
    csvfiles$dat[[ i ]] <- csvfiles$dat[[ i ]][ , 1 : charmatch("Produkt fliesst", colnames(csvfiles$dat[[ i ]]))]
  }

  csvfiles$dat <- lapply(csvfiles$dat, function( x ) x[ , Zeitstempel := as.character( Zeitstempel )])
  csvfiles$rbind <- do.call(plyr::rbind.fill,csvfiles$dat)

  csvfiles$rbind$Zeitstempel <- as.character(csvfiles$rbind$Zeitstempel)

  # spc ####
  if(any(grepl( "spc", typeof, fixed = TRUE))){

    setwd(read$wd)
    setwd("./spc")

    spc <- list()

    # list spc files
    spc$files <- dir(pattern = "_spc.csv")[which(substr(dir(pattern = "_spc.csv"), 1, 10)>=firstday & substr(dir(pattern = "_spc.csv"), 1, 10)<=lastday)]

    if(!any(is.na(MixerNumber))){spc$files <- spc$files[match(substr(csvfiles$files,1,10),substr(unlist(spc$files),1,10))]}
    spc$files <- sort(spc$files)

    spc$merge <-  suppressWarnings(lapply(spc$files,function(x) fread(x, dec = ",", sep = ";", encoding = "Latin-1", fill = T)))

    for(i in 1:length(spc$merge)){
      names(spc$merge[[i]])[grep("Produktn", names(spc$merge[[i]]))] <- "Produktnummer"
    }

    # German Acid name Problem äöüß
    for(i in 1:length(spc$merge)){
      find_acid <- grep("ure.",names(spc$merge[[i]]))
      names_acid <- names(spc$merge[[i]])[find_acid]

      if( sum(unlist(lapply(gregexpr("\\.", names_acid), min))) < 0 ){
        names_acid <- paste0("Sae", substr(names_acid, 4, nchar(names_acid)))
        names(spc$merge[[i]])[find_acid] <- names_acid
      }

      if( sum(unlist(lapply(gregexpr("\\.", names_acid), min))) > 0 ){
        names_acid1 <- substr(names_acid, 1, unlist(lapply(gregexpr("\\.", names_acid), min))-1)
        names_acid2 <- substr(names_acid, unlist(lapply(gregexpr("_", names_acid), max)), nchar(names_acid))

        names(spc$merge[[i]])[find_acid] <- gsub("SaeureSaeure", "Saeure", gsub("\\.","",paste(names_acid1, names_acid2, sep = ".")))}
    }

    # filter by MixerNumber
    if(!any(is.na(MixerNumber))) spc$merge <- lapply(spc$merge, function(x) x[x$Produktnummer %in% MixerNumber , ])

    # zerotime
    zerotime <- which(do.call(rbind,lapply(spc$merge,function(x) ifelse(length(grep("00-00-00",x[1,"time"]))==0,0,1)))==1)
    spc$merge.time <- lapply(spc$merge, function(x) as.POSIXct(as.character(x[1,"datetime"]),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        spc$merge.time[[zerotime[i]]] <- format(round(spc$merge.time[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}

    # merge
    spc$merge <- rbindlist(spc$merge, fill = T)

    if(slim == F) spc$merge$date <- as.Date(as.POSIXct(as.character(spc$merge$date),format="%Y-%m-%d",tz="UTC"))
    spc$merge$datetime <- as.character(spc$merge$datetime)

    # move wl to last columns if necessary
    if( suppressWarnings( is.na( as.numeric( gsub("X", "", names(spc$merge)[ ncol(spc$merge) ]))) ) ){
      suppressWarnings(spc$wl <- sort(as.numeric(gsub("X","",names(spc$merge)))))
      for(i in paste0("X", spc$wl)) suppressMessages(spc$merge <- spc$merge[ , moveme(names(spc$merge), paste(i, "last")), with = F])
    }

    if(length(which(!(spc$merge$datetime %in% csvfiles$rbind$Zeitstempel) == T))>0)  spc$merge <- spc$merge[ - which(!(spc$merge$datetime %in% csvfiles$rbind$Zeitstempel) == T),]
    spc$merge <- merge.data.frame(csvfiles$rbind,spc$merge,by.x="Zeitstempel",by.y="datetime")
    spc$merge <- spc$merge[colSums(!is.na(spc$merge)) > 0]

    # filter by typecode
    if( !is.na( Ringkessel )) if( length(grep( "Ringkessel", names(spc$merge)) ) != 0) spc$merge <- spc$merge[spc$merge$Ringkessel ==  Ringkessel | is.na(spc$merge$Ringkessel), ]
    if( !is.na( Produkt_fliesst )) if( length(grep( "Produkt fliesst", names(spc$merge)) ) != 0) spc$merge <- spc$merge[spc$merge$`Produkt fliesst` ==  Produkt_fliesst | is.na(spc$merge$`Produkt fliesst`), ]

    if(slim == F) spc$merge <- spc$merge[, moveme(names(spc$merge), "Zeitstempel date time first")]
    if(slim == T) spc$merge <- spc$merge[, moveme(names(spc$merge), "Zeitstempel first")]
    colnames(spc$merge)[1] <- "datetime"
    spc$merge$datetime <- as.POSIXct(as.character(spc$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")

    spc$merge <- spc$merge[order(spc$merge$datetime),]

    spc$merge <- spc$merge[which(!duplicated(spc$merge$datetime)),]
    spc$merge <- spc$merge[ , which(!names(spc$merge) == "Produktnummer.y")]

    names(spc$merge) <- gsub(".x", "", names(spc$merge))

    if(slim == T) spc$merge <- spc$merge[ , - unlist(lapply(coltoremove, function(x) grep(x, names(spc$merge))))]

    if(slim == F) spc$merge <- spc$merge[, moveme(names(spc$merge), "datetime date time first")]
    if(slim == T) spc$merge <- spc$merge[, moveme(names(spc$merge), "datetime first")]
    if(slim == T) spc$merge$date <- NULL
    if(slim == T) spc$merge$time <- NULL

    # Change col names by removing "X"
    if(!is.na(export_directory) & export){
      # write
      setwd(export_directory)

      names(spc$merge)[ names(spc$merge) %in% paste0("X", spc$wl)] <- spc$wl

      spc$merge$date <- as.character( spc$merge$date )

      fwrite(x = spc$merge, file = csvfiles$csv.file.names$spc <- paste0(read$name$file,"_spc.csv"), sep = ";", dec = ",")
      message("Produktions-Spektren exportiert")}
  }

  # Reference Spectra ####
  if(any(grepl( "ref", typeof, fixed = TRUE))){

    setwd(read$wd)
    setwd("./ref")
    ref <- list()

    # list ref files
    ref$files <- dir(pattern = "_ref.csv")[which(substr(dir(pattern = "_ref.csv"), 1, 10)>=firstday & substr(dir(pattern = "_ref.csv"), 1, 10)<=lastday)]

    # match csv and ref files by date
    if(!any(is.na(MixerNumber))){ref$files <- ref$files[match(substr(csvfiles$files,1,10),substr(unlist(ref$files),1,10))]}
    ref$files <- sort(ref$files )

    # read ref files
    ref$merge <- lapply(ref$files,function(x) fread(x, dec = ",", sep = ";"))

    # Zeitbug für 00-00-00 ####
    zerotime <- which(do.call(rbind,lapply(ref$merge,function(x) ifelse(length(grep("00-00-00",x[1,"time"]))==0,0,1)))==1)
    ref$datetime <- lapply(ref$merge, function(x) as.POSIXct(as.character(x[1,"datetime"]),format="%Y-%m-%d_%H-%M-%S",tz="UTC"))
    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        ref$datetime[[zerotime[i]]] <- format(round(ref$datetime[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}

    # merge
    ref$merge <- rbindlist(ref$merge, fill = T)

    # datetime
    ref$merge$date <- as.Date(as.POSIXct(as.character(ref$merge$date),format="%Y-%m-%d",tz="UTC"))
    ref$merge$datetime <- as.POSIXct(as.character(ref$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")

    # move wl to last columns if necessary
    if( suppressWarnings( is.na( as.numeric( gsub("X", "", names(ref$merge)[ ncol(ref$merge) ]))) ) ){
      suppressWarnings(ref$wl <- sort(as.numeric(gsub("X","",names(ref$merge)))))
      for(i in paste0("X", ref$wl)) suppressMessages(ref$merge <- ref$merge[ , moveme(names(ref$merge), paste(i, "last")), with = F])
    }

    # slim data
    if(slim == T) ref$merge <- ref$merge[ , names(ref$merge)[-unique(unlist(lapply(coltoremove, function(x) grep(x, names(ref$merge)))))], with = F]
    if(slim == T) ref$merge$date <- NULL
    if(slim == T) ref$merge$time <- NULL

    # Remove ref when it is not part of the product production ####
    if( !any(is.na(MixerNumber)) & "spc" %in% typeof){

      for(i in 1 : length( unique( as.Date( spc$merge$datetime, tz = "UTC")) )){

        ref$range$spc <- range(spc$merge$datetime[ which( as.Date( spc$merge$datetime, tz = "UTC") == unique( as.Date( spc$merge$datetime, tz = "UTC"))[ i ])]) + c(-60*15, 60*15)

        ref$merge$datetime[ max( which( ref$merge$datetime < ref$range$spc[ 1 ])) ]
        ref$merge$datetime[ min( which( ref$merge$datetime > ref$range$spc[ 1 ])) ]

        range.low <- which(ref$merge$date == unique( as.Date( spc$merge$datetime, tz = "UTC"))[ i ]
                           & ref$merge$datetime < ref$merge$datetime[ max( which( ref$merge$datetime < ref$range$spc[ 1 ])) ])
        if( length( range.low ) > 0) ref$merge <- ref$merge[ - range.low, ]
        range.high <- which(ref$merge$date == unique( as.Date( spc$merge$datetime, tz = "UTC"))[ i ]
                            & ref$merge$datetime > ref$merge$datetime[ min( which( ref$merge$datetime > ref$range$spc[ 2 ])) ])
        if( length( range.high ) > 0) ref$merge <- ref$merge[ - range.high, ]
      }
    }

    if(!is.na(export_directory) & export){
      # write
      setwd(export_directory)

      names(ref$merge)[ names(ref$merge) %in% paste0("X", ref$wl)] <- ref$wl

      ref$merge$date <- as.character( ref$merge$date )

      fwrite(x = ref$merge, file = csvfiles$csv.file.names$ref <- paste0(read$name$file,"_ref.csv"), sep = ";", dec = ",")
      message("Referenz-Spektren exportiert")}
  }

  # Dark Spectra ####
  if(any(grepl( "drk", typeof, fixed = TRUE))){

    setwd(read$wd)
    setwd("./drk")
    drk <- list()

    # list drk files
    drk$files <- dir(pattern = "_drk.csv")[which(substr(dir(pattern = "_drk.csv"), 1, 10)>=firstday & substr(dir(pattern = "_drk.csv"), 1, 10)<=lastday)]

    # match csv and drk files by date
    if(!any(is.na(MixerNumber))){drk$files <- drk$files[match(substr(csvfiles$files,1,10),substr(unlist(drk$files),1,10))]}
    drk$files <- sort(drk$files )
    # read drk files
    drk$merge <- lapply(drk$files,function(x) fread(x, dec = ",", sep = ";"))

    #zerotime
    zerotime <- which(do.call(rbind,lapply(drk$merge,function(x) ifelse(length(grep("00-00-00",x[1,"time"]))==0,0,1)))==1)
    drk$datetime <- lapply(drk$merge, function(x) as.POSIXct(as.character(x[1,"datetime"]),format="%Y-%m-%d_%H-%M-%S",tz="UTC"))
    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        drk$datetime[[zerotime[i]]] <- format(round(drk$datetime[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}

    # merge
    drk$merge <- rbindlist(drk$merge, fill = T)

    # datetime
    drk$merge$date <- as.Date(as.POSIXct(as.character(drk$merge$date),format="%Y-%m-%d",tz="UTC"))
    drk$merge$datetime <- as.POSIXct(as.character(drk$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")

    # move wl to last columns if necessary
    if( suppressWarnings( is.na( as.numeric( gsub("X", "", names(drk$merge)[ ncol(drk$merge) ]))) ) ){
      suppressWarnings(drk$wl <- sort(as.numeric(gsub("X","",names(drk$merge)))))
      for(i in paste0("X", drk$wl)) suppressMessages(drk$merge <- drk$merge[ , moveme(names(drk$merge), paste(i, "last")), with = F])
    }

    # slim data
    if(slim == T) drk$merge <- drk$merge[ , names(drk$merge)[-unique(unlist(lapply(coltoremove, function(x) grep(x, names(drk$merge)))))], with = F]
    if(slim == T) drk$merge$date <- NULL
    if(slim == T) drk$merge$time <- NULL

    # Remove drk when it is not part of the product production ####
    if( !any(is.na(MixerNumber)) & "spc" %in% typeof){

      for(i in 1 : length( unique( as.Date( spc$merge$datetime, tz = "UTC")) )){

        drk$range$spc <- range(spc$merge$datetime[ which( as.Date( spc$merge$datetime, tz = "UTC") == unique( as.Date( spc$merge$datetime, tz = "UTC"))[ i ])]) + c(-60*15, 60*15)

        drk$merge$datetime[ max( which( drk$merge$datetime < drk$range$spc[ 1 ])) ]
        drk$merge$datetime[ min( which( drk$merge$datetime > drk$range$spc[ 1 ])) ]

        range.low <- which(drk$merge$date == unique( as.Date( spc$merge$datetime, tz = "UTC"))[ i ]
                           & drk$merge$datetime < drk$merge$datetime[ max( which( drk$merge$datetime < drk$range$spc[ 1 ])) ])
        if( length( range.low ) > 0) drk$merge <- drk$merge[ - range.low, ]
        range.high <- which(drk$merge$date == unique( as.Date( spc$merge$datetime, tz = "UTC"))[ i ]
                            & drk$merge$datetime > drk$merge$datetime[ min( which( drk$merge$datetime > drk$range$spc[ 2 ])) ])
        if( length( range.high ) > 0) drk$merge <- drk$merge[ - range.high, ]
      }
    }

    if(!is.na(export_directory) & export){
      # write
      setwd(export_directory)

      names(drk$merge)[ names(drk$merge) %in% paste0("X", drk$wl)] <- drk$wl

      drk$merge$date <- as.character( drk$merge$date )

      fwrite(x = drk$merge, file = csvfiles$csv.file.names$drk <-  paste0(read$name$file,"_drk.csv"), sep = ";", dec = ",")
      message("Dunkelwert-Spektren exportiert")}
  }

  if(!is.na(export_directory) & export) setwd(export_directory)

  if(return.R){
    returnlist <- list()
    if(length(grep("spc",typeof))==1) returnlist$spc <- spc$merge
    if(length(grep("ref",typeof))==1) returnlist$ref <- ref$merge
    if(length(grep("drk",typeof))==1) returnlist$drk <- drk$merge

    returnlist$csv.file.names <- csvfiles$csv.file.names
    return(returnlist)
  }
}



