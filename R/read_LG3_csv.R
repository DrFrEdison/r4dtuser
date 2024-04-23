# read_csv_files_LG3
read.csv.LG3 <- function(firstday
                         , lastday
                         , customer
                         , location
                         , line

                         , MixerNumber = NA
                         , DTProductNumber = NA
                         , dt_Mixernummer

                         , typecode = NA

                         , export = T
                         , typeof = c("drk","ref","spc")
                         , slim = T
                         , return.R = F
                         , export_directory = "C://csvtemp"
                         , dir_wd = dir_wd){

  if(!is.na(MixerNumber[1])){if(any(MixerNumber[1] == 0) & location == "Mannheim") MixerNumber <- "NULL"}
  if(slim == T) coltoremove <- c("lightPath", "ID", "transferFunctionCoef", "location", "unit"
                                 , "measurementTypeCode", "DTproductName"
                                 , "UNSB", "Model", "Corr", "Deviation", "scores", "sT2", "sXS", "ualsL"
                                 , "statusTimestamp", "spectrumTimestamp")

  # create emtpy lists and setwd ####
  read <- list()
  read$csv.file.names <- list() # save file names

  read$wd.o <- getwd()
  read$wd <- service_backup_path(customer, location, line, dir_wd = dir_wd)
  setwd(read$wd)

  # name of export files
  read$name$date1 <- gsub("-","",substr(firstday,3,nchar(firstday)))
  read$name$date2 <- gsub("-","",substr(lastday,3,nchar(lastday)))

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

  if( !is.na(typecode) ) if( 0 %in% typecode ) read$name$file <- paste0(read$name$file, "_Produktion")
  if( !is.na(typecode) ) if( 1 %in% typecode ) read$name$file <- paste0(read$name$file, "_unbekannte_Produktion")
  if( !is.na(typecode) ) if( 2 %in% typecode ) read$name$file <- paste0(read$name$file, "_Produktionsstart")
  if( !is.na(typecode) ) if( 16 %in% typecode ) read$name$file <- paste0(read$name$file, "_Handmessung")

  read$name$file <- gsub("_DTNA", "", read$name$file)

  # get production df for each day
  read$line.product.date <- do.call(rbind, lapply(dir(pattern =  paste0(line,".csv")), read.csv2))
  names( read$line.product.date )[ 1 ] <- "date"
  read$line.product.date <- read$line.product.date[which(nchar(as.character(read$line.product.date$date)) == 10),]
  read$line.product.date <- read$line.product.date[which(read$line.product.date$date >= firstday & read$line.product.date$date <= lastday) ,]

  if(!is.na(MixerNumber[1]) & is.na(DTProductNumber)) read$line.product.date <- read$line.product.date[which(read$line.product.date$MixerNumber %in% MixerNumber),]
  if(is.na(MixerNumber[1]) & !is.na(DTProductNumber)) read$line.product.date <- read$line.product.date[which(read$line.product.date$DTProductNumber %in% DTProductNumber),]
  if(!is.na(MixerNumber[1]) & !is.na(DTProductNumber)) read$line.product.date <- read$line.product.date[ which(read$line.product.date$DTProductNumber %in% DTProductNumber | read$line.product.date$MixerNumber %in% MixerNumber), ]

  if(nrow(read$line.product.date) == 0){
    message("Keine Dateien für das Produkt gefunden")
    stop_quietly()
  }

  # spc ####
  if(any(grepl( "spc", typeof, fixed = TRUE))){
    setwd(read$wd)
    setwd("./spc")

    spc <- list()

    # list spc files
    spc$files <- dir(pattern = "_spc.csv")[which(substr(dir(pattern = "_spc.csv"), 1, 10)>=firstday & substr(dir(pattern = "_spc.csv"), 1, 10)<=lastday)]

    # list spc files
    if( any( !is.na(MixerNumber[1]), !is.na(DTProductNumber)) ){
      spc$merge <- lapply(spc$files[substr(spc$files, 1, 10) %in% read$line.product.date$date], function(x) fread(x, dec = ","))
    } else {
      spc$merge <- lapply(spc$files,function(x) fread(x, dec = ","))
    }

    # remove bug in spectrumTimestamp ####
    spc$ppp <- lapply(spc$merge, transfer_csv.num.col)

    for(i in which( unlist(  lapply(spc$merge, function( x ) is.character( x$spectrumTimestamp)) ))){

      spc$merge[[ i ]]$spectrumTimestamp[ which(spc$merge[[ i ]]$spectrumTimestamp == "NULL") ] <- NA
      spc$merge[[ i ]]$spectrumTimestamp[ which(nchar(spc$merge[[ i ]]$spectrumTimestamp) == 0) ] <- NA
      spc$merge[[ i ]]$spectrumTimestamp <- as.POSIXct( spc$merge[[ i ]]$spectrumTimestamp )

    }

    # remove bug in statusTimestamp ####
    for(i in which( unlist(  lapply(spc$merge, function( x ) is.character( x$statusTimestamp)) ))){

      spc$merge[[ i ]]$statusTimestamp[ which(spc$merge[[ i ]]$statusTimestamp == "NULL") ] <- NA
      spc$merge[[ i ]]$statusTimestamp[ which(nchar(spc$merge[[ i ]]$statusTimestamp) == 0) ] <- NA
      spc$merge[[ i ]]$statusTimestamp <- as.POSIXct( spc$merge[[ i ]]$statusTimestamp )

    }

    # merge
    spc$merge <-  rbindlist(spc$merge, fill = T)

    # filter by product
    if(!is.na(MixerNumber[1]) & is.na(DTProductNumber)){
      chooserows <- which( spc$merge$MixerNumber %in% MixerNumber)
      spc$merge <- spc$merge[chooserows,]
    }

    if(is.na(MixerNumber[1]) & !is.na(DTProductNumber)){
      chooserows <- which( spc$merge$DTproductNumber %in% DTProductNumber)
      spc$merge <- spc$merge[chooserows,]
    }

    if(!is.na(MixerNumber[1]) & !is.na(DTProductNumber)){
      chooserows <- which( spc$merge$MixerNumber %in% MixerNumber | spc$merge$DTproductNumber %in% DTProductNumber)
      spc$merge <- spc$merge[chooserows,]
    }

    # filter by typecode
    if(all( !is.na( typecode ))) if(length( which( spc$merge$measurementTypeCode == typecode)) == 0){
      message("Keine Messungen mit typecode ", typecode)
      stop_quietly()
    }

    if(all( !is.na( typecode ))) spc$merge <- spc$merge[measurementTypeCode %in% typecode,]

    if( names( spc$merge )[ 1 ] != "Day"){  # Old LG3 data type

      if(slim == F) spc$merge$date <- as.POSIXct(as.character(spc$merge$date),format="%Y-%m-%d",tz="Europe/Berlin")
      spc$merge$datetime <- as.POSIXct(as.character(spc$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")

      if(slim == F) spc$merge <- spc$merge[ , moveme(names(spc$merge), "datetime date time first"), with = F]
      if(slim == T) spc$merge <- spc$merge[ ,moveme(names(spc$merge), "datetime first"), with = F]

      if(slim == T) spc$merge <- spc$merge[ , names(spc$merge)[-unique(unlist(lapply(coltoremove, function(x) grep(x, names(spc$merge)))))], with = F]
      if(slim == T) spc$merge$date <- NULL
      if(slim == T) spc$merge$time <- NULL
    }

    # move wl to last columns if necessary
    if(is.na(as.numeric(gsub("X", "", names(spc$merge)[ncol(spc$merge)])))){
      suppressWarnings(spc$wl <- sort(as.numeric(gsub("X","",names(spc$merge)))))
      for(i in paste0("X", spc$wl)) spc$merge <- spc$merge[ , moveme(names(spc$merge), paste(i, "last")), with = F]}

    # spc$merge[ , (which(names(spc$merge) == "accumulations") + 1) : (which(names(spc$merge) == "190") - 1)] <- apply(spc$merge[ , (which(names(spc$merge) == "accumulations") + 1) : (which(names(spc$merge) == "190") - 1)], 2, as.numeric)
    spc$merge <- spc$merge[ , !sapply(spc$merge, function(x) all(is.na(x))), with = F]

    setwd(export_directory)

    if(!is.na(export_directory) & export){
      colnames( spc$merge ) <- gsub( "X", "", colnames( spc$merge ))

      spc$merge$date <- as.character(spc$merge$date)

      spc$wl <- unique(unlist( lapply(spc$ppp, function( x ) x$wl)))
      names(spc$merge)[ names(spc$merge) %in% paste0("X", spc$wl)] <- spc$wl

      fwrite(x = spc$merge, file = read$csv.file.names$spc <- paste0(read$name$file,"_spc.csv"), sep = ";", dec = ",", na = "NA", dateTimeAs = "ISO")
      message("Produktions-Spektren exportiert")}
  }

  # ref ####
  if(any(grepl( "ref", typeof, fixed = TRUE))){

    setwd(read$wd)
    setwd("./ref")

    ref <- list()

    # list ref files
    ref$files <- dir(pattern = "_ref.csv")[which(substr(dir(pattern = "_ref.csv"), 1, 10)>=firstday & substr(dir(pattern = "_ref.csv"), 1, 10)<=lastday)]

    # read  and merge ref files
    if( any( !is.na(MixerNumber[1]), !is.na(DTProductNumber)) ){ ref$merge <- lapply(ref$files[substr(ref$files, 1, 10) %in% read$line.product.date$date] ,function(x) fread(x, dec = ","))
    } else{ ref$merge <- lapply(ref$files,function(x) fread(x, dec = ","))}

    ref$merge <-  rbindlist(ref$merge, fill = T)

    # filter by product
    if(!is.na(MixerNumber[1]) & is.na(DTProductNumber)){
      chooserows <- which( ref$merge$MixerNumber %in% MixerNumber)
      ref$merge <- ref$merge[chooserows,]
    }

    if(is.na(MixerNumber[1]) & !is.na(DTProductNumber)){
      chooserows <- which( ref$merge$DTproductNumber %in% DTProductNumber)
      ref$merge <- ref$merge[chooserows,]
    }

    if(!is.na(MixerNumber[1]) & !is.na(DTProductNumber)){
      chooserows <- which( ref$merge$MixerNumber %in% MixerNumber | ref$merge$DTproductNumber %in% DTProductNumber)
      ref$merge <- ref$merge[chooserows,]
    }

    if( names( ref$merge )[ 1 ] != "Day"){  # Old LG3 data type

      ref$merge$date <- as.POSIXct(as.character(ref$merge$date),format="%Y-%m-%d",tz="Europe/Berlin")
      ref$merge$datetime <- as.POSIXct(as.character(ref$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")
      ref$merge <- ref$merge[, moveme(names(ref$merge), "datetime date time first"), with = F]

    }

    # move wl to last columns if necessary
    if(is.na(as.numeric(gsub("X", "", names(ref$merge)[ncol(ref$merge)])))){
      suppressWarnings(ref$wl <- sort(as.numeric(gsub("X","",names(ref$merge)))))
      for(i in paste0("X", ref$wl)) ref$merge <- ref$merge[ , moveme(names(ref$merge), paste(i, "last")), with = F]}

    if(slim == T) ref$merge <- ref$merge[ , names(ref$merge)[-unique(unlist(lapply(coltoremove, function(x) grep(x, names(ref$merge)))))], with = F]
    if(slim == T) ref$merge$date <- NULL
    if(slim == T) ref$merge$time <- NULL

    if(!is.na(export_directory) & export){
      setwd(export_directory)

      ref$merge$date <- as.character(ref$merge$date)

      ref$wl <- unique(unlist( lapply(ref$ppp, function( x ) x$wl)))
      names(ref$merge)[ names(ref$merge) %in% paste0("X", ref$wl)] <- ref$wl

      colnames( ref$merge ) <- gsub( "X", "", colnames( ref$merge ))
      fwrite(x = ref$merge, file = read$csv.file.names$ref <- paste0(read$name$file,"_ref.csv"), sep = ";", dec = ",", na = "NA")
      message("Referenz-Spektren exportiert")}
    if(!return.R)  rm(ref)
  }

  # drk ####
  if(any(grepl( "drk", typeof, fixed = TRUE))){

    setwd(read$wd)
    setwd("./drk")

    drk <- list()

    # list drk files
    drk$files <- dir(pattern = "_drk.csv")[which(substr(dir(pattern = "_drk.csv"), 1, 10)>=firstday & substr(dir(pattern = "_drk.csv"), 1, 10)<=lastday)]

    if( any( !is.na(MixerNumber[1]), !is.na(DTProductNumber)) ){ drk$merge <- lapply(drk$files[substr(drk$files, 1, 10) %in% read$line.product.date$date] ,function(x) fread(x, dec = ","))
    } else{ drk$merge <- lapply(drk$files,function(x) fread(x, dec = ","))}

    drk$merge <-  rbindlist(drk$merge, fill = T)

    # filter by product
    if(!is.na(MixerNumber[1]) & is.na(DTProductNumber)){
      chooserows <- which( drk$merge$MixerNumber %in% MixerNumber)
      drk$merge <- drk$merge[chooserows,]
    }

    if(is.na(MixerNumber[1]) & !is.na(DTProductNumber)){
      chooserows <- which( drk$merge$DTproductNumber %in% DTProductNumber)
      drk$merge <- drk$merge[chooserows,]
    }

    if(!is.na(MixerNumber[1]) & !is.na(DTProductNumber)){
      chooserows <- which( drk$merge$MixerNumber %in% MixerNumber | drk$merge$DTproductNumber %in% DTProductNumber)
      drk$merge <- drk$merge[chooserows,]
    }

    if( names( drk$merge )[ 1 ] != "Day"){  # Old LG3 data type

      drk$merge$date <- as.POSIXct(as.character(drk$merge$date),format="%Y-%m-%d",tz="Europe/Berlin")
      drk$merge$datetime <- as.POSIXct(as.character(drk$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")
      drk$merge <- drk$merge[, moveme(names(drk$merge), "datetime date time first"), with = F]

    }

    # move wl to last columns if necessary
    if(is.na(as.numeric(gsub("X", "", names(drk$merge)[ncol(drk$merge)])))){
      suppressWarnings(drk$wl <- sort(as.numeric(gsub("X","",names(drk$merge)))))
      for(i in paste0("X", drk$wl)) drk$merge <- drk$merge[ , moveme(names(drk$merge), paste(i, "last")), with = F]}

    if(slim == T) drk$merge <- drk$merge[ , names(drk$merge)[-unique(unlist(lapply(coltoremove, function(x) grep(x, names(drk$merge)))))], with = F]
    if(slim == T) drk$merge$date <- NULL
    if(slim == T) drk$merge$time <- NULL

    if(!is.na(export_directory) & export){
      setwd(export_directory)

      colnames( drk$merge ) <- gsub( "X", "", colnames( drk$merge ))

      drk$wl <- unique(unlist( lapply(drk$ppp, function( x ) x$wl)))
      names(drk$merge)[ names(drk$merge) %in% paste0("X", drk$wl)] <- drk$wl

      drk$merge$date <- as.character(drk$merge$date)

      fwrite(x = drk$merge, file = read$csv.file.names$drk <- paste0(read$name$file,"_drk.csv"), sep = ";", dec = ",", na = "NA")
      message("Dunkelwert-Spektren exportiert")}
    if(!return.R)  rm(drk)
  }

  if(!is.na(export_directory) & export) message(paste("Export der .csv-Dateien nach",export_directory, "abgeschlossen"))
  if(!is.na(export_directory) & export) setwd(export_directory)

  if(return.R){
    returnlist <- list()

    if(length(grep("spc",typeof))==1) returnlist$spc <- spc$merge
    if(length(grep("ref",typeof))==1) returnlist$ref <- ref$merge
    if(length(grep("drk",typeof))==1) returnlist$drk <- drk$merge


    returnlist$csv.file.names <- read$csv.file.names

    return(returnlist)
  }
}
