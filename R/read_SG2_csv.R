# read.csv.SG - Custom CSV Data Processing and Merging Function

# Define a function for reading and merging CSV files
read.csv.SG <- function(
    customer,         # Customer name
    location,         # Location identifier
    line,             # Production line identifier
    firstday,         # Start date for data selection
    lastday,          # End date for data selection
    MixerNumber = NA, # MixerNumber (optional)
    export_directory = "C://csvtemp",  # Export directory
    typeof = c("ref", "drk", "spc"),   # Data types to process
    export = TRUE,    # Export merged data to CSV files
    fastplot,         # Fast plot option (not defined in the script)
    dir_wd            # Working directory (not defined in the script)
) {

  # Create the export directory if it doesn't exist
  dir.create(export_directory, showWarnings = FALSE)

  # Initialize empty lists for different data types
  spc <- list()
  drk <- list()
  ref <- list()

  read <- list()
  # get production df for each day
  setwd(service_backup_path(customer, location, line, dir_wd = dir_wd))

  read$line.product.date <- do.call(rbind, lapply(dir(pattern =  paste0(line,".csv")), read.csv2))
  names( read$line.product.date )[ 1 ] <- "date"
  read$line.product.date <- read$line.product.date[which(nchar(as.character(read$line.product.date$date)) == 10),]
  read$line.product.date <- read$line.product.date[which(read$line.product.date$date >= firstday & read$line.product.date$date <= lastday) ,]

  if( !is.na( MixerNumber )) read$line.product.date <- read$line.product.date[ which(read$line.product.date$MixerNumber == MixerNumber), ]

  # Process "spc" data type
  if ("spc" %in% typeof) {
    # Define the working directory
    setwd(service_backup_path(customer, location, line, dir_wd = dir_wd))
    setwd("./spc")

    # Filter CSV files by date range
    spc$fileslist <- dir(pattern = ".csv")[which(substr(dir(pattern = ".csv"), 1, 10) >= firstday & substr(dir(pattern = ".csv"), 1, 10) <= lastday)]
    if( !is.na( MixerNumber )) spc$fileslist <- spc$fileslist[ substr(spc$fileslist, 1, 10) %in% read$line.product.date$date ]
    spc$fileslist <- lapply(spc$fileslist, function(x) if (!identical(x, character(0))) x else { "///NA" })
    spc$filesdir <- lapply(spc$fileslist, function(x) if (length(grep("///NA", x)) > 0) "///NA" else { x })

    # Remove null observations
    spc$filesdir <- rmNullObs(spc$filesdir)
    spc$fileslist <- rmNullObs(spc$fileslist)

    # Read and store data from CSV files
    spc$dat <- suppressWarnings(lapply(spc$filesdir, function(x) fread(x, sep = ";", dec = ",")))

    # Rename columns and preprocess data as needed
    for(i in 1:length(spc$dat)){
      names(spc$dat[[i]])[grep("Produktn", names(spc$dat[[i]]))] <- "Produktnummer"
    }

    for(i in 1:length(spc$dat)){
      find_acid <- grep("ure.",names(spc$dat[[i]]))
      names_acid <- names(spc$dat[[i]])[find_acid]

      names_acid1 <- substr(names_acid, 1, unlist(lapply(gregexpr("\\.", names_acid), min))-1)
      names_acid2 <- substr(names_acid, unlist(lapply(gregexpr("_", names_acid), max)), nchar(names_acid))

      names(spc$dat[[i]])[find_acid] <- gsub("SaeureSaeure", "Saeure", gsub("\\.","",paste(names_acid1, names_acid2, sep = ".")))
    }

    # Remove empty data frames
    for(i in length(spc$dat):1) if(nrow(spc$dat[[i]])==0) spc$dat[[i]] <- NULL

    # Further data cleaning and manipulation
    spc$dat <- lapply(spc$dat,function(x) x <- x[, colSums(is.na(x)) != nrow(x), with = F])

    # zerotime
    zerotime <- which(do.call(rbind,lapply(spc$dat,function(x) ifelse(length(grep("00-00-00",x[1,"time",with = F]))==0,0,1)))==1)

    spc$datetime <- lapply(spc$dat, function(x) as.POSIXct(as.character(x[1,"datetime",with = F]),format="%Y-%m-%d %H:%M:%S",tz="Europe/Berlin"))

    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        spc$datetime[[zerotime[i]]] <- format(round(spc$datetime[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}

    # Produktnumber
    spc$Produktnummer <- lapply(spc$dat,function(x) grep("Produkt",names(x)))
    spc$Produktnummer <- mapply(function(x , y) x[,y,with=F],spc$dat,spc$Produktnummer)

    # Column Names
    for(i in 1:length(spc$dat)){
      istext <- which(names(spc$dat[[i]]) %in% c("datetime","date","time","Tank","Produkt","Auftrag","Benutzer"))
      istextl <- ifelse(length(istext)>0,length(istext),0)
      isfac <- which(sapply(spc$dat[[i]][,-c(istext)], is.factor))+istextl
      if(length(isfac)>0) for(j in as.numeric(isfac)) spc$dat[[i]][,j] <- as.numeric(as.character(spc$dat[[i]][,j]))
    }

    # Merge data frames
    spc$merge <- rbindlist(spc$dat, fill = T)

    # Handle special cases and data type conversion
    suppressWarnings(movevec <- names(spc$merge)[which(!is.na(as.numeric(gsub("X","",names(spc$merge)))))])
    for(i in 1:length(movevec)){
      col_idx <- grep(movevec[i],names(spc$merge))
      spc$merge <- spc$merge[, c((1:ncol(spc$merge))[-col_idx],col_idx), with = F]}

    # Filter by MixerNumber ####
    if(!is.na(MixerNumber))  spc$merge <- spc$merge[ which( spc$merge$Produkt %in% MixerNumber |
                                                              spc$merge$Produkt %in% gsub("_", " ", MixerNumber) |
                                                              spc$merge$Produkt %in% gsub("-", " ", MixerNumber)) , ]
    if(nrow(spc$merge) == 0) stop(paste0("Keine Produktion von ", MixerNumber, " im definierten Zeitraum"))

    # Remove duplicated data ####
    spc$merge <- spc$merge[which(!duplicated(spc$merge$datetime)),]

    if(length(grep("Produktnummer",names(spc$merge)))>1) spc$merge <- spc$merge[,-grep("Produktnummer",names(spc$merge))[2], with = F]
    names(spc$merge)[1:2] <- c("datetime","date")

    names(spc$merge) <- gsub("X","",names(spc$merge))
  }

  # Check if "drk" exists in the environment
  if ("drk" %in% typeof) {
    # Set working directory to the specified path
    # Replace 'service_backup_path' with the actual function or variable
    setwd(service_backup_path(customer, location, line, dir_wd = dir_wd))

    # Change working directory to "./drk"
    setwd("./drk")

    # List CSV files in the directory that match the pattern
    drk$fileslist <- dir(pattern=".csv")[which(substr(dir(pattern=".csv"),1,10)>=firstday & substr(dir(pattern=".csv"),1,10)<=lastday)]
    if( !is.na( MixerNumber )) drk$fileslist <- drk$fileslist[ substr(drk$fileslist, 1, 10) %in% read$line.product.date$date ]

    # Replace empty entries with "///NA"
    drk$fileslist <- lapply(drk$fileslist, function(x) if (!identical(x, character(0))) x else "///NA")

    # Create 'filesdir' based on 'fileslist'
    drk$filesdir <- lapply(drk$fileslist, function(x) if (length(grep("///NA", x)) > 0) "///NA" else x)

    # Remove NULL observations from 'filesdir' and 'fileslist'
    drk$filesdir <- rmNullObs(drk$filesdir)
    drk$fileslist <- rmNullObs(drk$fileslist)

    # Read CSV files into 'dat', suppressing warnings
    drk$dat <- suppressWarnings(lapply(drk$filesdir, function(x) fread(x, sep = ";", dec = ",")))

    # Find records with "00-00-00" in the 'time' column and set 'datetime'
    zerotime <- which(do.call(rbind, lapply(drk$dat, function(x) ifelse(length(grep("00-00-00", x[1,"time"]))==0, 0, 1))) == 1)
    drk$datetime <- lapply(drk$dat, function(x) as.POSIXct(as.character(x[1,"datetime"]), format="%Y-%m-%d %H:%M:%S", tz="Europe/Berlin"))

    # Format 'datetime' values if 'zerotime' exists
    if (length(zerotime) > 0) {
      for (i in 1:length(zerotime)) {
        drk$datetime[[zerotime[i]]] <- format(round(drk$datetime[[zerotime[i]]], units="days"), "%Y-%m-%d %M:%H:%S")
      }
    }

    # Extract 'Produktnummer' from 'dat' and merge data
    drk$Produktnummer <- lapply(drk$dat, function(x) grep("Produkt", names(x)))
    drk$Produktnummer <- mapply(function(x , y) x[, y, with = F], drk$dat, drk$Produktnummer)
    drk$merge <- do.call(plyr::rbind.fill, drk$dat)
    names(drk$merge)[1:2] <- c("datetime", "date")

    # Remove "X" from column names in 'merge'
    names(drk$merge) <- gsub("X", "", names(drk$merge))

    # Check if "MixerNumber" exists and "spc" is in typeof
    if (!any(is.na(MixerNumber)) & "spc" %in% typeof) {
      # Define a time window of 15 minutes before and after midnight of the day where the product was produced

      # Check if there are no rows with datetime earlier than the specified window
      if (length(which(as.POSIXct( drk$merge$datetime, tz = "UTC") < range(spc$merge$datetime)[1] - 60 * 15)) == 0)
        drk$merge <- drk$merge[which(as.POSIXct( drk$merge$datetime, tz = "UTC") < range(spc$merge$datetime)[1] - 60 * 15), ]

      # Check if there are no rows with datetime later than the specified window
      if (length(which(as.POSIXct( drk$merge$datetime, tz = "UTC") > range(spc$merge$datetime)[2] + 60 * 15)) == 0)
        drk$merge <- drk$merge[which(as.POSIXct( drk$merge$datetime, tz = "UTC") < range(spc$merge$datetime)[2] + 60 * 15), ]

      # Find unique dates in "drk" data that match dates in "spc"
      drk$date.in.spc <- unique(drk$merge$date)[which(unique(drk$merge$date) %in% unique(as.Date(spc$merge$datetime, tz = "UTC")))]

      # Create lists to store information about time windows before and after midnight
      drk$before.midnight <- list()
      drk$after.midnight <- list()

      # Iterate through dates in "drk" data
      for (i in 1:length(drk$date.in.spc)) {
        # Check if datetime falls within 15 minutes before midnight
        drk$before.midnight[[i]] <- as.POSIXct(drk$merge$datetime, tz = "UTC") >=
          as.POSIXct(unique(as.Date(drk$date.in.spc[i], tz = "UTC")), format = "%Y-%m-%d %H:%M:%S") - 60 * 15 &
          as.POSIXct(drk$merge$datetime, tz = "UTC") <=
          as.POSIXct(unique(as.Date(drk$date.in.spc[i], tz = "UTC")), format = "%Y-%m-%d %H:%M:%S")

        # Check if datetime falls within 15 minutes after midnight
        drk$after.midnight[[i]] <- as.POSIXct(drk$merge$datetime, tz = "UTC") <=
          as.POSIXct(unique(as.Date(drk$date.in.spc[i], tz = "UTC")), format = "%Y-%m-%d %H:%M:%S") + 60 * 15 + 60 * 60 * 24 &
          as.POSIXct(drk$merge$datetime, tz = "UTC") >=
          as.POSIXct(unique(as.Date(drk$date.in.spc[i], tz = "UTC")) + 1, format = "%Y-%m-%d %H:%M:%S")
      }

      # Identify rows to remove based on conditions
      if( length( which(!drk$merge$date %in% spc$merge$date) ) > 0) drk$remove.rows <- which(!drk$merge$date %in% spc$merge$date) else(drk$remove.rows <- 0)

      # Check if there are any rows in the time windows
      if (length(which(apply(do.call(cbind, drk$before.midnight), 1, any))) > 0 |
          length(which(apply(do.call(cbind, drk$after.midnight), 1, any)))) {
        drk$remove.rows <- drk$remove.rows[!drk$remove.rows %in%
                                             c(which(apply(do.call(cbind, drk$before.midnight), 1, any)),
                                               which(apply(do.call(cbind, drk$after.midnight), 1, any)))]
      }
      # Remove identified rows from "drk" data
      if(drk$remove.rows[ 1 ] != 0 & length( drk$remove.rows ) > 0) drk$merge <- drk$merge[-drk$remove.rows, ]
    }

    # Time range check
    for (i in 1 : length(unique(as.Date(spc$merge$datetime, tz = "UTC")))) {
      # Calculate the range for "spc"
      drk$range$spc <- range(spc$merge$datetime[which(as.Date(spc$merge$datetime, tz = "UTC") == unique(as.Date(spc$merge$datetime, tz = "UTC"))[i])]) + c(-60*15, 60*.5)

      # Remove records in 'merge' outside the range
      range.low <- which(drk$merge$date == unique(as.Date(spc$merge$datetime, tz = "UTC"))[i] &
                           as.POSIXct( drk$merge$datetime, tz = "UTC") < drk$range$spc[1])
      range.low <- range.low[ which(drk$merge$date == unique(as.Date(spc$merge$datetime, tz = "UTC"))[i]) %in% range.low ]
      range.low <- sort( range.low )
      if (length(range.low) > 0) drk$merge <- drk$merge[-range.low, ]

      range.high <- which(drk$merge$date == unique(as.Date(spc$merge$datetime, tz = "UTC"))[i] &
                            as.POSIXct( drk$merge$datetime, tz = "UTC") > drk$range$spc[ 2 ])

      range.high <- range.high[ range.high %in% which(drk$merge$date == unique(as.Date(spc$merge$datetime, tz = "UTC"))[i]) ]
      range.high <- sort( range.high )
      if (length(range.high) > 0) drk$merge <- drk$merge[-range.high, ]
    }
  }

  # Check if "ref" exists in the environment
  if ("ref" %in% typeof) {
    # Set working directory to the specified path
    # Replace 'service_backup_path' with the actual function or variable
    setwd(service_backup_path(customer, location, line, dir_wd = dir_wd))

    # Change working directory to "./ref"
    setwd("./ref")

    # List CSV files in the directory that match the pattern
    ref$fileslist <- dir(pattern=".csv")[which(substr(dir(pattern=".csv"),1,10)>=firstday & substr(dir(pattern=".csv"),1,10)<=lastday)]
    if( !is.na( MixerNumber )) ref$fileslist <- ref$fileslist[ substr(ref$fileslist, 1, 10) %in% read$line.product.date$date ]

    # Replace empty entries with "///NA"
    ref$fileslist <- lapply(ref$fileslist, function(x) if (!identical(x, character(0))) x else "///NA")

    # Create 'filesdir' based on 'fileslist'
    ref$filesdir <- lapply(ref$fileslist, function(x) if (length(grep("///NA", x)) > 0) "///NA" else x)

    # Remove NULL observations from 'filesdir' and 'fileslist'
    ref$filesdir <- rmNullObs(ref$filesdir)
    ref$fileslist <- rmNullObs(ref$fileslist)

    # Read CSV files into 'dat', suppressing warnings
    ref$dat <- suppressWarnings(lapply(ref$filesdir, function(x) fread(x, sep = ";", dec = ",")))

    # Find records with "00-00-00" in the 'time' column and set 'datetime'
    zerotime <- which(do.call(rbind, lapply(ref$dat, function(x) ifelse(length(grep("00-00-00", x[1,"time"]))==0, 0, 1))) == 1)
    ref$datetime <- lapply(ref$dat, function(x) as.POSIXct(as.character(x[1,"datetime"]), format="%Y-%m-%d %H:%M:%S", tz="Europe/Berlin"))

    # Format 'datetime' values if 'zerotime' exists
    if (length(zerotime) > 0) {
      for (i in 1:length(zerotime)) {
        ref$datetime[[zerotime[i]]] <- format(round(ref$datetime[[zerotime[i]]], units="days"), "%Y-%m-%d %M:%H:%S")
      }
    }

    # Extract 'Produktnummer' from 'dat' and merge data
    ref$Produktnummer <- lapply(ref$dat, function(x) grep("Produkt", names(x)))
    ref$Produktnummer <- mapply(function(x , y) x[, y, with = F], ref$dat, ref$Produktnummer)
    ref$merge <- do.call(plyr::rbind.fill, ref$dat)
    names(ref$merge)[1:2] <- c("datetime", "date")

    # Remove "X" from column names in 'merge'
    names(ref$merge) <- gsub("X", "", names(ref$merge))

    # Check if "MixerNumber" exists and "spc" is in typeof
    if (!any(is.na(MixerNumber)) & "spc" %in% typeof) {
      # Define a time window of 15 minutes before and after midnight of the day where the product was produced

      # Check if there are no rows with datetime earlier than the specified window
      if (length(which(ref$merge$datetime < range(spc$merge$datetime)[1] - 60 * 15)) == 0)
        ref$merge <- ref$merge[which(ref$merge$datetime < range(spc$merge$datetime)[1] - 60 * 15), ]

      # Check if there are no rows with datetime later than the specified window
      if (length(which(ref$merge$datetime > range(spc$merge$datetime)[2] + 60 * 15)) == 0)
        ref$merge <- ref$merge[which(ref$merge$datetime < range(spc$merge$datetime)[2] + 60 * 15), ]

      # Find unique dates in "ref" data that match dates in "spc"
      ref$date.in.spc <- unique(ref$merge$date)[which(unique(ref$merge$date) %in% unique(as.Date(spc$merge$datetime, tz = "UTC")))]

      # Create lists to store information about time windows before and after midnight
      ref$before.midnight <- list()
      ref$after.midnight <- list()

      # Iterate through dates in "ref" data
      for (i in 1:length(ref$date.in.spc)) {
        # Check if datetime falls within 15 minutes before midnight
        ref$before.midnight[[i]] <- as.POSIXct(ref$merge$datetime, tz = "UTC") >=
          as.POSIXct(unique(as.Date(ref$date.in.spc[i], tz = "UTC")), format = "%Y-%m-%d %H:%M:%S") - 60 * 15 &
          as.POSIXct(ref$merge$datetime, tz = "UTC") <=
          as.POSIXct(unique(as.Date(ref$date.in.spc[i], tz = "UTC")), format = "%Y-%m-%d %H:%M:%S")

        # Check if datetime falls within 15 minutes after midnight
        ref$after.midnight[[i]] <- as.POSIXct(ref$merge$datetime, tz = "UTC") <=
          as.POSIXct(unique(as.Date(ref$date.in.spc[i], tz = "UTC")), format = "%Y-%m-%d %H:%M:%S") + 60 * 15 + 60 * 60 * 24 &
          as.POSIXct(ref$merge$datetime, tz = "UTC") >=
          as.POSIXct(unique(as.Date(ref$date.in.spc[i], tz = "UTC")) + 1, format = "%Y-%m-%d %H:%M:%S")
      }

      # Identify rows to remove based on conditions
      if( length( which(!ref$merge$date %in% spc$merge$date) ) > 0) ref$remove.rows <- which(!ref$merge$date %in% spc$merge$date) else(ref$remove.rows <- 0)

      # Check if there are any rows in the time windows
      if (length(which(apply(do.call(cbind, ref$before.midnight), 1, any))) > 0 |
          length(which(apply(do.call(cbind, ref$after.midnight), 1, any)))) {
        ref$remove.rows <- ref$remove.rows[!ref$remove.rows %in%
                                             c(which(apply(do.call(cbind, ref$before.midnight), 1, any)),
                                               which(apply(do.call(cbind, ref$after.midnight), 1, any)))]
      }
      # Remove identified rows from "ref" data
      if(ref$remove.rows[ 1 ] != 0 & length( ref$remove.rows ) > 0) ref$merge <- ref$merge[-ref$remove.rows, ]
    }

    # Time range check
    for (i in 1 : length(unique(as.Date(spc$merge$datetime, tz = "UTC")))) {
      # Calculate the range for "spc"
      ref$range$spc <- range(spc$merge$datetime[which(as.Date(spc$merge$datetime, tz = "UTC") == unique(as.Date(spc$merge$datetime, tz = "UTC"))[i])]) + c(-60*15, 60*.5)

      # Remove records in 'merge' outside the range
      range.low <- which(ref$merge$date == unique(as.Date(spc$merge$datetime, tz = "UTC"))[i] &
                           as.POSIXct( ref$merge$datetime, tz = "UTC") < ref$range$spc[1])
      range.low <- range.low[ which(ref$merge$date == unique(as.Date(spc$merge$datetime, tz = "UTC"))[i]) %in% range.low ]
      range.low <- sort( range.low )
      if (length(range.low) > 0) ref$merge <- ref$merge[-range.low, ]

      range.high <- which(ref$merge$date == unique(as.Date(spc$merge$datetime, tz = "UTC"))[i] &
                            as.POSIXct( ref$merge$datetime, tz = "UTC") > ref$range$spc[ 2 ])

      range.high <- range.high[ range.high %in% which(ref$merge$date == unique(as.Date(spc$merge$datetime, tz = "UTC"))[i]) ]
      range.high <- sort( range.high )
      if (length(range.high) > 0) ref$merge <- ref$merge[-range.high, ]
    }
  }

  if(exists("spc")) if(length(which(names(spc$merge) == "Benutzer")) > 0){
    if("spc" %in% typeof){
      spc$merge <- data.frame(spc$merge)
      spc$merge[ , (which(names(spc$merge) == "Benutzer") + 1) : ncol(spc$merge)] <- apply(spc$merge[ , (which(names(spc$merge) == "Benutzer") + 1) : ncol(spc$merge)], 2, function(x) as.numeric(as.character(gsub(",",".",x))))
    }
  }
  if(exists("drk")) if(length(which(names(drk$merge) == "Benutzer")) > 0){
    if("drk" %in% typeof) drk$merge[ , (which(names(drk$merge) == "Benutzer") + 1) : ncol(drk$merge)] <- apply(drk$merge[ , (which(names(drk$merge) == "Benutzer") + 1) : ncol(drk$merge)], 2, function(x) as.numeric(as.character(gsub(",",".",x))))
  }
  if(exists("ref")) if(length(which(names(ref$merge) == "Benutzer")) > 0){
    if("ref" %in% typeof) ref$merge[ , (which(names(ref$merge) == "Benutzer") + 1) : ncol(ref$merge)] <- apply(ref$merge[ , (which(names(ref$merge) == "Benutzer") + 1) : ncol(ref$merge)], 2, function(x) as.numeric(as.character(gsub(",",".",x))))
  }

  if(exists("spc")) if(length(which(names(spc$merge) == "Benutzer")) == 0){
    if("spc" %in% typeof){
      nums <- suppressWarnings(as.numeric(which(lapply(apply(apply(spc$merge, 2, as.numeric), 2, function(x) unique(!is.na(x))), any) == T)))
      spc$merge[, nums] <- apply(spc$merge[,nums],2,function(x) as.numeric(as.character(x)))

    }
  }
  if(exists("drk")) if(length(which(names(drk$merge) == "Benutzer")) > 0){
    if("drk" %in% typeof){
      nums <- suppressWarnings(as.numeric(which(lapply(apply(apply(drk$merge, 2, as.numeric), 2, function(x) unique(!is.na(x))), any) == T)))
      drk$merge[, nums] <- apply(drk$merge[,nums],2,function(x) as.numeric(as.character(x)))
    }
  }
  if(exists("ref")) if(length(which(names(ref$merge) == "Benutzer")) > 0){
    if("ref" %in% typeof){
      nums <- suppressWarnings(as.numeric(which(lapply(apply(apply(ref$merge, 2, as.numeric), 2, function(x) unique(!is.na(x))), any) == T)))
      ref$merge[, nums] <- apply(ref$merge[,nums],2,function(x) as.numeric(as.character(x)))
    }
  }

  # Export ####
  setwd(export_directory)

  if("spc" %in% typeof){
    spc$merge[ , grep("Benutzer",names(spc$merge))] <- NULL
    for(i in 7:ncol(spc$merge))   spc$merge[,i] <- as.numeric(as.character(spc$merge[,i]))
  }

  removecol <- c("Lambda", "Lamdba", "score")
  if("spc" %in% typeof) names(spc$merge) <- gsub("ü", "ue", names(spc$merge))
  if("spc" %in% typeof) names(spc$merge) <- gsub("\\.x", "", names(spc$merge))
  if("spc" %in% typeof) names(spc$merge) <- gsub("\\.y", "", names(spc$merge))

  if("drk" %in% typeof) if(length(unique (grep(paste(removecol,collapse="|"), names(drk$merge)))) > 0) drk$merge <- drk$merge[ , -unique (grep(paste(removecol,collapse="|"), names(drk$merge)))]
  if("ref" %in% typeof) if(length(unique (grep(paste(removecol,collapse="|"), names(ref$merge)))) > 0) ref$merge <- ref$merge[ , -unique (grep(paste(removecol,collapse="|"), names(ref$merge)))]
  if("spc" %in% typeof) if(length(unique (grep(paste(removecol,collapse="|"), names(spc$merge)))) > 0) spc$merge <- spc$merge[ , -unique (grep(paste(removecol,collapse="|"), names(spc$merge)))]

  if("ref" %in% typeof) suppressWarnings( ref$merge[ , (max(grep("time", names(ref$merge))) + 1):ncol(ref$merge)] <- apply(ref$merge[ , (max(grep("time", names(ref$merge))) + 1):ncol(ref$merge)], 2, function(x) as.numeric(as.character(gsub(",",".",x)))))

  if("drk" %in% typeof) suppressWarnings( drk$merge[ , (max(grep("time", names(drk$merge))) + 1):ncol(drk$merge)] <- apply(drk$merge[ , (max(grep("time", names(drk$merge))) + 1):ncol(drk$merge)], 2, function(x) as.numeric(as.character(gsub(",",".",x)))))

  if("spc" %in% typeof) colnames( spc$merge ) <- gsub( "X", "", colnames( spc$merge ))
  if("drk" %in% typeof) colnames( drk$merge ) <- gsub( "X", "", colnames( drk$merge ))
  if("ref" %in% typeof) colnames( ref$merge ) <- gsub( "X", "", colnames( ref$merge ))

  csv.file.names <- list()
  if("drk" %in% typeof & export) write.csv2(drk$merge, csv.file.names$drk <- gsub("-","",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",line,"_",gsub(" ", "_", gsub("%","p",dt$MixerNumber)),"_drk.csv")), row.names = F)
  if("ref" %in% typeof & export) write.csv2(ref$merge, csv.file.names$ref <- gsub("-","",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",line,"_",gsub(" ", "_", gsub("%","p",dt$MixerNumber)),"_ref.csv")), row.names = F)
  if("spc" %in% typeof & export) write.csv2(spc$merge, csv.file.names$spc <- gsub("-","",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",line,"_",gsub(" ", "_", gsub("%","p",dt$MixerNumber)),"_spc.csv")), row.names = F)

  exportlist <- list()
  if("spc" %in% typeof) exportlist$spc <- spc$merge
  if("ref" %in% typeof) exportlist$ref <- ref$merge
  if("drk" %in% typeof) exportlist$drk <- drk$merge

  exportlist$csv.file.names <- csv.file.names

  if(export) message(paste("Export nach",export_directory, "abgeschlossen"))
  return(exportlist)
  options(warn = oldw)
}



