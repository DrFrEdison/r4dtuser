read.mea.gcims.meta <- function( mea_file){

  # read mea file, encoding latin1 for e.g. °C. Last line is incomplete and will be removed without warning
  read.mea <- list()
  suppressWarnings( read.mea$raw <- readLines( mea_file, encoding = "latin1" ))

  if( length( grep("nom Drift Tube Length", read.mea$raw) ) > 0) read.mea$raw <- read.mea$raw[ 1 : grep("nom Drift Tube Length", read.mea$raw) ]

  read.mea$nchar$all <- lapply(read.mea$raw, nchar)
  read.mea$nchar$zero <- which( unlist( read.mea$nchar$all) == 0)
  read.mea$nchar$high <- which( unlist( read.mea$nchar$all) > 999)[ 1 ]

  if( !is.na( read.mea$nchar$high )) read.mea$raw <- read.mea$raw[ 1 : (read.mea$nchar$high - 1)]

  read.mea$raw <- read.mea$raw[ 1 : (length(read.mea$raw) - 1) ]

  # Split each line by "=", first parameter, second values.
  read.mea$split$parameter <- lapply(read.mea$raw, function( x ) trimws( strsplit(x, "\\=")[[ 1 ]][ 1 ]))
  read.mea$split$values <- lapply(read.mea$raw, function( x ) trimws( strsplit(x, "\\=")[[ 1 ]][ 2 ]))

  # In values the unit is implemented
  read.mea$split$unit <- lapply(read.mea$split$values, function( x ) substr(x , gregexpr("\\[", x)[[1]][1] , gregexpr("\\]", x)[[1]][1]))

  read.mea$split$morethanoneunit <- lapply(read.mea$split$values,
                                           function(z) which( unlist( lapply( z,
                                                                              function(x) length(gregexpr("\\[", x)[[1]]))) != 1))

  read.mea$split$morethanoneunit_test <- lapply(mapply( function(x, y) x[y], x = read.mea$split$values, y = read.mea$split$morethanoneunit) ,
                                                function(z) ifelse(length(z) > 0, strsplit(unlist(z), ";"), z)
  )

  # values without unit ####
  read.mea$split$values <- lapply(read.mea$split$values, function(xx) lapply(xx,
                                                                             function(x) substr(x, 1, ifelse(gregexpr("\\[", x)[[1]] > 0, gregexpr("\\[", x)[[1]], nchar(x)))))

  read.mea$split$values <- lapply(read.mea$split$values, function(x) trimws(gsub("\\[", "", x)))

  # extract gc_ims info ####

  # Some info in the file is in a string format, to output it to a table with a good overview their mean/sd is calculated
  read.mea$vector.para <- c("Flow Epc 1", "Flow Epc 2", "Pressure Ambient", "Pressure Epc 1", "Pressure Epc 2")
  read.mea$vector.para.names <- gsub(" ", "_", read.mea$vector.para)

  # Extract the "vector"-parameter
  read.mea$vector.para.value <- lapply(read.mea$vector.para,
                                       function( x ) strsplit( read.mea$split$values[[ grep(x, read.mea$split$parameter) ]], " "))
  read.mea$vector.para.value <- lapply(read.mea$vector.para.value, function( x ) as.numeric(gsub('\"', "", x[[ 1 ]])))
  names( read.mea$vector.para.value ) <- read.mea$vector.para.names

  # Calculate mean & sd
  read.mea$vector.para.mean <- lapply(read.mea$vector.para.value, mean)
  read.mea$vector.para.sd <- lapply(read.mea$vector.para.value, sd)

  # Single Parameter
  read.mea$single.para <- c("Snapshot", "Snapshot Current Values", "Recognized substances")
  read.mea$single.para.names <- gsub(" ", "_", read.mea$single.para)

  read.mea$single.para.names <- read.mea$single.para.names[ read.mea$single.para %in% unlist( read.mea$split$parameter )]
  read.mea$single.para <- read.mea$single.para[ read.mea$single.para %in% unlist( read.mea$split$parameter )]

  if( length( read.mea$single.para ) > 0){
    read.mea$single.para.value <- lapply(read.mea$single.para,
                                         function( x ) strsplit( read.mea$split$values[[ grep(x, read.mea$split$parameter)[ 1 ] ]], " "))
    names( read.mea$single.para.value ) <- read.mea$single.para.names
    read.mea$single.para.value$Recognized_substances[[ 1 ]] <- gsub('\"', "", read.mea$single.para.value$Recognized_substances[[ 1 ]])
  }

  # Diacetyl & Pentanedione
  read.mea$Diacetyl <- read.mea$single.para.value$Recognized_substances[[ 1 ]][[ grep("diacetyl", read.mea$single.para.value$Recognized_substances[[ 1 ]], ignore.case = T)]]
  read.mea$Diacetyl <- as.numeric(gsub("[^-0-9.]", "", read.mea$Diacetyl))
  if(read.mea$Diacetyl < 0) read.mea$Diacetyl <- 0

  read.mea$Pentanedione <- read.mea$single.para.value$Recognized_substances[[ 1 ]][[ grep("pentanedione", read.mea$single.para.value$Recognized_substances[[ 1 ]], ignore.case = T)]]
  read.mea$Pentanedione <- as.numeric(gsub("[^-0-9.]", "", read.mea$Pentanedione))
  if(read.mea$Pentanedione < 0) read.mea$Pentanedione <- 0

  # Extract name of the program, run time and loop time
  read.mea$program.para <- c("Program")
  read.mea$program.para.names <- gsub(" ", "_", read.mea$program.para)

  # Extract the "program"-parameter
  read.mea$program.para.value <- lapply(read.mea$program.para,
                                        function( x ) strsplit( read.mea$raw[[ grep(x, read.mea$split$parameter)  ]], "\\|"))

  read.mea$program.name <- read.mea$program.para.value[[ 1 ]][[ 1 ]][ grep("Name", read.mea$program.para.value[[ 1 ]][[ 1 ]]) ]
  read.mea$program.name <- substr(read.mea$program.name
                                  , gregexpr("Name", read.mea$program.name)[[ 1 ]] + nchar( "Name") + 1
                                  , nchar( read.mea$program.name ))
  read.mea$program.name <- gsub("`", "", read.mea$program.name)


  read.mea$loop.open <- read.mea$program.para.value[[ 1 ]][[ 1 ]][ grep("V1=1.", read.mea$program.para.value[[ 1 ]][[ 1 ]]) ]
  if( length( read.mea$loop.open) > 0){
    read.mea$loop.open <- substr(read.mea$loop.open
                                 , gregexpr("Time", read.mea$loop.open)[[ 1 ]] + nchar( "Time") + 1
                                 , gregexpr("V1=1.", read.mea$loop.open)[[ 1 ]] - 2)
    read.mea$loop.open <- as.numeric(read.mea$loop.open)
  } else{ read.mea$loop.open <- 0}

  read.mea$loop.close <- read.mea$program.para.value[[ 1 ]][[ 1 ]][ grep("V1=0.", read.mea$program.para.value[[ 1 ]][[ 1 ]]) ]
  if( length( read.mea$loop.close) > 0){
    read.mea$loop.close <- substr(read.mea$loop.close
                                  , gregexpr("Time", read.mea$loop.close)[[ 1 ]] + nchar( "Time") + 1
                                  , gregexpr("V1=0.", read.mea$loop.close)[[ 1 ]] - 2)
    read.mea$loop.close <- as.numeric(read.mea$loop.close)
  } else{ read.mea$loop.close <- 0}

  read.mea$run.time <- read.mea$program.para.value[[ 1 ]][[ 1 ]][ grep("R=0.", read.mea$program.para.value[[ 1 ]][[ 1 ]]) ]
  read.mea$run.time <- substr(read.mea$run.time
                              , gregexpr("Time", read.mea$run.time)[[ 1 ]] + nchar( "Time") + 1
                              , gregexpr("R=0.", read.mea$run.time)[[ 1 ]] - 2)
  read.mea$run.time <- as.numeric(read.mea$run.time)

  # merge ####
  read.mea$split$values <- lapply(read.mea$split$values, function(x) gsub('\"', "", x))

  read.mea$split$values[  grep("Flow Epc 1", read.mea$split$parameter)  ] <- paste0(round(read.mea$vector.para.mean$Flow_Epc_1,1), " ± ", round(read.mea$vector.para.sd$Flow_Epc_1,1))
  read.mea$split$values[  grep("Flow Epc 2", read.mea$split$parameter)  ] <- paste0(round(read.mea$vector.para.mean$Flow_Epc_2,1), " ± ", round(read.mea$vector.para.sd$Flow_Epc_2,1))
  read.mea$split$values[  grep("Pressure Ambient", read.mea$split$parameter)  ] <- paste0(round(read.mea$vector.para.mean$Pressure_Ambient,1), " ± ", round(read.mea$vector.para.sd$Pressure_Ambient,1))
  read.mea$split$values[  grep("Pressure Epc 1", read.mea$split$parameter)  ] <- paste0(round(read.mea$vector.para.mean$Pressure_Epc_1,1), " ± ", round(read.mea$vector.para.sd$Pressure_Epc_1,1))
  read.mea$split$values[  grep("Pressure Epc 2", read.mea$split$parameter)  ] <- paste0(round(read.mea$vector.para.mean$Pressure_Epc_2,1), " ± ", round(read.mea$vector.para.sd$Pressure_Epc_2,1))

  read.mea$split$values[  grep("Timestamp", read.mea$split$parameter)  ] <- as.character(as.POSIXct(gsub("T", " ", read.mea$split$values[  grep("Timestamp", read.mea$split$parameter)  ])
                                                                                                    , format = "%Y-%m-%d %H:%M:%S"
                                                                                                    , tz = as.character(read.mea$split$values[  grep("Timezone", read.mea$split$parameter)  ])))

  if(length(grep("Snapshot Current Values", read.mea$split$parameter)) > 0) read.mea$split$values[  grep("Snapshot Current Values", read.mea$split$parameter)  ]  <- NA
  if(length(grep("Snapshot", read.mea$split$parameter)) > 0) read.mea$split$values[  grep("Snapshot", read.mea$split$parameter)  ] <- NA

  if(length(grep("Recognized substances", read.mea$split$parameter)) > 0) read.mea$split$values[  grep("Recognized substances", read.mea$split$parameter)  ] <- NA

  # merge
  read.mea$merge <- rbindlist( list(read.mea$split$parameter, read.mea$split$unit, read.mea$split$values))
  names( read.mea$merge ) <- gsub(" ", "_", as.character( read.mea$merge[1,] ))
  read.mea$merge <- read.mea$merge[ -1, ]
  read.mea$merge$Diacetyl = c("ppb", read.mea$Diacetyl)
  read.mea$merge$Pentanedione = c("ppb", read.mea$Pentanedione)

  read.mea$merge$program.name = c("", read.mea$program.name)
  read.mea$merge$loop.open = c("ms", read.mea$loop.open)
  read.mea$merge$loop.close = c("", read.mea$loop.close)
  read.mea$merge$run.time = c("", read.mea$run.time)

  read.mea$merge$ID = c("", mea_file)

  for(i in grep("Time", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]

  for(i in grep("Flow", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("flow", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("Temp", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("temp", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]

  for(i in grep("Pressure", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("pressure", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]

  for(i in grep("Firmware", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("Machine", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("Sensor", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("Sample_loop", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("Status_loop", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("Pump", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]

  for(i in grep("Drift_Gas", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("GC_column", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("Filter", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("Class", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]

  for(i in grep("Chunk", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("ADIO", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]

  for(i in grep("Program", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("substances", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]
  for(i in grep("Snapshot", names(read.mea$merge), value = T)) read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), paste0(i, " last")), with = F]

  read.mea$merge <- read.mea$merge[ , moveme(names(read.mea$merge), "Timestamp ID Diacetyl Pentanedione program.name loop.open loop.close run.time first"), with = F]

  # remove unit row
  names(read.mea$merge) <- paste0( names(read.mea$merge), "_", read.mea$merge[1,])

  for(i in 1 : length( names( read.mea$merge ))){
    subname <- names( read.mea$merge )[ i ]
    if( substr( subname, nchar( subname ), nchar( subname )) == "_")
      names( read.mea$merge )[ i ] <- substr(subname
                                             , 1
                                             , nchar(subname) - 1)
  }

  read.mea$merge <- read.mea$merge[-1,]

  # Make numeric #####
  for (col_name in names(read.mea$merge)) {
    # Check if the column contains only numeric values
    if (all(!is.na(as.numeric( read.mea$merge[ , get(col_name)])))) {
      read.mea$merge[, (col_name) := as.numeric(read.mea$merge[[col_name]])]
    }
  }

  return( read.mea$merge)
}
