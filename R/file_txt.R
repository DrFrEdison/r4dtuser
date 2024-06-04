lambda <- expression(paste(lambda, " in nm"))
ylab_1st <- expression(paste(Delta, " AU / ", Delta, lambda))
ylab_2nd <- expression(paste(Delta, " AU / ", Delta, lambda^2))

# Extrahiere alle Informationen aus dem generierten Dateinamen ####
txt.file <- function(filename
                     , customer = dt_customer
                     , Mixernummer = dt_Mixernummer){

  # Leere liste
  txt <- list()

  txt$dt_customer <- data.frame(customer)
  txt$dt_Mixernummer <- data.frame(Mixernummer)

  # Dateityp drk, ref, spc, trans... ####
  txt$type <- mapply( function( x, y, z) substring(x, y + 1, z - 1)
                      , x = filename
                      , y = lapply(gregexpr("_", filename), function(x) max(x))
                      , z = lapply(gregexpr("\\.", filename), function(x) max(x)))
  txt$type <- as.character( unlist(txt$type))

  txt$filenamerest <- mapply( function( type,filename) gsub(type, "", filename)
                              , txt$type
                              , filename) # Abziehen vom Dateinamen

  # Für logDateien jetzt abbrechen
  if(any( unlist(lapply( txt$filenamerest, function( x ) grep("logfile", x))) == 1))
    return( txt)

  # Für drop-Dateien jetzt abbrechen
  if(any( unlist( lapply(txt$filenamerest, function( x ) length(grep("^[0-9]{6}_.csv", x)) == 1))))
    return(txt)

  # Standort
  txt$location <- unlist( lapply(txt$filenamerest, function( x ) unique( txt$dt_customer$location[ which(unlist( lapply(txt$dt_customer$location, gregexpr, x)) > 0)])))
  txt$filenamerest <- mapply( function( location,filename) gsub(location, "", filename)
                              , txt$location
                              , txt$filenamerest) # Abziehen vom Dateinamen

  # Linie
  txt$line <- unlist( lapply(txt$filenamerest, function( x ) unique( txt$dt_customer$line[ which(unlist( lapply(paste0("_", txt$dt_customer$line, "_"), gregexpr, x)) > 0)])))
  txt$filenamerest <- mapply( function( line,filename) gsub(line, "", filename)
                              , txt$line
                              , txt$filenamerest) # Abziehen vom Dateinamen

  # Startdatum
  txt$datefrom <- mapply( function( x, z) substring(x, 1, z - 1)
                          , x = txt$filenamerest
                          , z = lapply(gregexpr("_", txt$filenamerest), function(x) x[[1]]))
  txt$datefrom <- as.character( unlist( txt$datefrom))

  # Enddatum
  txt$dateto <- mapply( function( x, y, z) substring(x, y + 1, z - 1)
                        , x = txt$filenamerest
                        , y = lapply(gregexpr("_", txt$filenamerest), function(x) x[[1]])
                        , z = lapply(gregexpr("_", txt$filenamerest), function(x) x[[2]]))
  txt$dateto <- as.character( unlist( txt$dateto))

  txt$filenamerest <- mapply( function( datefrom,filename) gsub(datefrom, "", filename)
                              , txt$datefrom
                              , txt$filenamerest) # Abziehen vom Dateinamen
  txt$filenamerest <- mapply( function( dateto,filename) gsub(dateto, "", filename)
                              , txt$dateto
                              , txt$filenamerest) # Abziehen vom Dateinamen

  # Standort & Linie in einem
  txt$loc.line <- paste(txt$location, txt$line, sep = "_")

  # Produktnummern ####
  txt$dt_Mixernummer <- lapply(txt$location, function( x ) txt$dt_Mixernummer[txt$dt_Mixernummer$location == x,])
  # Mehr als eine Linie für Produktnamen?
  for(i in 1 : length( txt$dt_Mixernummer)){
    if( any(!is.na( unique(txt$dt_Mixernummer[[ i ]]$line) )) & length( unique(txt$dt_Mixernummer[[ i ]]$line)) > 1)
      txt$dt_Mixernummer[[ i ]] <- txt$dt_Mixernummer[[ i ]][txt$dt_Mixernummer[[ i ]]$line == txt$line[ i ],]

  }

  # Getränkenamen herausarbeiten Puhh! ####
  for(i in 1 : length( txt$dt_Mixernummer))
    txt$beverage[[ i ]] <- gsub("[- ]", "_", txt$dt_Mixernummer[[ i ]]$beverage[ order(nchar(txt$dt_Mixernummer[[ i ]]$beverage))])

  txt$beverage_in_filenamep <- mapply( function( beverage, rest) unlist( lapply( lapply( beverage, function( x ) gregexpr( x,rest, ignore.case = T)), function( y ) y[[1]][1]))
                                       , beverage = txt$beverage
                                       , rest = txt$filenamerest
                                       , SIMPLIFY = F)

  txt$beverage.sub <- mapply( function( beverage, filename) beverage[ which( filename > 0)]
                              , beverage = txt$beverage
                              , filename = txt$beverage_in_filenamep
                              , SIMPLIFY = F)

  txt$beverage.sub <- lapply(txt$beverage.sub, function( x ) x[ order( nchar( x), decreasing = T)])
  txt$beverage.sub <- lapply(txt$beverage.sub, unlist)

  for(j in 1 : length( txt$beverage.sub)){
    if( length( txt$beverage.sub[[ j ]] ) > 0)  for(i in 1 : length( txt$beverage.sub[[ j ]] ))
      if( length( which( unlist( gregexpr( txt$beverage.sub[[ j ]][[ i ]], txt$beverage.sub[[ j ]])) > 0)) > 1){
        if( lapply(gregexpr(txt$beverage.sub[[ j ]][ i ], txt$filenamerest), length)[[ 1 ]] < 2)
          txt$beverage.sub[[ j ]][[ i ]] <- NA

      }
  }

  txt$beverage.sub[[ j ]] <- sort(txt$beverage.sub[[ j ]])

  if( length( txt$beverage.sub[[ 1 ]]) == 0) txt$beverage <- list("")
  for(j in 1 : length( txt$beverage.sub)){
    if( length( txt$beverage.sub[[ j ]] ) > 0){
      txt$Mixernummer <- list()
      txt$DTProductNumber <- list()
      for(i in 1 : length( txt$beverage.sub[[ j ]] )){
        txt$Mixernummer[[ i ]] <- c(txt$dt_Mixernummer[[ j ]]$Mixer_ID_1[ which(txt$beverage.sub[[ j ]][[ i ]] == gsub("[- ]", "_", txt$dt_Mixernummer[[ j ]]$beverage)) ]
                                    , txt$dt_Mixernummer[[ j ]]$Mixer_ID_2[ txt$beverage.sub[[ j ]][[ i ]] == gsub("[- ]", "_", txt$dt_Mixernummer[[ j ]]$beverage) ]
                                    , txt$dt_Mixernummer[[ j ]]$Mixer_ID_3[ txt$beverage.sub[[ j ]][[ i ]] == gsub("[- ]", "_", txt$dt_Mixernummer[[ j ]]$beverage) ])

        txt$DTProductNumber[[ i ]] <- txt$dt_Mixernummer[[ j ]]$DTProductNumber[ txt$beverage.sub[[ j ]][[ i ]] == gsub("[- ]", "_", txt$dt_Mixernummer[[ j ]]$beverage) ]
      }

      for(i in 1 : length( txt$Mixernummer))
        if( length( txt$Mixernummer[[ i ]] ) > 0 ) txt$Mixernummer[[ i ]] <- sort( txt$Mixernummer[[ i ]][ txt$Mixernummer[[ i ]] != 0] )

      for(i in 1 : length( txt$DTProductNumber))
        if( length( txt$DTProductNumber[[ i ]] ) > 0 ) txt$DTProductNumber[[ i ]] <- sort( txt$DTProductNumber[[ i ]][ txt$DTProductNumber[[ i ]] != 0] )

      txt$beverage[[ j ]] <- paste(txt$beverage.sub[[ j ]], collapse = "_")
    }
  }
  return(txt)
}

wd_customer <- function(wd = dt$pfad$wd
                        , customer = unique(txt$dt_customer$customer)){

  dat <- lapply(customer, function( x ) gregexpr( x, wd)[[ 1 ]][ 1 ])
  dat <- unlist(dat)
  return( as.character( customer[ which( dat > 0) ]))

}

wd_location <- function(wd = dt$pfad$wd
                        , location = unique(txt$dt_customer$location)){

  dat <- lapply(location, function( x ) gregexpr( x, wd)[[ 1 ]][ 1 ])
  dat <- unlist(dat)
  return( as.character( location[ which( dat > 0) ]))

}

wd_line <- function(wd = dt$pfad$wd
                    , line = unique(txt$dt_customer$line)){

  dat <- lapply(line, function( x ) gregexpr( x, wd)[[ 1 ]][ 1 ])
  dat <- unlist(dat)
  return( as.character( line[ which( dat > 0) ]))

}



