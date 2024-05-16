fastplot.run <- function(  export_directory = dt$export_directory
                           , firstday = dt$firstday
                           , lastday = dt$lastday
                           , location = dt$info$location
                           , line = dt$line
                           , raw.export = dt$raw
                           , export = T
                           , to_predict = dt$to_predict){

  library( hms )
  setwd( export_directory )

  files <- raw.export$csv.file.names
  raw.export <- raw.export[ -length(raw.export)]
  raw.export <- lapply( raw.export, data.table)

  filessort <- list()
  for(i in 1 : length( files))
    filessort[[ which( names( files ) %in% names( raw.export )[ i ]) ]] <- names( files )[ i ]
  files <- files[  unlist(filessort) ]

  mapply( function( raw, file ) fastplot( spcdat = raw, filename = file, export = export, dt_Mixernummer = dt_Mixernummer, to_predict = to_predict)
          , raw = raw.export
          , file = files)

}

fastplot <- function( spcdat, filename
                      , day_week_year_month = c(2, 30)
                      , nm = c(200, "max", 486)
                      , ylim = NULL
                      , xaxistype = "points" #time
                      , export = T
                      , GUI = F
                      , firstday = dt$firstday
                      , lastday = dt$lastday
                      , location = dt$info$location
                      , line = dt$line
                      , dt_Mixernummer = dt_Mixernummer
                      , to_predict = NA){

  fplot <- list()

  fplot$raw <- spcdat
  if( !is.data.table(fplot$raw)) fplot$raw <- data.table(fplot$raw)
  fplot$ppp <- transfer_csv.num.col( spcdat )

  fplot$date <- as.Date( spcdat$datetime )
  fplot$week <- formatC( week( fplot$date ), width = 2, flag = "0")
  fplot$month <- formatC( month( fplot$date ), width = 2, flag = "0")
  fplot$year <- year( fplot$date )
  fplot$year.month <- paste0( fplot$year, "-", fplot$month )
  fplot$year.week <- paste0( fplot$year, "_KW", fplot$week )

  # file info
  if(export == T) fplot$info <- txt.file( filename)

  if(export != T & GUI == F) fplot$info$type <- export

  # type ####
  if( fplot$info$type == "spc") fplot$type = "Produktions-Spektren"
  if( fplot$info$type == "ref") fplot$type = "Referenz-Spektren"
  if( fplot$info$type == "drk") fplot$type = "Dunkelwert-Spektren"

  if( fplot$info$type == "spc") fplot$par$ylab = "AU"
  if( fplot$info$type == "ref") fplot$par$ylab = "Counts"
  if( fplot$info$type == "drk") fplot$par$ylab = "Counts"

  # set colors according to number of datetimes or dates ####
  if( length( unique( fplot$date ) ) <= day_week_year_month[ 1 ] ){

    fplot$par$colp <- colorRamps::blue2red( length( spcdat$datetime ) )
    fplot$par$colp2 <- fplot$par$colp[ factor( spcdat$datetime )]
    fplot$par$legendtext <- as.character( unique( fplot$date ) )
    fplot$par$legendtext <- substr(fplot$par$legendtext, 3, 10)
    fplot$par$xaxistype <- "day"

  }

  if( length( unique( fplot$date ) ) == 1 ){

    fplot$par$colp <- colorRamps::blue2red( length( spcdat$datetime ) )
    fplot$par$colp2 <- fplot$par$colp[ factor( spcdat$datetime )]

    if(length( as.character( unique( spcdat$datetime ) )) <= 35) fplot$par$legendtext <- fplot$par$legendtext <- as.character( unique( spcdat$datetime ) )
    if(length( as.character( unique( spcdat$datetime ) )) > 35){
      fplot$par$legendtext <- as.character( unique( fplot$date ) )
      fplot$par$legendtext <- substr(fplot$par$legendtext, 3, 10)
    }
    fplot$par$xaxistype <- "time"

  }

  if( length( unique( fplot$date ) ) > day_week_year_month[ 1 ] & length( unique( fplot$date ) ) <= day_week_year_month[ 2 ]  ){

    fplot$par$colp <- colorRamps::blue2red( length( unique( fplot$date ) ))
    fplot$par$colp2 <- fplot$par$colp[ factor( fplot$date )]
    fplot$par$legendtext <- as.character( unique( fplot$date ) )
    fplot$par$legendtext <- substr(fplot$par$legendtext, 3, 10)
    fplot$par$xaxistype <- "day"

  }

  if( length( unique( fplot$date ) ) > day_week_year_month[ 2 ] ){

    fplot$par$colp <- colorRamps::blue2red( length( unique( fplot$year.week ) ))
    fplot$par$colp2 <- fplot$par$colp[ factor( fplot$year.week )]
    fplot$par$legendtext <- as.character( unique( fplot$year.week ) )
    fplot$par$xaxistype <- "year-month"

  }

  if( any( is.na( fplot$par$legendtext ))) fplot$par$legendtext <- fplot$par$legendtext[ !is.na( fplot$par$legendtext )]

  # Products & Parameter ####
  # LG
  if( as.character( fplot$info$type ) == "spc" & length( grep( "Integrationszeit", names( fplot$raw )) ) > 0) {

    if( length( grep( "ypred", names ( fplot$raw), value = T) ) > 0){

      for(i in 1 : length( grep( "ypred", names ( fplot$raw), value = T) )){

        fplot$pro$para[[ i ]] <- grep( "ypred", names ( fplot$raw), value = T)[ i ]
        fplot$pro$unit[[ i ]] <- substr( fplot$pro$para[[ i ]], unlist( gregexpr(" in ", fplot$pro$para[[ i ]]) ) + 4, unlist( gregexpr("ypred", fplot$pro$para[[ i ]]) ) - 2)
        fplot$pro$para[[ i ]] <- substr( fplot$pro$para[[ i ]], 1, unlist( gregexpr(" ", fplot$pro$para[[ i ]]) ) - 1)

      }
    }
  }

  # LG
  if( as.character( fplot$info$type ) == "spc" & length( grep( "integrationTime", names( fplot$raw )) ) > 0) {

    if( length( grep( "yPredictedCorr", names ( fplot$raw), value = T) ) > 0){

      for(i in 1 : length( grep( "yPredictedCorr", names ( fplot$raw), value = T) )){

        fplot$pro$para[[ i ]] <- grep( "yPredictedCorr", names ( fplot$raw), value = T)[ i ]
        fplot$pro$unit[[ i ]] <- "%"
        fplot$pro$para[[ i ]] <- substr( fplot$pro$para[[ i ]], 1, unlist( gregexpr("yPredictedCorr", fplot$pro$para[[ i ]]) ) - 1)
        fplot$pro$check[[ i ]] <- any( as.character( unlist( fplot$raw[ , grep( fplot$pro$para[[ i ]], names( fplot$raw))[ 1 ], with = F] )) == "NULL" )
        if( is.na(fplot$pro$check[[ i ]])) fplot$pro$check[[ i ]] <- F
      }

      if( any( unlist( fplot$pro$check))){
        for(i in which(unlist( fplot$pro$check ))){
          fplot$pro$para[[ i ]] <- NULL
          fplot$pro$unit[[ i ]] <- NULL
        }
      }

    }
  }

  if( as.character( fplot$info$type ) == "spc"){

    if( as.character( location != "Dorsten" ) )
      fplot$pro$beverage <- as.character( unlist( fplot$dt_Mixernummer[ fplot$dt_Mixernummer$location == location , "beverage"]))[
        which( unlist( lapply( as.character( unlist( fplot$dt_Mixernummer[ fplot$dt_Mixernummer$location == location , "beverage"]))
                               , function( x ) gregexpr( x, filename))) > 0)
      ]

    if( as.character( location == "Dorsten" ) )
      fplot$pro$beverage <- as.character( unlist( fplot$dt_Mixernummer[ fplot$dt_Mixernummer$location == location & fplot$dt_Mixernummer$line == line , "beverage"]))[
        which( unlist( lapply( as.character( unlist( fplot$dt_Mixernummer[ fplot$dt_Mixernummer$location == location & fplot$dt_Mixernummer$line == line , "beverage"]))
                               , function( x ) gregexpr( x, filename))) > 0)
      ]

    if( length( fplot$pro$beverage > 1)){

      fplot$pro$beverage <- fplot$pro$beverage[ which.max( nchar( fplot$pro$beverage ) ) ]

    }

    if( length( fplot$pro$beverage > 0)){

      fplot$type <- paste0( fplot$type, " von ", fplot$pro$beverage)

    }
  }

  # main ####
  fplot$par$main <- fplot$type

  fplot$exportfile <- paste0(fplot$info$datefrom
                             , "_"
                             , fplot$info$dateto
                             , "_"
                             , fplot$info$location
                             , "_"
                             , fplot$info$line
                             , "_"
                             , fplot$info$beverage
                             , "_"
                             , ifelse( length( sort( unlist( fplot$info$Mixernummer ) ) ) > 0, paste0( unlist( fplot$info$Mixernummer ), collapse = "_"), "")
                             , "_"
                             , ifelse( length( unlist( fplot$info$DTProductNumber) ) > 0 & any(unique( unlist( fplot$info$DTProductNumber )) != 0), paste0(paste0("DT", unlist( fplot$info$DTProductNumber )), collapse = "_"), "")
                             , "_"
                             , ifelse( length( fplot$info$fileinfo ) > 0, fplot$info$fileinfo, "")
                             , "_"
                             , fplot$info$type
                             , ".png")

  fplot$exportfile <- gsub("[- ]", "", fplot$exportfile)
  fplot$exportfile <- gsub("__", "_", gsub("__", "_", gsub("__", "_", gsub("NA", "", fplot$exportfile))))

  # png layout par ####
  if(export == T & GUI == F) graphics.off()
  if(export == T & GUI == F) png( fplot$exportfile
                                  , xxx<-4800,xxx/16*9,"px",12,"white",res=500,"Eurostile Next LT Pro",T,"cairo")

  par( col.main = dauschblue, col.axis = dauschblue, col.lab = dauschblue)

  # LG2 ref and drk
  if( as.character( fplot$info$type ) != "spc" & length( grep( "Integrationszeit", names( fplot$raw )) ) > 0){
    layout(x <- matrix(c(0, 0, 0,
                    1, 1, 2,
                    1, 1, 3,
                    4, 5, 6)+1, nrow=4, byrow=TRUE), heights = heights <- c(.2, 1, 1, 1))
    max_plot <- max(x)
    }

  # LG2 spc
  if( as.character( fplot$info$type ) == "spc" & length( grep( "Integrationszeit", names( fplot$raw )) ) > 0){
    layout(x <- matrix(c(0, 0, 0,
                    1, 1, 2,
                    1, 1, 3,
                    4, 5, 6)+1, nrow=4, byrow=TRUE), heights = heights <- c(.2, 1, 1, 1))
    max_plot <- max(x)
    }

  # LG3 ref and drk
  if( as.character( fplot$info$type ) != "spc" & length( grep( "integrationTime", names( fplot$raw )) ) > 0){
    layout(x <- matrix(c(0, 0, 0, 0,
                    1, 1, 2, 5,
                    1, 1, 3, 7,
                    10, 11, 4, 6,
                    12,13,8,9)+1, nrow=5, byrow=TRUE), heights = heights <- c(.3, 1, 1, 1, 1))
  max_plot <- max(x)
  }

  # LG3 spc
  if( as.character( fplot$info$type ) == "spc" & length( grep( "integrationTime", names( fplot$raw )) ) > 0){
    layout(x <- matrix(c(0, 0, 0, 0, 0,
                    1, 1, 2, 4, 6,
                    1, 1, 3, 5, 7,
                    8, 9, 10, 11, 12) + 1, nrow=4, byrow=TRUE), heights = heights <- c(.2, 1, 1, 1))
  max_plot <- max(x)}

  # logo ####
  n_plots <- 0
  par(mar = c(0,0,0,0))
  plot(.5,.5,type = "n", xlab = "", ylab = "", axes = F, ylim = c(0, 1), xlim = c(0, 1))
  n_plots <- n_plots + 1

  segments(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[3], xpd = T, col = dauschblue)
  dauschlogo.draw( y = 1 - (heights[1] / sum( heights ) / 2), size = 1.3 )

  text(x1 <- .03, y1 <- .9
       , paste0( "Standort ", fplot$info$location
                 , ", Linie = ",  fplot$info$line
                 , "\n", fplot$type, " vom "
                 , format( as.Date(firstday), "%e. %B %Y"), " bis zum ", format( as.Date(lastday), "%e. %B %Y")
                 , ifelse(length(fplot$info$fileinfo) > 0, paste0(", Filter = ", fplot$info$fileinfo), "")
                 , "\n"
                 , ifelse(nchar(fplot$info$beverage) > 0, paste0("Getränk = ", fplot$info$beverage), "")
                 , ifelse( length( sort( unlist( fplot$info$Mixernummer ) ) ) > 0
                           , paste0(", Mixernummer = ", gsub("^c\\(|\\)$", "", paste0(fplot$info$Mixernummer, collapse = ", "))), "")
                 , ifelse( length( unlist( fplot$info$DTProductNumber) ) > 0 & any( unlist( fplot$info$DTProductNumber) != 0)
                           , paste0(", DTProductNumber = ", gsub("_", ", ", gsub("DT", "", paste0(paste0("DT", unlist( fplot$info$DTProductNumber )), collapse = "_"))))
                           , ""))
       , col = dauschblue, adj = c(0, 1)
       , cex = ifelse(GUI, .6, .8))

  text(x1 <- .825, y1 <- .9, paste0(paste0("Erstellt am ", as.character( format(as.POSIXct(Sys.time(), format="%Y-%m-%d %H:%M:%OS", tz="UTC"), "%d. %B %Y um %H:%M:%S"))
                                           , "\nErsteller = ", Sys.getenv("USERNAME")
                                           , "\nr4dtuser-Version = ", paste(packageDescription("r4dtuser")[4:5], collapse = " ")))
       , col = dauschblue, adj = c(0, 1)
       , cex = ifelse(GUI, .6, .8))

  # par ####
  if(export == T & GUI == F) par(mar=c(2.75,4,1.25,6), cex.main = .95, cex.axis = cex.axis <- .65, las = 2)
  if(export == T & GUI == T) par(mar=c(2.75,4,1.25,6), cex.main = .95, cex.axis = cex.axis <- .65, las = 2)
  if(export != T) par(mar=c(1.75,3,1,4), cex.main = .7, cex.axis = cex.axis <- .4, las = 2)

  cex.pch = .65

  if(export == T & length( fplot$par$legendtext ) < 25) cexlegend = .7
  if(export == T & length( fplot$par$legendtext ) >= 25) cexlegend = .65
  if(export == T & length( fplot$par$legendtext ) >= 35) cexlegend = .5

  if(export != F & length( fplot$par$legendtext ) < 25) cexlegend = .7
  if(export != F & length( fplot$par$legendtext ) >= 25) cexlegend = .65
  if(export != F & length( fplot$par$legendtext ) >= 35) cexlegend = .5

  if( any( nchar( fplot$par$legendtext ) > 18)) cexlegend = cexlegend * .8

  if(export == T & length( fplot$par$legendtext ) >= 45) cexlegend = .35
  if(export != F & length( fplot$par$legendtext ) >= 45) cexlegend = .35

  # matplot ####
  if( nrow(  fplot$raw ) <= 1000){

    matplot(fplot$ppp$wl
            , t( fplot$raw[ , fplot$ppp$numcol, with = F] )
            , lty = 1, type = "l"
            , xlab = "", ylab = ""
            , col = fplot$par$colp2
            , main = fplot$par$main
            , ylim = ylim, axes = F)
    n_plots <- n_plots + 1

    box(col = dauschblue)
    axis(1, las = 1)
    axis(2, las = 2)
    mtext(lambda, 1, 2, las = 1, col = dauschblue, cex = cex.axis)
    mtext(fplot$par$ylab, 2, 3, las = 3, col = dauschblue, cex = cex.axis)

    # if(export != T) mtext(lambda, 1, 3, las = 1, col = dauschblue, cex = cex.axis)
  }

  if( nrow(  fplot$raw ) > 1000 & nrow(  fplot$raw ) <= 5000){

    matplot(fplot$ppp$wl
            , t( fplot$raw[ seq( 1, nrow( fplot$raw), 5) , fplot$ppp$numcol, with = F] )
            , lty = 1, type = "l"
            , xlab = lambda, ylab = ""
            , col = fplot$par$colp2[ seq( 1, nrow( fplot$raw), 5) ]
            , main = paste0(fplot$par$main, ", jedes 5. Spektrum")
            , ylim = ylim, axes = F)
    n_plots <- n_plots + 1

    box(col = dauschblue)
    axis(1, las = 1)
    axis(2, las = 2)
    mtext(lambda, 1, 2, las = 1, col = dauschblue, cex = cex.axis)
    mtext(fplot$par$ylab, 2, 3, las = 3, col = dauschblue, cex = cex.axis)
    # if(export != T) mtext(lambda, 1, 3, las = 1, col = dauschblue, cex = cex.axis)
  }

  if( nrow(  fplot$raw ) > 5000 & nrow(  fplot$raw ) <= 20000){

    matplot(fplot$ppp$wl
            , t( fplot$raw[ seq( 1, nrow( fplot$raw), 10) , fplot$ppp$numcol, with = F] )
            , lty = 1, type = "l"
            , xlab = lambda, ylab = ""
            , col = fplot$par$colp2[ seq( 1, nrow( fplot$raw), 10) ]
            , main = paste0(fplot$par$main, ", jedes 10. Spektrum")
            , ylim = ylim, axes = F)
    n_plots <- n_plots + 1

    box(col = dauschblue)
    axis(1, las = 1)
    axis(2, las = 2)
    mtext(lambda, 1, 2, las = 1, col = dauschblue, cex = cex.axis)
    mtext(fplot$par$ylab, 2, 3, las = 3, col = dauschblue, cex = cex.axis)
    # if(export != T) mtext(lambda, 1, 3, las = 1, col = dauschblue, cex = cex.axis)
  }

  if( nrow(  fplot$raw ) > 20000){

    matplot(fplot$ppp$wl
            , t( fplot$raw[ seq( 1, nrow( fplot$raw), 100) , fplot$ppp$numcol, with = F] )
            , lty = 1, type = "l"
            , xlab = lambda, ylab = ""
            , col = fplot$par$colp2[ seq( 1, nrow( fplot$raw), 100) ]
            , main = paste0(fplot$par$main, ", jedes 100. Spektrum")
            , ylim = ylim, axes = F)
    n_plots <- n_plots + 1

    box(col = dauschblue)
    axis(1, las = 1)
    axis(2, las = 2)
    mtext(lambda, 1, 2, las = 1, col = dauschblue, cex = cex.axis)
    mtext(fplot$par$ylab, 2, 3, las = 3, col = dauschblue, cex = cex.axis)
    # if(export != T) mtext(lambda, 1, 3, las = 1, col = dauschblue, cex = cex.axis)
  }

  legend(par("usr")[ 2 ]
         , par("usr")[ 4 ]
         , fplot$par$legendtext
         , lty = 1, adj = 0
         , ncol = ifelse( length(  fplot$par$legendtext ) > 30, 2, 1)
         , col = fplot$par$colp
         , cex = cexlegend
         , xpd = T
         , lwd = .75)

  if(export == T & GUI == F) par(mar=c(2.75,3.5,1.25,.25), cex.main = .95, cex.axis = .65, las = 2)
  if(export != T | GUI == T) par(mar=c(2.75,3.5,1.25,.25), cex.main = .95, cex.axis = .65, las = 2)

  if( as.character( fplot$info$type ) != "spc" | length( grep( "Integrationszeit", names( fplot$raw )) ) > 0){
    if( length( grep( "integrationTime", names( fplot$raw )) ) > 0) fplot$Integrationszeit <- as.numeric( unlist( fplot$raw[ , "integrationTime"] ))
    if( length( grep( "Integrationszeit", names( fplot$raw )) ) > 0) fplot$Integrationszeit <- as.numeric( unlist( fplot$raw[ , "Integrationszeit"] ))

    if( xaxistype == "time"){
      plot( fplot$raw$datetime, as.numeric( fplot$Integrationszeit  )
            , col = fplot$par$colp2
            , xlab = "", ylab = ""
            , main = "Integrationszeit in ms"
            , cex = ifelse(GUI, cex.pch, 1))
      n_plots <- n_plots + 1
    }

    if( xaxistype == "points"){
      plot( as.numeric( fplot$Integrationszeit  )
            , col = fplot$par$colp2
            , xlab = "", ylab = ""
            , main = "Integrationszeit in ms", axes = F
            , cex = ifelse(GUI, cex.pch, 1))
      n_plots <- n_plots + 1

      axis( 2 )

      xaxisdate( fplot$raw$datetime, type = NA, formatd = fplot$par$xaxistype, las = 2, cex = cex.axis)
    }

    if( length( grep( "integrationTime", names( fplot$raw )) ) > 0) fplot$Mittelungen <- as.numeric( unlist( fplot$raw[ , "accumulations"] ))
    if( length( grep( "Mittelungen", names( fplot$raw )) ) > 0) fplot$Mittelungen <- as.numeric( unlist( fplot$raw[ , "Mittelungen"] ))

    if( xaxistype == "time"){
      plot( fplot$raw$datetime, as.numeric( fplot$Mittelungen  )
            , col = fplot$par$colp2
            , xlab = "", ylab = ""
            , main = "Mittelungen"
            , cex = ifelse(GUI, cex.pch, 1))
      n_plots <- n_plots + 1
    }

    if( xaxistype == "points"){
      plot( as.numeric( fplot$Mittelungen  )
            , col = fplot$par$colp2
            , xlab = "", ylab = ""
            , main = "Mittelungen", axes = F
            , cex = ifelse(GUI, cex.pch, 1))
      n_plots <- n_plots + 1

      axis( 2 )

      xaxisdate( fplot$raw$datetime, type = NA, formatd = fplot$par$xaxistype, las = 2, cex = cex.axis)
    }
  }

  # LG2 ref and drk ####
  if( length( grep( "Integrationszeit", names( fplot$raw )) ) > 0){
    nmp <- nm
    for(i in 1 : length( nm )){
      if( as.character( fplot$info$type ) == "spc" & i == 1) nm[ i ] <- 273
      if( as.character( fplot$info$type ) == "spc" ) if( i == 2 ) next # skip nm plot to  make space!

      if( nm[ i ] != "max" | fplot$info$type != "ref") mainp <- paste0( fplot$par$ylab, " bei ", nm[ i ], " nm")
      if( nm[ i ] == "max" & fplot$info$type == "ref") mainp <- paste0( fplot$par$ylab, " bei ", nm[ i ], " nm, Maximum-Peak")
      if( nm[ i ] == "max" & fplot$info$type == "drk") mainp <- paste0( fplot$par$ylab, " bei ", nm[ i ], " nm, Maximum-Peak")

      if( nm[ i ] == "max"  & fplot$info$type == "ref") nmp[ i ] <- fplot$ppp$wl[ as.numeric(names(sort(table(sort( unlist( apply(fplot$raw[ , fplot$ppp$numcol, with = F], 1, which.max)))), decreasing = T))[ 1 ]) ]
      if( nm[ i ] == "max"  & fplot$info$type == "drk") nmp[ i ] <- fplot$ppp$wl[ as.numeric(names(sort(table(sort( unlist( apply(fplot$raw[ , fplot$ppp$numcol, with = F], 1, which.max)))), decreasing = T))[ 1 ]) ]
      # if( nm[ i ] != "max" | fplot$info$type != "ref") nmp[ i ] <- "259"

      mainp <- gsub("max", nmp[ i ], mainp)

      # if( nm[ i ] == "max"  & fplot$info$type == "ref") ylimp <- c(0, 65000)
      # if( nm[ i ] != "max" ) ylimp <- ylim

      if( xaxistype == "time"){
        plot( fplot$raw$datetime, as.numeric( unlist( fplot$raw[ , grep( nmp[ i ] , names ( fplot$raw)), with = F] ))
              , col = fplot$par$colp2
              , xlab = "", ylab = ""
              , main = mainp
              , ylim = ylim
              , cex.axis = cex.axis * .85
              , cex = ifelse(GUI, cex.pch, 1))
      n_plots <- n_plots + 1
      }

      if( xaxistype == "points"){
        plot( as.numeric( unlist( fplot$raw[ , grep( nmp[ i ] , names ( fplot$raw)), with = F] ))
              , col = fplot$par$colp2
              , xlab = "", ylab = ""
              , main = mainp
              , ylim = ylim
              , axes = F
              , cex = ifelse(GUI, cex.pch, 1))
        n_plots <- n_plots + 1

        xaxisdate( fplot$raw$datetime, type = NA, formatd = fplot$par$xaxistype, las = 2, cex = cex.axis)
      }
    }
  }

  # LG3 ref and drk and spc ####
  if( length( grep( "integrationTime", names( fplot$raw )) ) > 0){

    fplot$LG3$refdrkcol <- c( "SpectrometerTemperature", "FluidFlow", "FluidTemperature", "FluidPressure", "RackTemperature", "AmbientTemperature")

    for(i in 1 : length( fplot$LG3$refdrkcol)){

      if(all( is.na( as.numeric( unlist( fplot$raw[ , grep( fplot$LG3$refdrkcol[ i ] , names ( fplot$raw)), with = F] )) ))) next
      if( xaxistype == "time"){
        plot( fplot$raw$datetime, as.numeric( unlist( fplot$raw[ , grep( fplot$LG3$refdrkcol[ i ] , names ( fplot$raw)), with = F] ))
              , col = fplot$par$colp2
              , xlab = "", ylab = ""
              , main = fplot$LG3$refdrkcol[ i ]
              , cex.axis = cex.axis * .85
              , cex = ifelse(GUI, cex.pch, 1))
        n_plots <- n_plots + 1
      }

      if( xaxistype == "points"){
        plot( as.numeric( unlist( fplot$raw[ , grep( fplot$LG3$refdrkcol[ i ] , names ( fplot$raw)), with = F] ))
              , col = fplot$par$colp2
              , xlab = "", ylab = ""
              , main = fplot$LG3$refdrkcol[ i ]
              , axes = F
              , cex.axis = cex.axis * .85
              , cex = ifelse(GUI, cex.pch, 1))
        n_plots <- n_plots + 1

        xaxisdate( fplot$raw$datetime, type = NA, formatd = fplot$par$xaxistype, las = 2, cex = cex.axis)
      }
    }

    nmp <- nm
    for(i in 1 : length( nm )){
      if( as.character( fplot$info$type ) == "spc" & i == 1) nm[ i ] <- 273
      if( as.character( fplot$info$type ) == "spc" & i == 1) nmp[ i ] <- 273
      if( as.character( fplot$info$type ) == "spc" ) if( i == 2 ) next # skip nm plot to  make space!
      if( as.character( fplot$info$type ) == "spc" ) if( i == 3 ) next # skip nm plot to  make space!

      if( nm[ i ] != "max" | fplot$info$type != "ref") mainp <- paste0( fplot$par$ylab, " bei ", nm[ i ], " nm")
      if( nm[ i ] == "max" & fplot$info$type == "ref") mainp <- paste0( fplot$par$ylab, " bei ", nm[ i ], " nm, Maximum-Peak")
      if( nm[ i ] == "max" & fplot$info$type == "drk") mainp <- paste0( fplot$par$ylab, " bei ", nm[ i ], " nm, Maximum-Peak")

      if( nm[ i ] == "max"  & fplot$info$type == "ref") nmp[ i ] <- fplot$ppp$wl[ as.numeric(names(sort(table(apply(fplot$raw[ , fplot$ppp$numcol, with = F], 1, which.max)), decreasing = T))[ 1 ]) ]
      if( nm[ i ] == "max"  & fplot$info$type == "drk") nmp[ i ] <- fplot$ppp$wl[ as.numeric(names(sort(table(apply(fplot$raw[ , fplot$ppp$numcol, with = F], 1, which.max)), decreasing = T))[ 1 ]) ]
      # if( nm[ i ] != "max" | fplot$info$type != "ref") nmp[ i ] <- "259"

      mainp <- gsub("max", nmp[ i ], mainp)

      # if( nm[ i ] == "max"  & fplot$info$type == "ref") ylimp <- c(0, 65000)
      # if( nm[ i ] != "max" ) ylimp <- ylim

      if( xaxistype == "time"){
        plot( fplot$raw$datetime, as.numeric( unlist( fplot$raw[ , grep( nmp[ i ] , names ( fplot$raw)), with = F] ))
              , col = fplot$par$colp2
              , xlab = "", ylab = ""
              , main = mainp
              , ylim = ylim
              , cex.axis = cex.axis * .85
              , cex = ifelse(GUI, cex.pch, 1))
        n_plots <- n_plots + 1
      }

      if( xaxistype == "points"){
        plot( as.numeric( unlist( fplot$raw[ , grep( nmp[ i ] , names ( fplot$raw)), with = F] ))
              , col = fplot$par$colp2
              , xlab = "", ylab = ""
              , main = mainp
              , ylim = ylim
              , axes = F
              , cex = ifelse(GUI, cex.pch, 1))
        n_plots <- n_plots + 1

        xaxisdate( fplot$raw$datetime, type = NA, formatd = fplot$par$xaxistype, las = 2, cex = cex.axis)
      }
    }
  }

  # LG2 spc ####
  if( as.character( fplot$info$type ) == "spc" & length( grep( "Integrationszeit", names( fplot$raw )) ) > 0){

    if( length( grep( "ypred", names ( fplot$raw), value = T) ) > 0){

      if( xaxistype == "time")
        for(i in ifelse( is.na(to_predict), 1, which(fplot$pro$para %in% to_predict))){
          plot( fplot$raw$datetime, as.numeric( unlist( fplot$raw[ , grep( "ypred", names ( fplot$raw))[ i ], with = F] ))
                , col = fplot$par$colp2
                , xlab = "", ylab = ""
                , main = paste0( fplot$pro$para[[ i ]], " in ", fplot$pro$unit[[ i ]])
                , cex = ifelse(GUI, cex.pch, 1))
                n_plots <- n_plots + 1

        }

      if( xaxistype == "points"){
        for(i in ifelse( is.na(to_predict), 1, which(fplot$pro$para %in% to_predict))){
          plot( as.numeric( unlist( fplot$raw[ , grep( "ypred", names ( fplot$raw))[ i ], with = F] ))
                , col = fplot$par$colp2
                , xlab = "", ylab = ""
                , main = paste0( fplot$pro$para[[ i ]], " in ", fplot$pro$unit[[ i ]])
                , axes = F
                , cex = ifelse(GUI, cex.pch, 1))
          n_plots <- n_plots + 1

          axis( 2 )

          xaxisdate( fplot$raw$datetime, type = NA, formatd = fplot$par$xaxistype, las = 2, cex = cex.axis)
        }
      }
    }
  }

  # LG3 spc ####
  if( as.character( fplot$info$type ) == "spc" & length( grep( "integrationTime", names( fplot$raw )) ) > 0){

    if( length( fplot$pro$para ) > 0){

      if( xaxistype == "time")
        for(i in unique(c(which(fplot$pro$para %in% to_predict), 1 : length( fplot$pro$para )))       ){
          if(i > 2) next
          if(n_plots > max_plot) next

          if( all( is.na( as.numeric( unlist( fplot$raw[ , grep( paste0( fplot$pro$para[ i ], "yPredictedCorr"), names ( fplot$raw)), with = F] ))))) next

          plot( fplot$raw$datetime, as.numeric( unlist( fplot$raw[ , grep( paste0( fplot$pro$para[ i ], "yPredictedCorr"), names ( fplot$raw)), with = F] ))
                , col = fplot$par$colp2
                , xlab = "", ylab = ""
                , main = paste0( fplot$pro$para[[ i ]], " in ", fplot$pro$unit[[ i ]])
                , cex = ifelse(GUI, cex.pch, 1))
          n_plots <- n_plots + 1

        }

      if( xaxistype == "points"){
        for(i in unique(c(which(fplot$pro$para %in% to_predict), 1 : length( fplot$pro$para )))       ){
          if(i > 2) next
          if(n_plots > max_plot) next

          if( all( is.na( as.numeric( unlist( fplot$raw[ , grep( paste0( fplot$pro$para[ i ], "yPredictedCorr"), names ( fplot$raw)), with = F] ))))) next

          plot( as.numeric( unlist( fplot$raw[ , grep( paste0( fplot$pro$para[ i ], "yPredictedCorr"), names ( fplot$raw)), with = F] ))
                , col = fplot$par$colp2
                , xlab = "", ylab = ""
                , main = paste0( fplot$pro$para[[ i ]], " in ", fplot$pro$unit[[ i ]])
                , axes = F
                , cex = ifelse(GUI, cex.pch, 1))
          n_plots <- n_plots + 1

          axis( 2 )

          xaxisdate( fplot$raw$datetime, type = NA, formatd = fplot$par$xaxistype, las = 2, cex = cex.axis)
        }
      }
    }
  }

  # LG3 spc extern ####
  if( as.character( fplot$info$type ) == "spc" & length( grep( "integrationTime", names( fplot$raw )) ) > 0){

    fplot$LG3$extern <- c( "brix", "diet", "co2", "conductivity")
    fplot$LG3$extern <- fplot$LG3$extern[ fplot$LG3$extern %in% names( fplot$raw ) ]

    fplot$LG3$extern <- as.list(fplot$LG3$extern)

    if( length( fplot$LG3$extern ) != 0){

      for(i in 1 : length( fplot$LG3$extern ))
        fplot$pro$externcheck[[ i ]] <- all( as.character( unlist( fplot$raw[ , grep( fplot$LG3$extern[[ i ]], names( fplot$raw)), with = F] )) == "NULL" )

      for(i in length( fplot$LG3$extern ) : 1){
        if( is.na(  fplot$pro$externcheck[[ i ]] ) ) fplot$LG3$extern[[ i ]] <- NULL
        if( is.na(  fplot$pro$externcheck[[ i ]] ) ) fplot$pro$externcheck[[ i ]] <- NULL
      }

      if( any( unlist( fplot$pro$externcheck))){
        for(i in which(unlist( fplot$pro$externcheck ))){
          fplot$LG3$extern[ i ] <- NULL
        }
      }

      if( length( fplot$LG3$extern) > 3)  fplot$LG3$extern <- fplot$LG3$extern[ c(1, 3, 4)]
      #if( length( fplot$LG3$para) > 1)
      fplot$LG3$extern <- fplot$LG3$extern[ c(1, 2)]

      for(i in 1 : length( fplot$LG3$extern)){

        if(n_plots > max_plot) next
        if( all( is.na( as.numeric( unlist( fplot$raw[ , grep( fplot$LG3$extern[ i ] , names ( fplot$raw)), with = F] ))))) next

        if( xaxistype == "time"){
          plot( fplot$raw$datetime, as.numeric( unlist( fplot$raw[ , grep( fplot$LG3$extern[ i ] , names ( fplot$raw)), with = F] ))
                , col = fplot$par$colp2
                , xlab = "", ylab = ""
                , main = fplot$LG3$extern[ i ]
                , cex = ifelse(GUI, cex.pch, 1))
          n_plots <- n_plots + 1
          }

        if( xaxistype == "points"){
          plot( as.numeric( unlist( fplot$raw[ , grep( fplot$LG3$extern[ i ] , names ( fplot$raw)), with = F] ))
                , col = fplot$par$colp2
                , xlab = "", ylab = ""
                , main = fplot$LG3$extern[ i ]
                , axes = F
                , cex = ifelse(GUI, cex.pch, 1))
          n_plots <- n_plots + 1

          axis( 2 )

          xaxisdate( fplot$raw$datetime, type = NA, formatd = fplot$par$xaxistype, las = 2, cex = cex.axis)
        }
      }
    }
  }

  if(export == T & GUI == F) dev.off()
  if( export == T) return( fplot$exportfile )
}
