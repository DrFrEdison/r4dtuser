wdfind <- function( force = NA # c(NA, "Map", "OneDrive", "UNC")
                    , wdstr = "/r4u/master/r4dt_path_location.R"){

  if( is.na(force)){

    if( Sys.getenv("OneDriveCommercial") != "" )
      wdpath <-paste0(Sys.getenv("OneDriveCommercial"), "/FE_Methoden", wdstr)

    if( !identical( dir( path = "//DC-01.dausch-tech.lan/OneDrive/OneDrive - Dausch Technologies GmbH/FE_Methoden/")
                    , character(0)) & identical( dir( path = "I:/"), character(0)) ){

      wdpath <- "//DC-01.dausch-tech.lan/OneDrive/OneDrive - Dausch Technologies GmbH/FE_Methoden"
      wdpath <- gsub("/", "\\\\", wdpath)
      wdpath <- paste0(wdpath, wdstr)

    }

    if( !identical( dir( path = "I:/"), character(0)) )
      wdpath <- paste0("I:", wdstr)
  }

  if( !is.na(force)){
    if( force == "Map")  wdpath <- paste0("I:", wdstr)
    if( force == "OneDrive")  wdpath <- paste0(Sys.getenv("OneDriveCommercial"), "/FE_Methoden", wdstr)
    if( force == "UNC"){

      wdpath <- "//DC-01.dausch-tech.lan/OneDrive/OneDrive - Dausch Technologies GmbH/FE_Methoden"
      wdpath <- gsub("/", "\\\\", wdpath)
      wdpath <- paste0(wdpath, wdstr)

    }
  }

  return( wdpath )
}
