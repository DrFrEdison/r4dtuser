DTProductNumber <- function( customer, dir_wd){

  if( customer == "CCEP"){
    dt_DTProductNumber <- openxlsx::read.xlsx( xlsxFile = paste0( dir_wd$fe[[ 1 ]]
                                                                  , dir( dir_wd$fe[[ 1 ]] )[
                                                                    max(grep("Q-xx-LIS-00033", dir( dir_wd$fe[[ 1 ]] )))
                                                                    ]
                                                                  )
                                               , sheet = "DTProductNumber")
  }

  return( dt_DTProductNumber)
}

customer.location.by.line <- function(line, customer.list){

  rowp <- which( customer.list$line == line )
  customer.list.sub <- customer.list[ rowp ,]

  customer.list.sub <- list(customer = customer.list.sub$customer
                            , LG = customer.list.sub$LG
                            , location = customer.list.sub$location
                            , line = customer.list.sub$line)
  return(customer.list.sub)
}

customer.location.line.productID <- function(customer, location, line, product_ID = dt_Mixernummer){
  product_ID <- data.frame(product_ID)
  product_ID <- product_ID[product_ID$customer == customer,]
  product_ID <- product_ID[product_ID$location == location,]
  if( length( unique( product_ID$line)) > 1)   product_ID <- product_ID[product_ID$line == line,]

  if( nrow( product_ID ) == 0) return( "No products defined yet")
  if( line == "BLT31_32"){
    message("Alle Getränke in ", location, ", Linie ", line )
    return(product_ID$beverage)
  }

  product_ID <- product_ID[ ,c("beverage", "Mixer_ID_1", "Mixer_ID_2", "Mixer_ID_3", "DTProductNumber")]

  product_ID <- product_ID[ , c(apply(product_ID, 2, function( x ) !all( is.na( x ))))]
  product_ID <- product_ID[ , c(apply(product_ID, 2, function( x ) !all( x == 0)))]

  message("Liste mit Mixernummern und DT-Produktnummern in ", location, ", Linie ", line )
  return(product_ID)
}

customer.location.line.products <- function(customer, location, line, firstday, lastday, product_ID = dt_Mixernummer, dir_wd = wd){

  suppressMessages(product_ID_sub <- customer.location.line.productID(customer, location, line, product_ID))
  if( length( product_ID_sub ) == 1 ) return( "No products defined yet")

  wd.o <- getwd()
  setwd(service_backup_path(customer, location, line, dir_wd = dir_wd))
  line.product.date <- do.call(plyr::rbind.fill, lapply(dir(pattern =  paste0(line,".csv")), read.csv2))
  names( line.product.date )[ 1 ] <- "date"
  line.product.date <- line.product.date[which(nchar(as.character(line.product.date$date)) == 10),]
  line.product.date <- line.product.date[which(line.product.date$date >= firstday & line.product.date$date <= lastday) ,]
  line.product.date <- line.product.date[ !duplicated( line.product.date$MixerNumber ) , ]

  message("Folgende Produkte wurden in ", location, ", Linie ", line, " im ausgewählten Zeitraum produziert:")
  if( line != "BLT31_32" ) message(paste0("Mixernummer"))
  if( line != "BLT31_32" ) message( capture.output( suppressWarnings( sort( as.numeric( line.product.date$MixerNumber))), type = "message"), collapse = "\n")

  if( line != "BLT31_32" ) message(paste0("DTProductNumber"))
  if( line != "BLT31_32" ) message(paste0(capture.output( sort( product_ID_sub$DTProductNumber[ product_ID_sub$Mixer_ID_1 %in% line.product.date$MixerNumber ] ), type = "message"), collapse = "\n"))

  message(paste0("Produktname"))
  #if( line == "BLT31_32" ) return( data.frame( Getränk = sort( line.product.date[ !duplicated( line.product.date$MixerNumber), "MixerNumber"] )))
  if( line == "BLT31_32" ) message(paste0(capture.output( sort( line.product.date[ !duplicated( line.product.date$MixerNumber), "MixerNumber"] ), type = "message"), collapse = "\n"))
  if( line != "BLT31_32" ) message(paste0(capture.output( product_ID_sub$beverage[ product_ID_sub$Mixer_ID_1 %in% line.product.date$MixerNumber ], type = "message"), collapse = "\n"))

}
