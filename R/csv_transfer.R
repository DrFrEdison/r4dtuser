transfer_csv.num.col <- function(csv.file){
  numcol <- suppressWarnings(as.numeric(gsub("X", "",  colnames(csv.file))))
  wavelength <-  numcol[suppressWarnings(which( !is.na(numcol) & numcol > 100))]
  numcol <- suppressWarnings(which( !is.na(numcol) & numcol > 100))
  returnlist <- list(numcol, wavelength)
  names(returnlist) <- c("numcol", "wl")
  return(returnlist)
}

#' Transfer CSV Data
#'
#' This function processes a CSV file and applies certain transformations
#' based on the parameters provided. It can handle first and second derivative
#' calculations among other operations.
#'
#' @param csv.file Path to the CSV file to be processed.
#' @param p Polynomial order for derivative calculations.
#' @param n1 Window size for the first derivative.
#' @param n2 Window size for the second derivative.
#' @param derivative Types of derivatives to calculate ('1st', '2nd').
#' @param tz Timezone setting for any time data within the CSV.
#' @param data.table.set Boolean to determine if the output should be a data.table object.
#'
#' @return A list containing the processed data and possibly derivatives, structured as specified.
#' @examples
#' transfer_csv("path/to/data.csv", p = 2, n1 = 7, n2 = 11, derivative = c("1st", "2nd"), tz = "UTC", data.table.set = TRUE)
#' @export
transfer_csv <- function(csv.file # input csv file
                         , p = 2 # polynomial order for derivative
                         , n1 = 7 # window size for 1st derivative
                         , n2 = 11 # window size for 2nd derivative
                         , derivative = c("1st", "2nd") # calculate derivatives or NA
                         , tz = "UTC"
                         , data.table.set = T){

  library(prospectr)
  library(data.table)
  if(!is.data.table(csv.file)) data.table.set = F

  # extract spectra columns by searching for numeric column names and number > 100
  numcol <- suppressWarnings(as.numeric(gsub("X", "",  colnames(csv.file))))
  if(!data.table.set) csv.file.spc.spc <- csv.file[ , suppressWarnings(which( !is.na(numcol) & numcol > 100))]
  if(data.table.set) csv.file.spc.spc <- csv.file[ , suppressWarnings(which( !is.na(numcol) & numcol > 100)), with = F]

  # Extract wavelengths ####
  wavelength <-  numcol[suppressWarnings(which( !is.na(numcol) & numcol > 100))]

  # homogenize column names ####
  colnames(csv.file.spc.spc) <- paste0("X", wavelength)

  # extract data columns by excluding numeric column names or number < 100 ####
  if(!data.table.set) csv.file.data <- csv.file[ , suppressWarnings(which( is.na(numcol) | numcol < 100))]
  if(data.table.set) csv.file.data <- csv.file[ , suppressWarnings(which( is.na(numcol) | numcol < 100)), with = F]

  # change date & time format ####
  if(any(grepl( "datetime" , names(csv.file.data), fixed = TRUE))) if(!is.na(unlist(gregexpr("T", substr(csv.file.data$datetime[1], 1, 19))))) if(unlist(gregexpr("T", substr(csv.file.data$datetime[1], 1, 19)))>0) csv.file.data$datetime <- as.POSIXct(strptime(csv.file.data$datetime, "%Y-%m-%dT%H:%M:%S") )
  if(any(grepl( "datetime" , names(csv.file.data), fixed = TRUE))) csv.file.data$datetime <- as.POSIXct(csv.file.data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  if(any(grepl( "date" , names(csv.file.data), fixed = TRUE))) csv.file.data$date <- as.POSIXct(csv.file.data$date, format = "%Y-%m-%d", tz = tz)
  if(any(grepl( "datetime" , names(csv.file.data), fixed = TRUE))){

    csv.file.data$time <- strftime(csv.file.data$datetime, format="%H:%M:%S")
  }

  colnames( csv.file.spc.spc ) <- paste0("X", wavelength)

  # Order wavelength ####
  if(data.table.set) csv.file.spc.spc <- csv.file.spc.spc[ , order(wavelength), with = F]
  if(!data.table.set) csv.file.spc.spc <- csv.file.spc.spc[ , order(wavelength)]
  wavelength <- sort( wavelength )

  csv.file.spc.spc <- data.table(csv.file.spc.spc)
  # first derivative ####
  if("1st" %in% derivative){

    csv.file.spc.1st <- copy(csv.file.spc.spc)

    # make columns numeric
    if(!any(apply(csv.file.spc.1st, 2, is.numeric)))  csv.file.spc.1st <- apply(csv.file.spc.1st , 2, function(x) as.numeric(as.character(x)))

    csv.file.spc.1st <- apply(t(csv.file.spc.1st),2,
                              function(x) savitzkyGolay(X = x, m = 1, p = p, w = n1))

    csv.file.spc.1st <- rbind(xmatrix <- matrix(data = 0, nrow = (n1 - 1) / 2, ncol = ncol(csv.file.spc.1st))
                              , csv.file.spc.1st
                              , xmatrix)

    if(ncol(csv.file.spc.1st) != length( wavelength)) csv.file.spc.1st <- t(csv.file.spc.1st)
    colnames(csv.file.spc.1st) <- colnames(csv.file.spc.spc)

    csv.file.spc.1st <- data.table(csv.file.spc.1st)
  }

  #2nd derivative ####
  if("2nd" %in% derivative){

    csv.file.spc.2nd <- copy(csv.file.spc.spc)
    # make columns numeric
    if(!any(apply(csv.file.spc.2nd, 2, is.numeric)))  csv.file.spc.2nd <- apply(csv.file.spc.2nd , 2, function(x) as.numeric(as.character(x)))

    csv.file.spc.2nd <- apply(t(csv.file.spc.2nd),2,
                              function(x) savitzkyGolay(X = x, m = 2, p = p, w = n2))

    csv.file.spc.2nd <- rbind(xmatrix <- matrix(data = 0, nrow = (n2 - 1) / 2, ncol = ncol(csv.file.spc.2nd))
                              , csv.file.spc.2nd
                              , xmatrix)

    if(ncol(csv.file.spc.2nd) != length( wavelength)) csv.file.spc.2nd <- t(csv.file.spc.2nd)

    colnames(csv.file.spc.2nd) <- colnames(csv.file.spc.spc)

    csv.file.spc.2nd <- data.table(csv.file.spc.2nd)

  }

  if(!is.data.table( csv.file.spc.spc )){
    # data.frame
    if("1st" %in% derivative) csv.file.spc.1st <- data.frame(csv.file.spc.1st)
    if("2nd" %in% derivative) csv.file.spc.2nd <- data.frame(csv.file.spc.2nd)

    # transpose
    if("1st" %in% derivative) csv.file.spc.1st <- t(csv.file.spc.1st)
    if("2nd" %in% derivative) csv.file.spc.2nd <- t(csv.file.spc.2nd)
  }

  # make columns numeric ####
  if(nrow( csv.file.data ) > 1)
    if(is.data.frame(csv.file.data)){
      nums <- suppressWarnings(as.numeric(which(lapply(apply(apply(csv.file.data, 2, as.numeric), 2, function(x) unique(!is.na(x))), any) == T)))
      if( length ( nums ) != 0){
        if(!data.table.set) if(length(nums)>1) csv.file.data[,nums] <- apply(csv.file.data[,nums],2,function(x) as.numeric(as.character(gsub(",",".",x))))
        if(!data.table.set) if(length(nums)==1) csv.file.data[,nums] <- as.numeric(as.character(gsub(",",".",csv.file.data[,nums])))
        if(data.table.set){
          csv.file.data <- data.table(csv.file.data)
          csv.file.data[, (names(csv.file.data)[nums]) := lapply(.SD, function(x) as.numeric(as.character(gsub(",", ".", x)))), .SDcols = nums]
        }
      }

    }

  # name list ####
  if("1st" %in% derivative & "2nd" %in% derivative)  csv.filelist <- list(csv.file.data, csv.file.spc.spc, csv.file.spc.1st, csv.file.spc.2nd, wavelength)
  if("1st" %in% derivative & !"2nd" %in% derivative) csv.filelist <- list(csv.file.data, csv.file.spc.spc, csv.file.spc.1st, wavelength)
  if(!"1st" %in% derivative & "2nd" %in% derivative) csv.filelist <- list(csv.file.data, csv.file.spc.spc, csv.file.spc.1st, csv.file.spc.2nd, wavelength)
  if( all( is.na(derivative)) ) csv.filelist <- list(csv.file.data, csv.file.spc.spc, wavelength)

  if("1st" %in% derivative & "2nd" %in% derivative)  names( csv.filelist ) <- c("data", "spc", "spc1st", "spc2nd", "wl")
  if("1st" %in% derivative & !"2nd" %in% derivative) names( csv.filelist ) <- c("data", "spc", "spc1st", "wl")
  if(!"1st" %in% derivative & "2nd" %in% derivative) names( csv.filelist ) <- c("data", "spc", "spc2nd", "wl")
  if( all( is.na(derivative)) ) names( csv.filelist ) <- c("data", "spc", "wl")

  return(csv.filelist)
}


transfer_csv_status <- function(csv_status_file){
  names(csv_status_file) <- c("datetime", "Pressure", "Flow", "TempFluid", "TempSPC", "TempRack", "TempAmbient")
  csv_status_file$datetime <- as.POSIXct(csv_status_file$datetime)
  csv_status_file$time <- strftime(csv_status_file$datetime, format = "%H:%M:%S")
  csv_status_file$date <- as.Date(csv_status_file$datetime)

  csv_status_file$Pressure <- as.numeric(gsub(",", ".", csv_status_file$Pressure))
  csv_status_file$Flow <- as.numeric(gsub(",", ".", csv_status_file$Flow))
  csv_status_file$TempFluid <- as.numeric(gsub(",", ".", csv_status_file$TempFluid))
  csv_status_file$TempSPC <- as.numeric(gsub(",", ".", csv_status_file$TempSPC))
  csv_status_file$TempRack <- as.numeric(gsub(",", ".", csv_status_file$TempRack))
  csv_status_file$TempAmbient <- as.numeric(gsub(",", ".", csv_status_file$TempAmbient))

  csv_status_file <- csv_status_file[ , moveme(names(csv_status_file), "datetime date time first")]
  csv_status_file <- csv_status_file[order(csv_status_file$datetime) , ]

  return(csv_status_file)
}
