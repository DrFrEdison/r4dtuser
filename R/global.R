dauschblue <- "#2f407a"

.onLoad <- function(libname, pkgname) {
  package_version <- packageDescription(pkgname)$Version
  version_name <- packageDescription(pkgname)$VersionName

  message(paste(pkgname, "version", package_version, "--", version_name))
  message("Check the GitHub repository at: https://github.com/DrFrEdison/r4dt")

  # Load any other necessary initialization code here

  # Make sure to return(TRUE) at the end
  return(TRUE)
}


date.dt <- function()  substr(gsub("-","",Sys.Date()),3,8)

# moveme function ####
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]],
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first",
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

freadr4dt <- function( csvfile){
  
  csvfile <- gsub(".csv.csv", ".csv", paste0(csvfile, ".csv"))
  
  csvfile <- fread(csvfile, sep = ";", dec = ",")
  
  ppp <- transfer_csv.num.col(csvfile)
  
  csvfile.names <- colnames(csvfile)
  
  csvfile.names[ ppp$numcol ] <- paste0( "X", csvfile.names[ ppp$numcol ])
  csvfile.names[ ppp$numcol ] <- gsub("XX", "X", csvfile.names[ ppp$numcol ])
  csvfile.names[ ppp$numcol ] <- gsub("XX", "X", csvfile.names[ ppp$numcol ])
  
  setnames(csvfile, new = csvfile.names)
  
  return(csvfile)
}

opendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    # if( length( grep("DC-01", dir) > 0 )) dir <- paste0(Sys.getenv("OneDriveCommercial"), substr(dir, gregexpr( "/", dir)[[ 1 ]][[ 3 ]], nchar(dir)))
    shell.exec(gsub("\\/", "\\\\", dir))
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

colp <- (c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
colp <- c(colp, "springgreen", "bisque3", "violetred1")

writeClipboardr4dt <- function(x){
  return( writeClipboard( as.character( gsub("\\.", "\\,", x) )))}

prediction_parameter <- function(dat = dt$raw){
  if("spc" %in% names(dat)){

    # Get the parameter names
    param_names <- paste0(substr(grep("ypred", colnames(dt$raw$spc), value = TRUE), 1,
                                 unlist(lapply(gregexpr(" in ", grep("ypred", colnames(dt$raw$spc), value = TRUE)),
                                               function(x) x[[1]] - 1))),
                          gsub("yPredictedCorr", "", grep("yPredictedCorr", colnames(dt$raw$spc), value = TRUE)))

    # Concatenate the strings for each parameter
    output <- paste0("Folgende Parameter innerhalb des Datensatzes können ausgewählt werden:\n",
                     paste("- ", param_names, collapse = "\n"))

    # Print the output
    cat(output, "\n")


  }
}
