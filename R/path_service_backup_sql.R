service_backup_path <- function(customer, location, line, dir_wd){
  service_backup_path <- list()

  service_backup_path$a <- which(names(dir_wd$servicebackup)==customer)
  if(length(service_backup_path$a)==0) stop("Wrong customer chosen")

  service_backup_path$b <- which(names(dir_wd$servicebackup[[service_backup_path$a]])==location)
  if(length(service_backup_path$b)==0) stop("Wrong location chosen")

  service_backup_path$c <- which(names(dir_wd$servicebackup[[service_backup_path$a]][[service_backup_path$b]])==line)
  if(length(service_backup_path$c)==0) stop("Wrong line chosen")

  return(dir_wd$servicebackup[[service_backup_path$a]][[service_backup_path$b]][[service_backup_path$c]])
}

service_path <- function(customer, location, line, dir_wd){
  service_path <- list()

  service_path$a <- which(names(dir_wd$service)==customer)
  if(length(service_path$a)==0) stop("Wrong customer chosen")

  service_path$b <- which(names(dir_wd$service[[service_path$a]])==location)
  if(length(service_path$b)==0) stop("Wrong location chosen")

  service_path$c <- which(names(dir_wd$service[[service_path$a]][[service_path$b]])==line)
  if(length(service_path$c)==0) stop("Wrong line chosen")

  return(dir_wd$service[[service_path$a]][[service_path$b]][[service_path$c]])
}

sql_path <- function(customer, location, unit, dir_wd){
  sql_path <- list()

  sql_path$a <- which(names(dir_wd$sql)==customer)
  if(length(sql_path$a)==0) stop("Wrong customer chosen")

  sql_path$b <- which(names(dir_wd$sql[[sql_path$a]])==location)
  if(length(sql_path$b)==0) stop("Wrong location chosen")

  sql_path$c <- which(names(dir_wd$sql[[sql_path$a]][[sql_path$b]])==unit)
  if(length(sql_path$c)==0) stop("Wrong unit chosen")

  return(dir_wd$sql[[sql_path$a]][[sql_path$b]][[sql_path$c]])
}

lastfile_serverbackup <- function(customer, dir_wd){

  dirlist <- list()
  dirlist$customer <- names(dir_wd$servicebackup)[-c(1:2)]

  if(customer == "CCEP"){
    dirlist$dirs$CCEP <- list.dirs(dir_wd$servicebackup$CCEP[[1]], recursive = T)
    dirlist$files$CCEP <- lapply(dirlist$dirs$CCEP[grep("spc", dirlist$dirs$CCEP)], function(x) list.files(x, pattern = ".csv$"))
    dirlist$max$CCEP <- lapply(dirlist$files$CCEP, function(x) x[which.max(as.Date(substr(x, 1, 10)))])
  }

  if(customer == "MEG"){
    dirlist$dirs$MEG <- list.dirs(dir_wd$servicebackup$MEG[[1]], recursive = T)
    dirlist$files$MEG <- lapply(dirlist$dirs$MEG[grep("spc", dirlist$dirs$MEG)], function(x) list.files(x, pattern = ".csv$"))
    dirlist$max$MEG <- lapply(dirlist$files$MEG, function(x) x[which.max(as.Date(substr(x, 1, 10)))])
  }

  if(customer == "Pepsi"){
    dirlist$dirs$Pepsi <- list.dirs(dir_wd$servicebackup$Pepsi[[1]], recursive = T)
    dirlist$files$Pepsi <- lapply(dirlist$dirs$Pepsi[grep("spc", dirlist$dirs$Pepsi)], function(x) list.files(x, pattern = ".csv$"))
    dirlist$max$Pepsi <- lapply(dirlist$files$Pepsi, function(x) x[which.max(as.Date(substr(x, 1, 10)))])
  }

  return(dirlist$max[grep(customer, names(dirlist$max))])
}

