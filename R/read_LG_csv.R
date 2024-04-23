read.csv.LG <- function(firstday
                        , lastday
                        , customer = NA
                        , location = NA
                        , line

                        , dt_customer = dt_customer
                        , dt_Mixernummer = dt_Mixernummer

                        , DTProductNumber = NA
                        , MixerNumber = NA

                        , typeof = c("drk","ref","spc")

                        , typecode = NA

                        , Produkt_fliesst
                        , Ringkessel = Produkt_fliesst

                        , export_directory = "C://csvtemp"
                        , slim = T
                        , return.R = F
                        , export = T
                        , dir_wd = wd){

  if( all( !is.na( MixerNumber) ) ) DTProductNumber <- NA

  if(!is.na(export_directory)) dir.create(export_directory,showWarnings = F)

  rowp <- which( dt_customer$customer == customer & dt_customer$location == location & dt_customer$line == line )
  LG <- as.character( dt_customer[ rowp , "LG"])

  if(LG == "LG 3" | LG == "3" | LG == "SG3"| LG == "LG3")
    dat <- read.csv.LG3(firstday = firstday
                        , lastday = lastday
                        , customer = customer
                        , location = location
                        , line = line

                        , dt_Mixernummer = dt_Mixernummer

                        , MixerNumber = MixerNumber
                        , DTProductNumber = DTProductNumber

                        , typeof = typeof

                        , typecode = typecode

                        , export_directory = export_directory
                        , slim = slim
                        , return.R = return.R
                        , export = export
                        , dir_wd = dir_wd)

  if(LG == "LG 2" | LG == "2"| LG == "LG2")
    dat <- read.csv.LG2(firstday = firstday
                        , lastday = lastday
                        , customer = customer
                        , location = location
                        , line = line

                        , dt_Mixernummer = dt_Mixernummer

                        , MixerNumber = MixerNumber
                        , DTProductNumber = DTProductNumber

                        , Produkt_fliesst = Produkt_fliesst
                        , Ringkessel = Ringkessel

                        , typeof = typeof

                        , export_directory = export_directory
                        , slim = slim
                        , return.R = return.R
                        , export = export
                        , dir_wd = dir_wd)

  if(LG == "LG 1" | LG == "1"| LG == "LG1")
    dat <- read.csv.LG2(firstday = firstday
                        , lastday = lastday
                        , customer = customer
                        , location = location
                        , line = line
                        , product = MixerNumber
                        , Ringkessel = Ringkessel
                        , typeof = typeof
                        , slim = slim
                        , return.R = return.R
                        , product_ID = dt_Mixernummer
                        , export_directory = export_directory
                        , export = export
                        , dir_wd = dir_wd)

  if(LG == "SG")
    dat <- read.csv.SG(firstday = firstday
                       , lastday = lastday
                       , customer = customer
                       , location = location
                       , line = line
                       , MixerNumber = MixerNumber
                       , typeof = typeof
                       , export_directory = export_directory
                       , fastplot = F
                       , export = export
                       , dir_wd = dir_wd)

  if(return.R) return(dat)
}
