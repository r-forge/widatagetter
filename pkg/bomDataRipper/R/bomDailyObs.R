bomDailyObs <- function (siteNumber, observation = "temp",...) 
{
  if (class(observation) != "character") 
    stop("observation must be; temp, rain or solar.")
  observation <- tolower(observation)
  if (is.na(match(observation, c("rain", "min_temp","max_temp", "solar")))) 
    stop("observation must be; min_temp, max_temp, rain or solar.")
  if (is.numeric(siteNumber)) {
    theurl <- paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=DATATYPE&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=0", 
                    siteNumber, sep = "")
  }  else {
    theurl <- paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=DATATYPE&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=", 
                    siteNumber, sep = "")
  }
  if (observation == "rain") {
    theurl <- gsub("DATATYPE", "136", theurl)
    dataCode <- 136
  }
  if (observation == "max_temp") {
    theurl <- gsub("DATATYPE", "122", theurl)
    dataCode <- 122
  }
  if (observation == "min_temp") {
    theurl <- gsub("DATATYPE", "123", theurl)
    dataCode <- 123
  }
  if (observation == "solar") {
    theurl <- gsub("DATATYPE", "193", theurl)
    dataCode <- 193
  }
  raw <- getURLContent(theurl,...)
  if (grepl("Unfortunately there are no data available", raw[[1]])) {
    stop("Unfortunately there are no data available for the site number you have entered.\nThis may be because either the station number is invalid, or the station has\nnot observed the type of data requested.")
  }
  
  split1 <- strsplit(raw[[1]], "1 year of data</a></li><li><a")
  split2 <- strsplit(split1[[1]][[2]], "title=\"Data file for daily rainfall data for all years")
  split3 <- strsplit(split2[[1]][[1]], "p_c=")
  split4 <- strsplit(split3[[1]][[2]], "&amp;p_ncc")
  uniqueID <- split4[[1]][[1]]
  start_year <- substr(strsplit(raw[[1]], "startYear=")[[1]][2],1,4)
  theurl <- paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=0", 
                  siteNumber, "&p_c=", uniqueID, "&p_nccObsCode=", dataCode, 
                  "&p_startYear=", start_year, sep = "")
  tmpdir <- tempdir()
  file <- basename(theurl)
  download.file(theurl, paste(tmpdir, "/tmp.zip", sep = ""))
  unzip(paste(tmpdir, "/tmp.zip", sep = ""), exdir = tmpdir)
  fileName <- list.files(paste(tmpdir, "/web/htdocs/tmp/cdio", 
                               sep = ""), pattern = "_Data.csv")
  dat <- read.csv(paste(tmpdir, "/web/htdocs/tmp/cdio/", fileName, 
                        sep = ""), as.is = TRUE)
  unlink(paste(tmpdir, "/web", sep = ""), recursive = T)
  unlink(paste(tmpdir, "/tmp.zip", sep = ""), recursive = T)
  if (observation == "rain") {
    names(dat) <- c("productCode", "stationNumber", "year", 
                    "month", "day", "rainfall", "daysRainMeasuredOver", 
                    "quality")
  }
  if (observation == "max_temp") {
    names(dat) <- c("productCode", "stationNumber", "year", 
                    "month", "day", "maxTemp", "daysTempMeasuredOver", 
                    "quality")
  }
  if (observation == "min_temp") {
    names(dat) <- c("productCode", "stationNumber", "year", 
                    "month", "day", "minTemp", "daysTempMeasuredOver", 
                    "quality")
  }
  if (observation == "solar") {
    names(dat) <- c("productCode", "stationNumber", "year", 
                    "month", "day", "dailyExposure")
  }
  dat$date <- dmy(paste(dat$day, dat$month, dat$year), quiet = TRUE)
  return(dat)
}