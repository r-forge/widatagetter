bomDailyObs <-
function(siteNumber,observation="temp") {
   if(class(observation)!="character") stop("observation must be; temp, rain or solar.")
   observation <- tolower(observation)
  if(is.na(match(observation,c("rain","temp","solar")))) stop("observation must be; temp, rain or solar.")
  # Some BOM sites start with a 0 (e.g 068166). If the R siteNumber is in numeric format, the 0 will be lost, so the for loop checks this and acts accordingly.
  #136 = rain, 122 = temp, 193 = solar
   if(is.numeric(siteNumber)){
    theurl <- paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=DATATYPE&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=0",siteNumber,sep="")
  } else {
    theurl <- paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=DATATYPE&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=",siteNumber,sep="")
  }
   if(observation=="rain") {
    theurl <- gsub("DATATYPE", "136",theurl)
    dataCode <- 136
   }
   if(observation=="temp") {
     theurl <- gsub("DATATYPE", "122",theurl)
     dataCode <- 122
   }
   if(observation=="solar") {
     theurl <- gsub("DATATYPE", "193",theurl)
     dataCode <- 193
   }
  # make the orginal request to the server - this is needed to get a unique number from the server.
  raw <- getURLContent(theurl)
  if(grepl("Unfortunately there are no data available",raw[[1]])){
    stop("Unfortunately there are no data available for the site number you have entered.
This may be because either the station number is invalid, or the station has
not observed the type of data requested.")
  }
  # Check to see if there is data avaliable for this site
 # if(!grepl("1 year of data</a></li><li><a",raw[[1]])) stop("no daily data at this site")
  # The next few lines pull out the unique id
  split1 <- strsplit(raw[[1]], "1 year of data</a></li><li><a")
  split2 <- strsplit(split1[[1]][[2]], "title=\"Data file for daily rainfall data for all years")
  split3 <- strsplit(split2[[1]][[1]], "p_c=")
  split4 <- strsplit(split3[[1]][[2]], "&amp;p_ncc")
  uniqueID <- split4[[1]][[1]]
  

  # Put the new url together
  theurl <-  paste("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=0",siteNumber,"&p_c=",uniqueID,"&p_nccObsCode=",dataCode,"&p_startYear=",format(Sys.time(),"%Y"),sep="")
  # get R's current temporary directory for this session.
  tmpdir <- tempdir()
  # Set up the file name.
  file <- basename(theurl)
  # download the file, and save it in the temp. dir.
  download.file(theurl, paste(tmpdir,"/tmp.zip",sep=""))
  # Unzip the file
  unzip(paste(tmpdir,"/tmp.zip",sep=""), exdir = tmpdir )
  # Get the file name for the actual data.
  fileName <- list.files(paste(tmpdir,"/web/htdocs/tmp/cdio",sep=""),pattern="_Data.csv")
  # read in the data file
  dat <- read.csv(paste(tmpdir,"/web/htdocs/tmp/cdio/",fileName,sep=""),as.is=TRUE)
  # Clean up downloaded files
  unlink(paste(tmpdir,"/web",sep=""),recursive=T)
  unlink(paste(tmpdir,"/tmp.zip",sep=""),recursive=T)
  # Change the names
  if(observation=="rain"){
  names(dat) <- c("productCode","stationNumber","year","month","day","rainfall","daysRainMeasuredOver","quality")
  }
  if(observation=="temp"){
   names(dat) <- c("productCode","stationNumber","year","month","day","maxTemp","daysTempMeasuredOver","quality")
  }
  if(observation=="solar"){
    names(dat) <- c("productCode","stationNumber","year","month","day","dailyExposure")
  }
  # Make a date column - using lubridate, it's faster then the stock date functions.
  dat$date <- dmy(paste(dat$day,dat$month,dat$year),quiet = TRUE)
  return(dat)
}
